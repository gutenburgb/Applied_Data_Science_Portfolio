"""
Leland Ball
IST 652 Final Project
Arlington County, VA Police Incidents

Requires conda environment with the following installed:
conda install pandas geopandas descartes gensim pyemd matplotlib pendulum

conda install -c conda-forge geoplot
conda install -c conda-forge wordcloud
conda install -c conda-forge pendulum
"""

import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
from pathlib import Path
import geoplot as gplt
import geoplot.crs as gcrs
import pendulum
from geopandas.tools import sjoin
from wordcloud import WordCloud, STOPWORDS
import re
from collections import OrderedDict
from gensim.models import Word2Vec
from gensim.test.utils import get_tmpfile
from gensim.models import KeyedVectors
from gensim.parsing.preprocessing import remove_stopwords

ideal_category_size = 60 # bin everything into X bins
similarity_thresh = 0.03 # LOWER MEANS STRICTER # the model similarity threshold below which we throw the police incident into the "other" category
force_recalc_word_model = False

path_base = Path.cwd()
path_data_csv = path_base / 'data' / 'PoliceIncidentLog.csv'
categories_model_path = path_base / 'data' / 'categorical_word_model.kv'
cat_lookup = {}  # category time/memory tradeoff dict
manual_cat_combos = OrderedDict({
    'PETIT,LARCENY': 'PETIT LARCENY',
    'GRAND,LARCENY': 'GRAND LARCENY',
    'LARCENY': 'LARCENY',
    'LARC': 'LARCENY',
    'THEFT': 'LARCENY',
    'MARIJUANA': 'MARIJUANA',
    'DUI': 'DUI',
    'DRIVING,UNDER,INFLUENCE': 'DUI'
})

zip_offset = 0
zip_colors = ['red', 'blue', 'green', 'yellow', 'pink', 'orange', 'purple', 'black']
zip_chosen = {}

zips_shape_path = path_base / 'gis' / 'ZipCode_Polygons-shp' / 'ZipCode_Polygons.shp'

# dates to truncate police incidents to
start_date = pendulum.datetime(2019, 1, 1, tz='EST')
end_date = pendulum.datetime(2019, 12, 31, tz='EST')


def zip_color_scheme(zip_str, bins, *kw, **kwargs):
    global zip_offset
    if zip_str not in zip_chosen:
        zip_chosen[zip_str] = zip_colors[zip_offset % len(zip_colors)]
        zip_offset += 1
    return zip_chosen[zip_str]

def read_geojson_version(shapefile_path):
    # check to make sure any shapes have a corresponding geojson file, (geojson flips lat/lon)
    orig_path_zips = Path(shapefile_path)
    json_path_zips = Path(orig_path_zips.with_suffix('.json'))
    if not json_path_zips.exists():
        print(f'Converting shape-file {orig_path_zips.name}')
        file = gpd.read_file(zips_shape_path)
        file.to_file(json_path_zips, driver="GeoJSON")
    return gpd.read_file(json_path_zips)

def cull_categories(cat_lists):
    # returns a shorter list of categories after renaming and combining categories into one or more
    # specified in the manual_cat_combos dict
    # cat_lists must be list of lists (a list of lists of terms separated by space (.split()))
    global manual_cat_combos  # holds a mapping from many categories into one or two
    new_cat_set = set()
    for cat_list in cat_lists:
        match_found = False
        for k, v in manual_cat_combos.items():
            split_k = k.split(',')
            if all([x in cat_list for x in split_k]):
                new_cat_set.add(v) # match! We can combine this into the category known as 'v'
                match_found = True
                break
        if not match_found:
            new_cat_set.add(' '.join(cat_list))  # unique category we want to keep. but store it as a str
    return [x.split() for x in new_cat_set]  # convert to list and return


def categorize(word_vectors, chosen_categories, this_one):
    # takes a model and two split lists of words, respectively. returns the category of chosen_categories that it
    # should be binned into, if it's under the threshold, then it returns OTHER
    # chosen_categories is a pre-split list-of-lists
    global cat_lookup  # dict of this_one -> chosen_category map. If it exists here, we don't need to do calculations
    this_one_str = ' '.join(this_one)
    lookup = cat_lookup.get(this_one_str, None)
    if lookup:
        return lookup
    else:  # gotta calculate
        best_score = 1
        chosen = ['OTHER']
        for category in chosen_categories:
            similarity = word_vectors.wmdistance(category, this_one)
            if similarity < best_score:
                chosen = category
                best_score = similarity
        if best_score > similarity_thresh:
            chosen = ['OTHER']
        chosen_cat_str = ' '.join(chosen)
        cat_lookup[this_one_str] = chosen_cat_str  # store for faster lookup, later
        return chosen_cat_str # combine it all into one space-separated string.

if __name__ == '__main__':
    df = pd.read_csv(path_data_csv, dtype=str)
    for col in df.columns:
        df[col] = df[col].astype(str)

    # get top X most common incidents, by name (number of bins)
    df['offenseDsc_mod'] = df['offenseDsc'].apply( lambda x: re.sub('[^A-Z0-9 ]', ' ', str(remove_stopwords(str(x.upper()))) ) ) # replaces non-alphanum with space
    val_counts = df['offenseDsc_mod'].value_counts()
    top_x = val_counts.nlargest(ideal_category_size)
    all_categories_split = [list(c.split()) for c in list(df['offenseDsc_mod'].value_counts().index)] # used for training
    categories_split = [list(c.split()) for c in list(df['offenseDsc_mod'].value_counts().nlargest(ideal_category_size).index)] # pre-split these for later

    print(f"Out of {len(df)} records, there are {len(val_counts)} unique categories of police incidents.")
    print(f"These {len(top_x)} categories account for {round((float(top_x.values.sum())/len(df))*100, 2)}% of all incidents.")
    print("Attempting to combine similar categories of data...")

    cat_count_before_heuristics = len(categories_split)
    categories_split = cull_categories(categories_split)
    cat_pct_red = round((1-(len(categories_split)/float(cat_count_before_heuristics)))*100,1)
    print(f"Applying heuristic category reduction results in a {cat_pct_red}% reduction ({len(categories_split)} categories now)")

    # determine if we must recalculate word vector model (for reducing number of categories) or if not
    categories_model = Path(categories_model_path)
    if force_recalc_word_model or not categories_model.exists():
        # recalc model
        print('No categorical word vectors found. Computing model...')
        model = Word2Vec(all_categories_split, size=150, window=5, min_count=1, workers=4)
        word_vectors = model.wv
        word_vectors.save(categories_model_path.resolve())
    else: # load model from disk
        print('Saved categorical word vectors found. Loading...')
        name = get_tmpfile(categories_model_path.resolve())
        word_vectors = KeyedVectors.load(name) # , nmap='r'  ???

    print('Vectors loaded. Re-categorizing data...')

    # compute categorical similarities for each item in the dataframe
    # each sentence is a list that is .upper() and .split()
    # df['offenseDsc_mod'].to_string(index=False)
    df['cat_calc'] = df['offenseDsc_mod'].apply(lambda x: categorize(word_vectors, categories_split, x.split()))

    new_cat_counts = df['cat_calc'].value_counts()


    # convert lat/lon to "real number" not str
    df.latitudeCrd = pd.to_numeric(df.latitudeCrd)
    df.longitudeCrd = pd.to_numeric(df.longitudeCrd)
    # cast to date and put in EST
    df.firstReportDtm = (pd.to_datetime(df.firstReportDtm)).dt.tz_localize('EST')
    df.lastReportDtm = (pd.to_datetime(df.lastReportDtm)).dt.tz_localize('EST')


    # df = df.set_index('firstReportDtm')  # index by time (we don't want to do this yet)
    # only 2019 data now
    old_length = len(df)
    df = df[(df['firstReportDtm'] > start_date) & (df['firstReportDtm'] < end_date)]
    new_length = len(df)
    print(f"Police incidents truncated from {old_length} to {new_length} after culling data outside of {start_date.to_date_string()} and {end_date.to_date_string()}")

    incidents_gdf = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df.longitudeCrd, df.latitudeCrd))
    gdf = incidents_gdf

    zips_df = read_geojson_version(zips_shape_path)
    zips_df.ZIP5DIG = zips_df.ZIP5DIG.apply(lambda x: str(x).zfill(5))  # 5 digit zips in string form only
    zips_df = zips_df.to_crs(epsg=4326)  # epsg=4326 is lat/lon, not weirdo super-floats

    both_df = sjoin(incidents_gdf, zips_df, how='left')

    both_df.set_index('firstReportDtm')
    ############################ plot timeplot #######################
    # plot the mean of a year's worth of police incidents, by hour, for each zip code

    # get counts of police incidents by hour, across each zip code
    year_df = both_df.groupby([pd.Grouper(key='firstReportDtm', freq='H'), both_df.ZIP5DIG])\
        .size().unstack(fill_value=0)\
        .stack().reset_index(name='count')

    # ok but what if we plotted mean counts per hour, over the whole year (for each zip code)
    #add hour column
    year_df['hour'] = year_df['firstReportDtm'].apply(lambda x: x.hour)
    hours_series = pd.Series(range(24))
    year_df = year_df.groupby(['ZIP5DIG', 'hour']).agg({'count': 'mean'})
    year_df = year_df.unstack().unstack().unstack().reset_index()
    year_df = year_df.drop(columns=['level_0']).set_index('hour')
    year_df.plot() # plot the mean of a year's worth of police incidents, by hour, for each zip code


    #############################################33
    # Where are incidents most common? Heatmap!
    ax = gplt.kdeplot(
        incidents_gdf, clip=zips_df.geometry,
        shade=True, cmap='Reds',
        projection=gplt.crs.AlbersEqualArea())
    #gplt.webmap(zips_df, ax=ax, projection=gcrs.WebMercator(), zorder=1)
    gplt.polyplot(zips_df, ax=ax, zorder=1)


    ############################################
    # Which incidents are most common?
    wordcloud = WordCloud(width=1000, height=800, background_color='black',
                          stopwords = STOPWORDS).generate(' '.join(incidents_gdf['cat_calc'].values))
    fig = plt.figure(figsize=(10,8), facecolor='k', edgecolor='k')
    plt.imshow(wordcloud, interpolation='bilinear')
    plt.axis('off')
    plt.tight_layout(pad=1)
    plt.show()

    ############################################
    # Which incidents are most common, in pie-chart form?
    df['cat_calc'].value_counts().plot.pie(legend=False, figsize=(15, 6))


    ############################################
    # Where are each incident in Arlington County, plotted as points on a web map?
    # as simple points
    ax = gplt.webmap(both_df, projection=gcrs.WebMercator())
    gplt.pointplot(both_df, ax=ax, marker=".", alpha=0.80)  # marker could be o or .
    _ = ax.axis('off')
    ax.set_title("Police incidents in Arlington County, VA")
    plt.show()

    # give a 1-to-n (0 to 1) number for each category and assign to 'arbitrary_hue' value based on 'cat_calc' value
    categories_split_str = [' '.join(c) for c in categories_split] + ['OTHER']
    both_df['arbitrary_hue'] = both_df['cat_calc'].apply(lambda c: float(categories_split_str.index(c))/(len(categories_split_str)-1) )
    ax = gplt.webmap(both_df, projection=gcrs.WebMercator())
    gplt.pointplot(both_df, ax=ax, marker=".", hue='arbitrary_hue', alpha=0.80)  # marker could be o or .
    _ = ax.axis('off')
    ax.set_title("Police incidents in Arlington County, VA")
    plt.show()
    key_df = pd.DataFrame(both_df.groupby(['arbitrary_hue', 'cat_calc']).size()).reset_index().drop(columns=0).iloc[::-1]
    print(key_df.to_string())