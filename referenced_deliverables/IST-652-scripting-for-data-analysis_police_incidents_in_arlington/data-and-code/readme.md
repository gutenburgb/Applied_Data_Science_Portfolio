# Installing Conda Environment
## Preferred (cross platform) way
```
conda env create -f environment_from_history.yml
```

## Alternative environment: exact packages used
```
conda env create -f environment.yml
```

## Installing from scratch
```
conda create -n police_incidents python=3.8 pandas geopandas descartes gensim pyemd matplotlib
conda activate police_incidents
conda install -c conda-forge geoplot
conda install -c conda-forge wordcloud
conda install -c conda-forge pendulum
```

# Running the code
```
# must execute __main__.py from inside its directory
cd data-and-code
conda activate police_incidents
python.exe __main__.py
conda deactivate
```