"""
SMS Entity Analysis
By Leland Ball, December 2020
conda install pandas spacy matplotlib seaborn nltk

"""
from pathlib import Path
import csv
import pandas as pd
import spacy
from spacy import displacy
import random
from collections import Counter

import matplotlib.pyplot as plt
import seaborn as sn

from nltk.corpus import words

# downloaded from https://spacy.io/models/en#en_core_web_md via python -m spacy download en_core_web_md
import en_core_web_md
nlp = en_core_web_md.load()

def load_data(path_input, bin_size=None, plots=False):
    # load data
    df_initial = pd.read_csv(path_input)

    # preprocess
    df_initial['date'] = pd.to_datetime(df_initial['date'], unit='ms')
    df_initial.index = pd.DatetimeIndex(df_initial['date'])
    df_initial['address'] = df_initial['address'].astype('string')
    df_initial['body'] = df_initial['body'].astype('string').fillna('')

    # bin by phone number
    df_by_number = df_initial
    df_by_number['concat'] = df_initial.groupby(['address'])['body'].transform(lambda x: '. '.join(x))
    # truncate to 1000 characters
    df_by_number['trunc'] = df_by_number['concat'].str[:1000]
    df_by_number = df_by_number.drop_duplicates(subset=['trunc']).drop(columns=['body', 'direction', 'image', 'type'])

    if plots:
        # count sms, mms
        df_initial.groupby([pd.Grouper(freq='1M'), 'type'])['type'].count().plot.bar(x='date')
        plt.show(block=True)
        plt.interactive(False)

        # broken down by sms, mms
        df_w = df_initial.reset_index(drop=True)
        pd.crosstab(df_w['date'].dt.year, df_w['type']).plot.bar()

        # graph by month
        df_initial.groupby(pd.Grouper(freq='1M'))['date'].count().plot.bar(x='date')
        plt.show(block=True)
        plt.interactive(False)

    return df_by_number


def to_csv(nlps, path_csv_out):
    # prints out columns for each token, for importing (and marking) in excel
    # label, T1, T2, T3, T4
    # Raw1, Tina, Boucher, ate, cheese
    # in1, 1,       1,      0,  0           <-- user-entered
    # Raw2, the next sentence here
    # in2, 0,0,0,0,
    path_csv_out = Path(path_csv_out)

    # open csv
    with path_csv_out.open('w', encoding='utf-8', newline='') as csv_file:
        csv_writer = csv.writer(csv_file)
        # create list of correct length for T1, T2
        longest = 0
        for i, nlp in enumerate(nlps):
            next_longest = len(nlp)
            if next_longest > longest:
                longest = next_longest
        headers = [f'label'] + [f'T-{x}' for x in range(longest)]
        # write headers the first time only
        csv_writer.writerow(headers)

        for i,nlp in enumerate(nlps):
            raw = [f'raw-{i}'] + list(nlp)
            input = [f'in-{i}'] + [0] * len(nlp) # oops. don't include the predictions: + to_list(nlp)
            # write lines
            csv_writer.writerow(raw)
            csv_writer.writerow(input)

def from_csv(path_csv_in):
    # takes the inverse of the above^ and produces a list of lists with words denoting named PERSON entities
    # returns a list of lists of predictions (0/1) in order they were read from the csv file
    path_csv_in = Path(path_csv_in)
    lists = []
    with path_csv_in.open('r', encoding='utf-8') as csv_file:
        csv_reader = csv.reader(csv_file)
        for row in csv_reader:
            if 'in-' in row[0]:
                row_str = row[1:]
                row_int = [int(float(i)) for i in row_str if i.strip() != ''] # convert to ints. should be 0 or 1
                lists.append(row_int)  # remove the first 'in-#'
    return lists

def to_list(nlp):
    # takes an nlp object and produces a list of 1 or 0 for denoting PERSON entities
    tokens = [x for x in nlp]
    NPEs = [0] * len(tokens)  # [0, 0, 0, 0 ... ]
    for ent in nlp.ents:
        if ent.label_ == 'PERSON':
            for i in range(ent.start, ent.end):
                NPEs[i] = 1
    return NPEs

class Compare:
    # nlp.ents[0].start  <- id offset of token. Single length token has "end" of +1
    # takes two lists, allows for computation of
    def __init__(self, nlp, actual_list):
        # convert nlp_list to comparable list
        if type(nlp) == spacy.tokens.doc.Doc:
            self.predicted_list = to_list(nlp)
        else:
            self.predicted_list = nlp
        self.actual_list = actual_list
        self.true_positives = None
        self.true_negatives = None
        self.false_positives = None
        self.false_negatives = None
        self.accuracy = None

    def get_true_positive(self):
        if not self.true_positives:
            self.true_positives = sum([p == a and p == 1 for p, a in zip(self.predicted_list, self.actual_list)])
        return self.true_positives

    def get_true_negative(self):
        if not self.true_negatives:
            self.true_negatives = sum([p == a and p == 0 for p, a in zip(self.predicted_list, self.actual_list)])
        return self.true_negatives

    def get_false_positive(self):
        if not self.false_positives:
            self.false_positives = sum([a == 0 and p == 1 for p, a in zip(self.predicted_list, self.actual_list)])
        return self.false_positives

    def get_false_negative(self):
        if not self.false_negatives:
            self.false_negatives = sum([a == 1 and p == 0 for p, a in zip(self.predicted_list, self.actual_list)])
        return self.false_negatives

    def get_accuracy(self):
        if not self.accuracy:  # calculate only once
            correct = sum([x == y for x, y in zip(self.predicted_list, self.actual_list)])
            self.accuracy = correct / len(self.predicted_list)
        return self.accuracy

    def get_precision(self):
        # true_positives / (true_positive + false_positives)
        denom = (self.get_true_positive() + self.get_false_positive())
        return self.get_true_positive() / denom if denom > 0 else 0

    def get_recall(self):
        # true_positives / (true_positives + false_negatives)
        denom = (self.get_true_positive() + self.get_false_negative())
        return self.get_true_positive() / denom if denom > 0 else 0

    def get_f1(self):
        # 2 * (precision * recall) / (precision + recall)
        denom = (self.get_precision() + self.get_recall())
        return 2 * (self.get_precision() * self.get_recall()) / denom if denom > 0 else 0

def show_confusion_from_compares(compare_objs, show=False):
    tp = sum([x.get_true_positive() for x in compare_objs])
    fn = sum([x.get_false_negative() for x in compare_objs])
    fp = sum([x.get_false_positive() for x in compare_objs])
    tn = sum([x.get_true_negative() for x in compare_objs])

    df_confusion = pd.DataFrame(
        [[tp, fp],
         [fn, tn]],
        list(reversed(range(2))),
        list(reversed(range(2)))
    )

    sn.set(font_scale=1.3)
    ax = sn.heatmap(df_confusion, annot=True, fmt="d", annot_kws={'size': 14})
    if show:
        plt.show(block=True)
        plt.interactive(False)
    return ax

def show_stats_from_compares(compare_objs, show=False):
    dict_compares = {
        'accuracy': [x.get_accuracy() for x in compare_objs],
        'precision': [x.get_precision() for x in compare_objs],
        'recall': [x.get_recall() for x in compare_objs],
        'f1': [x.get_f1() for x in compare_objs]
    }

    df_compare = pd.DataFrame(dict_compares)

    print(f"Means of each measurement for all {len(df_compare)} documents:")
    print(df_compare.mean())

    hist = df_compare.hist()
    if show:
        plt.show(block=True)
        plt.interactive(False)
    return hist

def load_baseline(path_to_given_names, path_to_surnames):
    path_to_given_names = Path(path_to_given_names)
    path_to_surnames = Path(path_to_surnames)

    df_given_names = pd.read_csv(path_to_given_names, names=['name', 'gender', 'rank'])
    df_given_names = df_given_names.drop(columns=['gender', 'rank'])
    df_given_names = df_given_names.drop_duplicates(subset=['name'])
    df_given_names['type'] = 'given_name'

    df_surnames = pd.read_csv(path_to_surnames, names=['name'])
    df_surnames = df_surnames.drop_duplicates(subset=['name'])
    df_surnames['type'] = 'surname'

    df_names = pd.concat([df_given_names, df_surnames])
    df_names['name'] = df_names['name'].str.lower()
    return df_names


def preprocess(df, path_export_csv=None, path_export_raw=None, col_name='trunc'):
    # SLOW. reduce sample size
    random.seed(3829)  # Final paper used the following seed: 3829
    pct_to_sample = 0.05
    num_to_sample = int(len(df)*pct_to_sample)
    sample = random.sample(range(0, len(df)), num_to_sample)
    df_sample = df.iloc[sample]
    df_sample['nlp'] = df_sample[col_name].map(lambda x: nlp(x))  # uses Spacey to tokenize

    if path_export_csv:
        path_export_csv = Path(path_export_csv)
        to_csv(list(df_sample['nlp']), path_export_csv)

    if path_export_raw:
        path_export_raw = Path(path_export_raw)
        df_sample[col_name].to_csv(path_export_raw, index=False)

    return df_sample

def preprocess_from_file(path_to_import_raw, col_name='trunc'):
    # instead of running preprocess (on full data) take prior 'process' run and load
    path_to_import_raw = Path(path_to_import_raw)
    df_sample = pd.read_csv(path_to_import_raw)
    df_sample['nlp'] = df_sample[col_name].map(lambda x: nlp(x))
    return df_sample

def analyze_spacey(df_sample, path_ner_labels_csv):
    path_ner_labels_csv = Path(path_ner_labels_csv)
    label_lists = from_csv(path_ner_labels_csv)
    compare_objs = []
    for nlp_obj, labels in zip(list(df_sample['nlp']), label_lists):
        compare_objs.append(Compare(nlp_obj, labels))

    show_confusion_from_compares(compare_objs, show=True)
    show_stats_from_compares(compare_objs, show=True)


def analyze_baseline(df_sample, df_names, path_ner_labels_csv, remove_dictionary_words=False, just_given_names=False):
    # compare with just a dictionary of names
    # df_names contains 'name' and 'type'
    path_ner_labels_csv = Path(path_ner_labels_csv)
    label_lists = from_csv(path_ner_labels_csv)

    if just_given_names:
        # remove surnames
        df_names_subset = df_names[df_names['type'] == 'given_name']
    else:
        df_names_subset = df_names

    if remove_dictionary_words:
        all_words = set(words.words())
        set_tmp_names = set(df_names_subset['name'])
        set_names = set_tmp_names.difference(all_words)
    else:
        set_names = set(df_names_subset['name'])

    # create 'label_lists' from Spacey-tokenized df_sample['nlp'] data
    names_prediction_lists = []
    for label_list in list(df_sample['nlp']):
        tokens = [str(t).lower() for t in label_list]  # lowercase both (df_sample_modified already lowered)
        names_prediction_list = [0] * len(tokens)
        names_prediction_list = [1 if t in set_names else 0 for t in tokens]
        names_prediction_lists.append(names_prediction_list)

    compare_objs = []
    for nlp_obj, labels in zip(names_prediction_lists, label_lists):
        compare_objs.append(Compare(nlp_obj, labels))

    show_confusion_from_compares(compare_objs, show=True)
    show_stats_from_compares(compare_objs, show=True)

