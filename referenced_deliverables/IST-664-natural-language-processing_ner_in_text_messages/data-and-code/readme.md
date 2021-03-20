# Data
please see 'data\original' for data used in paper. Full dataset contains sensitive information

# Step 1: Installing Conda Environment
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
conda create -n ball_sms_env python=3.7 pandas pyprind spacy matplotlib seaborn nltk 
```

# Step 2: Install additional dependencies
## Download dependencies for Spacey
```
conda activate ball_sms_env
python -m spacy download en_core_web_md
```

# Running the code
## Run data extraction step (not needed for demo purposes)
```
# must execute __main__.py from within same directory
cd data-and-code
conda activate ball_sms_env
python __main.py__ extract
```

## Run data analysis step (recommended)
```
# must execute __main__.py from within same directory
cd data-and-code
conda activate ball_sms_env
python __main.py__ analyze
```




