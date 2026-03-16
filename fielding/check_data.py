import pandas as pd

# %% import data 

file_name = "raw-data/GBF_150326.csv"

df = pd.read_csv(file_name, skiprows=[1, 2])