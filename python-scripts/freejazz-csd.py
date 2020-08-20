import pandas as pd 
import numpy as np 
from glob import glob
import re

IN_DIR = "../Librosa-Pipeline/mfcc/"
OUT_DIR = "../Librosa-Pipeline/critical-slowing/"
HOP_SIZE = 0.2 # seconds

#########################################
# Functions for computing CSD indices
#########################################

def first_differencing(mfcc_df):
    mfcc_cols = [c for c in yuma_matt.columns if re.match('X[0-9]{1,2}$',c)]
    for col in mfcc_cols:
        mfcc_df["diff-"+col] = mfcc_df[col].diff()
    return mfcc_df

def csd_indices_window(mfcc_df, window_size):
	# how to compute variability and acf ?
	# Proposal: variability = mean pairwise distance, acf = mean auto correlation
	

def csd_indices(mfcc_df):
	# detrend mfcc_df
	mfcc_df = first_differencing(mfcc_df)

	# compute csd indices across range of window sizes
	windows = [2, 5, 10, 20]
	for w in windows:
		csd_indices_window(mfcc_df, w)

	# concatenate into one dataframe and write to csv
	return




#########################################
# Compute CSD for every track
#########################################
infiles = glob(IN_DIR+"*.csv")
for f in infiles:
	print(f)
	csd_indices(pd.read_csv(f))