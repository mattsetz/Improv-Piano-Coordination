########################################
# Compute tonal entropy assuming a uniform prior
# MS 5/7
########################################

import numpy as np
import pandas as pd
from glob import glob
import math

PRIOR_HEIGHT = 0.5
HOP_SIZE = 0.5 # sec

inDir = '../Pipeline/onsets/'
outDir = '../Pipeline/tonal-entropy/onset-density/'

def compute_entropy(hist):
    entropy = 0
    for i in range(12):
        pi = hist[i]/np.sum(hist)
        entropy += (pi*math.log2(pi))
    return -entropy

def get_entropy_window(onsets, window):
	time = np.arange(HOP_SIZE, np.max(onsets.time)+HOP_SIZE, HOP_SIZE)
	num_onsets = [0]*len(time)

	i = 0
	for t in time:
		if (t-window < 0):
			i += 1
			continue
		onsets_t = onsets[(onsets.time>(t-window))&(onsets.time<=t)]	
		num_onsets[i] = len(onsets_t)
		i += 1

	return num_onsets


def get_onsets(infile):
	onsets = pd.read_csv(infile)

	entropy2 = get_entropy_window(onsets, 2)
	entropy5 = get_entropy_window(onsets, 5)
	entropy10 = get_entropy_window(onsets, 10)
	entropy20 = get_entropy_window(onsets, 20)

	time = np.arange(HOP_SIZE, np.max(onsets.time)+HOP_SIZE, HOP_SIZE)
	df = pd.DataFrame({'time': time,
				  'onsets2': entropy2,
				  'onsets5': entropy5,
				  'onsets10': entropy10,
				  'onsets20': entropy20})
	outname = infile.split('/')[-1].replace('.csv','-onsets.csv')
	df.to_csv(outDir+outname, index=False)
	return


infiles = glob(inDir+'*.csv')
for f in infiles:
	print(f)
	get_onsets(f)