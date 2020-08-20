# MS 5/7/19

import numpy as np
import pandas as pd
from glob import glob
import re


def get_combined_density(df1, df2, name_, condition_):
	df_combined = None # this will be updated later
	if len(df1) < len(df2):
		combined = df2['num_onsets'].copy()
		combined[:len(df1)] += df1['num_onsets']
		df_combined = pd.DataFrame({'time': df2['time'],
								   'density': combined,
								   'name': name_,
								   'condition': condition_})
	else:
		combined = df1['num_onsets'].copy()
		combined[:len(df2)] += df2['num_onsets']
		df_combined = pd.DataFrame({'time': df1['time'],
								    'density': combined,
								    'name': name_,
								    'condition': condition_})

	# write to csv
	out_name = name_+'-combined-onset-density.csv'
	df_combined.to_csv(outDir+out_name, index = False)

# for every individual onset density file...
inDir = '../Pipeline/onset-density/'
outDir = '../Pipeline/onset-density/combined/'
summary = pd.read_csv('../summary.csv')

in_files = glob(inDir+'*.csv')
for f in in_files:
	print(f)
	person_combo = f.split('.')[-3]
	this_person = f.split('.')[-2]
	other_person = person_combo.replace(this_person,'')
	condition = summary.loc[summary.person_trial==this_person].condition.values[0]

	other_filename = [fname for fname in in_files if fname.split('.')[-2]==other_person][0]
	get_combined_density(pd.read_csv(f), pd.read_csv(other_filename),
						 person_combo, condition)