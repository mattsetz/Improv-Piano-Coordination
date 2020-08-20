################################################################
# Sliding window histograms of pitch class (pc) and interval class
# 
# Matt Setzler 5/30/19
################################################################

import pandas as pd
import numpy as np
from glob import glob

##################
# constants
##################
IN_DIR = '../Pipeline/tonal/pc-window/individual/'
OUT_DIR = '../Pipeline/tonal/pc-window/combined/'
HOP_SIZE = 0.2


##################
# functions
##################

def get_person_name(f):
	return f.split('.')[-2].split('-')[0]

def get_combined_pcs(filepath1, filepath2, name):
	# read in input file, convert to long pitch class representation
	pc_1 = pd.read_csv(filepath1)
	pc_2 = pd.read_csv(filepath2)

	# get pitch histograms and interval histograms across a range of windows
	pc_combined = pd.concat((pc_1,pc_2))
	pc_combined = pc_combined.groupby(by=["time","window"]).sum()
	pc_combined = pc_combined.reset_index().sort_values(["window","time"])

	# write pitch and intervals to csv
	pc_combined.to_csv(OUT_DIR+name+"-pc-window.csv",index=False)

	return


##################
# main
##################

infiles = glob(IN_DIR+"*")
for f in infiles:
	dyad_name = f.split('.')[-3]
	this_person = get_person_name(f)
	other_person = dyad_name.replace(this_person,'')
	file2 = [i for i in infiles if get_person_name(i) == other_person][0]

	print(this_person, other_person)
	print(f, file2)

	get_combined_pcs(f,file2,dyad_name)