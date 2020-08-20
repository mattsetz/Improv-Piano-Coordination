###################################################################################################
#  Combined dissonance matrices
#
#  MS 8/18/19
###################################################################################################


import numpy as np
import pandas as pd
import tonal_model
import sys
from glob import glob

WINDOW_SIZE = 10.0 # sec -- use 10 sec window in pc dataframe
HOP_SIZE = 2.0 # sec
DIAG_WINDOW = 20 # sec
IN_DIR = "../Pipeline/tonal/pc-window/individual/virtual/"
OUT_DIR = "../Pipeline/tonal/diss-matrix/"

##########################################################################################################
#  FUNCTIONS
##########################################################################################################

# INPUT: 2 pitch class vectors
# OUTPUT: combined dissonance score
def combined_dissonance(pc1, pc2):
	pc_comb = [pc1[i]+pc2[i] for i in range(12)]
	return tonal_model.get_weighted_avg_dist(pc_comb)

def pc_row_to_array(pc_row):
	if (len(pc_row) == 0):
		return [0]*12
	return pc_row.values[0]

# generate dissonance matrix from 2 performances
def get_dissonance_matrix(filepath1, filepath2, dyad_name, player1_name):
	pc1 = pd.read_csv(filepath1)
	pc1 = pc1.loc[pc1.window == WINDOW_SIZE]
	pc1.time = pc1.time.round(1)
	pc2 = pd.read_csv(filepath2)
	pc2 = pc2.loc[pc2.window == WINDOW_SIZE]
	pc2.time = pc2.time.round(1)

	# initialize dataframe
	max_length = max(max(pc1.time), max(pc2.time))
	times = np.arange(WINDOW_SIZE, max_length+HOP_SIZE, HOP_SIZE)
	diss_m = pd.DataFrame()

	# compute combined dissonance
	print("compute")
	pitch_cols = [str(p) for p in range(12)]
	for i in times:
		pc1_i = pc1.loc[pc1.time == i, pitch_cols]
		pc1_i = pc_row_to_array(pc1_i)
		pc1_i_diss = tonal_model.get_weighted_avg_dist(pc1_i)
		for j in np.arange(i-DIAG_WINDOW,(i+DIAG_WINDOW)+HOP_SIZE,HOP_SIZE):
			print('time1: '+str(i)+' time2: '+str(j))
			pc2_j = pc2.loc[pc2.time == j, pitch_cols]
			pc2_j = pc_row_to_array(pc2_j)
			pc2_j_diss = tonal_model.get_weighted_avg_dist(pc2_j)
			diss = combined_dissonance(pc1_i, pc2_j)
			diss_m = pd.concat((diss_m,
							   pd.DataFrame({'time1':[i],
							   				 'time2':[j],
							   				 'diss1': [pc1_i_diss],
							   				 'diss2': [pc2_j_diss],
							   				 'dissonance':[diss]})))

	# write data frame out to file
	diss_m.to_csv(OUT_DIR+dyad_name+'.'+player1_name+'-diss-matrix.csv', index=False)
	return



###################################################################################################
# MAIN
###################################################################################################
def get_person_name(f):
	return f.split('.')[-2].split('-')[0]

infiles = glob(IN_DIR+"*")
for f in infiles:
	dyad_name = f.split('.')[-3]
	this_person = get_person_name(f)
	other_person = dyad_name.replace(this_person,'')
	file2 = [i for i in infiles if get_person_name(i) == other_person][0]

	print(this_person, other_person)
	print(f, file2)

	get_dissonance_matrix(f,file2,dyad_name,this_person)