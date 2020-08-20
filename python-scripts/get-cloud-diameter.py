###################################################################################################
#  Given time series of interval data, compute consonance time series
#
#  MS 6/5/19
###################################################################################################


import numpy as np
import pandas as pd
import sys
from glob import glob

IN_DIR = "../Pipeline/tonal/pc-window/combined/"
OUT_DIR = "../Pipeline/tonal/cloud-diameter/chew-2-11/combined/"


###################################################################################################
# TONAL TOPOLOGIES
# Each interval class is assigned a consonance/dissonance rating
###################################################################################################


# Chew's ordering (with parameter h/r=sqrt(2/15)): {(P5/P4) == (M3/m6), (m3/M6), (M2/m7), (m2/M7), (d5/A4)}
# Chew h/r = sqrt(2/15)
diss_dict_chew_2_15 =   {0: 1.0,
						1: 2.309401076758503,
 						2: 2.1291625896895083,
 						3: 1.7888543819998317,
						4: 1.4605934866804429,
						5: 1.4605934866804429,
						6: 2.966479394838265}
# h/r = sqrt(2/11)
diss_dict_chew_2_11 = {0: 1.0,
 1: 2.5584085962673253,
 2: 2.174229226018436,
 3: 1.9069251784911847,
 4: 1.7056057308448835,
 5: 1.4770978917519928,
 6: 3.247376563543955}

# h/r = sqrt(2/7)
diss_dict_chew_2_7 = {0: 1.0,
 1: 3.0237157840738176,
 2: 2.2677868380553634,
 3: 2.138089935299395,
 4: 2.138089935299395,
 5: 1.5118578920369088,
 6: 3.779644730092272}

# Closed distance in Riemman's tonnetz
cons_dict_2Dtonnetz = {0: 0, 1: 2, 2: 2, 3: 1, 4: 1, 5: 1, 6: 2}

# Basic/intuitive notions
cons_dict_simple = {0: 1, 1: 0, 2: 0, 3: 0, 4: 0, 5: .8, 6: 0}
diss_dict_simple = {0: 0, 1: .8, 2: .6, 3: 0, 4: 0, 5: 0, 6: 1}
cons_dict_chew_rough = {0: 1, 1: .2, 2: .4, 3: .6, 4: .6, 5: .8, 6: 0}
diss_dict_chew_rough = {0: 0, 1: .8, 2: .6, 3: .4, 4: .4, 5: .2, 6: 1}



##########################################################################################################
#  FUNCTIONS
##########################################################################################################

#########################################################
# Input: two pitch class histograms, tonal topology map
# Output: scalar distance between two pitch clouds
#########################################################
def between_cloud_distance(pc_hist1, pc_hist2, topology):
	if (is_empty_pc_hist(pc_hist1) | is_empty_pc_hist(pc_hist2)): return None

	# get inner cloud distances
	ic_hist1 = pitch_to_interval_hist(pc_hist1)
	ic_hist2 = pitch_to_interval_hist(pc_hist2)
	inner_dist1 = ic_hist_to_distance(ic_hist1, topology)
	inner_dist2 = ic_hist_to_distance(ic_hist2, topology)
	
	# get inter cloud distance
	ic_hist_combined = [0]*7
	for pc1 in range(12):
		for pc2 in range(pc1, 12):
			ic = get_ic(pc1, pc2)
			ic_freq = ic_hist1[pc1]*ic_hist2[pc2]
			ic_hist_combined[ic] += ic*ic_freq
	inter_dist = ic_hist_to_distance(ic_hist_combined, topology)

	distance = inter_dist/min(inner_dist1, inner_dist2)  # ToDo: is this the best formalization?
	return distance

def is_empty_pc_hist(pc_hist):
	pc_hist = np.nan_to_num(pc_hist)
	for i in range(len(pc_hist)):
		if pc_hist[i] > 0: return False
	return True

def get_interval(pitch1, pitch2):
    interval = abs(pitch1-pitch2)
    if interval > 6:
        interval = 6-(interval-6)
    return interval

def pitch_to_interval_hist(pitch_histogram):
	intervals = [0]*7
	# count intervals
	for i in range(12):
		for j in range(i, 12):
			interval = get_interval(i, j)
			intervals[interval] += pitch_histogram[i]*pitch_histogram[j] # ToDo: is this the right way to go?
	return intervals


def ic_hist_to_avg_distance(ic_hist, topology):
	ic_hist = ic_hist/np.sum(ic_hist) # normalize histogram

	distance = 0
	for i in range(7):
		distance += ic_hist[i]*topology[i]
	return distance

def get_max_dist(pc_hist, topology):
	if is_empty_pc_hist(pc_hist): return None

	ic_hist = pitch_to_interval_hist(pc_hist)
	max_dist = 0
	for i in range(7):
		if ((ic_hist[i]>0)&(topology[i]>max_dist)):
			max_dist = topology[i]
	return max_dist

def get_weighted_avg_dist(pc_hist, topology):
	if is_empty_pc_hist(pc_hist): return None

	ic_hist = pitch_to_interval_hist(pc_hist)
	return ic_hist_to_avg_distance(ic_hist, topology)

def get_cloud_diameter(filein, topology):
	# read in interval class (ic) dataframe
	pc_df = pd.read_csv(filein)

	# compute cloud_diameter at each time step
	cloud_diameter_df = pd.DataFrame()
	for i in range(len(pc_df)):
		row = pc_df.iloc[i]
		pc_hist = row.loc[[str(i) for i in range(12)]].values
		max_dist = get_max_dist(pc_hist, topology)
		avg_dist = get_weighted_avg_dist(pc_hist, topology)
		cloud_diameter_df = pd.concat((cloud_diameter_df, 
									   pd.DataFrame({'time': row['time'],
										  		     'window': row['window'],
												     'max_dist': [max_dist],
												     'avg_dist': [avg_dist]})))

	# write dataframe to csv
	fileout = filein.split("/")[-1].replace('-pc-window.csv','-cloud-diameter.csv')
	cloud_diameter_df.to_csv(OUT_DIR+fileout, index=False)
	return


###################################################################################################
# MAIN
###################################################################################################
infiles = glob(IN_DIR+"*")
for f in infiles:
	print(f)
	get_cloud_diameter(f, diss_dict_chew_2_11)
