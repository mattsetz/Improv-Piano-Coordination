################################################################
# Sliding window histograms of interval class for combined performances
# 
# Matt Setzler 5/30/19
################################################################

import pandas as pd
import numpy as np
from glob import glob

##################
# constants
##################
IN_DIR = '../Pipeline/pc-window/combined/'
OUT_DIR_INTERVAL = '../Pipeline/tonal/intervals/window/combined/'
HOP_SIZE = 0.2


##################
# functions
##################
def get_interval(pitch1, pitch2):
    interval = abs(pitch1-pitch2)
    if interval > 6:
        interval = 6-(interval-6)
    return interval


def get_interval_class_histogram(pitch_histogram):
	intervals = [0]*7
	# count intervals
	for i in range(12):
		for j in range(i+1, 12):
			interval = get_interval(i, j)
			intervals[interval] += pitch_histogram[i]*pitch_histogram[j] # ToDo: frequency of unisons might explode because of sustained notes
	return intervals


def get_combined_pitch_class_histogram(long_pitch1, long_pitch2):
    pc_hist = long_pitch.groupby(by='note').sum().sort_values('note')['active'].values

    return pc_hist


def get_intervals_window(pitch_series, window):
	time = np.arange(HOP_SIZE, max(pitch_series.t)+HOP_SIZE, HOP_SIZE)
	interval_histograms = [[None]*7]*len(time)
	i = 0
	for t in time:
		if (t-window) < 0: i += 1; continue
		pitches_t = pitch_series.loc[(pitch_series.t >= (t-window))&
								     (pitch_series.t < t)]
		pitch_hist_t = get_pitch_class_histogram(pitches_t)
		pitch_histograms[i] = pitch_hist_t
		interval_histograms[i] = get_interval_class_histogram(pitch_hist_t)
		i += 1

	return (time, np.array(pitch_histograms), np.array(interval_histograms))


def package_dataframe(time_, hist_series, window):
	df_dict = {'time': time_}
	for i in range(hist_series.shape[1]):
		df_dict[i] = hist_series[:,i]
	df_dict['window'] = window

	df = pd.DataFrame(df_dict)
	return df


###########
ToDo:
combined intervals: simply compute intervals from combined pc_window Dataframes

def get_intervals(filepath):
	# read in input file, convert to long pitch class representation
	notes = pd.read_csv(filepath)
	pc_long = pd.melt(notes,notes.columns[0],notes.columns[1:],'note','velocity')
	pc_long.note = [int(i)%12 for i in pc_long.note]
	pc_long['active'] = [int(vel > 0) for vel in pc_long['velocity']]

	# get pitch histograms and interval histograms across a range of windows
	windows = [2, 5, 10, 20]
	pitches_df = pd.DataFrame()
	intervals_df = pd.DataFrame()
	for window in windows:
		time, intervals = get_pitch_and_intervals_window(pc_long, window)
		intervals_df = pd.concat((intervals_df, package_dataframe(time, intervals, window)))

	# write pitch and intervals to csv
	out_name = filepath.split('/')[-1].replace('-notes.csv','-interval-window.csv')
	intervals_df.to_csv(OUT_DIR+out_name, index=False)

	return

##################
# main
##################
infiles = glob(IN_DIR+"*")
i = 0
for f in infiles[:2]:
	print(str(i)+"/"+str(len(infiles)))
	print(f)
	get_intervals(f)
	i += 1