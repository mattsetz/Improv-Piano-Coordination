'''
Export MIDI files to csv
MS 8/17/18

Velocity for each note in midi range [0,127] at every time step.
Format:

i, t (sec), 0, 1, 2, 3, 4, 5 ... 127
0, .02, 0, 0, 0, 0, 16, 0, 0, 14 ... 0
1, .04, 0, 0, 0, 78, 0 ... 0
etc.
'''

import mido
import pandas as pd
from glob import glob


in_dir = '../Pipeline/MIDI/'
out_dir = '../Pipeline/notes-time/individual/'
STEP = .1 # seconds
LOWEST_NOTE = 36
HIGHEST_NOTE = 96

def frange(start, stop, step):
	r = []
	tmp = start
	while (tmp < stop):
		r.append(tmp)
		tmp += step
	return r

def midi_to_csv(filepath):
	# Import MIDI
	mid = mido.MidiFile(filepath)

	# Construct intermediate dataframe, all note_on/note_off events
	df0 = pd.DataFrame(columns = ['t','note','vel','type'])
	t = 0
	i = 0
	for msg in mid:
		t += msg.time
		if (msg.type == 'note_on' or msg.type == 'note_off'):
			df0.loc[i] = [t, msg.note, msg.velocity, msg.type]
			i += 1

	# Construct dataframe of all notes each timestep
	df = pd.DataFrame(columns = ['t']+list(range(LOWEST_NOTE,HIGHEST_NOTE+1)))
	i = 0
	note_vels = [0]*(1+(HIGHEST_NOTE-LOWEST_NOTE)) # initialize note columns to zero
	increments = frange(STEP, df0['t'].max(), STEP)
	for t in increments:
		start = df0['t'] >= (t-STEP)
		end = df0['t'] < t
		on = df0['type'] == 'note_on'
		off = df0['type'] == 'note_off'
		# find note onsets
		onsets = df0[start & end & on]
		for row in range(onsets.shape[0]):
			onset = onsets.iloc[row]
			note_vels[onset['note']-LOWEST_NOTE] = onset['vel']

		df.loc[i] = [t] + note_vels

		# find note offs
		offs = df0[start & end & off]
		for row in range(offs.shape[0]):
			off = offs.iloc[row]
			note_vels[off['note']-LOWEST_NOTE] = 0

		i += 1

	# Output to csv
	outname = filepath.split('/')[-1].replace('.mid','-notes.csv')
	df.to_csv(out_dir+outname, index=False)

	return


infiles = glob(in_dir+'*.mid')
for f in infiles:
	print(f)
	midi_to_csv(f)
