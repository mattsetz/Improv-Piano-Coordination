'''
Export MIDI files to csv
MS 2/18/19

What pitch-classes are being played each time step? (normalize octaves)
Format:

'''

import mido
import pandas as pd
import sys

STEP = .2 # seconds

def frange(start, stop, step):
	r = []
	tmp = start
	while (tmp < stop):
		r.append(tmp)
		tmp += step
	return r

# Import track
print("Importing track")
f = sys.argv[1]
mid = mido.MidiFile(f)

# Construct intermediate dataframe, all note_on/note_off events
print("Construting intermediate dataframe")
df0 = pd.DataFrame(columns = ['t','note','vel','type'])
t = 0
i = 0
for msg in mid:
	t += msg.time
	if (msg.type == 'note_on' or msg.type == 'note_off'):
		df0.loc[i] = [t, msg.note % 12, msg.velocity, msg.type]
		i += 1
print("df0")
print(df0)


# Construct dataframe of all pitch classes each timestep
print("Constructing final dataframe")
df = pd.DataFrame(columns = ['t']+range(12))
i = 0
note_vels = [0]*12 # initialize note columns to zero
increments = frange(STEP, df0['t'].max(), STEP)
print("starting loop")
for t in increments:
	start = df0['t'] >= (t-STEP)
	end = df0['t'] < t
	on = df0['type'] == 'note_on'
	off = df0['type'] == 'note_off'
	# find note onsets
	onsets = df0[start & end & on]
	for row in range(onsets.shape[0]):
		onset = onsets.iloc[row]
		note_vels[onset['note']] = 1

	df.loc[i] = [t*1000] + note_vels

	# find note offs
	offs = df0[start & end & off]
	for row in range(offs.shape[0]):
		off = offs.iloc[row]
		note_vels[off['note']] = 0

	i += 1



# Output to csv
print("Outputing to csv")
df.to_csv(f.replace('.mid','_time.csv'),index=False)