'''
Export MIDI files to csv
MS 8/17/18

csv of note onsets. Format:

i, t (sec), note, vel
0, .34, 56, 67
1, .52, 98, 12
2, 1.6, 98, 13
etc.
'''

import mido
import csv
import sys

# Import track
f = sys.argv[1]
mid = mido.MidiFile(f)

# export to csv
csv_f = f.replace(".mid",".csv")
with open(csv_f, 'w') as csvfile:
    writer = csv.writer(csvfile, delimiter=',')
    writer.writerow(['i','time','note','vel'])
    i = 0
    t = 0
    for msg in mid:
    	t += msg.time
    	if (msg.type == 'note_on'):
    		writer.writerow([i,t,msg.note,msg.velocity])
    		i += 1