#################################################
# Merge audio files from same trial and 
# obtain tempogram
#################################################
import scipy.io.wavfile
import numpy as np
import pandas as pd
from glob import glob
import librosa
import librosa.display
import matplotlib.pyplot as plt

inDir = '../Pipeline/audio/individual/'
audioDir = '../Pipeline/audio/combined/'
tempoDir = '../Pipeline/tempogram/combined/'
hop_length = 512


##########################################
# generate combined audio files
##########################################
def write_combined_audio(audio1_name,audio2_name,out_name):
	sr, player1 = scipy.io.wavfile.read(audio1_name)
	sr, player2 = scipy.io.wavfile.read(audio2_name)

	# normalize length
	max_len = max(len(player1),len(player2))
	player1 = np.concatenate((player1,[0]*(max_len-len(player1))))
	player2 = np.concatenate((player2,[0]*(max_len-len(player2))))

	# merge audio arrays and output to wav
	combined = scipy.vstack((player1,player2))
	combined = np.mean(combined,axis=0)
	scipy.io.wavfile.write(audioDir+out_name,sr,combined)
	return


in_files = glob(inDir+'*.wav')
for f in in_files:
	print("get audio: "+f)
	person_combo = f.split('.')[-3]
	this_person = f.split('.')[-2]
	other_person = person_combo.replace(this_person,'')

	other_filename = [fname for fname in in_files if fname.split('.')[-2]==other_person][0]
	write_combined_audio(f,other_filename,person_combo)




##########################################
# generate combined tempograms
##########################################
def get_tempo(audio):
	name = audio.split('/')[-1]
	y, sr = librosa.load(audio)
	oenv = librosa.onset.onset_strength(y=y, sr=sr, hop_length=hop_length)
	tempogram = librosa.feature.tempogram(onset_envelope=oenv, sr=sr,
                                           hop_length=hop_length)

	# save image
	librosa.display.specshow(tempogram, sr=sr, hop_length=hop_length,
	                           x_axis='time', y_axis='tempo')
	plt.savefig(tempoDir+'figs/'+name.replace('.wav','-tempo.png'))
	plt.close()

	# save to csv
	tempogram = tempogram.transpose()
	df = pd.DataFrame(tempogram)
	time = [(i+1)*(hop_length/sr) for i in range(len(tempogram))]
	df.columns = librosa.core.tempo_frequencies(384)
	df['time'] = time

	out_name = name.replace('.wav','-tempogram.csv')
	df.to_csv(tempoDir+out_name, index=False)
	return

print("get tempograms")
files = glob(audioDir+'*')
for f in files:
	print("tempogram: "+f)
	get_tempo(f)