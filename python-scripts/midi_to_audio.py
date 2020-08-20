# Get audio files from midi
import pretty_midi
import scipy.io.wavfile
from glob import glob

inDir = '../Pipeline/MIDI/'
outDir = '../Pipeline/audio/'

def write_audio(mid):
	midi = pretty_midi.PrettyMIDI(mid)
	audio = midi.synthesize()
	out_name = mid.split('/')[-1].replace('.mid','.wav')
	scipy.io.wavfile.write(outDir+out_name, 44100, audio)
	return

files = glob(inDir+'*')
for f in files:
	print(f)
	write_audio(f)