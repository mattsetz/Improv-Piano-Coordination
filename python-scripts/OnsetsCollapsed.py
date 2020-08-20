#!/usr/bin/env python
# coding: utf-8

# In[12]:


import librosa
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


# In[4]:


inDir = "../Pipeline/audio/individual/"
outDirOnsets = "../Pipeline/onsets/collapsed/individual/"


# In[42]:


def get_onsets(audio_name):
    y, sr = librosa.load(audio_name)
    onset_frames = librosa.onset.onset_detect(y=y,sr=sr)
    onset_times = pd.DataFrame({'t': librosa.frames_to_time(onset_frames,sr=sr)})
    onset_times.to_csv(ouDirOnsets+audio_name.replace('wav','csv'),index=False)
    return


# In[ ]:


inFiles = glob(inDir+'*')
for f in inFiles:
    print(f)
    get_onsets(f)


# In[43]:


get_ipython().run_line_magic('ipython', 'nbconvert --to=python')


# In[ ]:




