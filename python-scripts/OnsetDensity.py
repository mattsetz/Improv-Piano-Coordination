#!/usr/bin/env python
# coding: utf-8

# In[2]:


import pandas as pd
import numpy as np
from glob import glob


# In[3]:


WINDOW_SIZE = 2.0 # sec
STEP_SIZE = 0.2 # sec

inDir = '../Pipeline/onsets/'
outDir = '../Pipeline/onset-density/'


# In[42]:


def get_onset_density(onsets):
    density = pd.DataFrame()
    
    length = max(onsets.time)+STEP_SIZE
    for t in np.arange(WINDOW_SIZE, length, STEP_SIZE):
        onsets_t = onsets[(onsets.time>=(t-WINDOW_SIZE))&
                              (onsets.time<t)]
        density = density.append(pd.DataFrame({'time': t,
                                               'num_onsets': len(onsets_t),
                                               'avg_vel': [np.mean(onsets_t.vel)]}))
    
    return density


# In[ ]:


infiles = glob(inDir+'*')
for f in infiles:
    print(f)
    density = get_onset_density(pd.read_csv(f))
    name = f.split('/')[-1]
    density.to_csv(outDir+name,index=False)

