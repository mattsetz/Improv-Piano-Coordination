#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
from glob import glob


# In[4]:


summary = pd.read_csv('../summary.csv')
summary.head()


# In[12]:


##########################################################
# Merge Onset data from all trials into one DataFrame
##########################################################

from glob import glob

onset_files = glob('../Pipeline/onsets/*.csv')

# iterate through every file, add onsets to master_onsets
master_onsets = pd.DataFrame()

for f in onset_files:
    print(f)
    # get info
    fname = f.split('/')[-1]
    if (fname.count('.') != 3): continue # weed out solo tracks
    personCombo = fname.split('.')[1]
    person_trial = fname.split('.')[2]
    name = fname.replace('.csv','')
    other_person = personCombo.replace(person_trial,'')
    cond = summary[(summary.Trial == int(person_trial[-1]))&
                    (summary.Subject==person_trial[:-1])].condition.values[0]
        
    # get onsets
    onsets = pd.read_csv(f, index_col = False)
    onsets['personCombo'] = personCombo
    onsets['person_trial'] = person_trial
    onsets['name'] = name
    onsets['other_person'] = other_person
    onsets['cond'] = cond
    print(len(onsets))
    print(len(master_onsets))
    master_onsets = master_onsets.append(onsets)
        


# In[10]:


###########################
# Generate Asyncs
###########################

import numpy as np
master_asyncs = pd.DataFrame()

# generate asyncs
MIN_THRESH = 0.100 # 100 ms window
for p in np.unique(master_onsets.person_trial):
    print(p)
    onsets_p = master_onsets[master_onsets.person_trial==p]
    other_person = np.unique(onsets_p.other_person)[0]
    onsets_other = master_onsets[master_onsets.person_trial==other_person]
    
    for i in range(len(onsets_p)):
        if (i % 1000 ==0): print(str(i)+'/'+str(len(onsets_p)))
        onset_i = onsets_p.iloc[i]
        near_onsets = onsets_other[abs(onset_i.time-onsets_other.time)<MIN_THRESH]
        
        asyncs_i = pd.DataFrame({'cond': onset_i.cond,
                                 'asyncs': onset_i.time-near_onsets.time,})
        master_asyncs = master_asyncs.append(asyncs_i)


# In[4]:


master_onsets = pd.read_csv(glob('../Pipeline/*.csv')[0])


# In[5]:


master_onsets.head()


# In[ ]:


def get_asyncs_fast(person, other):
    condition = person.cond[0]
    person_i = 0
    other_i = 0

    asyncs = pd.DataFrame()
    while ((person_i < len(person)) & (other_i < len(other))):
        onset_person = person.iloc[person_i].time
        onset_other = other.iloc[other_i].time
        asynchrony = onset_person - onset_other
        if (abs(asynchrony) < MIN_THRES):
            # append async
            asyncs = asyncs.append(pd.DataFrame({'cond': condition,
                                                 'asyncs': asynchrony}))
            # increment other_i
            other_i += 1
        elif (asynchrony > 0):
            # increment other_i
            other_i += 1
        else:
            # increment person_i
            person_i += 1
    
    return asyncs

