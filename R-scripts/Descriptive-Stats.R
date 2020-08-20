#################################
# Summarize data set
#################################

library(tidyverse)

summary <- read.csv('summary.csv')
subjective <- read.csv('Participant-Data/subjective-ratings.csv')
subjective$perfID <- subjective %>% group_indices(Session, Trial)

# number of trials in each condition
num.trials <- subjective %>% group_by(Condition) %>%
              summarize(num_trals = length(unique(perfID)))

# distribution of piece durations
onsets <- read.csv('Pipeline/master_onsets.csv')
duration.summary <- onsets %>% group_by(personCombo) %>%
                    summarize(duration=max(time))

ggplot(duration.summary, aes(x=duration)) +
  geom_histogram() +
  xlab('length (sec)') +
  ggtitle('Distribution of Piece Lengths')
