library(tidyverse)

# Input Data ####
summary <- read.csv('summary-combined.csv')
diss <- read.csv('Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-combined.csv')
diss <- merge(summary, diss)
onsets <- read.csv('Pipeline/rhythm/onset-density/collapsed/master-onset-density-combined.csv')
onsets$window <- 2
head(onsets)

var.df <- diss %>% rename(Value = avg_dist) %>% select(-max_dist)
var.df <- onsets %>% rename(Value = num_onsets) %>% select(-max_dist)



# Analyze Variability ####
head(var.df)

# more variability overall in coupled versus one-way?
ggplot(var.df, aes(x=Value,color=condition,fill=condition)) +
  geom_density(alpha=.2) + facet_wrap(~window)

W = 2
var(filter(var.df,condition=="coupled",window==W)$Value, na.rm = TRUE)
var(filter(var.df,condition=="one-way",window==W)$Value, na.rm = TRUE)

# variability by-trial
var.summary <- var.df %>% group_by(pair,yolked_id,condition,window) %>% 
  summarise(variability = var(Value,na.rm=TRUE)) %>% ungroup()
var.summary <- var.summary %>% group_by(yolked_id,condition,window) %>% 
  summarise(variability = mean(variability,na.rm=TRUE))

var.summary.wide <- var.summary %>% spread(condition,variability)
var.summary.wide$difference <- var.summary.wide$coupled - var.summary.wide$`one-way`

ggplot(var.summary.wide, aes(x=difference)) + geom_histogram() + facet_wrap(~window)

W = 2
t.test(filter(var.summary.wide,window==W)$coupled,
       filter(var.summary.wide,window==W)$`one-way`,
       paired = TRUE)

var.summary$range <- var.summary$max_val - var.summary$min_val
ggplot(var.summary, aes(x=range,color=condition,fill=condition)) +
  geom_density(alpha=.2) + facet_wrap(~window)
