library(tidyverse)

#######################
# COMBINED
#######################

#######################
# read in data
#######################
diam <- read.csv('Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-combined.csv')
summary <- read.csv("summary-combined.csv")
diam <- merge(diam,summary)
diam <- diam %>% group_by(pair) %>% mutate(n_t=ntile(time,10)) %>% ungroup()

View(filter(diam,pair=="a1b1",window==2))


#################################
# overall effect of condition -- no, there isn't one
#################################
head(diam)
diam$window <- paste(diam$window,"sec window")
ggplot(diam, aes(x=avg_dist,color=condition,fill=condition)) +
  geom_density(alpha=.2) + facet_wrap(~window) + xlab("Dissonance")
t.test(na.omit(filter(diam,window==2,condition=="coupled")$avg_dist),
       na.omit(filter(diam,window==2,condition=="one-way")$avg_dist))

# grouping by yolked id -- no effect
diam.cond <- diam %>% group_by(yolked_id,condition,window) %>% summarise(avg.diss=mean(avg_dist,na.rm=TRUE))
diam.oneway <- diam.cond %>% filter(condition=="one-way")
diam.coupled <- diam.cond %>% filter(condition=="coupled",yolked_id %in% diam.oneway$yolked_id)

ggplot(diam.cond, aes(x=avg.diss,color=condition,fill=condition)) +
  geom_density(alpha=.2) +
  facet_wrap(~window)

t.test(filter(diam.coupled[order(diam.coupled$window,diam.coupled$yolked_id),],window==5)$avg.diss,
       filter(diam.oneway[order(diam.oneway$window,diam.oneway$yolked_id),],window==5)$avg.diss, paired=TRUE)

#################################
# visualize over normalized time
#################################
diam.sum <- diam %>% group_by(pair,condition,n_t,window) %>%
  summarise(mean_dist=mean(avg_dist,na.rm=TRUE))
diam.sum <- diam.sum %>% ungroup() %>% group_by(condition,n_t,window) %>%
  summarise(dist=mean(mean_dist,na.rm=TRUE),
            se=sd(mean_dist,na.rm=TRUE)/sqrt(n())) %>% ungroup()
head(diam.sum.2)

ggplot(filter(diam.sum,window==2), aes(x=10*n_t,y=dist,color=condition,fill=condition))+
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=dist-se,
                  ymax=dist+se),alpha=.3) +
  xlab("normalized time (%)") +
  ylab("dissonance") +
  theme_classic() +
  ggtitle('combined dissonance over normalized time')


ggplot(filter(diam,window==2), aes(x=avg_dist,color=condition,fill=condition)) +
  geom_density(alpha=0.3) +
  facet_wrap(~n_t)

t.test(filter(diam,window==2,n_t==8,condition=="coupled")$avg_dist,
       filter(diam,window==2,n_t==8,condition=="one-way")$avg_dist)

###################################################
# mixed-effects: interaction of condition + time?
###################################################
library(lme4)

# set up data: average across one-way trials with same yolked_id
diam.oneway <- diam %>% filter(condition=="one-way",window==2) %>%
  group_by(yolked_id, time, condition) %>% summarise(diss=mean(avg_dist,na.rm=TRUE)) %>% ungroup()
diam.oneway <- diam.oneway %>% group_by(yolked_id,condition) %>% mutate(n_t=ntile(time,10)) %>% ungroup()

diam.coupled <- diam %>% filter(condition=="coupled",window==2) %>%
  select(yolked_id,time,condition,avg_dist, n_t)
diam.coupled$diss <- diam.coupled$avg_dist
diam.coupled <- diam.coupled %>% select(-avg_dist)
diam.abbr <- rbind(diam.coupled, diam.oneway)
diam.abbr$n_t_squared <- diam.abbr$n_t*diam.abbr$n_t
  
# model comparison w/ and w/o linear interaction of condition*time
diss.model <- lmer(diss ~ n_t_squared + n_t + n_t*condition + condition + 
                     (1|yolked_id), data=diam.abbr, REML = FALSE)
summary(diss.model)

diss.null <- lmer(diss ~ n_t_squared + n_t + condition + 
                    (1|yolked_id), data=diam.abbr, REML = FALSE)

anova(diss.model, diss.null)

diam.abbr$predict1 <- predict(diss.model)

###################################################
# mixed-effects: interaction of condition + time^2?
###################################################
diss.model.quad <- lmer(diss ~ n_t_squared + n_t + n_t*condition + n_t_squared*condition + 
                     condition + (1|yolked_id), data=diam.abbr, REML = FALSE)
summary(diss.model)

anova(diss.model.quad,diss.model)

###################################################
# mixed-effects: diss tell us more than onset density?
###################################################

# actually, how do I do this?

# prepare dataframe w/ num_onsets
onsets <- read.csv('Pipeline/rhythm/onset-density/collapsed/master-onset-density-combined.csv')
onsets <- merge(onsets,summary)
onsets.oneway <- onsets %>% filter(condition=="one-way")
onsets.oneway <- onsets.oneway %>% group_by(yolked_id,time,condition) %>% 
  summarise(density=mean(num_onsets,na.rm=TRUE))

onsets.coupled <- onsets %>% filter(condition=="coupled")
onsets.coupled$density <- onsets.coupled$num_onsets
onsets.coupled <- onsets.coupled %>% select(yolked_id,time,condition,density)
onsets.abbr <- rbind(onsets.coupled)
onsets.abbr <- onsets.abbr %>% group_by(yolked_id) %>% mutate(n_t=ntile(time,10)) %>% ungroup()
onsets.abbr$n_t_squared <- onsets.abbr$n_t*onsets.abbr$n_t

diam.onsets <- merge(diam.abbr,onsets.abbr)
View(diam.onsets)


#################################
# onsets + dissonance
#################################
onsets <- read.csv('Pipeline/rhythm/onset-density/collapsed/master-onset-density-combined.csv')
head(onsets)
head(diam)
diam.onsets <- merge(filter(diam,window==2),onsets)
head(diam.onsets)

diam.onsets <- diam.onsets %>% select(-n_t)
diam.onsets <- diam.onsets %>% group_by(pair) %>% 
  mutate(n_t=ntile(time,4)) %>% ungroup()
ggplot(diam.onsets,aes(x=num_onsets,y=avg_dist,color=condition,fill=condition))+
  geom_smooth(method='lm') +
  facet_wrap(~n_t)






#######################
# INDIVIDUAL
# read in data
#######################
diss <- read.csv('Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-individual.csv')
summary <- read.csv("summary-individual.csv")
diss <- merge(diss,summary)

onsets <- read.csv('Pipeline/rhythm/onset-density/collapsed/master-onset-density-individual.csv')
diss.onsets <- merge(filter(diss,window==2),onsets)

#########################
# individual dissonance over normal time
########################
head(diss)
diss <- diss %>% group_by(player) %>% mutate(n_t=ntile(time,10)) %>% ungroup()
diss.sum <- diss %>% group_by(player,window,condition,n_t) %>%
            summarise(diss=mean(avg_dist,na.rm=TRUE)) %>% ungroup()
diss.sum <- diss.sum %>% group_by(window,condition,n_t) %>%
  summarise(mean_diss=mean(diss,na.rm=TRUE),
          se=sd(diss,na.rm=TRUE)/sqrt(n())) %>% ungroup()

ggplot(filter(diss.sum,window==2), aes(x=10*n_t,y=mean_diss,color=condition,fill=condition))+
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=mean_diss-se,
                  ymax=mean_diss+se),alpha=.3) +
  xlab("normalized time (%)") +
  ylab("dissonance") +
  theme_classic()
  ggtitle('Dissonance over Normalized Time')

###################
# individual onsets over normal time
###################

head(onsets)
onsets <- onsets %>% group_by(player) %>% 
          mutate(n_t=ntile(time,10)) %>% ungroup()
onsets <- merge(onsets,summary)
onsets.sum <- onsets %>% group_by(player,n_t,condition) %>%
              summarise(density=mean(num_onsets,na.rm=TRUE)) %>% ungroup()
onsets.sum <- onsets.sum %>% group_by(n_t,condition) %>%
              summarise(mean_density=mean(density,na.rm=TRUE),
                        se=sd(density)/sqrt(n())) %>% ungroup()

ggplot(onsets.sum, aes(x=n_t*10,y=mean_density,color=condition,fill=condition)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=mean_density-se,
                  ymax=mean_density+se),alpha=.3) +
  ylab('Onset Density') +
  xlab('normalized time') +
  theme_classic()




########################
# Onsets vs. dissonance
########################
diss.onsets <- diss.onsets %>% group_by(player) %>% mutate(n_t=ntile(time,10)) %>% ungroup()

ggplot(filter(diss.onsets,n_t<4), aes(x=num_onsets,y=avg_dist)) +
  geom_point() +
  geom_smooth(method='lm')

ggplot(filter(diss.onsets,n_t==8), aes(x=num_onsets,y=avg_dist)) +
  geom_point() +
  geom_smooth(method='lm')


