library(tidyverse)

diss <- read.csv('Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-individual.csv')
summary <- read.csv("summary-individual.csv")
diss <- merge(diss,summary)


# Compute Cross-Correlation -----------------------------------------------

cc.diss <- NULL
for (person in unique(diss$player)) {
  print(person)
  # subset 2 trials
  person <- diss %>% filter(player==person,window==2)
  other <- diss %>% filter(player==as.character(person$other[[1]]),window==2)
  
  cc <- ccf(person$avg_dist, other$avg_dist, lag.max = 450, plot=FALSE, na.action=na.pass)
  cc_i <- data.frame(cc = cc$acf,
                     lag = cc$lag,
                     player = person$player[[1]],
                     condition = person$condition[[1]])
  cc.diss <- rbind(cc.diss,cc_i)
}
cc.diss$lag <- .2*cc.diss$lag

head(cc.diss)
write.csv(x = cc.diss,file = "Pipeline/tonal/cc-dissonance-window2s.csv",row.names = FALSE)


# Visualize Cross-Correlation ---------------------------------------------

head(diss)

ggplot(cc.diss, aes(x=lag,y=cc,color=condition)) + stat_summary(alpha=.3) + xlab("lag (sec)") + ylab("cross-correlation of dissonance") + theme_bw() + theme(legend.position=c(.8,.8))
ggplot(filter(cc.diss,abs(lag)<20), aes(x=lag,y=cc,color=condition)) + stat_summary(alpha=.3) + xlab("lag (sec)") + ylab("cross-correlation of dissonance") + theme_bw() + theme(legend.position = c(.8,.8))
ggplot(filter(cc.diss,abs(lag)<5), aes(x=lag,y=cc,fill=condition)) + stat_summary(geom="ribbon",alpha=.4) + stat_summary(geom="point",aes(color=condition)) + xlab("lag (sec)") + ylab("cross-correlation of dissonance") + theme_bw() + theme(legend.position = c(.9,.9))

"
* looks like cc.coupled > cc.one-way for lags ~= [-5,0] sec. 
* looks like cc.ghost-live > cc.live-ghost, within the range of ~[-5,5] sec (which we already know from some t-tests).
* looks like both cc.coupled and cc.one-way converge at around 6 seconds, and -10 seconds.
* looks like correlation bottoms out (i.e. converges to 0) at around +/- 50 seconds for each condition. This tells us something about the memory of the system.
This is a summary plot. What do actually distributions look like?
"

head(cc.diss)
ggplot(filter(cc.diss,abs(lag)<5), aes(x=cc,color=condition,fill=condition)) + geom_density(position="dodge",alpha=.2) + facet_wrap(~lag)

"
Looks like cc.coupled > cc.one-way for negative lags (unsurprising) and possibly at 0. but its close at zero.
Maybe we should now consider these are yoked.
"


# Visualize Cross-Correlation Yolked --------------------------------------

cc.diss.avg <- cc.diss %>% group_by(lag,yolked_id,condition) %>% summarise(cc=mean(cc,na.rm = TRUE)) %>% ungroup()
cc.diss.avg.wide <- cc.diss.avg %>% spread(condition,cc)
cc.diss.avg.wide$cc_diff <- cc.diss.avg.wide$coupled-cc.diss.avg.wide$`one-way`

ggplot(filter(cc.diss.avg.wide,abs(lag)<5), aes(x=cc_diff)) + geom_density() + facet_wrap(~lag)



# Statistical Tests -------------------------------------------------------

head(cc.diss.avg.wide)

t.test(filter(cc.diss.avg.wide,lag==0)$coupled,filter(cc.diss.avg.wide,lag==0)$`one-way`,paired = TRUE) # t(42)=2.05; p = 0.047

"
cc.coupled > cc.one-way at simultaneous time points (paired-t(42)=2.05; p<0.05). 
From above plots it looks like this is true up until around .8 seconds.
"
