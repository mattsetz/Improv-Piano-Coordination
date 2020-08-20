library(tidyverse)


# Read in data ------------------------------------------------------------

diss <- read.csv('Pipeline/tonal/diss-matrix/master-diss-matrix.csv')
summary <- read.csv('summary-combined.csv')
summary.ind <- read.csv('summary-individual.csv')
diss <- diss %>% na.omit()
diss <- merge(diss, select(summary,pair,condition))
diss$window <- factor(diss$window, labels = c("2 second window", "5 second window", "10 second window"))
diss <- diss %>% group_by(pair,window) %>% mutate(n_time1=ntile(time1,10),
                                                  n_time2=ntile(time2,10)) %>% ungroup()


diss.oneway <- diss %>% filter(condition=="one-way")
diss.oneway.ghost.live <- diss.oneway %>% filter(time1>time2)
diss.oneway.ghost.live$direction <- "ghost-to-live"
diss.oneway.simultaneous <- diss.oneway %>% filter(time1==time2)
diss.oneway.simultaneous$direction <- "simultaneuos"
diss.oneway.live.ghost <- diss.oneway %>% filter(time1<time2)
diss.oneway.live.ghost$direction <- "live-to-ghost"
diss.oneway <- rbind(diss.oneway.ghost.live,diss.oneway.live.ghost,diss.oneway.simultaneous)
diss.oneway$time_diff <- abs(diss.oneway$time1-diss.oneway$time2)
diss.oneway$lag <- diss.oneway$time1 - diss.oneway$time2
diss.oneway <- diss.oneway %>% group_by(pair) %>% mutate(n_time=ntile(time1,10)) %>% ungroup()
diss.oneway$diss.emerge <- diss.oneway$dissonance-((diss.oneway$diss1+diss.oneway$diss2)/2)

diss.coupled <- diss %>% filter(condition=="coupled")
diss.coupled.ghost.live <- diss.coupled %>% filter(time1>time2)
diss.coupled.ghost.live$direction <- "live-to-live" 
diss.coupled.simultaneous <- diss.coupled %>% filter(time1==time2)
diss.coupled.simultaneous$direction <- "simultaneuos"
diss.coupled.live.ghost <- diss.coupled %>% filter(time1<time2)
diss.coupled.live.ghost$direction <- "live-to-live"
diss.coupled <- rbind(diss.coupled.ghost.live,diss.coupled.live.ghost,diss.coupled.simultaneous)
diss.coupled$time_diff <- abs(diss.coupled$time1-diss.coupled$time2)
diss.coupled$lag <- diss.coupled$time1 - diss.coupled$time2
diss.coupled <- diss.coupled %>% group_by(pair) %>% mutate(n_time=ntile(time1,10)) %>% ungroup()
diss.coupled$diss.emerge <- diss.coupled$dissonance-((diss.coupled$diss1+diss.coupled$diss2)/2)

diss <- rbind(diss.coupled, diss.oneway)
diss <- diss %>% rename(combined_dissonance=dissonance)
write.csv(diss,'master-diss-matrix.csv',row.names = FALSE)
View(diss)

# Sanity Check ------------------------------------------------------------

ggplot(diss, aes(x=lag,color=condition)) + geom_density(alpha=.2) + facet_wrap(~n_time1)
ggplot(diss, aes(x=lag,color=condition)) + geom_density(alpha=.2) + facet_wrap(~n_time2)
"
lags are represented unevenly in first and last decile. so these should be removed from the analysis.
"

# Visualize combined dissonance by lag -------------------------------------------
diss.coupled.abbrv <- diss.coupled %>% filter(n_time1>1,n_time1<10,n_time2>1,n_time2<10) %>% group_by(pair,lag,condition,window,direction) %>% summarise(dissonance=mean(dissonance,na.rm = TRUE),diss.emerge=mean(diss.emerge,na.rm = TRUE)) %>% ungroup()
diss.oneway.abbrv <- diss.oneway %>% filter(n_time1>1,n_time1<10,n_time2>1,n_time2<10) %>% group_by(pair,lag,condition,window,direction) %>% summarise(dissonance=mean(dissonance,na.rm = TRUE),diss.emerge=mean(diss.emerge,na.rm = TRUE)) %>% ungroup()

# one-way
ggplot(diss.oneway.abbrv, aes(x=lag,y=2-dissonance,color=direction)) + stat_summary() +geom_smooth(method=lm,se=TRUE) +xlab("lag (sec)") +ylab("Combined Consonance") +facet_wrap(~window, scales = "free") +theme_bw() +theme(legend.position = "bottom")
# coupled
ggplot(diss.coupled.abbrv, aes(x=lag,y=2-dissonance)) +stat_summary() +geom_smooth(data = filter(diss.coupled.abbrv,lag<0),method=lm,se=TRUE) +geom_smooth(data = filter(diss.coupled.abbrv,lag>0),method=lm,se=TRUE) +xlab("lag (sec)") +ylab("Combined Consonance") + facet_wrap(~window,scales="free") + theme_bw() + theme(legend.position="bottom")


# Visualize emergent dissonance by lag ------------------------------------
# one-way
ggplot(diss.oneway.abbrv, aes(x=lag,y=-diss.emerge,color=direction)) + stat_summary() +geom_smooth(method=lm,se=TRUE) +xlab("lag (sec)") +ylab("Emergent Consonance") +facet_wrap(~window, scales = "free") +theme_bw() +theme(legend.position = "bottom")
# coupled
ggplot(diss.coupled.abbrv, aes(x=lag,y=-diss.emerge)) +stat_summary() +geom_smooth(data = filter(diss.coupled.abbrv,lag<0),method=lm,se=TRUE) +geom_smooth(data = filter(diss.coupled.abbrv,lag>0),method=lm,se=TRUE) +xlab("lag (sec)") +ylab("Emergent Consonance") + facet_wrap(~window,scales="free") + theme_bw() + theme(legend.position="bottom")

"
In all cases consonance peaks at around 0 (simultaneous time points), and then falls away as you look at greater lags. It would be
interesting to look at this across greater ranges of lags. This is to be expected. Musicians harmonize at simultaneous time points. There
is some tonal memory, becuase they play in a given tonal area for some time, but after some lags this tends to shrink fall. Pretty cool.
I guess you could compute these slopes and do a statistical test to verify that they are indeed negative (sig non-zero), but that seems
like overkill kind of.

ghost->live > live-> ghost for combined and emergent consonance it looks like.
there appears to be a lot of variance though. (looked way better before I averaged within pieces and took SE overall!).
"

# coupled versus oneway
diss.abbrv <- rbind(diss.oneway.abbrv,diss.coupled.abbrv)
# emergent consonance; 5 sec window
library(cowplot)
plt <- ggplot(filter(diss.abbrv,window=="5 second window"), aes(x=lag,y=-diss.emerge)) + stat_summary(geom="point",aes(color=condition)) + stat_summary(geom="ribbon",alpha=.4,aes(fill=condition)) +  xlab("lag (sec)")+ylab("Emergent Consonance")+theme_bw()+theme(legend.position = "bottom") +
  geom_smooth(method=lm,se=FALSE,aes(group=direction,color=condition))

ggdraw(plt) + draw_text("High Consonance",x=.27,y=.95,size = 7) + draw_text("Low Consonance",x=.27,y=.275,size = 7)

t.test(filter(diss.abbrv,window=="5 second window",condition=="coupled",lag==0)$diss.emerge,
       filter(diss.abbrv,window=="5 second window",condition=="one-way",lag==0)$diss.emerge)

"
more emergent consonance in coupled versus one-way (we already knew that). Also, there appears to be the above asymmetry
for one-way pieces discussed above.
"


# Visualize lagged combined and emergent consonance -----------------------------

head(diss.oneway.abbrv)
head(diss.coupled.abbrv)
diss.abbrv <- rbind(diss.coupled.abbrv,diss.oneway.abbrv)
diss.abbrv[diss.abbrv$direction=="ghost->live",]$direction = "ghost-to-live"
diss.abbrv[diss.abbrv$direction=="live->ghost",]$direction = "live-to-ghost"
diss.abbrv[diss.abbrv$direction=="live-live",]$direction = "live-to-live"

ggplot(filter(diss.abbrv,window=="5 second window"), aes(x=lag,y=1.8-dissonance,color=condition)) + stat_summary() + theme_bw() + theme(legend.position = "bottom") + ylab("Combined Consonance")
ggplot(filter(diss.abbrv,window=="5 second window"), aes(x=lag,y=1.8-dissonance,fill=direction,color=direction)) + stat_summary() + facet_wrap(~condition,scales="free") + theme_bw() + theme(legend.position = "bottom") + ylab("Combined Consonance") + xlab("Lag (sec)")
ggplot(filter(diss.abbrv,window=="5 second window"), aes(x=lag,y=2-dissonance)) + stat_summary(geom="ribbon",alpha=.3) + stat_summary(geom="point") + facet_wrap(~condition,scales="free") + theme_bw() + theme(legend.position = "bottom") + ylab("Combined Consonance") + xlab("Lag (sec)")
ggplot(filter(diss.abbrv,window=="5 second window"), aes(x=lag,y=-diss.emerge)) + stat_summary(geom="ribbon",alpha=.3) + stat_summary(geom="point") + facet_wrap(~condition,scales="free") + theme_bw() + theme(legend.position = "bottom") + ylab("Emergent Consonance") + xlab("Lag (sec)")

# Stat test of ghost-live vs live-ghost (within piece) --------------------------------------------------------
diss.abbrv.avg <- diss.abbrv %>% group_by(condition,pair,window,direction) %>% summarise(dissonance=mean(dissonance,na.rm = TRUE),diss.emerge=mean(diss.emerge,na.rm=TRUE)) %>% ungroup()
diss.abbrv.wide <- diss.abbrv.avg %>% pivot_wider(names_from = direction, values_from = c("dissonance","diss.emerge"))


t.test(filter(diss.abbrv.wide,condition=="coupled",window=="2 second window")$`dissonance_live1->live2`,
       filter(diss.abbrv.wide,condition=="coupled",window=="2 second window")$`dissonance_live2->live1`,paired = TRUE)

"
Results from above t-test:
10 second window, emergent consonance:
  diff of means = 0.0026; t(85)=-2.38; p=0.019.
5 second window, emergent consonance:
  diff of means = 0.0026; t(85)=-2.29; p=0.025.
2 second window, emergent consonance:
  diff of means = 0.0029; t(85)=-2.56; p=0.012.
  
10 second window, combined consonance:
  diff of means = 0.0026; t(85)=-2.45; p=0.016.
5 second window, combined consonance:
  diff of means = 0.0026; t(85)=-2.50; p=0.014.
2 second window, combined consonance:
  diff of means = 0.00397; t(85)=-2.78; p=0.0067.

So very cool. This demonstrates that live musicians harmonize with the past notes of the recording (but not vice versa).

None of these tests are sig for coupled trials (sanity check).

But what do these distributions actually look like?
"

diss.abbrv.wide$diss_emerge_diff = diss.abbrv.wide$`diss.emerge_ghost->live`-diss.abbrv.wide$`diss.emerge_live->ghost`
diss.abbrv.wide$diss_combined_diff = diss.abbrv.wide$`dissonance_ghost->live`-diss.abbrv.wide$`dissonance_live->ghost`

ggplot(filter(diss.abbrv.wide,condition=="one-way"), aes(x=-diss_emerge_diff)) + geom_histogram() + facet_wrap(~window) + xlab("emergent consonance\nghost-live minus live-ghost") + theme_bw() + scale_y_continuous(breaks=1:10)
ggplot(filter(diss.abbrv.wide,condition=="one-way"), aes(x=-diss_combined_diff)) + geom_histogram() + facet_wrap(~window) + xlab("emergent consonance\nghost-live minus live-ghost") + theme_bw() + scale_y_continuous(breaks=1:10)

"
I wanted to see if these were normal distributions, because I believe this is an assumption of the above t-test.
Emergent consonance plots appear to be more or less normally distributed (though there are some outliers).
Combined consonance distributions look to be more dominated by an outlier. But even disregarding this point, the means look just a bit to the right of 0.

So my hesitation is more or less at ease now. Thing is, I might be shooting myself in the foot with this analysis (even though it worked
out in my favor), because I'm including points up to 20 seconds out. This might wash out some of the effects. So now I'll do the 
same analysis on just the range of +/-6 seconds.
"



# Stat test of ghost-live versus live-ghost (restricted lag range) -------------------------------------
diss.abbrv.avg <- diss.abbrv %>% filter(abs(lag)<=10) %>% group_by(condition,pair,window,direction) %>% summarise(dissonance=mean(dissonance,na.rm = TRUE),diss.emerge=mean(diss.emerge,na.rm=TRUE)) %>% ungroup()
diss.abbrv.wide <- diss.abbrv.avg %>% pivot_wider(names_from = direction, values_from = c("dissonance","diss.emerge"))

diss.abbrv.wide$diss_emerge_diff = diss.abbrv.wide$`diss.emerge_ghost->live`-diss.abbrv.wide$`diss.emerge_live->ghost`
diss.abbrv.wide$diss_combined_diff = diss.abbrv.wide$`dissonance_ghost->live`-diss.abbrv.wide$`dissonance_live->ghost`

ggplot(filter(diss.abbrv.wide,condition=="one-way"), aes(x=-diss_emerge_diff)) + geom_histogram() + facet_wrap(~window) + xlab("emergent consonance within pieces\n(ghost-live minus live-ghost)") + theme_bw() + scale_y_continuous(breaks=1:100)
ggplot(filter(diss.abbrv.wide,condition=="one-way"), aes(x=-diss_combined_diff)) + geom_histogram() + facet_wrap(~window) + xlab("combined consonance within pieces\n(ghost-live minus live-ghost)") + theme_bw() + scale_y_continuous(breaks=1:100)

t.test(filter(diss.abbrv.wide,condition=="one-way",window=="5 second window")$`diss.emerge_ghost->live`,
       filter(diss.abbrv.wide,condition=="one-way",window=="5 second window")$`diss.emerge_live->ghost`,paired = TRUE)

"
These plots are encouraging. Looks like the effect I found above is robust. They kind of fall apart
at 10 second windows, but thats okay.

Results of above paired-t:
Emergent Consonance, 2 second window:
  mean of differences = 0.0029; paired-t(85)=-3.76; p=0.00031

Emergent Consonance, 5 second window:
  mean of differences = 0.002; paired-t(85)=-3.097; p=0.0026

Results of analogous test are non-sig for coupled (sanity  check).
"



# Slope analysis ----------------------------------------------------------

diss.abbrv <- diss %>% filter(n_time1>1,n_time1<10,n_time2>1,n_time2<10) %>% group_by(pair,condition,window,direction) %>% mutate(dissonance=mean(dissonance,na.rm = TRUE),diss.emerge=mean(diss.emerge,na.rm = TRUE)) %>% ungroup()
# include 0 in here
diss.tmp <- diss.abbrv %>% filter(direction=="simultaneuos",condition=="coupled")
diss.tmp$direction <- "live1->live2"
diss.abbrv <- rbind(diss.abbrv,diss.tmp)
diss.tmp$direction <- "live2->live1"
diss.abbrv <- rbind(diss.abbrv,diss.tmp)
diss.tmp <- diss.abbrv %>% filter(direction=="simultaneuos",condition=="one-way")
diss.tmp$direction <- "ghost->live"
diss.abbrv <- rbind(diss.abbrv,diss.tmp)
diss.tmp$direction <- "live->ghost"
diss.abbrv <- rbind(diss.abbrv,diss.tmp)

diss_slopes <- diss.abbrv %>% group_by(pair,window,direction,condition) %>% 
  summarise(diss_comb_slope=lm(dissonance ~ abs(lag))$coefficients[[2]],
            diss_emerge_slope=lm(diss.emerge ~ abs(lag))$coefficients[[2]])

ggplot(filter(diss_slopes,direction!="simultaneuos"), aes(x=-diss_comb_slope,fill=direction)) + geom_density(alpha=.2) + facet_wrap(~condition+window) + ylab("count") + xlab("slopes: combined consonance ~ lag") + theme_bw() + theme(legend.position="bottom") + scale_y_continuous(1:100)
ggplot(filter(diss_slopes,direction!="simultaneuos"), aes(x=-diss_emerge_slope,fill=direction)) + geom_density(alpha=.2) + facet_wrap(~condition+window) + ylab("count") + xlab("slopes: emergent consonance ~ lag") + theme_bw() + theme(legend.position="bottom") + scale_y_continuous(1:100)

"
Interesting, looks like these slopes are distributed around 0, when I would have guessed they would be negative
(for consonance decreasing as you get away from 0). Perhaps the issue is that +/- 20 seconds isn't long enough of a range.
"

# Check if above results are artifacts of collapsed onset density ----

onsets_collapsed <- read.csv('Pipeline/rhythm/onset-density/collapsed/master-onset-density-individual.csv')
lagged_onsets <- NULL
for (p in unique(onsets_collapsed$player)) {
  print(p)
  onsets_p <- onsets_collapsed %>% filter(player==p) %>% rename(player_onsets=num_onsets)
  onsets_other <- onsets_collapsed %>% filter(player==as.character(onsets_p$other[1])) %>% rename(other_onsets=num_onsets) %>% select(time,other_onsets)
  onsets_pair <- merge(onsets_p,onsets_other,all=TRUE)
  onsets_pair$player_onsets <- replace_na(onsets_pair$player_onsets,0)
  onsets_pair$other_onsets <- replace_na(onsets_pair$other_onsets,0)
  for (l in seq(-20*5,20*5,2*5)) {
    onsets_pair_tmp <- onsets_pair
    onsets_pair_tmp$other_onsets <- shift(onsets_pair$other_onsets,l)
    onsets_pair_tmp$time_other <- shift(onsets_pair$time,l)
    onsets_pair_tmp <- onsets_pair_tmp %>% mutate(ntime=ntile(time,10),ntime_other=ntile(time_other,10)) %>% ungroup() %>% filter(ntime>1,ntime<10,ntime_other>1,ntime_other<10)
    mean_onsets <- mean(onsets_pair_tmp$player_onsets+onsets_pair_tmp$other_onsets,na.rm = TRUE)
    lagged_onsets <- rbind(lagged_onsets, data.frame(player=p,lag=l/5,onsets=mean_onsets))
  }
}

summary <- read.csv("summary-individual.csv")
lagged_onsets <- merge(lagged_onsets,select(summary,player,condition))

ggplot(lagged_onsets, aes(x=lag,y=onsets))+stat_summary()+theme_bw()+facet_wrap(~condition)

lagged_onsets$direction <- ifelse(lagged_onsets$lag<0,"neg",ifelse(lagged_onsets$lag>0,"pos","simultaneous"))
lagged_onsets_summary <- lagged_onsets %>% group_by(player,condition,direction) %>% summarise(onsets=mean(onsets))
head(lagged_onsets_summary)
lagged_onsets_wide <- lagged_onsets_summary %>% pivot_wider(names_from = direction,values_from = onsets)
head(lagged_onsets_wide)
t.test(filter(lagged_onsets_wide,condition=="one-way")$neg,filter(lagged_onsets_wide,condition=="one-way")$pos,paired=TRUE)

"
So far it looks like there is no effect of direction on total onset density in one-way trials.
But this matrix analysis is incomplete because, earlier time windows of ghost recording are underrepresented
in negative lags (and vice versa with late time windows). This is corrected for below...
"

lagged_onsets_opp <- NULL
for (p in unique(onsets_collapsed$player)) {
  print(p)
  onsets_p <- onsets_collapsed %>% filter(player==p) %>% rename(player_onsets=num_onsets)
  onsets_other <- onsets_collapsed %>% filter(player==as.character(onsets_p$other[1])) %>% rename(other_onsets=num_onsets) %>% select(time,other_onsets)
  onsets_pair <- merge(onsets_p,onsets_other,all=TRUE)
  onsets_pair$player_onsets <- replace_na(onsets_pair$player_onsets,0)
  onsets_pair$other_onsets <- replace_na(onsets_pair$other_onsets,0)
  for (l in seq(-20*5,20*5,2*5)) {
    onsets_pair_tmp <- onsets_pair
    onsets_pair_tmp$player_onsets <- shift(onsets_pair$player_onsets,l)
    onsets_pair_tmp$time_player <- shift(onsets_pair$time,l)
    onsets_pair_tmp$time_other <- onsets_pair_tmp$time
    onsets_pair_tmp <- onsets_pair_tmp %>% mutate(ntime=ntile(time_player,10),ntime_other=ntile(time_other,10)) %>% ungroup() %>% filter(ntime>1,ntime<10,ntime_other>1,ntime_other<10)
    mean_onsets <- mean(onsets_pair_tmp$player_onsets+onsets_pair_tmp$other_onsets,na.rm = TRUE)
    lagged_onsets_opp <- rbind(lagged_onsets_opp, data.frame(player=p,lag=l/5,onsets=mean_onsets))
  }
}
lagged_onsets_opp$lag <- -1*lagged_onsets_opp$lag
summary <- read.csv("summary-individual.csv")
lagged_onsets_opp <- merge(lagged_onsets_opp,select(summary,player,condition))

ggplot(lagged_onsets_opp, aes(x=lag,y=onsets))+stat_summary()+theme_bw()+facet_wrap(~condition)

lagged_onsets_opp$direction <- ifelse(lagged_onsets_opp$lag<0,"neg",ifelse(lagged_onsets_opp$lag>0,"pos","simultaneous"))
lagged_onsets_total <- rbind(lagged_onsets,lagged_onsets_opp)
lagged_onsets_total <- lagged_onsets_total %>% group_by(player,lag,condition,direction) %>% summarise(onsets=mean(onsets)) %>% ungroup()

ggplot(lagged_onsets_total, aes(x=lag,y=onsets))+stat_summary()+theme_bw()+facet_wrap(~condition)
lagged_onsets_summary <- lagged_onsets_total %>% group_by(player,condition,direction) %>% summarise(onsets=mean(onsets))
head(lagged_onsets_summary)
lagged_onsets_wide <- lagged_onsets_summary %>% pivot_wider(names_from = direction,values_from = onsets)
head(lagged_onsets_wide)
t.test(filter(lagged_onsets_wide,condition=="one-way")$neg,filter(lagged_onsets_wide,condition=="one-way")$pos,paired=TRUE)

"
Good news, there appears to be no confound of onset density in our consonance matrix analysis. If there
were, we would expect less onsets in ghost-->live compared to live-->ghost overall. 

Results of paired t-test.

	Paired t-test

data:  filter(lagged_onsets_wide, condition == one-way)$neg and filter(lagged_onsets_wide, condition == "one-way")$pos
t = 1.0773, df = 85,
p-value = 0.2844
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.03314095  0.11152565
sample estimates:
mean of the differences 
             0.03919235 
             
By the way, it should be noted that this analysis was conducted with collapsed onset density. Still
could be that there is a confound with raw onset density, but I'm not sure that I'm willing to test that right now.
"

# Check if above results are artifacts of raw onset density ----

onsets_collapsed <- read.csv('Pipeline/rhythm/onset-density/total/master-raw-onset-density-individual.csv')
summary <- read.csv("summary-individual.csv")
summary$other <- str_remove(as.character(summary$pair),as.character(summary$player))
onsets_collapsed <- merge(onsets_collapsed, dplyr::select(summary,player,other))
lagged_onsets <- NULL
for (p in unique(onsets_collapsed$player)) {
  print(p)
  onsets_p <- onsets_collapsed %>% filter(player==p) %>% rename(player_onsets=num_onsets)
  onsets_other <- onsets_collapsed %>% filter(player==as.character(onsets_p$other[1])) %>% rename(other_onsets=num_onsets) %>% select(time,other_onsets)
  onsets_pair <- merge(onsets_p,onsets_other,all=TRUE)
  onsets_pair$player_onsets <- replace_na(onsets_pair$player_onsets,0)
  onsets_pair$other_onsets <- replace_na(onsets_pair$other_onsets,0)
  for (l in seq(-20*5,20*5,2*5)) {
    onsets_pair_tmp <- onsets_pair
    onsets_pair_tmp$other_onsets <- shift(onsets_pair$other_onsets,l)
    onsets_pair_tmp$time_other <- shift(onsets_pair$time,l)
    onsets_pair_tmp <- onsets_pair_tmp %>% mutate(ntime=ntile(time,10),ntime_other=ntile(time_other,10)) %>% ungroup() %>% filter(ntime>1,ntime<10,ntime_other>1,ntime_other<10)
    mean_onsets <- mean(onsets_pair_tmp$player_onsets+onsets_pair_tmp$other_onsets,na.rm = TRUE)
    lagged_onsets <- rbind(lagged_onsets, data.frame(player=p,lag=l/5,onsets=mean_onsets))
  }
}

summary <- read.csv("summary-individual.csv")
lagged_onsets <- merge(lagged_onsets,select(summary,player,condition))

ggplot(lagged_onsets, aes(x=lag,y=onsets))+stat_summary()+theme_bw()+facet_wrap(~condition)

lagged_onsets$direction <- ifelse(lagged_onsets$lag<0,"neg",ifelse(lagged_onsets$lag>0,"pos","simultaneous"))
lagged_onsets_summary <- lagged_onsets %>% group_by(player,condition,direction) %>% summarise(onsets=mean(onsets))
head(lagged_onsets_summary)
lagged_onsets_wide <- lagged_onsets_summary %>% pivot_wider(names_from = direction,values_from = onsets)
head(lagged_onsets_wide)
t.test(filter(lagged_onsets_wide,condition=="one-way")$neg,filter(lagged_onsets_wide,condition=="one-way")$pos,paired=TRUE)



lagged_onsets_opp <- NULL
for (p in unique(onsets_collapsed$player)) {
  print(p)
  onsets_p <- onsets_collapsed %>% filter(player==p) %>% rename(player_onsets=num_onsets)
  onsets_other <- onsets_collapsed %>% filter(player==as.character(onsets_p$other[1])) %>% rename(other_onsets=num_onsets) %>% select(time,other_onsets)
  onsets_pair <- merge(onsets_p,onsets_other,all=TRUE)
  onsets_pair$player_onsets <- replace_na(onsets_pair$player_onsets,0)
  onsets_pair$other_onsets <- replace_na(onsets_pair$other_onsets,0)
  for (l in seq(-20*5,20*5,2*5)) {
    onsets_pair_tmp <- onsets_pair
    onsets_pair_tmp$player_onsets <- shift(onsets_pair$player_onsets,l)
    onsets_pair_tmp$time_player <- shift(onsets_pair$time,l)
    onsets_pair_tmp$time_other <- onsets_pair_tmp$time
    onsets_pair_tmp <- onsets_pair_tmp %>% mutate(ntime=ntile(time_player,10),ntime_other=ntile(time_other,10)) %>% ungroup() %>% filter(ntime>1,ntime<10,ntime_other>1,ntime_other<10)
    mean_onsets <- mean(onsets_pair_tmp$player_onsets+onsets_pair_tmp$other_onsets,na.rm = TRUE)
    lagged_onsets_opp <- rbind(lagged_onsets_opp, data.frame(player=p,lag=l/5,onsets=mean_onsets))
  }
}
lagged_onsets_opp$lag <- -1*lagged_onsets_opp$lag
summary <- read.csv("summary-individual.csv")
lagged_onsets_opp <- merge(lagged_onsets_opp,select(summary,player,condition))

ggplot(lagged_onsets_opp, aes(x=lag,y=onsets))+stat_summary()+theme_bw()+facet_wrap(~condition)

lagged_onsets_opp$direction <- ifelse(lagged_onsets_opp$lag<0,"neg",ifelse(lagged_onsets_opp$lag>0,"pos","simultaneous"))
lagged_onsets_total <- rbind(lagged_onsets,lagged_onsets_opp)
lagged_onsets_total <- lagged_onsets_total %>% group_by(player,lag,condition,direction) %>% summarise(onsets=mean(onsets)) %>% ungroup()

ggplot(lagged_onsets_total, aes(x=lag,y=onsets))+stat_summary()+theme_bw()+facet_wrap(~condition)
lagged_onsets_summary <- lagged_onsets_total %>% group_by(player,condition,direction) %>% summarise(onsets=mean(onsets))
head(lagged_onsets_summary)
lagged_onsets_wide <- lagged_onsets_summary %>% pivot_wider(names_from = direction,values_from = onsets)
head(lagged_onsets_wide)
t.test(filter(lagged_onsets_wide,condition=="coupled")$neg,filter(lagged_onsets_wide,condition=="coupled")$pos,paired=TRUE)



head(diss.coupled)
a1b1 <- diss.coupled %>% filter(pair=="a1b1",window=="5 second window")
ggplot(a1b1, aes(x=time1,y=time2)) + geom_tile(aes(fill=1.8-dissonance)) + theme_bw()

# construct toy consonance matrix
time1 <- seq(1,150,10)
time2 <- seq(2,150,10)

get_cons <- function(time1, time2) {
  return(rnorm(1,abs(time1-time2),sd = .5))
}

toy_matrix <- NULL
for (t1 in time1) {
  for (t2 in time2) {
    print(t1)
    print(t2)
    toy_matrix = rbind(toy_matrix, data.frame("Time 1"= t1,"Time 2"=t2,
                                              "Consonance"=get_cons(t1,t2)))
  }
}

toy_matrix$lag <- abs(toy_matrix$Time.1-toy_matrix$Time.2)
toy_matrix$Consonance <- (100-toy_matrix$Consonance)/1000
toy_matrix$Consonance2 <- toy_matrix$Consonance + rnorm(nrow(toy_matrix),0,.01)


ggplot(toy_matrix, aes(x=Time.1,y=Time.2)) + geom_tile(aes(fill=Consonance2)) + theme_bw() + xlab("Player A Time") + ylab("Player B Time")

