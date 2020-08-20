########################
# MS 5/2/19
# Analyze onset density
########################

library(tidyverse)

#######################################
# read in individual onset density files
#######################################

density <- read.csv('Pipeline/rhythm/onset-density/collapsed/master-onset-density-individual.csv')
summary <- read.csv('summary-individual.csv')
density <- merge(summary,density)

# visualize collapsed onset density ---------------------------------------------------------------
# zero density time series and create ntiles
density_tmp <- density %>% filter(num_onsets>0) %>% group_by(player) %>% summarise(start_time=min(time))
density_zero <- merge(density,density_tmp)
density_zero$time <- density_zero$time-density_zero$start_time
density_zero <- density_zero %>% filter(time>=0)
density_zero <- density_zero %>% group_by(player) %>% mutate(n_time10=ntile(time,10),n_time20=ntile(time,20),n_time50=ntile(time,50),n_time100=ntile(time,100))

density_avg <- density_zero %>% group_by(player,n_time10,condition) %>% summarise(num_onsets=mean(num_onsets))
ggplot(density_avg, aes(x=n_time10*10,y=num_onsets,color=condition,fill=condition)) + stat_summary(geom="ribbon",alpha=.2) + stat_summary(geom = "point") + stat_summary(geom="line") + theme_bw() + xlab("normalized time (%)") + ylab("Onset Density") + theme(legend.position = "bottom")
density_avg <- density_zero %>% group_by(player,n_time20,condition) %>% summarise(num_onsets=mean(num_onsets))
ggplot(density_avg, aes(x=n_time20*5,y=num_onsets,color=condition,fill=condition)) + stat_summary(geom="ribbon",alpha=.2) + stat_summary(geom = "point") + stat_summary(geom="line") + theme_bw() + xlab("normalized time (%)") + ylab("Onset Density") + theme(legend.position = "bottom")
density_avg <- density_zero %>% group_by(player,n_time50,condition) %>% summarise(num_onsets=mean(num_onsets))
ggplot(density_avg, aes(x=n_time50*2,y=num_onsets,color=condition,fill=condition)) + stat_summary(geom="ribbon",alpha=.2) + stat_summary(geom = "point") + stat_summary(geom="line") + theme_bw() + xlab("normalized time (%)") + ylab("Onset Density") + theme(legend.position = "bottom")
density_avg <- density_zero %>% group_by(player,n_time100,condition) %>% summarise(num_onsets=mean(num_onsets))
ggplot(density_avg, aes(x=n_time100,y=num_onsets,color=condition,fill=condition)) + stat_summary(geom="ribbon",alpha=.2) + stat_summary(geom = "point") + stat_summary(geom="line") + theme_bw() + xlab("normalized time (%)") + ylab("Onset Density") + theme(legend.position = "bottom")

"
These plots look very similar to emergent consonance over time. Basically the exact same. And I expect
there will be positive correlation between emergent consonance and individual onset density, bc more individual
notes will tend to have higher dissonance, which will normalize the emergent consonance measure to be high.

I think the dip in onset density in one-way trials is a result of people just giving up at this point in the piece.
"

# cross-correlation of collapsed onset density -------------------------------------------------------

cc_density <- NULL
for (person in unique(density$player)) {
  print(person)
  person <- density %>% filter(player==person)
  other <- density %>% filter(player==as.character(person$other[[1]]))
  max.len = max(length(person$num_onsets), length(other$num_onsets))
  person_density = c(person[order(person$time),"num_onsets"], rep(0, max.len-length(person$num_onsets)))
  other_density = c(other[order(other$time),"num_onsets"], rep(0, max.len-length(other$num_onsets)))
  cc <- ccf(person_density, other_density, lag.max=800, plot=FALSE)
  cc_i <- data.frame(cc = cc$acf,
                     lag = cc$lag,
                     player = person$player[[1]],
                     pair = person$pair[[1]],
                     condition = person$condition[[1]])
  cc_density <- rbind(cc_density, cc_i)
}
head(cc_density)
cc_density$lag <- .2*cc_density$lag

# visualize cc by condition, collapsing over lags
ggplot(filter(cc_density,abs(lag)<6), aes(x=lag,y=cc,color=condition,fill=condition)) + stat_summary() + theme_bw() + theme(legend.position = "bottom") + xlab("lag (sec)") + ylab("cross-correlation of onset density")
ggplot(cc_density, aes(x=lag,y=cc,color=condition,fill=condition)) + stat_summary() + theme_bw() + theme(legend.position = c(.8,.8)) + xlab("lag (sec") + ylab("cross-correlation of onset density")

"
* coupled >> oneway
* ghost-live >> live-ghost
I wonder how much these results have to do with the fact that one-way players give up 3/4s through the piece.

it looks like live musicians are more correlated at 2 second lag versus simultaneous time points. is this true?

Strange that cc becomes negative for very large lags. I observed this in consonance too. 
Maybe it has to do with the fact that density increases at the beginning of pieces but decreases at the end.
"

cc_coupled <- rbind(filter(cc_density,lag==0,condition=="coupled"),filter(cc_density,lag==2.2,condition=="coupled"))
cc_coupled$lag <- factor(cc_coupled$lag)
ggplot(cc_coupled, aes(x=cc,color=lag,fill=lag))+geom_density(alpha=.2) + theme_bw()

"
No, it doesn't seem like there is actually more cc at 2 second lags. They are about even.
"



# cross-correlation of raw onset density ----------------------------------

library(tidyverse)
density <- read.csv('Pipeline/rhythm/onset-density/total/master-raw-onset-density-individual.csv')
summary <- read.csv('summary-individual.csv')
density <- merge(summary,density)
density$other <- str_remove(as.character(density$pair),as.character(density$player))

cc_density <- NULL
for (person in unique(density$player)) {
  print(person)
  person <- density %>% filter(player==person)
  other <- density %>% filter(player==as.character(person$other[[1]]))
  max.len = max(length(person$num_onsets), length(other$num_onsets))
  person_density = c(person[order(person$time),"num_onsets"], rep(0, max.len-length(person$num_onsets)))
  other_density = c(other[order(other$time),"num_onsets"], rep(0, max.len-length(other$num_onsets)))
  cc <- ccf(person_density, other_density, lag.max=800, plot=FALSE)
  cc_i <- data.frame(cc = cc$acf,
                     lag = cc$lag,
                     player = person$player[[1]],
                     pair = person$pair[[1]],
                     condition = person$condition[[1]])
  cc_density <- rbind(cc_density, cc_i)
}
head(cc_density)
cc_density$lag <- .2*cc_density$lag
cc_density$direction <- ifelse(cc_density$lag>0,"positive",ifelse(cc_density$lag<0,"negative","simultaneous"))

# visualize cc by lag
ggplot(filter(cc_density,abs(lag)<10), aes(x=lag,y=cc,fill=condition)) + stat_summary(geom="ribbon",alpha=.4) + stat_summary(geom="point",aes(color=condition)) + theme_bw() + theme(legend.position = "bottom") + xlab("lag (sec)") + ylab("Cross-Correlation of Onset Density")
ggplot(cc_density, aes(x=lag,y=cc,fill=condition)) + stat_summary(geom="ribbon",alpha=.4) + stat_summary(geom="point",aes(color=condition)) + theme_bw() + theme(legend.position = c(.8,.8)) + xlab("lag (sec)") + ylab("Cross-Correlation of Onset Density")


"
It appears that:
(0) overall cc is greater than 0
(1) ghost --> live > live --> ghost
(2) live --> live > ghost --> live
(3) CC dips at 0 for coupled trials (maybe)
"

# Statistical test for (0)
head(cc_density)
cc_avg <- cc_density %>% filter(abs(lag)<=20) %>% group_by(pair) %>%
  summarise(cc=mean(cc))
t.test(cc_avg$cc)

# Statistical test for (1)
cc_density_wide <- cc_density %>% filter(condition=="one-way",abs(lag)<10,lag!=0) %>%
  group_by(player,direction) %>% summarise(cc=mean(cc)) %>% 
  pivot_wider(names_from = "direction", values_from = cc)

t.test(cc_density_wide$positive, cc_density_wide$negative, paired = TRUE)
"
	Paired t-test

data:  cc_density_wide$positive and cc_density_wide$negative
t = 5.2099, df = 85, p-value = 1.302e-06
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 0.02414677 0.05395208
sample estimates:
mean of the differences 
             0.03904943 
"

# Statistical test for (2)
cc_density_avg <- cc_density %>% filter(abs(lag)<10) %>% group_by(pair,condition) %>% summarise(cc=mean(cc))
t.test(filter(cc_density_avg,condition=="coupled")$cc,
       filter(cc_density_avg,condition=="one-way")$cc,paired = FALSE)
"
	Welch Two Sample t-test

data:  filter(cc_density_avg, condition == coupled)$cc and filter(cc_density_avg, condition == one-way)$cc
t = 2.6562, df = 99.629, p-value = 0.009203
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 0.03151451 0.21755923
sample estimates:
mean of x mean of y 
0.4285617 0.3040248
"

"
Things you could still do:
- You still haven't tested (3), although I believe I tested this before and it wasn't sig.
- model cc as continuous function of lag and lag^2.
"

# Relationship between emergent consonance and raw onset density --------------
inDir <- "Pipeline/rhythm/onset-density/total/individual"
files <- list.files(inDir)
density_raw <- NULL
for (f in files) {
  print(f)
  df <- read.csv(paste(inDir,f,sep="/"))
  df$player = str_split(f,"[.]")[[1]][3]
  density_raw <- rbind(density_raw,df)
}

write.csv(density_raw, "Pipeline/rhythm/onset-density/total/master-raw-onset-density-individual.csv",row.names = FALSE)
density_raw <- read.csv("Pipeline/rhythm/onset-density/total/master-raw-onset-density-individual.csv")
summary <- read.csv("summary-individual.csv")
density_raw <- merge(density_raw, select(summary,player,condition))

diss <- read.csv("Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-individual.csv")
diss$time <- round(diss$time,1)
density_raw$time <- round(density_raw$time,1)

diss_density <- merge(select(filter(diss,window==2),player,time,avg_dist),select(density_raw,condition,player,time,num_onsets))
diss_density <- diss_density %>% drop_na()

diss_density_avg <- diss_density %>% group_by(player) %>% mutate(ntime100=ntile(time,100)) %>% ungroup() %>% group_by(player,condition,ntime100) %>% summarise(diss=mean(avg_dist),num_onsets=mean(num_onsets))
ggplot(diss_density_avg, aes(x=round(num_onsets),y=2-diss)) + geom_point(alpha=.2) + theme_bw() + xlab("Total Onset Density") + ylab("Consonance")
ggplot(filter(diss_density_avg,num_onsets<45), aes(x=num_onsets,y=1.8-diss)) + stat_bin_2d() + geom_smooth(method=lm) + theme_bw() + xlab("Total Onset Density") + ylab("Consonance")
ggplot(filter(diss_density_avg,num_onsets<45), aes(x=round(num_onsets),y=1.8-diss)) + geom_point(alpha=.1) + geom_smooth(method=lm) + theme_bw() + xlab("Total Onset Density") + ylab("Consonance")

cor.test(diss_density$avg_dist, diss_density$num_onsets)

"
	Pearson's product-moment correlation

data:  diss_density$avg_dist and diss_density$num_onsets
t = 222.73, df = 299466, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.3739107 0.3800559
sample estimates:
      cor 
0.3769875

In other words, there is a significant negative correlation between onset density and consonance.
For low levels of onset density (which seems to be the norm) there is a good range of consonance values though.

Hmm, probably this correlation is a good thing (it would be weird if they weren't correlated in this 
fashion) ... but there is a concern that our Emergent Consonance results are artifactual of the above-noted
trend that musicians play more notes in coupled trials. This would generally lead to more individual-level
consonance, which would in turn contribute to high Emergent Consonance.
"


# Consonance versus collapsed onset density -------------------------------
density$time <- round(density$time,1)
diss_density <- merge(select(filter(diss,window==2),player,time,avg_dist),select(density,condition,player,time,num_onsets))
diss_density <- diss_density %>% drop_na()

diss_density_avg <- diss_density %>% group_by(player) %>% mutate(ntime100=ntile(time,100)) %>% ungroup() %>% group_by(player,condition,ntime100) %>% summarise(diss=mean(avg_dist),num_onsets=mean(num_onsets))
ggplot(diss_density_avg, aes(x=num_onsets,y=1.8-diss)) + stat_bin_2d() + geom_smooth(method=lm) + theme_bw() + xlab("Collapsed Onset Density") + ylab("Consonance")
ggplot(diss_density_avg, aes(x=round(num_onsets),y=1.8-diss)) + geom_point(alpha=.2) + geom_smooth(method=lm) + theme_bw() + xlab("Collapsed Onset Density") + ylab("Consonance")

cor.test(diss_density$avg_dist, diss_density$num_onsets)

"
	Pearson's product-moment correlation

data:  diss_density$avg_dist and diss_density$num_onsets
t = 222.82, df = 298633, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.3744791 0.3806297
sample estimates:
      cor 
0.3775586

Same results as above.
"




# Emergent consonance versus raw onset density ####
diss.combined <- read.csv('Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-combined.csv')
summary <- read.csv('summary-combined.csv')
diss.combined <- merge(diss.combined, summary)
diss.ind <- read.csv('Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-individual.csv')

diss <- NULL
for (p in unique(diss.combined$pair)) {
  print(p)
  p1 <- str_extract_all(p,'[a-z]{1,2}[0-9]')[[1]][[1]]
  p2 <- str_extract_all(p,'[a-z]{1,2}[0-9]')[[1]][[2]]
  
  p12.diss <- diss.combined %>% filter(pair==p) %>% 
    select(-max_dist) %>% rename(diss.comb=avg_dist)
  p1.diss <- diss.ind %>% filter(player==p1) %>% 
    select(-max_dist,-player,-other) %>% rename(diss.1=avg_dist)
  p2.diss <- diss.ind %>% filter(player==p2) %>% 
    select(-max_dist,-player,-other) %>% rename(diss.2=avg_dist)
  diss.p <- merge(p12.diss, p1.diss, all.x = TRUE)
  diss.p <- merge(diss.p, p2.diss, all.x = TRUE)
  diss.p$diss.emerge <- diss.p$diss.comb - ((diss.p$diss.1+diss.p$diss.2)/2)
  diss.p$diss.emerge2 <- diss.p$diss.comb - (diss.p$diss.1+diss.p$diss.2)
  
  diss <- rbind(diss, diss.p)
}

onsets_ind <- read.csv("Pipeline/rhythm/onset-density/total/master-raw-onset-density-individual.csv")
summary <- read.csv("summary-individual.csv")
onsets_ind <- merge(onsets_ind, select(summary,player,pair,condition))
onsets_ind$other <- str_remove(as.character(onsets_ind$pair),as.character(onsets_ind$player))
for (p in unique(filter(onsets_ind,condition=="one-way")$player)) {
  onsets_p <- onsets_ind %>% filter(player==p)
  onsets_other <- onsets_ind %>% filter(player==as.character(onsets_p$other[1]))
  onsets_other$pair <- as.character(onsets_p$pair[1])
  onsets_other$condition = "one-way"
  onsets_ind <- rbind(onsets_ind, onsets_other)
}

onsets_ind$time <- round(onsets_ind$time,1)
onsets_comb <- onsets_ind %>% group_by(pair,condition,time) %>% summarise(num_onsets=sum(num_onsets))
  

diss$time <- round(diss$time,1)
onsets_comb$time <- round(onsets_comb$time,1)
onsets_cons <- merge(select(filter(diss,window==5),pair,time,condition,diss.emerge),
                     select(onsets_comb,pair,condition,time,num_onsets),all = TRUE)
head(onsets_cons)
onsets_cons$num_onsets <- replace_na(onsets_cons$num_onsets,0)
onsets_cons_full <- onsets_cons %>% drop_na()
cor.test(onsets_cons_full$diss.emerge, onsets_cons_full$num_onsets)
ggplot(onsets_cons,aes(x=num_onsets,y=1.8-diss.emerge))+geom_point(alpha=.3) + xlab("total combined onset density") + ylab("emergent consonance") + theme_bw()

# Test mediation analysis on toy data --------------------------------------------------------

toy_data <- read.csv("toy-mediation-data/no-mediation-ts.csv")
toy_data$condition = "no-mediation"
toy_data2 <- read.csv("toy-mediation-data/mediation-ts.csv")
toy_data2$condition = "mediation"
toy_data <- rbind(toy_data, toy_data2)

# Step 1: compare R^2 values of linear models

cons_predict <- NULL
for (c in unique(toy_data$condition)) {
  for (l in seq(-20,20,2)) {
    print(lag)
    pair_df <- toy_data %>% filter(condition==c)
    pair_df$diss_live_lead <- shift(pair_df$diss_live, l)
    fit.diss_player_full <- lm(diss_live_lead ~ diss_ghost + density_ghost, data = pair_df)
    fit.diss_player_restricted <- lm(diss_live_lead ~ density_ghost, data = pair_df)
    cons_predict <- rbind(cons_predict, data.frame(condition=c,lag=l, 
                                                   r2_full=summary(fit.diss_player_full)$r.squared, 
                                                   adj_r2_full=summary(fit.diss_player_full)$adj.r.squared,
                                                   r2_restricted=summary(fit.diss_player_restricted)$r.squared,
                                                   adj_r2_restricted=summary(fit.diss_player_restricted)$adj.r.squared))
  }
}

ggplot(cons_predict, aes(x=lag, y=r2_full-r2_restricted, color=condition)) + geom_point() + theme_bw() + theme(legend.position = "bottom")
ggplot(cons_predict, aes(x=lag, y=adj_r2_full-adj_r2_restricted, color=condition)) + geom_point() + theme_bw() + theme(legend.position = "bottom")

"
This works as expected. The casual influence at lag 10 is picked up in the correct dataset.
"

# Step 2: using mediation package


# Mediation Analysis on relationship between Raw Onset Density and Dissonance --------------------------------------------
library(tidyverse)
library(data.table)

density <- read.csv("Pipeline/rhythm/onset-density/total/master-raw-onset-density-individual.csv")
diss <- read.csv("Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-individual.csv")
summary <- read.csv("summary-individual.csv")

density$time <- round(density$time,1)
diss$time <- round(diss$time,1)
diss <- diss %>% filter(window==2)
density_diss <- merge(merge(density,diss),summary)
density_diss <- density_diss %>% dplyr::select(player,pair,other,condition,time,avg_dist,num_onsets)

cons_predict <- NULL
for (p in unique(density_diss$player)) {
  print(p)
  player_df <- density_diss %>% filter(player==p)
  player_df <- player_df %>% arrange(time)
  player_df <- player_df[seq(1,nrow(player_df),3),]
  other_df <- density_diss %>% filter(player==as.character(player_df$other[1]))
  other_df <- other_df %>% arrange(time)
  other_df <- other_df[seq(1,nrow(other_df),3),]
  player_df$player <- "player"
  other_df$player <- "other"
  pair_df <- rbind(player_df,other_df)
  pair_df <- pair_df %>% pivot_wider(id_cols=c("time"),names_from=player,values_from = c("avg_dist","num_onsets")) %>% arrange(time)
  fit.mediator <- lm(avg_dist_other ~ num_onsets_other, data = pair_df)
  for (l in c(seq(-40,-32,2),seq(32,40,2))) {
    print(lag)
    pair_df$avg_dist_player_lead <- shift(pair_df$avg_dist_player, l)
    fit.diss_player_full <- lm(avg_dist_player_lead ~ avg_dist_other + num_onsets_other, data = pair_df)
    fit.diss_player_restricted <- lm(avg_dist_player_lead ~ num_onsets_other, data = pair_df)
    cons_predict <- rbind(cons_predict, data.frame(player=p,lag=l*.6, 
                                                   r_full=summary(fit.diss_player_full)$r.squared, 
                                                   adj_r_full=summary(fit.diss_player_full)$adj.r.squared,
                                                   r_restricted=summary(fit.diss_player_restricted)$r.squared,
                                                   adj_r_restricted=summary(fit.diss_player_restricted)$adj.r.squared))
  }
}

head(cons_predict)
cons_predict <- merge(cons_predict,dplyr::select(summary,player,condition))
write.csv(cons_predict, "Pipeline/onsets-consonance-mediation/raw-onsets-consonance-mediation.csv",row.names = FALSE)

cons_predict <- read.csv("Pipeline/onsets-consonance-mediation/raw-onsets-consonance-mediation.csv")
ggplot(filter(cons_predict,abs(lag)<5), aes(x=lag, y=r_full-r_restricted, fill=condition)) + stat_summary(geom="ribbon",alpha=.4) + stat_summary(geom="point",aes(color=condition)) + xlab("lag (sec)") + ylab("full r^2 - restricted r^2") + theme_bw() + theme(legend.position = "bottom")
ggplot(filter(cons_predict,abs(lag)<5), aes(x=lag, y=adj_r_full-adj_r_restricted, fill=condition)) + stat_summary(geom="ribbon",alpha=.4) + stat_summary(geom="point",aes(color=condition)) + xlab("lag (sec)") + ylab("full adjusted r^2 - restricted adjusted r^2") + theme_bw() + theme(legend.position = "bottom")


"
This analysis is mean to show how consonance of Player A predicts future consonance of Player B,
conditioned on Player A's raw onset density.
It appears that:
* there may be an asymmetry in one-way trials, but only for far out lags --> this is actually kind of weird.
* there is higher causality in coupled versus one-way trials
* causality spikes around 0 for coupled trials

You can test these sorts of things out below. But I'm starting to think this is a wild goose chase -- we already
know that onsets of Player A predict onsets of Player B. And we already know that there are interesting tonal leader/follower
results with the consonance matrix analysis. So I think it is more straightforward to just stick with those and not worry about
this mediation analysis.
"

# test for asymm in one-way trials
cons_predict_wide <- cons_predict %>% filter(condition=="one-way",lag!=0)
cons_predict_wide$predict_val <- cons_predict_wide$r_full-cons_predict_wide$r_restricted
cons_predict_wide$adj_predict_val <- cons_predict_wide$adj_r_full-cons_predict_wide$adj_r_restricted
cons_predict_wide$direction <- ifelse(cons_predict_wide$lag<0,"negative","positive")
cons_predict_wide <- cons_predict_wide %>% group_by(player,direction) %>% summarise(predict_val=mean(predict_val), adj_predict_val=mean(adj_predict_val))
cons_predict_wide <- cons_predict_wide %>% dplyr::select(player,direction,predict_val,adj_predict_val) %>% pivot_wider(names_from = direction, values_from = c("predict_val","adj_predict_val"))

t.test(cons_predict_wide$predict_val_negative,
       cons_predict_wide$predict_val_positive, paired = TRUE)
t.test(cons_predict_wide$adj_predict_val_negative,
       cons_predict_wide$adj_predict_val_positive, paired = TRUE)

# test for greater predictiveness in coupled trials versus coupled trials
cons_predict_sum <- cons_predict %>% filter(abs(lag)<5) %>% group_by(player,condition) %>% 
  summarise(r2_full=mean(r_full),r2_restricted=mean(r_restricted),
            adj_r2_full=mean(adj_r_full),adj_r2_restricted=mean(adj_r_restricted))


cons_predict_sum$predict_val = cons_predict_sum$r2_full-cons_predict_sum$r2_restricted
cons_predict_sum$adj_predict_val = cons_predict_sum$adj_r2_full - cons_predict_sum$adj_r2_restricted

ggplot(cons_predict_sum,aes(x=predict_val,color=condition,fill=condition)) + geom_density(alpha=.2) + theme_bw() + theme(legend.position = "bottom")
t.test(filter(cons_predict_sum,condition=="coupled")$predict_val,
       filter(cons_predict_sum,condition=="one-way")$predict_val,paired=FALSE)
"
	Welch Two Sample t-test

data:  filter(cons_predict_sum, condition == coupled)$predict_val and filter(cons_predict_sum, condition == one-way)$predict_val
t = 2.1283, df = 171.67, p-value = 0.03474
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 0.0007897613 0.0209794615
sample estimates:
 mean of x  mean of y 
0.02714123 0.01625662 
"

t.test(filter(cons_predict_sum,condition=="coupled")$adj_predict_val,
       filter(cons_predict_sum,condition=="one-way")$adj_predict_val,paired=FALSE)

"
	Welch Two Sample t-test

data:  filter(cons_predict_sum, condition == coupled)$adj_predict_val and filter(cons_predict_sum, condition == one-way)$adj_predict_val
t = 2.1493, df = 171.4, p-value = 0.03301
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 0.0008994514 0.0211402211
sample estimates:
 mean of x  mean of y 
0.02511695 0.01409711 
"

# paired t-test with yoked trials; r2 value
head(cons_predict)
cons_predict <- merge(cons_predict, summary)
cons_predict$predict_val <- cons_predict$r_full - cons_predict$r_restricted
cons_predict$adj_predict_val <- cons_predict$adj_r_full - cons_predict$adj_r_restricted
cons_predict_wide <- cons_predict %>% filter(abs(lag)<10) %>% 
  group_by(condition,yolked_id) %>% summarise(predict_val=mean(predict_val)) %>%
  pivot_wider(names_from = condition, values_from = predict_val) %>% drop_na()

ggplot(pivot_longer(cons_predict_wide,cols = c("coupled","one-way"),names_to = "condition",values_to = "predict_val"),
       aes(x=predict_val,color=condition,fill=condition)) + geom_density(alpha=.2) + theme_bw() + theme(legend.position = c(.8,.8))
t.test(cons_predict_wide$coupled,cons_predict_wide$`one-way`,paired=TRUE)

"
Not Sig.

	Paired t-test

data:  cons_predict_wide$coupled and cons_predict_wide$`one-way`
t = 1.7329, df = 42, p-value = 0.09046
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.001764737  0.023208586
sample estimates:
mean of the differences 
             0.01072192 
"

# paired t-test with yoked trials; adjusted r2 value
cons_predict$predict_val <- cons_predict$r_full - cons_predict$r_restricted
cons_predict$adj_predict_val <- cons_predict$adj_r_full - cons_predict$adj_r_restricted
cons_predict_wide <- cons_predict %>% filter(abs(lag)<10) %>% 
  group_by(condition,yolked_id) %>% summarise(predict_val=mean(adj_predict_val)) %>%
  pivot_wider(names_from = condition, values_from = predict_val) %>% drop_na()

ggplot(pivot_longer(cons_predict_wide,cols = c("coupled","one-way"),names_to = "condition",values_to = "predict_val"),
       aes(x=predict_val,color=condition,fill=condition)) + geom_density(alpha=.2) + theme_bw() + theme(legend.position = c(.8,.8))
t.test(cons_predict_wide$coupled,cons_predict_wide$`one-way`,paired=TRUE)

# Mediation Analysis on Detrended Raw Onset Density and Detrended Consonance ---------




library(tidyverse)
library(data.table)

density <- read.csv("Pipeline/rhythm/onset-density/total/master-raw-onset-density-individual.csv")
diss <- read.csv("Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-individual.csv")
summary <- read.csv("summary-individual.csv")

density$time <- round(density$time,1)
diss$time <- round(diss$time,1)
diss <- diss %>% filter(window==2)
density_diss <- merge(merge(density,diss),summary)
density_diss <- density_diss %>% dplyr::select(player,pair,other,condition,time,avg_dist,num_onsets)

cons_predict <- NULL
for (p in unique(density_diss$player)) {
  print(p)
  player_df <- density_diss %>% filter(player==p)
  player_df <- player_df %>% arrange(time)
  player_df <- player_df[seq(1,nrow(player_df),3),]
  other_df <- density_diss %>% filter(player==as.character(player_df$other[1]))
  other_df <- other_df %>% arrange(time)
  other_df <- other_df[seq(1,nrow(other_df),3),]
  player_df$player <- "player"
  other_df$player <- "other"
  pair_df <- rbind(player_df,other_df)
  pair_df <- pair_df %>% pivot_wider(id_cols=c("time"),names_from=player,values_from = c("avg_dist","num_onsets")) %>% arrange(time)
  pair_df$num_onsets_player <- c(NA,diff(pair_df$num_onsets_player))
  pair_df$avg_dist_player <- c(NA,diff(pair_df$avg_dist_player))
  pair_df$num_onsets_other <- c(NA,diff(pair_df$num_onsets_other))
  pair_df$avg_dist_other <- c(NA,diff(pair_df$avg_dist_other))
  for (l in seq(-30,30,2)) {
    print(lag)
    pair_df$avg_dist_player_lead <- shift(pair_df$avg_dist_player, l)
    fit.diss_player_full <- lm(avg_dist_player_lead ~ avg_dist_other + num_onsets_other, data = pair_df)
    fit.diss_player_restricted <- lm(avg_dist_player_lead ~ num_onsets_other, data = pair_df)
    cons_predict <- rbind(cons_predict, data.frame(player=p,lag=l*.6, 
                                                   r_full=summary(fit.diss_player_full)$r.squared, 
                                                   adj_r_full=summary(fit.diss_player_full)$adj.r.squared,
                                                   r_restricted=summary(fit.diss_player_restricted)$r.squared,
                                                   adj_r_restricted=summary(fit.diss_player_restricted)$adj.r.squared))
  }
}

cons_predict <- merge(cons_predict,dplyr::select(summary,player,condition))
write.csv(cons_predict,"Pipeline/onsets-consonance-mediation/detrended-raw-onsets-consonance-mediation.csv",row.names = FALSE)
cons_predict <- read.csv("Pipeline/onsets-consonance-mediation/detrended-raw-onsets-consonance-mediation.csv")
ggplot(cons_predict, aes(x=lag, y=r_full-r_restricted, color=condition)) + stat_summary() + xlab("lag (sec)") + theme_bw() + theme(legend.position = "bottom")
ggplot(cons_predict, aes(x=lag, y=adj_r_full-adj_r_restricted, color=condition)) + stat_summary() + xlab("lag (sec)") + theme_bw() + theme(legend.position = "bottom")

"
Picture seems much more noisy here as compared to non-detrended time series. Looks to be basically zero prediction.
"


# Mediation analysis detrended collpased onset density and consona --------

density <- read.csv("Pipeline/rhythm/onset-density/collapsed/master-onset-density-individual.csv")
diss <- read.csv("Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-individual.csv")
summary <- read.csv("summary-individual.csv")

density$time <- round(density$time,1)
diss$time <- round(diss$time,1)
diss <- diss %>% filter(window==2)
density_diss <- merge(merge(density,diss),summary)
density_diss <- density_diss %>% dplyr::select(player,pair,other,condition,time,avg_dist,num_onsets)

cons_predict <- NULL
for (p in unique(density_diss$player)) {
  print(p)
  player_df <- density_diss %>% filter(player==p)
  player_df <- player_df %>% arrange(time)
  player_df <- player_df[seq(1,nrow(player_df),3),]
  other_df <- density_diss %>% filter(player==as.character(player_df$other[1]))
  other_df <- other_df %>% arrange(time)
  other_df <- other_df[seq(1,nrow(other_df),3),]
  player_df$player <- "player"
  other_df$player <- "other"
  pair_df <- rbind(player_df,other_df)
  pair_df <- pair_df %>% pivot_wider(id_cols=c("time"),names_from=player,values_from = c("avg_dist","num_onsets")) %>% arrange(time)
  pair_df$num_onsets_player <- c(NA,diff(pair_df$num_onsets_player))
  pair_df$avg_dist_player <- c(NA,diff(pair_df$avg_dist_player))
  pair_df$num_onsets_other <- c(NA,diff(pair_df$num_onsets_other))
  pair_df$avg_dist_other <- c(NA,diff(pair_df$avg_dist_other))
  for (l in seq(-30,30,2)) {
    print(lag)
    pair_df$avg_dist_player_lead <- shift(pair_df$avg_dist_player, l)
    fit.diss_player_full <- lm(avg_dist_player_lead ~ avg_dist_other + num_onsets_other, data = pair_df)
    fit.diss_player_restricted <- lm(avg_dist_player_lead ~ num_onsets_other, data = pair_df)
    cons_predict <- rbind(cons_predict, data.frame(player=p,lag=l*.6, 
                                                   r_full=summary(fit.diss_player_full)$r.squared, 
                                                   adj_r_full=summary(fit.diss_player_full)$adj.r.squared,
                                                   r_restricted=summary(fit.diss_player_restricted)$r.squared,
                                                   adj_r_restricted=summary(fit.diss_player_restricted)$adj.r.squared))
  }
}

"
Prediction seems higher in coupled trials at small lags, but strangely 
prediction drops to 0 around lag 0.
"

cons_predict <- merge(cons_predict,dplyr::select(summary,player,condition))
write.csv(cons_predict,"Pipeline/onsets-consonance-mediation/detrended-collapsed-onsets-consonance-mediation.csv",row.names = FALSE)

ggplot(cons_predict, aes(x=lag, y=r_full-r_restricted, color=condition)) + stat_summary() + xlab("lag (sec)") + theme_bw() + theme(legend.position = "bottom")
ggplot(cons_predict, aes(x=lag, y=adj_r_full-adj_r_restricted, color=condition)) + stat_summary() + xlab("lag (sec)") + theme_bw() + theme(legend.position = "bottom")

# Cross-correlation of raw onset density with consonance ----------------------

density <- read.csv("Pipeline/rhythm/onset-density/total/master-raw-onset-density-individual.csv")
diss <- read.csv("Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-individual.csv")
summary <- read.csv("summary-individual.csv")

density$time <- round(density$time,1)
diss$time <- round(diss$time,1)
diss <- diss %>% filter(window==2)

density_diss <- merge(merge(density,diss),summary)
density_diss <- density_diss %>% select(player,pair,other,condition,time,avg_dist,num_onsets)
density_diss$other <- str_remove(as.character(density_diss$pair),as.character(density_diss$player))

ggplot(density_diss, aes(x=num_onsets,y=1.8-avg_dist))+geom_point(alpha=.2)+theme_bw() + xlab("Individual Onset Density") + ylab("Individual Consonance")
ggplot(filter(density_diss,num_onsets<=50), aes(x=num_onsets,y=1.8-avg_dist))+geom_point(alpha=.2)+theme_bw() + xlab("Individual Onset Density") + ylab("Individual Consonance")

cc_density <- NULL
for (person in unique(density_diss$player)) {
  print(person)
  person <- density_diss %>% filter(player==person)
  other <- density_diss %>% filter(player==as.character(person$other[[1]]))
  max.len = max(length(person$num_onsets), length(other$num_onsets))
  person_density = c(person[order(person$time),"num_onsets"], rep(0, max.len-length(person$num_onsets)))
  other_density = c(other[order(other$time),"avg_dist"], rep(0, max.len-length(other$num_onsets)))
  cc <- ccf(person_density, other_density, lag.max=800, plot=FALSE, na.action = na.pass)
  cc_i <- data.frame(cc = cc$acf,
                     lag = cc$lag,
                     player = person$player[[1]],
                     pair = person$pair[[1]],
                     condition = person$condition[[1]])
  cc_density <- rbind(cc_density, cc_i)
}
head(cc_density)
cc_density$lag <- .2*cc_density$lag

# these dataframes were obtained by running above code twice, switching order of 'num_onsets' and 'avg_dist'
cc_diss_density <- cc_density
cc_density_diss <- cc_density

cc_diss_density <- rbind(filter(cc_diss_density,lag>0),filter(cc_density_diss,lag<0))

ggplot(filter(cc_diss_density,abs(lag)<6), aes(x=lag,y=cc,color=condition,fill=condition)) + stat_summary() + theme_bw() + theme(legend.position = "bottom") + xlab("lag (sec)") + ylab("cross-correlation of consonance with raw onset density")
"
What is at stake here? We want to test whether the onsets of ghost recording predict consonance of live musician moreso
than the other way around. Indeed, this seems to be the case.
"