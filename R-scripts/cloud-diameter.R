###########################
#  analyze cloud-diameter
#  MS 6/10/19
###########################

library(tidyverse)


# import data ####
diam.ind <- read.csv('Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-individual.csv')

summary <- read.csv("summary-individual.csv")
diss <- merge(diam.ind,summary)
diam.ind <- diss


# Granger Causality ####
library(lmtest)

get.gc <- function(diss, p, LAG_ORDER, NUM_STEPS) {
  gc.diss <- NULL
  windows <- c(2)
  for (w in windows) {
    print(w)
    diss.player <- diss %>% filter(player==p,window==w)
    diss.other <- diss %>% filter(player==as.character(diss.player$other[1]),
                                  window==w)
    condition <- as.character(diss.player$condition[1])
    
    gc = grangertest(diss.player$avg_dist, 
                     diss.other$avg_dist, order = LAG_ORDER)
    gc.diss <- rbind(gc.diss, data.frame(player=p,
                                         other=as.character(diss.player$other[1]),
                                         condition=condition,
                                         direction=if_else(condition=="one-way","live.ghost","live.live"),
                                         shuffled=FALSE,
                                         order=LAG_ORDER,
                                         order_sec=LAG_ORDER*(.2*NUM_STEPS),
                                         window=w,
                                         p.val=gc$`Pr(>F)`[2]))
    
    if (condition=="one-way") {
      gc.reverse <- grangertest(diss.other$avg_dist,
                                diss.player$avg_dist,order = LAG_ORDER)
      gc.diss <- rbind(gc.diss, data.frame(player=as.character(diss.player$other[1]),
                                           other=p,
                                           condition=condition,
                                           direction=if_else(condition=="one-way","ghost.live","live.live"),
                                           shuffled=FALSE,
                                           order=LAG_ORDER,
                                           order_sec=LAG_ORDER*(.2*NUM_STEPS),
                                           window=w,
                                           p.val=gc.reverse$`Pr(>F)`[2]))
    }
    
    gc.shuffled = tryCatch(grangertest(diss.other[sample(nrow(diss.other)),]$avg_dist,
                                       diss.player[sample(nrow(diss.player)),]$avg_dist, order = LAG_ORDER),
                           error = function(e) {
                             print(paste(p, "failed to execute"))
                             return(NA)
                           })
    gc.diss <- rbind(gc.diss, data.frame(player=as.character(diss.player$other[1]),
                                         other=p,
                                         condition=as.character(diss.player$condition[1]),
                                         direction='shuffled',
                                         shuffled=TRUE,
                                         order=LAG_ORDER,
                                         order_sec=LAG_ORDER*(.2*NUM_STEPS),
                                         window=w,
                                         p.val=gc.shuffled$`Pr(>F)`[2]))
  }
  print("writing to file")
  write.csv(gc.diss, paste('Pipeline/tonal/gc-dissonance/',
                           as.character(LAG_ORDER),'-',
                           as.character(p),'.csv',sep=''))
  return()
}

get.gc(diss[seq(1,nrow(diss),STEPS),],"x6",20,STEPS)

STEPS = 3
lags <- c(5,10,20)
for (LAG_ORDER in lags) {
  for (p in unique(diss$player)) {
    tryCatch(get.gc(diss[seq(1,nrow(diss),STEPS),],p,LAG_ORDER, STEPS),error=function(e) {return(NA)})
  }
}

head(gc.diss)
gc.diss$sig <- gc.diss$p.val < .05
gc.summary <- gc.diss %>% group_by(condition,order,window,sig) %>% summarise(n())

gc.diss %>% filter(condition=="one-way") %>% group_by(direction,sig,window,order) %>% summarise(n()) %>% View()
write.csv(gc.diss,'Pipeline/tonal/master-granger-causality-dissonance.csv',row.names = FALSE)
ggplot(gc.diss, aes())

ggplot(gc.diss, aes(x=p.val,color=condition,fill=condition)) +
  geom_histogram(position="dodge")
  
# surrogate analysis
diss.uncoupled <- read.csv('Pipeline/tonal/master-emergent-diss-surrogate.csv')
for (p in unique(diss$player)) {
  print(p)
  diss.player <- diss %>% filter(player==p)
  diss.other <- diss %>% filter(player==as.character(diss.player$other[1]))
  print("here?")
  gc <- grangertest(diss.player$avg_dist, diss.other$avg_dist, order = LAG_ORDER)
  print("yup")
  gc.diss <- rbind(gc.diss, data.frame(player=p,
                                       other=as.character(diss.player$other[1]),
                                       condition="uncoupled",
                                       direction='live.ghost', # only relevant for one-way trials
                                       p.val=gc$`Pr(>F)`[2]))
}

help("grangertest")


#####################################
# Descriptive analysis
#####################################
diam.ind$window <- factor(diam.ind$window)
ggplot(diam.ind, aes(x=avg_dist,fill=condition,color=condition)) +
  geom_density(alpha=0.3) +
  facet_wrap(~window) +
  ggtitle("Cloud-Diameter")

diam.ind <- diam.ind %>% group_by(window) %>% 
            mutate(n_dist = ntile(avg_dist,10))
filter(diam.ind,window==10,n_dist==10)

# order from least to highest cloud diameter
diam.ind <- diam.ind[order(diam.ind$avg_dist),]
diam.ind10 <- diam.ind %>% filter(window==10)
diam.ind10 <- diam.ind10[order(diam.ind10$avg_dist),] %>% filter(avg_dist>0)


################################################
# Cross-correlation
################################################
cc.diss <- NULL
for (person in unique(diss$player)) {
  print(person)
  # subset 2 trials
  person <- diss %>% filter(player==person,window==2)
  other <- diss %>% filter(player==as.character(person$other[[1]]),window==2)
  
  cc <- ccf(person$avg_dist, other$avg_dist, lag.max = 100, plot=FALSE, na.action=na.pass)
  cc_i <- data.frame(cc = cc$acf,
                     lag = cc$lag,
                     player = person$player[[1]],
                     condition = person$condition[[1]])
  cc.diss <- rbind(cc.diss,cc_i)
}
cc.diss$lag <- .2*cc.diss$lag

ggplot(filter(cc.diss,abs(lag)<=6), aes(x=lag, y=cc)) +
  stat_summary() +
  #geom_smooth(se=TRUE) +
  facet_wrap(~condition) +
  theme_classic() +
  xlab("lag (sec)") +
  ylab("cross correlation of Dissonance")

# test for asymettry within one-way
cc.diss$direction <- ifelse(cc.diss$lag<0,"neg",ifelse(cc.diss$lag==0,"zero","pos"))
cc.diss.agg <- cc.diss %>% filter(abs(lag)<=6) %>% 
  group_by(condition,player,direction) %>% summarise(cc=mean(cc,na.rm=TRUE)) %>% 
  ungroup() 
cc.diss.wide <- cc.diss.agg %>% spread(direction,cc)
t.test(filter(cc.diss.wide,condition=="one-way")$neg,
       filter(cc.diss.wide,condition=="one-way")$pos, paired = TRUE, alternative = "less")


# test for difference btw coupled and one-way
cc.diss.agg <- cc.diss %>% filter(abs(lag)<5) %>%
  group_by(condition,player) %>% summarise(cc=mean(cc,na.rm=TRUE)) %>% ungroup()
t.test(filter(cc.diss.agg,condition=="coupled")$cc,
       filter(cc.diss.agg,condition=="one-way")$cc,
       alternative="greater")
ggplot(cc.diss.agg, aes(x=cc,color=condition,fill=condition)) +
  geom_density(alpha=.2)
ggplot(filter(cc.diss,abs(lag)<5), aes(x=cc,color=condition,fill=condition)) +
  geom_density(alpha=.2)


ggplot(filter(cc.diss.avg,abs(lag)<5.8),aes(x=lag,y=avg_cc)) +
  geom_density(stat="identity") +
  geom_ribbon(aes(ymin=avg_cc-se,
                    ymax=avg_cc+se),
              alpha=0.3,
              fill="grey") +
  coord_cartesian(ylim=c(.05, .13)) +
  facet_wrap(~condition) +
  ylab("Cross Correlation of Dissonance") +
  xlab("lag (sec)") +
  theme_classic()

cc.diss.abs.avg <- cc.diss %>% group_by(condition,lag) %>%
  summarise(avg_cc=mean(abs(cc),na.rm=TRUE),
            se=sd(abs(cc),na.rm=TRUE)/sqrt(n()))

ggplot(filter(cc.diss.abs.avg,abs(lag)<6),aes(x=lag,y=avg_cc)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=avg_cc-se,
                    ymax=avg_cc+se)) +
  facet_wrap(~condition) +
  ylab("Avg Abs Value Cross-Cor\n
       Tonal Dissonance (2 sec window)") +
  xlab("lag (sec)") +
  theme_classic()

ggplot(cc.diss, aes(x=cc,fill=condition,color=condition)) +
  geom_density(alpha=0.2) +
  xlab("cross correlation\n
       tonal dissonance (2 sec window)") +
  theme_classic()

cc.oneway.pos <- cc.diss %>% filter(condition=="one-way",lag>0)
cc.oneway.neg <- cc.diss %>% filter(condition=="one-way",lag<0)

t.test(cc.oneway.pos[order(cc.oneway.pos$player,cc.oneway.pos$lag),]$cc,
       cc.oneway.neg[order(cc.oneway.neg$player,-cc.oneway.neg$lag),]$cc,
       paired = TRUE)
        

t.test(filter(cc.diss,condition=="one-way")$cc,
       filter(cc.diss,condition=="coupled")$cc) # p <<< 0.0001

# greater cc at 2 second lags than simultaneous time points?
head(cc.diss)
t.test(filter(cc.diss,condition=="coupled",lag==2)$cc,
       filter(cc.diss,condition=="coupled",lag==0)$cc,
       paired = TRUE)

#####################################
# Combined dissonance
#####################################
inDir <- "Pipeline/tonal/cloud-diameter/chew-2-11/combined/"
infiles <- list.files(inDir)
diss.combined <- NULL
for (f in infiles) {
  print(f)
  df <- read.csv(paste(inDir,f,sep=""))
  df$name <- str_split(f,"-")[[1]][[1]]
  diss.combined <- rbind(diss.combined,df)
}

head(diss.combined)
diss.combined$pair <- diss.combined$name
diss.combined <- diss.combined %>% select(-name)
summary.combined <- read.csv("summary-combined.csv")
head(summary.combined)
diss.combined <- diss.combined %>% select(time,window,max_dist,avg_dist,pair)
head(diss.combined)
diss.combined <- read.csv('Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-combined.csv')

diss.combined <- merge(diss.combined, summary.combined)

# is there more overall dissonance in coupled or one-way trials?
head(diss.combined)
ggplot(filter(diss.combined,window==2), aes(x=avg_dist,color=condition,fill=condition)) +
  geom_density(alpha=0.4) +
  facet_wrap(~window) +
  theme_classic()

t.test(filter(diss.combined,condition=="coupled",window==2)$avg_dist,
       filter(diss.combined,condition=="one-way",window==2)$avg_dist)
# t(181420)=-5.8; p << 0.001
# more overall dissonance in one-way trials

# how does dissonance evolve over the course of a performance?
diss.combined <- diss.combined %>% group_by(window,pair) %>% 
                  mutate(n_t=ntile(time,10))
diss.combined <- diss.combined %>% ungroup() %>% group_by(window,pair,n_t) %>%
                  mutate(dist=mean(avg_dist,na.rm=TRUE),
                         se=sd(avg_dist,na.rm=TRUE)/sqrt(n()))
diss.combined$nameid <- diss.combined %>% ungroup() %>% group_indices(pair)
ggplot(filter(diss.combined,window==2,condition=="one-way",nameid<20), aes(x=n_t,y=dist))+
  geom_point() +
  geom_line() +
  facet_wrap(~pair) + 
  theme_classic()

# how does dissonance evolve over the course of a performance in coupled versus one-way?
diss.combined <- diss.combined %>% group_by(window,yolked_id,condition) %>% 
                  mutate(n_t=ntile(time,10)) %>% ungroup()
diss.combined.sum <- diss.combined %>% group_by(window,n_t,yolked_id,pair) %>%
                  mutate(dist = mean(avg_dist,na.rm=TRUE),
                         se = sd(avg_dist,na.rm=TRUE)/sqrt(n())) %>% ungroup()

ggplot(filter(diss.combined.sum,yolked_id<10,window==2), aes(x=n_t,y=dist,color=condition,fill=condition))+
  geom_line(aes(group=pair)) +
  geom_point(alpha=0.5,size=0.1) +
  facet_wrap(~yolked_id) +
  theme_classic()


#####################################
# Case Study: bb5
#####################################

# visualize dissonance with onset density + tonal entropy for example trial
bb5.onsets <- read.csv("Pipeline/rhythm/onsets/t7.aa5bb5.bb5.csv")
bb5.density <- read.csv("Pipeline/rhythm/onset-density/individual/t7.aa5bb5.bb5.csv")
bb5.entropy <- read.csv("Pipeline/tonal/tonal-entropy/t7.aa5bb5.bb5-entropy.csv")
bb5.diam <- diam.ind %>% filter(person_trial=="bb5")
ggplot(filter(bb5.diam,window=="10"),aes(color="diameter",x=time,y=avg_dist*100))+
  geom_line() +
  geom_line(data = bb5.entropy, aes(x=time,y=50*entropy10,color="entropy")) +
  geom_line(data=bb5.density,aes(x=time,y=4*num_onsets,color="onset density")) +
  geom_point(data = bb5.onsets, aes(x=time,y=note),color="black",alpha=.1) +
  ylab("normalized value") +
  theme_classic() +
  ggtitle("Case Study: bb5")

# visualize combined dissonance time series + individual dissonance time series
aa5.diam <- diam.ind %>% filter(person_trial=="aa5")
aa5bb5.diam <- diss.combined %>% filter(name=="aa5bb5")
aa5.diam$player = "individual1"
bb5.diam$player = "individual2"
aa5bb5.diam$person_trial = "aa5bb5"

aa5bb5 <- rbind(select(aa5.diam,person_trial,time,window,avg_dist,player),
                select(bb5.diam,person_trial,time,window,avg_dist,player),
                select(aa5bb5.diam,person_trial,time,window,avg_dist,player))
ggplot(aa5bb5, aes(x=time,y=avg_dist,color=person_trial,fill=person_trial)) +
  geom_point(alpha=0.5) +
  facet_wrap(~window) +
  theme_classic()


############################################
# Surrogate analysis
############################################
oneway.trials <- unique(filter(diss,condition=="one-way")$player)
coupled.trials <- unique(filter(diss,condition=="coupled")$player)
for (p in coupled.trials) {
  person <- diss %>% filter(player==p)
  
  # get random recording to pair
  other.name <- as.character(sample(coupled.trials,1))
  while ((other.name == as.character(p)) | (other.name == as.character(person$other[1]))) {
    other.name <- sample(coupled.trials,1)
  }
  other <- diss %>% filter(player==other.name)
  print(paste(p,other.name))

  # get cross-correlations of tonal dissonance
  cc <- ccf(person$max_dist, other$max_dist, plot=FALSE, na.action=na.pass)
  cc_i <- data.frame(cc = cc$acf,
                     lag = .2*cc$lag,
                     player = person$player[[1]],
                     condition = "uncoupled")
  cc.diss <- rbind(cc.diss,cc_i)
}

cc.diss.avg <- cc.diss %>% group_by(condition,lag) %>%
  summarise(avg_cc=mean(cc,na.rm=TRUE),
            se=sd(cc,na.rm=TRUE)/sqrt(n()))
cc.diss.avg <- cc.diss.avg %>% ungroup()

ggplot(filter(cc.diss.avg,abs(lag)<6),aes(x=lag,y=avg_cc)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=avg_cc-se,
                    ymax=avg_cc+se)) +
  facet_wrap(~condition) +
  ylab("Avg Cross-Cor \n Tonal Dissonance (2 sec window)") +
  xlab("lag (sec)") +
  theme_classic()


#########################################
# Dissonance over normalized time
#########################################
head(diss)
diss.avg <- diss %>% group_by(window,player) %>% mutate(n_t=ntile(time,10)) %>% ungroup()
diss.avg <- diss.avg %>% group_by(player,window,condition,n_t) %>%
            summarise(avg_diss = mean(avg_dist,na.rm=TRUE)) %>% ungroup()
diss.avg <- diss.avg %>% group_by(window,condition,n_t) %>%
            summarise(diss = mean(avg_diss,na.rm=TRUE),
                      se = sd(avg_diss,na.rm=TRUE)/sqrt(n())) %>% ungroup()

ggplot(diss.avg, aes(x=n_t,y=diss,color=condition,fill=condition))+
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=diss-se,
                  ymax=diss+se),alpha=.3) +
  xlab("normalized time (%)") +
  ylab("dissonance") +
  theme_classic() +
  facet_wrap(~window) +
  ggtitle('Dissonance over Normalized Time')


onsets <- read.csv('Pipeline/rhythm/onset-density/collapsed/master-onset-density-combined.csv')
head(onsets)
head(diss)

diss.2.onsets <- merge(filter(diss,window==2),onsets)

# order data from low to high dissonance
unique(diss.combined$window)
diss.combined.summary <- diss.combined %>% group_by(window, pair, condition) %>%
                          summarise(avg_diss=mean(avg_dist,na.rm=TRUE)) %>% ungroup()

diss.sum.5 <- diss.combined.summary %>% filter(window==5,condition=='coupled')
diss.sum.10 <- diss.combined.summary %>% filter(window==10,condition=='coupled')
diss.sum.10[order(diss.sum.10$avg_diss),]
View(diss.sum.5[order(diss.sum.5$avg_diss),])

diss.combined.10 <- diss.combined %>% filter(window==10)
head(diss.combined.10)
