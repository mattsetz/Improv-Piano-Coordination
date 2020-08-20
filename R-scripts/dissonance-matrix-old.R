####################################
# analyze dissonance matrices
# MS 8/20/19
####################################

library(tidyverse)



# get one-way trials ##################
oneway.pairs <- unique(filter(summary,condition=="one-way")$pair)
oneway.players <- unique(filter(summary.ind,condition=="one-way")$player)
windows <- c(2,5,10)
diss.master <- NULL
for (window in windows) {
  print(window)
  
  # read in all dissonance matrix files
  inDir <- paste('Pipeline/tonal/diss-matrix/',as.character(window),'-sec/',sep='')
  infiles <- list.files(inDir)
  diss.window <- NULL
  for (f in infiles) {
    print(f)
    pair <- str_split(f,"[.]")[[1]][1]
    if (!pair %in% oneway.pairs) {
      print("skipping")
      next
    }
    
    df <- read.csv(paste(inDir,f,sep=''))
    df$pair <- str_split(f,"[.]")[[1]][1]
    df$player <- str_replace(str_split(f,"[.]")[[1]][2],'-diss-matrix','')
    df$window <- window
    diss.window <- rbind(diss.window, df)
  }
  
  # add individual dissonance matrices into master csv
  diss.window$other <- str_replace(diss.window$pair, diss.window$player, "")
  for (p in unique(diss.window$pair)) {
    print(p)
    
    diss.pair <- diss.window %>% filter(pair==p)
    player1 <- str_extract_all(p,"[a-z]{1,2}[0-9]{1,2}")[[1]][1]
    player2 <- str_extract_all(p,"[a-z]{1,2}[0-9]{1,2}")[[1]][2]
    # one-way trials, player1 --> live player
    if (player2 %in% oneway.players) {
      player1.tmp = player1
      player1 = player2
      player2 = player1.tmp
    }
    
    diss.player1 <- diss.pair %>% filter(player==player1)
    diss.player2 <- diss.pair %>% filter(player==player2)
    
    # construct dataframe
    diss.player2$time1.tmp <- diss.player2$time1
    diss.player2$time1 <- diss.player2$time2
    diss.player2$time2 <- diss.player2$time1.tmp
    diss.player2$diss1.tmp <- diss.player2$diss1
    diss.player2$diss1 <- diss.player2$diss2
    diss.player2$diss2 <- diss.player2$diss1.tmp
    diss.player2 <- diss.player2 %>% select(-time1.tmp,-diss1.tmp)
    diss.pair <- rbind(select(diss.player1,-player,-other), select(diss.player2,-player,-other)) %>% distinct()
    diss.pair$player1 <- player1
    diss.pair$player2 <- player2
    
    # concatenate diss.pair to diss.master
    diss.master <- rbind(diss.master,diss.pair)
  }
}

write.csv(diss.master, 'Pipeline/tonal/diss-matrix/master-diss-matrix.csv',row.names = FALSE)



# read in data ##################################

diss <- read.csv('Pipeline/tonal/diss-matrix/master-diss-matrix.csv')
summary <- read.csv('summary-combined.csv')
summary.ind <- read.csv('summary-individual.csv')
diss <- diss %>% na.omit()
diss <- merge(diss, select(summary,pair,condition))
diss$window <- factor(diss$window)
diss <- diss %>% group_by(pair,window) %>% mutate(n_time1=ntile(time1,10),
                                                  n_time2=ntile(time2,10)) %>% ungroup()


diss.oneway <- diss %>% filter(condition=="one-way")
diss.oneway.ghost.live <- diss.oneway %>% filter(time1>time2)
diss.oneway.ghost.live$direction <- "ghost->live"
diss.oneway.simultaneous <- diss.oneway %>% filter(time1==time2)
diss.oneway.simultaneous$direction <- "simultaneuos"
diss.oneway.live.ghost <- diss.oneway %>% filter(time1<time2)
diss.oneway.live.ghost$direction <- "live->ghost"
diss.oneway <- rbind(diss.oneway.ghost.live,diss.oneway.live.ghost,diss.oneway.simultaneous)
diss.oneway$time_diff <- abs(diss.oneway$time1-diss.oneway$time2)
diss.oneway$lag <- diss.oneway$time1 - diss.oneway$time2
diss.oneway <- diss.oneway %>% group_by(pair) %>% mutate(n_time=ntile(time1,10)) %>% ungroup()
diss.oneway$diss.emerge <- diss.oneway$dissonance-((diss.oneway$diss1+diss.oneway$diss2)/2)

diss.coupled <- diss %>% filter(condition=="coupled")
diss.coupled.ghost.live <- diss.coupled %>% filter(time1>time2)
diss.coupled.ghost.live$direction <- "live1->live2" 
diss.coupled.simultaneous <- diss.coupled %>% filter(time1==time2)
diss.coupled.simultaneous$direction <- "simultaneuos"
diss.coupled.live.ghost <- diss.coupled %>% filter(time1<time2)
diss.coupled.live.ghost$direction <- "live2->live1"
diss.coupled <- rbind(diss.coupled.ghost.live,diss.coupled.live.ghost,diss.coupled.simultaneous)
diss.coupled$time_diff <- abs(diss.coupled$time1-diss.coupled$time2)
diss.coupled$lag <- diss.coupled$time1 - diss.coupled$time2
diss.coupled <- diss.coupled %>% group_by(pair) %>% mutate(n_time=ntile(time1,10)) %>% ungroup()
diss.coupled$diss.emerge <- diss.coupled$dissonance-((diss.coupled$diss1+diss.coupled$diss2)/2)

diss <- rbind(diss.coupled, diss.oneway)

head(diss)


# Model diss by lag -------------------------------------------------------
# is there flatter slope for ghost_live as opposed to live_ghost? how about for live1_live2 and live2_live1?
colnames(diss)
unique(diss$direction)
diss$abs_lag <- abs(diss$lag)
diss_live1_live2 <- diss %>% filter(condition=="coupled",lag<=0)
diss_live1_live2$direction <- "live1_live2"
diss_live2_live1 <- diss %>% filter(condition=="coupled",lag>=0)
diss_live2_live1$direction <- "live2_live1"
diss_ghost_live <- diss %>% filter(condition=="one-way",lag>=0)
diss_ghost_live$direction <- "ghost_live"
diss_live_ghost <- diss %>% filter(condition=="one-way",lag<=0)
diss_live_ghost$direction <- "live_ghost"
diss <- rbind(diss_live1_live2,diss_live2_live1,diss_ghost_live,diss_live_ghost)
diss_m_avg <- diss %>% filter(n_time1>1,n_time1<10,n_time2>1,n_time2<10) %>% group_by(pair,window,condition,lag,abs_lag,direction) %>% summarise(diss.comb=mean(dissonance,na.rm = TRUE), diss.emerge=mean(diss.emerge,na.rm = TRUE)) %>% ungroup()

# fit individual slopes for each trial
diss_m_slopes <- diss_m_avg %>% group_by(window,pair,condition,direction) %>% summarise(intercept_comb=lm(diss.comb~abs_lag)$coefficients[[1]],slope_comb=lm(diss.comb~abs_lag)$coefficients[[2]],
                                                                                  intercept_emerge=lm(diss.emerge~abs_lag)$coefficients[[1]],slope_emerge=lm(diss.emerge~abs_lag)$coefficients[[2]])

colnames(diss_m_slopes)
ggplot(diss_m_slopes, aes(x=slope_comb,color=direction,fill=direction))+geom_density(alpha=.2)+facet_wrap(~window)
ggplot(diss_m_slopes, aes(x=slope_emerge,color=direction,fill=direction))+geom_density(alpha=.2)+facet_wrap(~window)

"
In terms of combined dissonance, it appears that there is not much difference in slopes between direction.
Same deal for emergent dissonance. Maybe we'd see a difference if we computed matrices out to greater lags.
But I don't think its worth doing that for the purpose of this paper.
"

library(rjags)
dataList <- list(
  y=diss_m_avg$diss.emerge-mean(diss_m_avg$diss.emerge,na.rm = TRUE),
  x=diss_m_avg$abs_lag-mean(diss_m_avg$abs_lag,na.rm=TRUE),
  s=group_indices(diss_m_avg,pair),
  Ntrials=length(unique(diss_m_avg$pair)),
  Ntotal=length(diss_m_avg$diss.emerge)
)

diss_m_slopes = jags.model( file="Analysis/jags/DissMatrixSlopes.txt" , data=dataList ,
                                  n.chains=3 , n.adapt=500 )
update( diss_m_slopes , n.iter=500 )
diss_m_samples = coda.samples( diss_m_slopes , 
                              variable.names = c("sigma", "beta0sigma","beta1sigma",
                                                 "beta1mu", "beta0mu"), n.iter=4000)
save(diss_m_samples,file="Analysis/jags/diss_matrix_slopes_live_ghost_window2.rds")
plot(diss_m_samples)
summary(diss_m_samples)
samples_ghost_live <- get(load("Analysis/jags/diss_matrix_slopes_ghost_live_window2.rds"))
summary(samples_ghost_live)

library(lme4)
lme(diss.emerge)

# sanity check #############################################
head(diss)
diss$lag <- diss$time1 - diss$time2
diss %>% group_by(n_time1,lag) %>% summarize(count=n()) %>% View()
ggplot(filter(diss,window==5), aes(x=lag,color=condition,fill=condition)) +
  geom_density(alpha=0.2) +
  facet_wrap(~n_time1)

diss %>% filter(condition=="one-way") %>% select(pair) %>% unique()
c1d1 <- diss %>% filter(pair=="c1d1")
c1f1 <- diss %>% filter(pair=="c1f1")

ggplot(c1d1, aes())





# dissonance as function of time + lag #############################################

diss.oneway <- diss.oneway %>% group_by(pair,window) %>%
  mutate(n_time1=ntile(time1,10),
         n_time2=ntile(time2,10)) %>% ungroup()
diss.coupled <- diss.coupled %>% group_by(pair,window) %>%
  mutate(n_time1=ntile(time1,10),
         n_time2=ntile(time2,10)) %>% ungroup()

# one-way
# dissonance by window + lag
ggplot(diss.oneway, aes(x=lag,y=dissonance,color=direction)) + 
  stat_summary() +
  geom_smooth(method=lm,se=TRUE) +
  xlab("lag (sec)") +
  ylab("Dissonance") +
  facet_wrap(~window,scales = "free") +
  ggtitle("one-way")#stat_summary makes a summary of the points - one for each cell in the design, defaulting to mean

diss.coupled.abbr <- diss.coupled %>% filter(n_time1>1,n_time1<10,n_time2>1,n_time2<10)
diss.coupled.abbr <- diss.coupled
ggplot(diss.coupled.abbr, aes(x=lag,y=dissonance)) +
  stat_summary() +
  geom_smooth(data = filter(diss.coupled.abbr,lag<0),
              method=lm,se=TRUE) +
  geom_smooth(data = filter(diss.coupled.abbr,lag>0),
              method=lm,se=TRUE) +
  xlab("lag (sec)") +
  ylab("Dissonance") +
  facet_wrap(~window,scales = "free") +
#  theme_classic() +
  ggtitle("coupled")

# what about emergent dissonance?
diss.oneway.abbr <- diss.oneway
diss.oneway.abbr <- diss.oneway %>% filter(n_time1>1,n_time1<10,n_time2>1,n_time2<10)
emerge.oneway.plt <- ggplot(filter(diss.oneway.abbr,window==5), aes(x=lag,y=diss.emerge,color=direction)) + 
  stat_summary() +
  geom_smooth(method=lm,se=TRUE) +
  xlab("lag (sec)") +
  ylab("Emergent Dissonance") +
  theme_classic() +
  theme(legend.title = element_text(size=10),
        legend.justification=c(1,0), 
        legend.position=c(0.95, 0.5),  
        legend.background = element_blank(),
        legend.key = element_blank()) 
# facet_wrap(~window,scales = "free") +
  theme_classic()
  ggtitle("one-way")

ggplot(filter(diss.coupled,window==5,n_time1>1,n_time1<10,n_time2>1,n_time2<10), aes(x=lag,y=diss.emerge)) + 
  stat_summary() +
  geom_smooth(data = filter(diss.coupled,window==5,n_time1>1,n_time1<10,n_time2>1,n_time2<10,
                            lag<0),
              method=lm,se=TRUE) +
  geom_smooth(data = filter(diss.coupled,window==5,n_time1>1,n_time1<10,n_time2>1,n_time2<10,
                            lag>0),
              method=lm,se=TRUE) +
  xlab("lag (sec)") +
  ylab("Emergent Dissonance") +
  #facet_wrap(~window,scales = "free") 
  theme_classic()
  ggtitle("coupled")

  
# sig testing for one-way trials
head(diss.oneway.abbr)
diss.oneway.abbr <- diss.oneway.abbr %>% group_by(pair,direction,window) %>%
                      summarise(diss.emerge = mean(diss.emerge,na.rm=TRUE),
                                diss.combined = mean(dissonance,na.rm=TRUE))
diss.emerge.wide <- diss.oneway.abbr %>% select(-diss.combined) %>%
  spread(direction,diss.emerge)

W = 5
t.test(filter(diss.emerge.wide,window==5)$'ghost->live',
       filter(diss.emerge.wide,window==5)$'live->ghost', paired=TRUE)

t.test(filter(diss.oneway,lag>0,n_time1>1,n_time1<10,n_time2>1,n_time2<10,window==10)$diss.emerge,
       filter(diss.oneway,lag<0,n_time1>1,n_time1<10,n_time2>1,n_time2<10,window==10)$diss.emerge)

library(cowplot)
plot_grid(emerge.coupled.plt, emerge.oneway.plt, 
          align="v",
          labels=c("A","B"))

# sig testing ###########################################
diss.coupled$sign <- ifelse(diss.coupled$lag<0,"neg",ifelse(diss.coupled$lag==0,"zero","pos"))
diss.coupled.agg <- diss.coupled %>% filter(n_time1>1,n_time1<10,n_time2>1,n_time2<10) %>%
  group_by(pair,window,sign) %>%
  summarise(diss.combined=mean(dissonance,na.rm=TRUE),
            diss.emergent=mean(diss.emerge,na.rm=TRUE)) %>% ungroup()
head(diss.coupled.agg)
diss.coupled.agg <- diss.coupled.agg %>% select(-diss.emergent) %>% spread(sign,diss.combined)
head(diss.coupled.agg)
diss.coupled.agg.w <- filter(diss.coupled.agg,window==5)
t.test(diss.coupled.agg.w$neg,
       diss.coupled.agg.w$pos, paired=TRUE)


head(diss.oneway)
diss.oneway.agg <- diss.oneway %>% filter(n_time1>1,n_time1<10,n_time2>1,n_time2<10) %>% 
  group_by(pair,window,direction) %>%
  summarise(diss.combined=mean(dissonance,na.rm=TRUE)) %>% ungroup() %>%
  spread(direction,diss.combined)
diss.oneway.agg.w <- filter(diss.oneway.agg,window==5)
t.test(diss.oneway.agg.w$`ghost->live`,
       diss.oneway.agg.w$`live->ghost`, paired=TRUE)

diss.coupled.lm.agg <- diss.coupled %>% filter(direction!="simultaneous",abs(lag)<10) %>%
  group_by(pair,window,direction) %>% summarise(intercept=lm(dissonance~abs(lag))$coefficients[1],
                                                slope=lm(dissonance~abs(lag))$coefficients[2])

diss.oneway.lm.agg <- diss.oneway %>% filter(direction!="simultaneous",abs(lag)<10) %>%
  group_by(pair,window,direction) %>% summarise(intercept=lm(dissonance~abs(lag))$coefficients[1],
                                                slope=lm(dissonance~abs(lag))$coefficients[2])

diss.lm <- rbind(select(diss.coupled.lm.agg,intercept,slope,direction),
            select(diss.oneway.lm.agg,intercept,slope,direction))
ggplot(diss.lm, aes(x=slope,color=direction,fill=direction)) + geom_density(alpha=.2) + facet_wrap(~window)

ggplot(diss.oneway.lm.agg, aes(x=slope,color=direction,fill=direction)) +
  geom_density(alpha=.2)+facet_wrap(~window)

# sig testing for memory ####
head(diss)
diss$abs_lag <- abs(diss$lag)
diss.middle.w <- diss %>% filter(n_time1>1,n_time1<10,n_time2>1,n_time2<10,window==2) %>% group_by(pair,condition,lag,abs_lag) %>% summarise(diss=mean(dissonance,na.rm = TRUE),diss.emerge=mean(diss.emerge,na.rm=TRUE))

library(brms)
coupled_diss_fit <- brm(diss ~ abs_lag + (pair|abs_lag), filter(diss.middle.w,condition=="coupled"), cores=2, file = "Analysis/brms/diss-matrix-fit.rds")
ghost_live_diss_fit <- update(coupled_diss_fit, newdata = filter(diss.middle.w,condition=="one-way",lag>0), file="Analysis/brms/diss-matrix-ghost-live-fit.rds")
live_ghost_diss_fit <- update(coupled_diss_fit, newdata = filter(diss.middle.w,condition=="one-way",lag<0), file="Analysis/brms/diss-matrix-live-ghost-fit.rds")

coupled_emerge_diss_fit <- update(coupled_diss_fit, newdata = filter(diss.middle.w,condition=="one-way",lag>0), file="Analysis/brms/emerge-diss-matrix-ghost-live-fit.rds")
ghost_live_diss_fit <- update(coupled_diss_fit, newdata = filter(diss.middle.w,condition=="one-way",lag>0), file="Analysis/brms/emerge-diss-matrix-ghost-live-fit.rds")
live_ghost_diss_fit <- update(coupled_diss_fit, newdata = filter(diss.middle.w,condition=="one-way",lag<0), file="Analysis/brms/emerge-diss-matrix-live-ghost-fit.rds")
# Visualize dissonance by lag #############################################

head(diss.summary)
diss.oneway <- diss.oneway[order(diss.oneway$time_diff),]
diss.oneway$time_diff_factor <- factor(diss.oneway$time_diff)

# all points
ggplot(filter(diss.oneway,time_diff<20), aes(x=time_diff_factor,y=dissonance,fill=direction)) +
  geom_boxplot() +
  scale_x_discrete("lag (sec)") +
  scale_y_continuous("dissonance (5 sec window)") +
  ggtitle("Dissonance by lag\none-way")
  geom_jitter()

# averaging within performance
diss.summary$time_diff_factor <- factor(diss.summary$time_diff)
ggplot(filter(diss.summary,time_diff<20), aes(x=time_diff_factor,y=avg_diss,fill=direction)) +
  geom_boxplot() +
  scale_x_discrete("lag (sec)") +
  scale_y_continuous("dissonance (2 sec window)") +
  ggtitle("Dissonance by lag\none-way; average within performance")



# Visualize dissonance matrix #############################################

diss.a1b1 <- diss %>% filter(pair=="a1b1")
head(diss.a1b1)
ggplot(diss.a1b1, aes(x=time1,y=time2)) +
  geom_tile(aes(fill=dissonance)) +
  geom_line(aes(x=time1,y=time1),color="green",alpha=0.8) +
  ggtitle("abbrv dissonance matrix a1b1")

diss.c1f1 <- diss %>% filter(pair=="b4d7")
ggplot(diss.c1f1, aes(x=time1,y=time2)) +
  geom_tile(aes(fill=dissonance)) +
  geom_line(aes(x=time1,y=time1),color="green",alpha=0.8) 
  ggtitle("dissonance matrix c1f1")

diss.c1f1 <- diss %>% filter(pair=="c1f1")
ggplot(diss.c1f1, aes(x=time1,y=time2)) +
  geom_tile(aes(fill=dissonance)) +
  geom_line(aes(x=time1,y=time1),color="green",alpha=0.2) +
  ggtitle("dissonance matrix c1f1")

diss.a1b1.full <- read.csv('Pipeline/tonal/diss-matrix/2-sec/a1b1-diss-matrix.csv')
ggplot(diss.a1b1.full, aes(x=time1,y=time2)) +
  geom_tile(aes(fill=dissonance)) +
  geom_line(aes(x=time1,y=time1),color="green",alpha=0.2) +
  theme_classic() +
  ggtitle("full dissonance matrix a1b1")
