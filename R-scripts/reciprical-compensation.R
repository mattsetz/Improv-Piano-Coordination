####################################
# reciprical compensation analysis
# MS 9/18/19
####################################

library(tidyverse)

####################################
# read-in and format dissonance data
####################################
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


diss.old.02 <- diss.old
diss.old.2 <- diss.old %>% filter(time %% 2 == 0)

# shuffle time

diss <- diss %>% group_by(pair, window) %>% 
  mutate(time.rand = sample(time)) %>% ungroup()
diss <- diss %>% group_by(pair,window) %>% mutate(n_time=ntile(time,10),
                                                  n_time_rand=ntile(time.rand,10)) %>% ungroup()

diss <- diss %>% na.omit()
#######################################################
# dimensionality reduction -- dynamic control variable
#######################################################
head(diss)

diss.ratio <- diss %>% group_by(pair,n_time,condition,window) %>%
  summarise(var.ind=mean(c(var(diss.1),var(diss.2))),
            var.group=var(diss.comb)) %>% ungroup()
diss.ratio$compensate <- diss.ratio$var.ind/diss.ratio$var.group
diss.ratio$condition <- paste(diss.ratio$condition, ".ordered", sep="")

# shuffle time
diss.ratio.shuffled <- diss %>% group_by(pair,n_time_rand,condition,window) %>%
  summarise(var.ind=mean(c(var(diss.1),var(diss.2))),
            var.group=var(diss.comb)) %>% ungroup()
diss.ratio.shuffled$compensate <- diss.ratio.shuffled$var.ind/diss.ratio.shuffled$var.group
diss.ratio.shuffled$n_time <- diss.ratio.shuffled$n_time_rand
diss.ratio.shuffled$condition <- paste(diss.ratio.shuffled$condition, ".shuffled", sep="")
diss.ratio.shuffled <- diss.ratio.shuffled %>% select(-n_time_rand)

diss.ratio <- rbind(diss.ratio, diss.ratio.shuffled)
diss.ratio.02 <- diss.ratio

# visualize summary stats by window
ggplot(filter(diss.ratio,window<=10), aes(x=window,y=compensate,color=condition,fill=condition)) +
  stat_summary() +
  xlab("window size (sec)") +
  ylab("compensation ratio") +
  theme_classic()


diss.pair <- diss.ratio %>% group_by(pair,condition,window) %>% summarise(avg.compensation=mean(compensate,na.omit=TRUE))
ggplot(filter(diss.pair,window==2), aes(x=avg.compensation,color=condition,fill=condition)) +
  geom_density(alpha=0.2) +
  xlim(0,25) +
  xlab("compensation") +
  theme_classic()
