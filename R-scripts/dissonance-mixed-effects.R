# mixed-effects models of dissonance
# 10/23/19

library(tidyverse)
library(lme4)
library(lmerTest)

# input data (old) ####
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


# clean data and correct for time (old) ####
onsets <- read.csv('Pipeline/master_onsets.csv')
onsets$pair <- onsets$personCombo
onsets <- onsets %>% group_by(pair) %>% summarise(beg=min(time))

head(diss)
diss.time <- merge(diss,onsets)
diss <- diss.time
diss$time <- diss$time-diss$beg
diss <- diss %>% filter(time>=0)
diss <- diss %>% group_by(pair, window) %>% mutate(n_time10=ntile(time,10),
                                                   n_time20=ntile(time,20))

write.csv(diss,'Pipeline/tonal/master-emergent-dissonance-zeroed.csv',row.names = FALSE)

# read-in data ####
diss <- read.csv('Pipeline/tonal/master-emergent-dissonance-zeroed.csv')

diss.agg <- diss %>% group_by(pair,condition,yolked_id,window,n_time10) %>%
  summarise(diss.emerge=mean(diss.emerge,na.rm=TRUE),
            diss.comb=mean(diss.comb,na.rm=TRUE))

# visualize Combined Dissonance ####
diss.agg <- diss %>% group_by(pair,condition,window,n_time20) %>%
  summarise(diss.emerge=mean(diss.emerge,na.rm=TRUE),
            diss.comb=mean(diss.comb,na.rm=TRUE))

ggplot(diss.agg, aes(x=diss.comb,color=condition,fill=condition)) +
  geom_density(alpha=.2) + facet_wrap(~window)

ggplot(filter(diss.agg,window==5), aes(x=n_time10,y=diss.comb,color=condition,fill=condition)) +
  geom_point(alpha=.2) + geom_line(alpha=.2)

comb.diss.plt <- ggplot(filter(diss.agg,window==5), aes(x=10*n_time10,y=diss.comb,color=condition,fill=condition)) +
  stat_summary() + xlab("Normalized time (%)") + ylab("Combined Dissonance")

ggplot(filter(diss.agg,window==5), aes(x=diss.comb,color=condition,fill=condition)) +
  geom_density(alpha=.2) + facet_wrap(~n_time10)

ggplot(filter(diss,window==5), aes(x=diss.comb,color=condition,fill=condition)) +
  geom_density(alpha=.2) + facet_wrap(~n_time10)

# visualize Emergent Dissonance ####
ggplot(filter(diss.agg,window==5,condition=="coupled"), aes(x=n_time20,y=diss.emerge)) +
  geom_line() + facet_wrap(~pair)

ggplot(filter(diss.agg,window==5), aes(x=n_time10,y=diss.emerge,color=condition,fill=condition)) +
  geom_point(alpha=.2)

emerge.diss.plt <- ggplot(filter(diss.agg,window==5), aes(x=10*n_time10,y=diss.emerge,color=condition,fill=condition)) +
  stat_summary() + xlab("Normalized time (%)") + ylab("Emergent Dissonance")

ggplot(filter(diss,window==5,n_time10<6), aes(x=diss.emerge,color=condition,fill=condition)) +
  geom_density(alpha=.2) + facet_wrap(~n_time10)

# visualize Combined + Emergent Dissonance ####

library(cowplot)
plot_grid(comb.diss.plt,emerge.diss.plt,
          nrow=2,ncol=1,labels=c("A","B"))

# mixed-effects model -- Combined Dissonance ####

W = 5
diss.full.yok <- lmer(diss.comb ~ n_time10*condition + (1|yolked_id),
                      data=filter(diss,window==W), REML = FALSE)
summary(diss.full.yok)

diss.full.piece <- lmer(diss.comb ~ n_time10*condition + (1|pair),
                        data=filter(diss,window==W), REML=FALSE)
summary(diss.full.piece)


diss.abbr <- lmer(diss.combined ~ n_time*condition + (1|yolked_id), 
                  data=filter(diss.agg,window==W), REML = FALSE)
summary(diss.abbr)
summary(diss.full)



# mixed-effects model -- Emergent Dissonance ####
W=5
emerge.full <- lmer(diss.emerge ~ condition*n_time10 + (1+n_time10|pair),
                    data=filter(diss,window==W))
rand.effects <- data.frame(ranef(emerge.full))
summary(emerge.full)
head(diss)
head(diss.agg)
summary <- read.csv('summary.csv')

diss.agg$n_time2 <- diss.agg$n_time10*diss.agg$n_time10
emerge.abbr <- lmer(diss.emerge ~ n_time10*condition + (1|yolked_id),
                    data=filter(diss.agg,window==W))
summary(emerge.abbr)

diss$pair <- factor(diss$pair)
W=5
emerge.full.yok.pair <- lmer(diss.emerge ~ n_time10*condition + (1|yolked_id) + (1|pair),
                             data=filter(diss,window==W), REML = FALSE)
emerge.full.yok.pair.main <- lmer(diss.emerge ~ n_time10 + condition + (1|yolked_id) + (1|pair),
                             data=filter(diss,window==W), REML = FALSE)
emerge.full.yok <- lmer(diss.emerge ~ n_time10*condition + (1|yolked_id),
                        data=filter(diss,window==W), REML = FALSE)
emerge.full.pair <- lmer(diss.emerge ~ n_time10*condition + (1|pair),
                         data=filter(diss,window==W), REML = FALSE)

summary(emerge.full.yok.pair)
random.effects <- ranef(emerge.full.yok.pair)
random.effects.df <- data.frame(random.effects)
random.effects.pair <- random.effects.df %>% select(grp,condval) %>%
  rename(pair=grp,intercept=condval)
random.effects.pair <- merge(random.effects.pair,summary)
                             
ggplot(random.effects.pair,aes(x=intercept,color=condition,fill=condition)) +
  geom_density(alpha=.2) + ggtitle("Random Intercepts per Piece ID")

t.test(random.effects.wide$coupled,
       random.effects.wide$`one-way`,paired=TRUE)
       
       filter(random.effects.pair,condition=="one-way")$intercept)


# individual models -- Emergent Dissonance ####
lms.norm <- NULL
for (p in unique(diss$pair)) {
  print(p)
  data.p <- diss %>% filter(window==W,pair==p)
  model.p <- lm(diss.emerge ~ n_time10, data.p)
  lms.norm <- rbind(lms.norm, 
               data.frame(pair=p,int=model.p$coefficients[[1]],
                          slope=model.p$coefficients[[2]]))
}

summary <- read.csv('summary-combined.csv')
head(lms.norm)
lms.norm <- merge(lms.norm,summary)
write.csv('Pipeline/tonal/master-emerg-diss-model-ind.csv',row.names=FALSE)
ggplot(lms.norm, aes(x=slope)) +
  geom_histogram() + facet_wrap(~condition,ncol = 1)
  ggtitle("time coefficients for individual pieces")
head(lms)
t.test(filter(lms.norm,condition=="coupled")$slope,
       filter(lms.norm,condition=="one-way")$slope)
lms.norm$condition <- factor(lms.norm$condition)
lms.norm.wide <- lms.norm %>% group_by(yolked_id,condition) %>% 
  summarise(slope=mean(slope)) %>% ungroup() %>% select(yolked_id,condition,slope) %>% 
  spread(condition,slope)
t.test(lms.norm.wide$coupled,lms.norm.wide$`one-way`,paired=TRUE)

ggplot(lms, aes(x=int,color=condition,fill=condition)) +
  geom_density(alpha=.2)

summary(a1b1.quad)
anova(a1b1.lm,a1b1.t)


# individual models -- 1st half of Emergent Dissonance ####
lms.norm.half <- NULL
for (p in unique(diss$pair)) {
  print(p)
  data.p <- diss %>% filter(window==W,pair==p,n_time10<=5)
  model.p <- lm(diss.emerge ~ n_time10, data.p)
  lms.norm.half <- rbind(lms.norm.half, 
                    data.frame(pair=p,int=model.p$coefficients[[1]],
                               slope=model.p$coefficients[[2]]))
}

lms.norm.half <- merge(lms.norm.half,summary)
ggplot(lms.norm.half, aes(x=slope,color=condition,fill=condition)) +
  geom_density(alpha=.2)
