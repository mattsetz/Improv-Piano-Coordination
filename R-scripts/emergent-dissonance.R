####################################
# emergent dissonance analysis
# MS 8/27/19
####################################

library(tidyverse)


# Input Data ##################################

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

View(filter(diss[order(diss$time),],pair=="a1b1",window==2))






# Dimensionality reduction -- dynamic control variable #######################################################
head(diss)

diss.ratio <- diss %>% group_by(pair,n_time,condition,window) %>%
    summarise(var.ind=mean(c(var(diss1),var(diss2))),
              var.group=var(diss.emerge)) %>% ungroup()
diss.ratio$compensate <- diss.ratio$var.ind/diss.ratio$var.group
diss.ratio$condition <- paste(diss.ratio$condition, ".ordered", sep="")

# shuffle time
diss.ratio.shuffled <- diss %>% group_by(pair,n_time_rand,condition,window) %>%
  summarise(var.ind=mean(c(var(diss1),var(diss2))),
            var.group=var(diss.emerge)) %>% ungroup()
diss.ratio.shuffled$compensate <- diss.ratio.shuffled$var.ind/diss.ratio.shuffled$var.group
diss.ratio.shuffled$n_time <- diss.ratio.shuffled$n_time_rand
diss.ratio.shuffled$condition <- paste(diss.ratio.shuffled$condition, ".shuffled", sep="")
diss.ratio.shuffled <- diss.ratio.shuffled %>% select(-n_time_rand)

diss.ratio <- rbind(diss.ratio, diss.ratio.shuffled)

# visualize coupled versus one-way by window
ggplot(diss.ratio, aes(x=window,y=compensate,color=condition,fill=condition)) +
  stat_summary() +
  xlab("window size (sec)") +
  ylab("compensation ratio") +
  theme_classic()

diss.ratio %>% group_by(pair,window,condition) %>%
  summarise(avg.compensate=mean(compensate,na.omit=TRUE)) %>%
  ungroup() %>% group_by(condition,window) %>% 
  summarise(avg.compensate.cond=mean(avg.compensate),
            se.compensate.cond=sd(avg.compensate)/sqrt(n()))


diss.pair <- diss.ratio %>% group_by(pair,condition,window) %>% summarise(avg.compensation=mean(compensate,na.omit=TRUE))
ggplot(filter(diss.pair,window==10), aes(x=avg.compensation,color=condition,fill=condition)) +
  geom_density(alpha=0.2) +
  xlim(0,25) +
  xlab("compensation") +
  theme_classic()

t.test(filter(diss.pair,window==5,condition=="coupled")$avg.compensation,
       filter(diss.pair,window==5,condition=="one-way")$avg.compensation)

### ^^^ significant ^^^

# uncoupled pairs
diss.uncoupled <- read.csv('Pipeline/tonal/master-emergent-diss-surrogate.csv')
head(diss.uncoupled)
diss.uncoupled$pair <- paste(diss.uncoupled$player1,diss.uncoupled$player2,sep="")
diss.uncoupled$condition <- "uncoupled"
uncoupled.sum <- diss.uncoupled %>% group_by(pair) %>% mutate(n_time=ntile(time,10)) %>% ungroup() %>% na.omit()
head(uncoupled.sum)
uncoupled.sum <- uncoupled.sum %>% group_by(pair,n_time,window,condition) %>%
  summarise(diss1.var=var(diss1),
            diss2.var=var(diss2),
            diss12.var=var(diss12)) %>% ungroup()
uncoupled.sum$var.ratio <- uncoupled.sum$diss12.var/((uncoupled.sum$diss1.var+uncoupled.sum$diss2.var)/2)

head(uncoupled.sum)
head(diss.summary)
diss.summary <- rbind(diss.summary,uncoupled.sum)
diss.summary %>% group_by(condition,window) %>% summarise(mean.ratio=mean(var.ratio),
                                                          sd.ratio=sd(var.ratio)) %>% ungroup()





# what is the relationship btw dissonance and emergent dissonance? ############################
head(diss)
ggplot(diss, aes(x=diss.comb, y=diss.emerge)) +
  geom_point(alpha=0.3) +
  geom_smooth(method=lm,se=TRUE)

a1b1 <- diss %>% filter(pair=="e2f2")
ggplot(filter(a1b1,time1<25), aes(x=time1,y=dissonance)) +
  geom_point(alpha=.3) + geom_line() +
  geom_point(aes(y=diss.emerge),alpha=.3,color="blue") + geom_line(aes(y=diss.emerge),color="blue")

# look for differences between condition + time function
ggplot(diss, aes(x=diss.emerge,color=condition,fill=condition)) +
  geom_density(alpha=.4)
t.test(filter(diss,condition=="coupled")$diss.emerge,
       filter(diss,condition=="one-way")$diss.emerge)




# Emergent Consonance ##############
diss$cons.emerge <- diss$diss.emerge < 0
consonance.summary <- diss %>% group_by(condition,cons.emerge) %>% summarise(n=n())
diss <- diss %>% group_by(pair) %>% mutate(n_time=ntile(time1,10)) %>% ungroup()
ggplot(filter(diss,cons.emerge), aes(x=diss.emerge,color=condition)) +
  geom_density() +
  facet_wrap(~n_time)

ggplot(filter(diss, cons.emerge), aes(x=diss.emerge,color=condition,fill=condition)) +
  geom_density(alpha=.4)
t.test(filter(diss, cons.emerge, condition=="coupled")$diss.emerge,
       filter(diss, cons.emerge, condition=="one-way")$diss.emerge)




# Dissonnace and Emergent Dissonance By Condition (ignoring time) ####
head(diss)

diss.sum <- diss %>% group_by(yolked_id,window,condition) %>% drop_na() %>%
  summarise(diss = mean(diss.comb, na.rm=TRUE),
            diss.emerge = mean(diss.emerge, na.rm=TRUE),
            diss_ind = mean(diss.1+diss.2)) %>% ungroup()

# test for difference in individual consonance
diss_ind_wide <- diss.sum %>% select(-diss,-diss.emerge) %>% pivot_wider(names_from = c("condition","window"),values_from = diss_ind)
t.test(-diss_ind_wide$coupled_2,-diss_ind_wide$`one-way_2`,paired=TRUE)                                        

ks.test(filter(diss.sum,window==5,condition=="coupled")$diss,
        filter(diss.sum,window==5,condition=="one-way")$diss)
diss.sum %>% group_by(window,condition) %>% summarise(var.diss=var(diss))
# https://www.statisticshowto.datasciencecentral.com/probability-and-statistics/hypothesis-testing/f-test/#targetText=F%20Test%20to%20Compare%20Two%20Variances,-A%20Statistical%20F&targetText=If%20the%20variances%20are%20equal,when%20running%20an%20F%20Test.
.00672/.00318 = 2.113
numerator.df = 49
denominator.df = 42
pf(2.113,49,42,lower.tail = FALSE)

# combined dissonance
ggplot(diss.sum, aes(x=diss, fill=condition, color=condition)) +
  geom_density(alpha=.2) + facet_wrap(~window) + xlab('Combined Dissonance')

W = 5
diss.sum.wide <- diss.sum %>% select(-diss) %>% spread(condition,diss.emerge)
t.test(filter(diss.sum.wide, window==W)$coupled,
       filter(diss.sum.wide, window==W)$'one-way', paired = TRUE)


# emergent dissonance
ggplot(diss.sum, aes(x=diss.emerge, fill=condition, color=condition)) +
  geom_density(alpha=.2) + facet_wrap(~window) + xlab('Emergent Dissonance')

emerge.sum.wide <- diss.sum %>% select(-diss) %>% spread(condition,diss.emerge)
W = 5
t.test(filter(emerge.sum.wide,window==W)$coupled,
       filter(emerge.sum.wide,window==W)$'one-way', paired=TRUE)

ggplot(diss, aes(x=diss.emerge,color=condition,fill=condition)) +
  geom_density(alpha=0.2) + facet_wrap(~window)


# Dissonance + Emergent Dissonance Over Time ###############

# visualize
head(diss)
diss <- diss %>% group_by(pair,window) %>% mutate(n_time=ntile(time,10)) %>% ungroup()
diss.agg <- diss %>% group_by(yolked_id,condition,n_time,window) %>%
                        summarise(diss.combined = mean(diss.comb,na.rm = TRUE),
                                  diss.emergent = mean(diss.emerge,na.rm = TRUE)) %>% ungroup()
diss.comb.agg <- diss.agg %>% select(-diss.emergent) %>% spread(condition,diss.combined)

# combined dissonance
diss.comb.agg$diff <- diss.comb.agg$coupled - diss.comb.agg$`one-way`
ggplot(filter(diss.comb.agg,window<10), aes(x=diff)) +
  geom_density(alpha=.2) + facet_wrap(~window+n_time) + xlab('coupled - oneway') + 
  ggtitle('combined dissonance')
ggplot(filter(diss.agg,window==5,n_time==8), aes(x=diss.combined,fill=condition,color=condition)) +
  geom_density(alpha=.2)+xlab('combined dissonance')
ggplot(filter(diss.agg,window==5), aes(x=n_time,y=diss.combined,color=condition,fill=condition)) +
  stat_summary()

# emergent dissonance 
diss.emerge.agg <- diss.agg %>% select(-diss.combined) %>% spread(condition,diss.emergent)
diss.emerge.agg$diff <- diss.emerge.agg$coupled - diss.emerge.agg$`one-way`
ggplot(filter(diss.emerge.agg,window<10), aes(x=diff)) +
  geom_density(alpha=.2) + facet_wrap(~window+n_time) + xlab('coupled - oneway') + 
  ggtitle('emergent dissonance by window and time')
ggplot(filter(diss.agg,window<10), aes(x=diss.emergent,fill=condition,color=condition)) +
  geom_density(alpha=.2)+facet_wrap(~window+n_time)+xlab('emergent dissonance')
ggplot(filter(diss.agg,window==5), aes(x=n_time,y=diss.emergent,color=condition,fill=condition)) +
  stat_summary()

# model + significance testing
library(lme4)
# diss ~ n_t + n_t_squared + n_t*condition + n_t_squared*condition + condition + (1|yolked_id)

diss.agg$n_time_sq = diss.agg$n_time*diss.agg$n_time

# model comparison w/ and w/o condition
W = 5
diss$n_time_sq <- diss$n_time*diss$n_time
glm.diss.comb.agg <- lmer(diss.combined ~ n_time*condition + n_time_sq +
                     (1|yolked_id), data=filter(diss.agg,window==W), REML = FALSE)
summary(glm.diss.comb.agg)
null.diss.comb.agg <- lmer(diss.combined ~ n_time + n_time_sq + (1|yolked_id), data=filter(diss.agg,window==W), REML = FALSE)
summary(glm.diss.comb.agg)

anova(glm.diss.comb.agg, null.diss.comb.agg)
anova(glm.diss.comb.agg)
library(lmerTest)
install.packages("lmerTest")

ranef(glm.diss.comb.agg)

glm.diss.comb <- lmer(diss.comb ~ n_time*condition + n_time_sq + (1|yolked_id),
                      data=filter(diss,window==W), REML = FALSE)
null.diss.comb <- lmer(diss.comb ~ n_time + n_time_sq + (1|yolked_id),
                       data=filter(diss,window==W), REML = FALSE)
anova(glm.diss.comb, null.diss.comb)

# Variability in dissonance #######
head(diss)
diss <- diss %>% group_by(pair,window,condition) %>% mutate(quartile_time=ntile(time1,4))
diss.var.summary <- diss %>% group_by(pair,window,condition) %>% summarise(diss.var=var(dissonance),
                                                    emerge.diss.var=var(diss.emerge))
diss.var.summary <- diss %>% group_by(pair,window,condition,quartile_time) %>% 
  summarise(avg.diss=mean(dissonance))
diss.var.summary <- diss.var.summary %>% group_by(pair,window,condition) %>%
  summarise(diss.var=var(avg.diss))

diss.var.summary.long <- diss.var.summary %>% gather(diss.metric,var,c(diss.var,emerge.diss.var))
ggplot(diss.var.summary, aes(x=condition,y=diss.var)) +
  stat_summary()

ggplot(diss.var.summary, aes(x=diss.var,color=condition,fill=condition)) +
  geom_density(alpha=.2)

t.test(filter(diss.var.summary,condition=="coupled",window==2)$diss.var,
       filter(diss.var.summary,condition=="one-way",window==2)$diss.var)



# SURROGATE ANALYSIS ####
surrogate.diss <- read.csv('Pipeline/tonal/master-emergent-diss-surrogate.csv')
surrogate.diss$condition <- 'uncoupled'
surrogate.diss$pair <- paste(surrogate.diss$player1, surrogate.diss$player2, sep='')
surrogate.diss <- surrogate.diss %>% rename(diss.emerge=diss_emerge,
                                            diss.emerge2=diss_emerge2)
head(surrogate.diss)
head(diss)
diss.total <- rbind(select(diss,time,condition,window,pair,diss.emerge,diss.emerge2),
                    select(surrogate.diss,time,condition,window,pair,diss.emerge,diss.emerge2))
head(diss.total)

# summary
diss.total %>% group_by(condition,window) %>% summarise(avg_diss=mean(diss.emerge,na.rm=TRUE),
                                                 se_diss=sd(diss.emerge,na.rm=TRUE))


ggplot(diss.total, aes(x=diss.emerge,color=condition,fill=condition)) +
  geom_density(alpha=.4) +
  facet_wrap(~window) +
  ggtitle("diss.comb-mean(diss1,diss2)")

# emergent consonance
diss.total$cons.emerge <- diss.total$diss.emerge < 0
consonance.summary <- diss.total %>% group_by(condition,cons.emerge,window) %>% summarise(n=n())
ggplot(filter(diss, cons.emerge), aes(x=diss.emerge,color=condition,fill=condition)) +
  geom_density(alpha=.4)
t.test(filter(diss, cons.emerge, condition=="coupled")$diss.emerge,
       filter(diss, cons.emerge, condition=="one-way")$diss.emerge)

# emergent dissonance over time
diss.sum <- diss.total %>% group_by(pair,window,condition) %>%
  mutate(n_time=ntile(time, 10))
diss.sum <- diss.sum %>% group_by(pair,n_time,window,condition) %>% 
  summarise(emerge.diss.avg = mean(diss.emerge,na.rm=TRUE),
            emerge.diss.avg2 = mean(diss.emerge2,na.rm=TRUE)) %>% ungroup()
diss.sum <- diss.sum %>% group_by(n_time,window,condition) %>% 
  summarise(avg.emerge.diss = mean(emerge.diss.avg, na.rm=TRUE),
            se = sd(emerge.diss.avg, na.rm=TRUE)/sqrt(n()),
            avg.emerge.diss2 = mean(emerge.diss.avg2, na.rm=TRUE),
            se2 = sd(emerge.diss.avg2, na.rm=TRUE)/sqrt(n())) %>% ungroup()

ggplot(diss.sum, aes(x=n_time, y=avg.emerge.diss, color=condition, fill=condition)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=avg.emerge.diss-se,
                  ymax=avg.emerge.diss+se),
              alpha = 0.4) +
  facet_wrap(~window) +
  ggtitle("diss.comb-mean(diss1,diss2)")

ggplot(diss.sum, aes(x=n_time, y=avg.emerge.diss2, color=condition, fill=condition)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=avg.emerge.diss2-se2,
                  ymax=avg.emerge.diss2+se2),
              alpha = 0.4) +
  facet_wrap(~window) +
  ggtitle("diss.comb-(diss1+diss2)")
