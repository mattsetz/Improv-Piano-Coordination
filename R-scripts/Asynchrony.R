#################################
# Asynchrony analysis
#################################

library(tidyverse)

asyncs <- read.csv('Pipeline/master_asyncs.csv')
asyncs$cond <- ifelse(asyncs$cond=='Real','coupled','one-way')
asyncs$condition <- asyncs$cond
asyncs$asyncs <- 1000*asyncs$asyncs

ggplot(asyncs, aes(x=asyncs, fill=condition, color=condition)) +
  geom_density(alpha=.3,) + coord_cartesian(ylim=c(0.003, 0.0065)) +
  theme_bw() + theme(axis.text.y=element_blank(),legend.position = c(.85,.85)) +
  xlab("asynchrony (ms)") +
  ylab("normalized frequency")

ks.test(filter(asyncs,condition=="coupled")$asyncs,
        filter(asyncs,condition=="one-way")$asyncs)

head(asyncs)
asyncs$pos <- asyncs$asyncs>0
asyncs.sum <- asyncs %>% filter(condition=="one-way",abs(asyncs)<50) %>% 
  group_by(person_trial, pos) %>% summarise(num=n())
asyncs.wide <- asyncs.sum %>% spread(pos,num)
asyncs.wide$diff <- asyncs.wide$'FALSE' - asyncs.wide$'TRUE'
ggplot(asyncs.wide,aes(x=diff))+geom_histogram()
head(asyncs.wide)
t.test(asyncs.wide$'FALSE',asyncs.wide$'TRUE',paired=TRUE)

asyncs.50 <- asyncs %>% filter(abs(asyncs)<.05)
ggplot(asyncs.50, aes(x=asyncs, fill=cond, color=cond)) +
  geom_histogram(position='dodge') +
  ggtitle("Asynchronies By Condition (50 ms)") +
  xlab('asynchrony (sec)')

asyncs.50.virtual <- asyncs.50 %>% filter(cond=="one-way")
mean(asyncs.50.virtual$asyncs) # -.7 ms, live leads ghost. weird, not sure what to make of this
asyncs.50.real <- asyncs.50 %>% filter(cond=="Real")

# Rate of synchrony
onsets <- read.csv('Pipeline/master_onsets.csv')
num_onsets <- onsets %>% group_by(person_trial) %>%
              mutate(total_onsets = n())
num_onsets <- num_onsets %>% group_by(personCombo) %>%
              summarize(person_trial = person_trial[1],
                        condition = cond[1],
                        total_onsets = min(total_onsets))

asyncs.summary <- asyncs %>% group_by(person_trial) %>%
                  summarize(num_onsets100 <- n())

asyncs.summary.50 <- asyncs %>% filter(abs(asyncs)<.05) %>%
                      group_by(person_trial) %>%
                      summarize(num_onsets50 <- n())
asyncs.summary <- merge(asyncs.summary,asyncs.summary.50)
asyncs.summary <- merge(asyncs.summary, num_onsets)
asyncs.summary$sync_rate50 <- asyncs.summary$`num_onsets50 <- n()`/asyncs.summary$total_onsets
asyncs.summary$sync_rate100 <- asyncs.summary$`num_onsets100 <- n()`/asyncs.summary$total_onsets

mean(unique(filter(asyncs.summary,condition=="one-way")$sync_rate50))
mean(unique(filter(asyncs.summary,condition=="coupled")$sync_rate50))

ggplot(asyncs.summary, aes(x=sync_rate50, color=condition, fill=condition))+
  geom_histogram(position="dodge") +
  ggtitle("Rate of Synchrony\n50ms window")

ggplot(asyncs.summary, aes(x=sync_rate100, color=condition, fill=condition))+
  geom_histogram(position="dodge") +
  ggtitle("Rate of Synchrony\n100ms window")


# significance test
ks.test(filter(asyncs,cond=='coupled')$asyncs,filter(asyncs,cond=='one-way')$asyncs)

asyncs.summary.real <- asyncs.summary %>% filter(condition=="coupled")
asyncs.summary.virtual <- asyncs.summary %>% filter(condition=="one-way")

t.test(asyncs.summary.real$sync_rate50, asyncs.summary.virtual$sync_rate50)
t.test(asyncs.summary.real$sync_rate100, asyncs.summary.virtual$sync_rate100)

asyncs.summary.real <- asyncs.summary.real[order(asyncs.summary.real$personCombo),]
asyncs.summary.virtual <- asyncs.summary.virtual[order(asyncs.summary.virtual$personCombo),]