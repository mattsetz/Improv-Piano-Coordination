########################
# Endings analysis
# MS 8/20/19
########################

library(tidyverse)

onsets <- read.csv('Pipeline/master_onsets.csv')
head(onsets)

endings <-NULL
for (p in unique(onsets$person_trial)) {
  print(p)
  onsets.p <- onsets %>% filter(person_trial==p)
  
  end1 = max(onsets.p$time)
  other = as.character(onsets.p$other_person[[1]])
  end2 = max(filter(onsets,person_trial==other)$time)
  
  dyad = as.character(onsets.p$personCombo[[1]])
  endings = rbind(endings, data.frame(personCombo=dyad,
                                      async=end1-end2,
                                      cond=onsets.p$cond[[1]]))
}

endings$abs_async <- abs(endings$async)

t.test(filter(endings,cond=="coupled")$abs_async,
       filter(endings,cond=="one-way")$abs_async)

# t(95)=-3.38; p < 0.005
# mean coupled: 6.2
# mean one-way: 18.2

head(endings)
ggplot(endings, aes(abs_async,fill=cond,color=cond)) +
  geom_histogram(alpha=0.3,position="dodge")

ggplot(endings, aes(abs_async,fill=cond,color=cond)) +
  geom_density(alpha=0.3)
