#################################
# Analysis of combined tonal entropy
#################################

library(tidyverse)

#################################
# input data
#################################
entropy <- NULL
inDir <- 'Pipeline/tonal/tonal-entropy/combined/'
infiles <- list.files(inDir)
for (f in infiles) {
  print(f)
  df <- read.csv(paste(inDir,f,sep=''))
  df$name <- str_replace(f,'.csv','')
  entropy <- rbind(entropy, df)
}

length(unique(entropy$name))

# more entropy in virtual trials?
ggplot(entropy, aes(x=entropy10,color=condition,fill=condition))+
  geom_density(alpha=0.3)
t.test(filter(entropy,condition=="coupled")$entropy10,
       filter(entropy,condition=="one-way")$entropy10) # more overall entropy in one-way pieces


#################################
# Entropy over normalized time
#################################
entropy <- entropy %>% gather(window,entropy,-time,-condition,-name)
entropy <- entropy %>% group_by(name) %>% mutate(n_t = ntile(time,10))
entropy <- entropy %>% ungroup() %>% group_by(name, n_t, window) %>%
              mutate(avg_entropy_trial = mean(entropy,na.rm=TRUE)) %>% ungroup()

entropy.summary <- entropy %>% group_by(n_t, window, condition) %>% 
                   summarize(mean_entropy=mean(avg_entropy_trial,na.rm=TRUE),
                             sd_entropy = sd(avg_entropy_trial,na.rm=TRUE)/sqrt(length(unique(name))))

# aggregate
entropy.summary$n_t <- 10*entropy.summary$n_t
ggplot(entropy.summary, aes(x=n_t,y=mean_entropy,color=condition,fill=condition)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin=mean_entropy-sd_entropy,
                  ymax=mean_entropy+sd_entropy),
              alpha=0.3) +
  xlab('Normalized Time (%)') +
  ylab('Entropy') +
  facet_wrap(~window)

# individual plots
ggplot(filter(entropy,condition=='coupled'), aes(x=n_t,y=avg_entropy_trial)) +
  geom_line() +
  facet_wrap(~name) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) +
  ylim(1.5,3.5) +
  ggtitle("Coupled Trials")


ggplot(filter(entropy,condition=='one-way'), aes(x=n_t,y=avg_entropy_trial)) +
  geom_line() +
  facet_wrap(~name) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  ) +
  ggtitle("One-Way Trials")

  
# listen to examples of max/min entropy
entropy <- entropy %>% filter(window=='entropy5')
entropy.sum <- entropy %>% group_by(name) %>% summarise(n_entropy5=ntile(entropy5,10)) %>% ungroup()
entropy.sum <- entropy.sum[order(entropy.sum$n_entropy5),]

# vizualize onset density per individual
ggplot(filter(entropy, window=="entropy10"), aes(x=entropy)) +
  geom_density() +
  facet_wrap(~name) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
entropy$name <- str_replace_all(entropy$name,'[0-9]','')
summary <- entropy %>% filter(window=="entropy5") %>% group_by(name, condition) %>% summarize(mean_entropy=mean(entropy),
                                                           sd=sd(entropy))
summary <- summary %>% filter(condition=="Real")
summary <- summary[order(summary$mean_entropy),]

  
################################
# Onset Density
################################
onsets <- NULL
inDir <- 'Pipeline/onset-density/'
infiles <- list.files(inDir)
for (f in infiles) {
  print(f)
  df <- read.csv(paste(inDir,f,sep=''))
  df$name <- f
  onsets <- rbind(onsets,df)
}

# is there a correlation between entropy and onset density?
onset_names <- c()
for (name in strsplit(onsets.tmp$name,'[.]')) {
  print(name[[2]])
  onset_names <- c(onset_names,name[[2]])
}

onsets$name <- str_replace(str_replace(onsets$name,".*?[.]",""), "[.].*","")
onsets <- onsets %>% select(-avg_vel)

onsets <- onsets %>% group_by(name) %>% mutate(n_t=ntile(time,10)) %>% ungroup()
onsets <- onsets %>% group_by(name, n_t) %>% summarize(mean_density=mean(num_onsets)) %>% ungroup()
onsets <- onsets %>% select(n_t,mean_density,name)

entropy <- read.csv('Pipeline/tonal-entropy-combined.csv')
entropy <- entropy %>% gather(window,entropy,-time,-condition,-name)
entropy <- entropy %>% group_by(name,window) %>% mutate(n_t=ntile(time,10)) %>% ungroup()
entropy <- entropy %>% group_by(name,n_t,window) %>% summarize(avg_entropy=mean(entropy))
entropy <- entropy %>% select(n_t,avg_entropy,window,name)

entropy.onsets <- merge(onsets,entropy)
ggplot(entropy.onsets, aes(x=mean_density, y=avg_entropy)) +
  geom_point(alpha=.3)

# view aggregate onset density over normalized time
onsets.summ <- onsets %>% group_by(n_t) %>% summarize(avg_density=mean(mean_density),
                                                      density_var=sd(mean_density,na.rm=TRUE)/sqrt(length(unique(onsets$name))))

ggplot(onsets.summ, aes(x=n_t,y=avg_density)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin=avg_density-density_var,
                  ymax=avg_density+density_var),
              alpha=0.3) +
  xlab('Normalized Time') +
  ylab('Onset Density') +
  ggtitle('Onset Density Over Normalized Time
          All Conditions Combined')

# view onsets over normal time faceted
names <- unique(onsets$name)
onsets.20 <- onsets %>% filter(name %in% names[1:20])
ggplot(onsets.20, aes(x=n_t,y=mean_density)) +
  geom_line() +
  facet_wrap(~name)
