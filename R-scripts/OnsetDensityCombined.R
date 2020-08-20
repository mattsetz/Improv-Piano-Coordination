########################
# MS 5/7/19
# Analyze combined onset density
########################

library(tidyverse)

#inDir <- "Pipeline/onset-density/combined/"
inDir <- "Pipeline/rhythm/onset-density/collapsed/combined/"
density <- NULL
infiles <- list.files(inDir)
for (f in infiles) {
  print(f)
  df <- read.csv(paste(inDir,f,sep=''))
  density <- rbind(density, df)
}
density$num_onsets = density$density
density <- read.csv("Pipeline/rhythm/onset-density/collapsed/master-onset-density-combined.csv")
summary <- read.csv("summary-combined.csv")
density <- merge(density, summary)

# sanity check
length(unique(density$name))
head(density)
durations <- density %>% group_by(name, condition) %>% summarise(max_length=max(time))
durations <- durations[order(durations$max_length),]

############################################
# onset density over absolute time
############################################
# visualize individual trials
ggplot(filter(density,condition=='one-way'), aes(x=time,y=density)) +
  geom_line() +
  facet_wrap(~name) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

ggplot(filter(density,condition=='coupled'), aes(x=time,y=density)) +
  geom_line() +
  facet_wrap(~name) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

# visualize aggregate
density.agg <- density %>% group_by(condition, time) %>%
                summarise(avg_num_onsets = mean(num_onsets),
                          se = sd(num_onsets)/sqrt(n()))
ggplot(density.agg, aes(x=time, y=avg_num_onsets, color=condition, fill=condition))+
  geom_line() +
  geom_ribbon(aes(ymin=avg_num_onsets-se,
              ymax=avg_num_onsets+se),alpha=.3)

############################################
# onset density over normalized time
############################################
density <- density %>% ungroup() %>% group_by(yolked_id) %>%
            mutate(n_t = ntile(time, 10)) %>% ungroup()
density.agg <- density %>% group_by(yolked_id,condition,n_t) %>%
                summarise(avg_density = mean(num_onsets)) %>% ungroup()
density.agg <- density.agg %>% group_by(condition,n_t) %>%
                summarise(mean_density = mean(avg_density),
                          se = sd(avg_density)/sqrt(n()))
density.agg$n_t = density.agg$n_t*10

ggplot(density.agg, aes(x=n_t,y=mean_density,color=condition,fill=condition))+
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=mean_density-se,
                  ymax=mean_density+se),alpha=.3) +
  xlab("normalized time (%)") +
  ylab("mean onset density") +
  theme_classic() +
  ggtitle('Combined Onset Density over Normalized Time')

# mixed-effects model of onset density
library(lme4)
density$n_t_squared <- density$n_t*density$n_t
density.model <- lmer(num_onsets ~ n_t + n_t_squared + n_t*condition + condition +
                        (1|yolked_id), data=density,REML = FALSE)
summary(density.model)
density.null <- lmer(num_onsets ~ n_t + n_t_squared + condition +
                        (1|yolked_id), data=density, REML=FALSE)
anova(density.model, density.null)

############################################
# distribution of onset density
############################################
density.coupled = density %>% filter(condition=='coupled')
density.one.way = density %>% filter(condition=='one-way')

t.test(density.coupled$num_onsets, density.one.way$num_onsets, paired = FALSE)

ggplot(filter(density,n_t>6), aes(x=num_onsets,color=condition,fill=condition))+
  geom_histogram(position="dodge",bins=15,stat="frequency")

############################################
# Surrogate Analysis
############################################
density <- density %>% select(name,condition,time,density)

inDir <- "Pipeline/rhythm/onset-density/collapsed/surrogate/"
infiles <- list.files(inDir)
for (f in infiles) {
  print(f)
  df <- read.csv(paste(inDir,f,sep=''))
  df$name <- str_split(f,'-')[[1]][[1]]
  df$density <- df$num_onsets
  df$condition <- "surrogate"
  density <- rbind(density,select(df,-num_onsets))
}


############################################
# onset density over normalized time
############################################

density <- density %>% group_by(name) %>%
  mutate(n_t = ntile(time, 10)) %>% ungroup()
density.agg <- density %>% group_by(name, condition, n_t) %>%
  summarise(avg_density = mean(density)) %>% ungroup()
density.agg <- density.agg %>% group_by(condition,n_t) %>%
  summarise(mean_density = mean(avg_density),
            se = sd(avg_density)/sqrt(n()))
density.agg$n_t = density.agg$n_t*10

ggplot(density.agg, aes(x=n_t,y=mean_density,color=condition,fill=condition))+
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin=mean_density-se,
                  ymax=mean_density+se),alpha=.3) +
  xlab("normalized time (%)") +
  ylab("mean onset density") +
  theme_classic() +
  ggtitle('Combined Onset Density over Normalized Time')
