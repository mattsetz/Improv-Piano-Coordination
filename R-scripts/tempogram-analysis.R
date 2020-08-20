library(tidyverse)
library(lme4)
library(lmerTest)

tempo <- read.csv("Pipeline/tempograms-master.csv") %>% rename(trial_name=pair)
summary <- read.csv("summary-combined.csv")
tempo <- merge(tempo, summary)

model <- lmer(tempo_power ~ condition+(1|yoked_pair_id)+(1|yoked_piece_id), data=tempo)
summary(model)

ggplot(tempo,aes(x=tempo_power,color=condition))+geom_density(alpha=.2)