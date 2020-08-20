library(tidyverse)
library(lme4)
library(lmerTest)
library(brms)

onsets_cc <- read.csv("~/Desktop/cross-correlation-onset-density.csv")
yoked_piece_ids <- read.csv('yoked-piece-ids.csv')
onsets_cc <- merge(onsets_cc,yoked_piece_ids)
head(onsets_cc)
summary <- read.csv('summary-combined.csv') %>% select(trial_name,yoked_pair_id,pair)
onsets_cc <- merge(onsets_cc,summary)

# linear model of onsets cc by condition ----------------------------------
onsets_cc_avg <- onsets_cc %>% group_by(trial_name,condition,yoked_pair_id,yoked_piece_id) %>% 
                  summarise(avg_cc=mean(cross_correlation))


cc_model <- lmer(avg_cc ~ 1 + condition + (1|yoked_pair_id) + (1|yoked_piece_id),data=onsets_cc_avg)
summary(cc_model)

cc_brms_fit <- brm(avg_cc ~ 1 + condition + (1|yoked_pair_id) + (1|yoked_piece_id),
                     data=onsets_cc_avg, file = "brms-fits/cc-onset-density-condition")
cc_brms_fit <- update(cc_brms_fit, newdata=onsets_cc_avg, iter = 4000)
saveRDS(cc_brms_fit, file = "brms-fits/cc-onset-density-condition.rds")
summary(cc_brms_fit)
plot(cc_brms_fit)



# model of oneway onsets --------------------------------------------------

onsets_oneway <- onsets_cc %>% filter(condition=="one-way",lag!=0)
onsets_oneway$lag_sign <- if_else(onsets_oneway$lag>0,"pos","neg")
onsets_oneway_avg <- onsets_oneway %>% group_by(trial_name,pair,lag_sign) %>%
                      summarise(avg_cc=mean(cross_correlation))

cc_oneway_model <- lmer(avg_cc ~ 1 + lag_sign + (1|pair) + (1|trial_name), data=onsets_oneway_avg)
summary(cc_oneway_model)

cc_oneway_model_brms <- brm(avg_cc ~ 1 + lag_sign + (1|pair) + (1|trial_name),
                            data=onsets_oneway_avg, file="brms-fits/cc-onset-density-oneway")
cc_oneway_model_brms <- update(cc_oneway_model_brms, newdata=onsets_oneway_avg,
                               iter=5000, control = (list("adapt_delta"=.99)))
plot(cc_oneway_model_brms)
summary(cc_oneway_model_brms)
saveRDS(cc_brms_fit, file = "brms-fits/cc-onset-density-oneway.rds")
"
Def sig main effect, just weird sampling issues I think because of the prior on pair intercept.
Not sure how to change the prior to be honest, but whatever not worth it right now.
"


# is cc greater at 2.4 lags vs simultaneous in coupled trials? --------------------------------------------

head(onsets_cc)

onsets_coupled <- onsets_cc %>% filter(condition=="coupled",lag==2.4|lag==0)
onsets_coupled$lag <- factor(onsets_coupled$lag)

onsets_coupled_wide <- onsets_coupled %>% pivot_wider(names_from = lag,values_from = cross_correlation)
head(onsets_coupled_wide)
onsets_coupled_wide$cc_diff <- onsets_coupled_wide$`0`-onsets_coupled_wide$`2.4`
ggplot(onsets_coupled_wide, aes(x=cc_diff)) + geom_histogram()
onsets_avg <- onsets_coupled_wide %>% group_by(pair) %>% summarise(cc_diff=mean(cc_diff))

model <- lmer(cc_diff ~ 1+(1|pair), data=onsets_coupled_wide)
summary(model)

brms_fit <- brm(cross_correlation ~ 1+lag+(1|trial_name)+(1|pair), data=onsets_coupled)
brms_fit <- update(brms_fit, iter=8000)
plot(brms_fit)
print(summary(brms_fit),digits=5)
hypothesis(brms_fit,"lag2.4>0")

brms_fit <- brm(cc_diff ~ 1+(1|pair),data=onsets_coupled_wide)
plot(brms_fit)
summary(brms_fit)
hypothesis(brms_fit,"Intercept<0")

"
depending on how you look at it, cc is not higher at 2.4 compared to 0.
looks like there are more pieces with low cc at 0, and perhaps this is because of more trading in
these pieces, but I'm not going to go into too much detail about this in the OpenMind paper.
"