library(tidyverse)
library(lme4)
library(lmerTest)
library(brms)

diss <- read.csv('master-diss-matrix.csv')
diss <- diss %>% rename(trial_name=pair)
diss$pair <- str_remove_all(diss$trial_name,'[0-9]')
yoked_piece_ids <- read.csv('yoked-piece-ids.csv')
diss <- merge(diss,yoked_piece_ids)
summary <- read.csv('summary-combined.csv')
summary <- summary %>% select(yoked_pair_id,trial_name)
diss <- merge(diss,summary)

# model lagged combined consonance in oneway trials -------------------------------------------------

# trim to avoid boundary effects
diss_oneway <- diss %>% filter(n_time1>1,n_time1<10,n_time2>1,n_time<10,
                               condition=="one-way",direction != "simultaneuos")
diss_avg <- diss_oneway %>% group_by(trial_name,pair,direction,window) %>% 
              summarise(avg_comb_cons=-mean(combined_dissonance),
                        avg_emerge_cons=-mean(diss.emerge))

# 2 second window
comb_cons_model_2 <- lmer(avg_comb_cons ~ 1 + direction + (1|trial_name) + (1|pair),
                          data=filter(diss_avg,window=="2 second window"))
summary(comb_cons_model_2)
emerge_cons_model_2 <- lmer(avg_emerge_cons ~ 1 + direction + (1|trial_name) + (1|pair),
                          data=filter(diss_avg,window=="2 second window"))
summary(emerge_cons_model_2)

# brms
comb_cons_brms_fit_2 <- brm(avg_comb_cons ~ 1 + direction + (1|trial_name) + (1|pair),
                            data=filter(diss_avg,window=="2 second window"),
                            file = "brms-fits/lagged-combined-consonance-oneway-2sec")
plot(comb_cons_brms_fit_2)
print(summary(comb_cons_brms_fit_2), digits = 10)

emerge_cons_brms_fit_2 <- update(comb_cons_brms_fit_2, formula. = avg_emerge_cons ~ .,
                                 newdata=filter(diss_avg,window=="2 second window"),
                                 file = "brms-fits/lagged-emergent-consonance-oneway-2sec")
summary(emerge_cons_brms_fit_2)
plot(emerge_cons_brms_fit_2)

# 5 second window
comb_cons_model_5 <- lmer(avg_comb_cons ~ 1 + direction + (1|trial_name) + (1|pair),
                          data=filter(diss_avg,window=="5 second window"))
summary(comb_cons_model_5)

comb_cons_brms_fit_5 <- update(comb_cons_brms_fit_2,
                               newdata=filter(diss_avg,window=="5 second window"),
                               file = "brms-fits/lagged-combined-consonance-oneway-5sec")
print(summary(comb_cons_brms_fit_5), digits= 10)
plot(comb_cons_brms_fit_5)

emerge_cons_brms_fit_5 <- update(comb_cons_brms_fit_2,
                               newdata=filter(diss_avg,window=="5 second window"),
                               file = "brms-fits/lagged-emergent-consonance-oneway-5sec")
emerge_cons_brms_fit_5 <- readRDS("brms-fits/lagged-emergent-consonance-oneway-5sec.rds")
emerge_cons_brms_fit_5$formula

# 10 second window
comb_cons_model_10_trial <- lmer(avg_comb_cons ~ 1 + direction + (1|trial_name),
                          data=filter(diss_avg,window=="10 second window"))
summary(comb_cons_model_10_trial)

comb_cons_model_10_pair <- lmer(avg_comb_cons ~ 1 + direction + (1|pair),
                                 data=filter(diss_avg,window=="10 second window"))
summary(comb_cons_model_10_pair)

comb_cons_brms_fit_10 <- update(comb_cons_brms_fit_2,
                                newdata=filter(diss_avg,window=="10 second window"))
summary(comb_cons_brms_fit_10)
plot(comb_cons_brms_fit_10)


# lagged combined consonance ----------------------------------------------

diss_oneway <- diss %>% filter(n_time1>1,n_time1<10,n_time2>1,n_time<10,
                               condition=="one-way",direction != "simultaneuos")
diss_avg <- diss_oneway %>% group_by(trial_name,pair,direction,window) %>% 
  summarise(avg_comb_cons=-mean(combined_dissonance),
            avg_emerge_cons=-mean(diss.emerge))

comb_cons_brms_fit_2 <- brm(avg_comb_cons ~ 1 + direction + (1|trial_name) + (1|pair),
                            data=filter(diss_avg,window=="2 second window"),
                            file = "brms-fits/lagged-combined-consonance-oneway-2sec")
plot(comb_cons_brms_fit_2)
print(summary(comb_cons_brms_fit_2), digits = 10)

comb_cons_brms_fit_5 <- update(comb_cons_brms_fit_2,
                               newdata=filter(diss_avg,window=="5 second window"),
                               file = "brms-fits/lagged-combined-consonance-oneway-5sec")
plot(comb_cons_brms_fit_5)
print(summary(comb_cons_brms_fit_5), digits = 10)

comb_cons_brms_fit_10 <- update(comb_cons_brms_fit_2,
                               newdata=filter(diss_avg,window=="10 second window"),
                               file = "brms-fits/lagged-combined-consonance-oneway-10sec")
plot(comb_cons_brms_fit_10)
print(summary(comb_cons_brms_fit_10), digits = 10)

emerge_cons_brms_fit_2 <- brm(avg_emerge_cons ~ 1 + direction + (1|trial_name) + (1|pair),
                              data=filter(diss_avg,window=="2 second window"),
                              file = "brms-fits/lagged-emergent-consonance-oneway-2sec")
emerge_cons_brms_fit_2 <- update(emerge_cons_brms_fit_2, newdata=filter(diss_avg,window=="2 second window"),
                                 control=list(adapt_delta=.9),
                                 prior=prior(student_t(3, 0, 1),class=sd,coef=Intercept,group=pair))
plot(emerge_cons_brms_fit_2)
print(summary(emerge_cons_brms_fit_2), digits = 10)

emerge_cons_brms_fit_5 <- update(emerge_cons_brms_fit_2,
                                 newdata=filter(diss_avg,window=="5 second window"),
                                 file = "brms-fits/lagged-emergent-consonance-oneway-5sec")
plot(emerge_cons_brms_fit_5)
print(summary(emerge_cons_brms_fit_5), digits = 10)

emerge_cons_brms_fit_10 <- update(emerge_cons_brms_fit_2,
                                  newdata=filter(diss_avg,window=="10 second window"),
                                  file = "brms-fits/lagged-emergent-consonance-oneway-10sec")
plot(emerge_cons_brms_fit_10)
print(summary(emerge_cons_brms_fit_10), digits = 10)

# coupled higher EC than oneway at simultaneous time points? ------------------------------------------

head(diss)
diss_avg <- diss %>% filter(direction=="simultaneuos") %>% 
            group_by(trial_name,yoked_pair_id,yoked_piece_id,condition,window) %>%
            summarise(comb_cons=-mean(combined_dissonance),
                      emerge_cons=-mean(diss.emerge))
head(diss_avg)

# combined consonance for all windows
for (w in unique(diss_avg$window)) {
  print(w)
  outpath = paste("brms-fits/simultaneous-combined-consonance-",
                  str_extract(w,'[0-9]*'),"sec",sep="")
  fit <- brm(comb_cons ~ 1+condition+(1|yoked_pair_id)+(1|yoked_piece_id),
             data=filter(diss_avg,window==w),
             save_model = "combined_cons_fit", file = outpath)
  fit <- update(fit,newdata=filter(diss_avg,window==w))
  saveRDS(fit,paste(outpath,".rds",sep=''))
}

comb_fit_2 <- readRDS('brms-fits/simultaneous-combined-consonance-10sec.rds')
plot(comb_fit_2)
print(summary(comb_fit_2), digits = 5)
print(hypothesis(comb_fit_2,"conditiononeMway<0",alpha = .05),digits=5)

# emergent consonance for all windows
for (w in unique(diss_avg$window)) {
  print(w)
  outpath = paste("brms-fits/simultaneous-emergent-consonance-",
                  str_extract(w,'[0-9]*'),"sec",sep="")
  fit <- brm(emerge_cons ~ 1+condition+(1|yoked_pair_id)+(1|yoked_piece_id),
             data=filter(diss_avg,window==w),
             save_model = "emerge_cons_fit", file = outpath)
  fit <- update(fit, newdata=filter(diss_avg,window==w))
  saveRDS(fit,paste(outpath,".rds",sep=''))
}

emerge_fit <- readRDS('brms-fits/simultaneous-emergent-consonance-5sec.rds')
plot(emerge_fit)
print(summary(emerge_fit), digits=5)
hypothesis(emerge_fit,"conditiononeMway<0",alpha = .05)

# stuff I already tried ....

cons_model_2 <- lmer(emerge_cons ~ 1+condition+(1|yoked_pair_id)+(1|yoked_piece_id),
                     data=filter(diss_avg,window=="2 second window"))
summary(cons_model_2)
"
If we take out (1|pair), suddenly there is an effect of condition.
"



cons_brms_fit_2 <- brm(emerge_cons ~ 1+condition+(1|yoked_pair_id)+(1|yoked_piece_id),
                       data=filter(diss_avg,window=="2 second window"),
                       save_model = "emerge_cons_fit")
cons_brms_fit_2 <- update(cons_brms_fit_2,iter=4000,control=list(adapt_delta=.9))
summary(cons_brms_fit_2)
plot(cons_brms_fit_2)
(hyp1 <- hypothesis(cons_brms_fit_2,"conditiononeMway<0"))
plot(hyp1)

cons_model_5 <- lmer(emerge_cons ~ 1+condition+(1|pair)+(1|yoked_piece_id),
                     data=filter(diss_avg,window=="5 second window"))
cons_model_5 <- lmer(emerge_cons ~ 1+condition+(1|yoked_piece_id),
                     data=filter(diss_avg,window=="5 second window"))
summary(cons_model_5)

cons_brms_fit_5 <- update(cons_brms_fit_2,
                          newdata=filter(diss_avg,window=="5 second window"),
                          control=list(adapt_delta=.9))
summary(cons_brms_fit_5)
plot(cons_brms_fit_5)
summary(cons_brms_fit_5,prob=0.9)
(hyp1 <- hypothesis(cons_brms_fit_5,"conditiononeMway<0"))
plot(hyp1)


# EC greater than 0 ? -----------------------------------------------------

head(diss)
diss_simul <- diss %>% filter(n_time1==n_time2)
ggplot(diss_simul, aes(x=-diss.emerge,fill=condition)) + geom_density(alpha=.3) + facet_wrap(~window)
diss_simul$is_pos_emerge <- diss_simul$diss.emerge < 0
diss_simul_abbrv <- diss_simul %>% group_by(trial_name,window,condition,yoked_piece_id,yoked_pair_id) %>%
  summarise(n=n(),n_pos=sum(is_pos_emerge))
head(diss_simul_abbrv)
diss_simul_abbrv$rate <- diss_simul_abbrv$n_pos/diss_simul_abbrv$n

ggplot(diss_simul_abbrv, aes(x=window,y=rate,color=condition)) + stat_summary()
library(lmerTest)
rate_model2 <- lmer(rate ~ 1 + condition + (1|yoked_piece_id) + (1|yoked_pair_id),
                   data=filter(diss_simul_abbrv,window=="2 second window"))
summary(rate_model2)

rate_model5 <- lmer(rate ~ 1 + condition + (1|yoked_pair_id),
                    data=filter(diss_simul_abbrv,window=="5 second window"))
summary(rate_model5)

rate_model10 <- lmer(rate ~ 1 + condition + (1|yoked_pair_id),
                    data=filter(diss_simul_abbrv,window=="10 second window"))
summary(rate_model10)

"
Okay, so it looks like emergent consonance isn't significantly more positive in coupled trials.
"

# exploratory model including all variables -------------------------------

head(diss)
unique(diss$lag)
diss$abs_lag <- abs(diss$lag)
diss_avg <- diss %>% group_by(trial_name,abs_lag,direction,yoked_piece_id,yoked_pair_id,condition,window) %>%
            summarise(cons_emerge = -mean(diss.emerge))

emerge_cons_brms <- brm(cons_emerge ~ 1 + condition + abs_lag + direction + direction*condition + 
                          (1|yoked_piece_id)+(1|yoked_pair_id),
                        data=filter(diss_avg,window=="2 second window"))
