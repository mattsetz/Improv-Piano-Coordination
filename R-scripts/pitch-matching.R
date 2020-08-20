
# new shit ----------------------------------------------------------------

library(tidyverse)

inDir = "Pipeline/tonal/tonal-entropy/combined/"
files = list.files(inDir)
pitch <- NULL
for (f in files) {
  print(f)
  df <- read.csv(paste(inDir,f,sep=''))# %>% filter(time%%2==0)
  df$trial_name <- str_remove(f,".csv")
  pitch <- rbind(pitch,df)
}

head(pitch)
pitch_long <- pitch %>% pivot_longer(cols = starts_with("entropy"),
                                     names_to = "window",values_to = "entropy")
head(pitch_long)

summary <- read.csv('summary-combined.csv')
yoked_piece_ids <- read.csv('yoked-piece-ids.csv')

pitch <- merge(merge(pitch_long,select(summary,trial_name,yoked_pair_id)),yoked_piece_ids)
head(pitch)

diss <- read.csv('master-diss-matrix.csv')
diss <- diss %>% rename(trial_name=pair)
diss <- diss %>% filter(time1==time2) %>% select(trial_name,time1,
                                                 combined_dissonance,diss.emerge,
                                                 condition,window) %>% rename(time=time1)
diss <- diss %>% distinct()

pitch$window <- pitch$window %>% recode(entropy2="2 second window",
                                        entropy5="5 second window",
                                        entropy10="10 second window",
                                        entropy20="20 second window")
pitch <- pitch %>% filter(window!="20 second window")

pitch_diss <- merge(pitch,diss)


# mediation analysis ------------------------------------------------------

# is greater EC in coupled trials from matching or complementary tonal coordination?

pitch_diss_avg <- pitch_diss %>% group_by(trial_name,condition,window,yoked_pair_id,yoked_piece_id) %>%
                  summarise(comb_cons=-mean(combined_dissonance,na.rm=TRUE),
                            emerge_cons=-mean(diss.emerge,na.rm=TRUE),
                            entropy=mean(entropy,na.rm=TRUE))

restricted_model2 <- lmer(emerge_cons ~ entropy+(1|yoked_piece_id)+(1|pair_yoked_id),
                          data=filter(pitch_diss_avg,window=="2 second window"))
full_model2 <- lmer(emerge_cons ~ entropy+condition+(1|yoked_piece_id)+(1|pair_yoked_id),
                    data=filter(pitch_diss_avg,window=="2 second window"))
summary(restricted_model2)
summary(full_model2)
anova(restricted_model2, full_model2)

restricted_model5 <- lmer(emerge_cons ~ entropy+(1|pair_yoked_id)+(1|yoked_piece_id),
                          data=filter(pitch_diss_avg,window=="5 second window"))
full_model5 <- lmer(emerge_cons ~ entropy+condition+(1|pair_yoked_id)+(1|yoked_piece_id),
                    data=filter(pitch_diss_avg,window=="5 second window"))
summary(restricted_model5)
summary(full_model5)
anova(restricted_model5, full_model5)

restricted_model10 <- lmer(emerge_cons ~ entropy+(1|pair_yoked_id)+(1|yoked_piece_id),
                          data=filter(pitch_diss_avg,window=="10 second window"))
full_model10 <- lmer(emerge_cons ~ entropy+condition+(1|pair_yoked_id)+(1|yoked_piece_id),
                     data=filter(pitch_diss_avg,window=="10 second window"))
summary(restricted_model10)
summary(full_model10)
anova(restricted_model10, full_model10)

# mediation analysis with bayesian
library(brms)
restricted_fit2 <- brm(emerge_cons ~ entropy+(1|pair_yoked_id)+(1|yoked_piece_id),
                       data=filter(pitch_diss_avg,window=="2 second window"),
                       file="brms-fits/emerge-cons-mediation-restricted-2sec")
restricted_fit2 <- update(restricted_fit2,
                          newdata=filter(pitch_diss_avg,window=="2 second window"))
full_fit2 <- update(restricted_fit2, formula= . ~ . + condition,
                    newdata=filter(pitch_diss_avg,window=="2 second window"))
plot(full_fit2)
print(summary(restricted_fit2),digits=5)
print(summary(full_fit2),digits=5)


restricted_fit2 <- add_criterion(restricted_fit2, "waic")
full_fit2 <- add_criterion(full_fit2, "waic")
loo_compare(restricted_fit2,full_fit2,criterion = "waic")

restricted_fit5 <- update(restricted_fit2,iter=4000,
                          newdata=filter(pitch_diss_avg,window=="5 second window"))
full_fit5 <- update(full_fit2,
                    newdata=filter(pitch_diss_avg,window=="5 second window"),
                    iter=4000)
restricted_fit5 <- add_criterion(restricted_fit5, "waic")
full_fit5 <- add_criterion(full_fit5, "waic")
loo_compare(restricted_fit5,full_fit5,criterion = "waic")

print(summary(restricted_fit5),digits=5)
print(summary(full_fit5),digits=5)

restricted_fit10 <- update(restricted_fit2,
                          newdata=filter(pitch_diss_avg,window=="10 second window"),
                          iter=4000,file="brms-fits/emerge-cons-mediation-restricted-10sec")
full_fit10 <- update(full_fit2,
                    newdata=filter(pitch_diss_avg,window=="10 second window"),
                    iter=4000,file="brms-fits/emerge-cons-mediation-full-10sec")
restricted_fit10 <- add_criterion(restricted_fit10, "waic")
full_fit10 <- add_criterion(full_fit10, "waic")
loo_compare(restricted_fit10, full_fit10, criterion = "waic")
print(summary(restricted_fit10),digits=5)
print(summary(full_fit10),digits=5)

# entropy analysis --------------------------------------------------------

library(lme4)
library(lmerTest)

entropy_model2 <- lmer(entropy ~ 1+condition+(1|pair_yoked_id)+(1|yoked_piece_id),
                       data=filter(pitch,window=="entropy2"))

pitch_avg <- pitch %>% group_by(trial_name,condition,window,yoked_pair_id,yoked_piece_id) %>% 
  summarise(entropy=mean(entropy,na.rm=TRUE),
            sd_entropy=sd(entropy,na.rm=TRUE))
ggplot(pitch_avg, aes(x=window,y=entropy,color=condition)) + stat_summary()

library(brms)
entropy_fit2 <- brm(entropy ~ 1+condition+(1|yoked_pair_id)+(1|yoked_piece_id),
                    data=filter(pitch_avg,window=="2 second window"),file="brms-fits/combined-entropy-2sec")
entropy_fit2 <- update(entropy_fit2,iter=6000)
saveRDS(entropy_fit2, "brms-fits/combined-entropy-2sec.rds")
entropy_fit2 <- readRDS("brms-fits/combined-entropy-2sec.rds")
print(summary(entropy_fit2),digits=4)
plot(entropy_fit2)
hypothesis(entropy_fit2,"conditiononeMway>0",alpha=.05)

entropy_fit5 <- update(entropy_fit2,newdata=filter(pitch_avg,window=="5 second window"),
                       iter=6000, file="brms-fits/combined-entropy-5sec")
hypothesis(entropy_fit5,"conditiononeMway>0")
entropy_fit5 <- readRDS("brms-fits/combined-entropy-5sec.rds")
plot(entropy_fit5)
print(summary(entropy_fit5), digits = 4)

entropy_fit10 <- update(entropy_fit2,newdata=filter(pitch_avg,window=="10 second window"),
                        iter=6000, file="brms-fits/combined-entropy-10sec")
hypothesis(entropy_fit10,"conditiononeMway>0")
print(summary(entropy_fit10), digits = 4)

# old shit ----------------------------------------------------------------

library(tidyverse)

inDir = "Pipeline/tonal/pc-window/individual/"
files = list.files(inDir)
pitch <- NULL
for (f in files) {
  print(f)
  df <- read.csv(paste(inDir,f,sep='')) %>% filter(time%%2==0)
  df$pair <- str_split(f,'[.]')[[1]][[2]]
  df$player <- str_split(f,'[.]')[[1]][[3]]
  pitch <- rbind(pitch,df)
}

head(pitch)
pitch_long <- pitch %>% pivot_longer(cols = starts_with("X"),
                                     names_to = "pitch_class",
                                     values_to = "duration")
pitch_long <- pitch_long %>% group_by(time,window,pair,player) %>% 
  mutate(total_duration = sum(duration))
pitch_long$duration <- pitch_long$duration/pitch_long$total_duration

pitch_long$player_tmp <- if_else(pitch_long$player==str_extract(pitch_long$pair,"^[a-z]{1,2}[1-9]"),"A","B")


cols <- c("total_duration","player")
pitch_wide <- pitch_long %>% select(-one_of(cols)) %>%
  pivot_wider(id_cols = c("time","window","pair","pitch_class"),
              names_from = player_tmp, values_from = duration, )
View(pitch_wide)
pitch_wide$pitch_diff <- abs(pitch_wide$A-pitch_wide$B)
summary <- read.csv("summary-individual.csv")
pitch_wide <- merge(pitch_wide,select(summary,condition,pair,yolked_id))

# all data points
ggplot(pitch_wide, aes(x=window,y=pitch_diff,color=condition)) + stat_summary()

library(brms)