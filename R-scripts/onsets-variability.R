library(tidyverse)

onsets <- read.csv('Pipeline/rhythm/onset-density/collapsed/master-onset-density-combined.csv')
head(onsets)
onsets <- onsets %>% rename(trial_name = pair)
onsets$pair <- str_remove_all(onsets$trial_name,'[0-9]')
summary <- read.csv("summary-combined.csv")
head(summary)
onsets <- merge(onsets,summary)
head(onsets)
onsets <- onsets %>% filter(num_onsets>0) %>% group_by(trial_name) %>% mutate(start=min(time)) %>% ungroup()
onsets$time <- onsets$time - onsets$start
onsets <- onsets %>% filter(time>=0)

# Variability across pieces -- duo-level differences ----------------------

onsets_avg <- onsets %>% drop_na() %>% group_by(trial_name, condition, pair, yoked_pair_id) %>% 
  summarise(avg_onset_density = mean(num_onsets,na.rm=TRUE), total_onsets = sum(num_onsets)) %>% ungroup()
head(onsets_avg)
onsets_pair_var <- onsets_avg %>% group_by(pair,condition,yoked_pair_id) %>%
  summarise(avg_onset_density_var = var(avg_onset_density),
            total_onsets_var = var(total_onsets))
onsets_pair_var <- onsets_pair_var %>% ungroup() %>% group_by(condition,yoked_pair_id) %>%
          summarise(avg_onset_density_var=mean(avg_onset_density_var),
                    total_onsets_var=mean(total_onsets_var))
head(onsets_pair_var)
ggplot(onsets_pair_var, aes(x=avg_onset_density_var,color=condition,fill=condition)) + geom_density(alpha=.2)

library(lme4)
library(lmerTest)
var_model <- lmer(avg_onset_density_var ~ condition+(1|yoked_pair_id),
                  data=onsets_pair_var)
summary(var_model)
"
Doesn't seem to be an effect of condition here
"

pair_var_wide <- onsets_pair_var %>% ungroup() %>% pivot_wider(names_from=condition,
                                                 values_from=c("avg_onset_density_var","total_onsets_var"))
head(pair_var_wide)
pair_var_wide$onset_density_diff <- pair_var_wide$avg_onset_density_var_coupled-pair_var_wide$`avg_onset_density_var_one-way`
ggplot(pair_var_wide,aes(x=onset_density_diff)) + geom_histogram() + theme_bw()

"
Okay so it looks like there is not more variability in onset density for coupled vs one-way.
Maybe this is different in the second half of pieces? Not quite sure.
"



# variability across pieces -----------------------------------------------

diss_avg <- diss %>% drop_na %>% group_by(trial_name, window, condition) %>% summarise(diss_comb = mean(diss.comb,na.rm=TRUE),
                                                                                       diss_emerge = mean(diss.emerge,na.rm=TRUE))
head(diss_avg)

ggplot(diss_avg, aes(x=diss_comb,color=condition,fill=condition)) + geom_density(alpha=.2) +
  facet_wrap(~window) + theme_bw()
"
oneway appears to be a bit more peaked (i.e. less variable) around central values of dissonance,
which differ depending on the window. how to formalize this?
"
W=20
var.test(filter(diss_avg,window==W,condition=="coupled")$diss_comb,
         filter(diss_avg,window==W,condition=="one-way")$diss_comb,alternative = "greater")
"
Of course F val is greater than 1, indicating more variability in coupled duos. However,
this is not significant (although it is very close to sig) except for at 20 second windows.
To get more power, you could factor in individual differences at the group-level.
"

ggplot(diss_avg, aes(x=diss_emerge,color=condition,fill=condition)) + geom_density(alpha=.2) +
  facet_wrap(~window) + theme_bw()
"
Doesn't really appear to be the case here...
"

# Redo the analysis on 2nd half of pieces (because they seem to be more special)
diss_avg_2 <- diss %>% drop_na() %>% filter(n_time10>5) %>% group_by(trial_name, window, condition) %>% summarise(diss_comb = mean(diss.comb,na.rm=TRUE),
                                                                                                                  diss_emerge = mean(diss.emerge,na.rm=TRUE))
ggplot(diss_avg_2, aes(x=diss_comb,color=condition,fill=condition)) + geom_density(alpha=.2) +
  facet_wrap(~window) + theme_bw()
"
Looks like more variability in coupled trials. In particular, there are more cases of extreme high CC
on coupled trials versus one-way.
"
W=20
var.test(filter(diss_avg_2,window==W,condition=="coupled")$diss_comb,
         filter(diss_avg_2,window==W,condition=="one-way")$diss_comb,alternative = "greater")
"
Okay, even less sig than before actually.
"

