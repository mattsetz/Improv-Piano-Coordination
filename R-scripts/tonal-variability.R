library(tidyverse)


diss <- read.csv('Pipeline/tonal/master-emergent-dissonance-zeroed.csv')
diss <- diss %>% rename(trial_name = pair)
diss$pair <- str_remove_all(diss$trial_name,'[0-9]')
summary <- read.csv("summary-combined.csv")
head(summary)
diss <- merge(diss,summary)

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


# Variability across pieces -- duo-level differences ----------------------

diss_avg <- diss %>% drop_na() %>% group_by(trial_name, window, condition, pair, yoked_pair_id) %>% summarise(diss_comb = mean(diss.comb,na.rm=TRUE),
                                                                                       diss_emerge = mean(diss.emerge,na.rm=TRUE))
head(diss_avg)
diss_pair_var <- diss_avg %>% group_by(pair,window,condition,yoked_pair_id) %>%
  summarise(diss_comb_var = var(diss_comb),
            diss_emerge_var = var(diss_emerge))
head(diss_pair_var)
ggplot(diss_pair_var, aes(x=diss_comb_var,color=condition,fill=condition)) + geom_density(alpha=.2) +
  facet_wrap(~window)

diss_pair_var_avg <- diss_pair_var %>% group_by(window,condition,yoked_pair_id) %>%
  summarise(diss_comb_var=mean(diss_comb_var),
            diss_emerge_var=mean(diss_emerge_var))
ggplot(diss_pair_var_avg, aes(x=diss_comb_var,color=condition)) + geom_density() + facet_wrap(~window)

library(lme4)
library(lmerTest)
W=5
var_model <- lmer(diss_comb_var ~ condition+(1|yoked_pair_id),
                  data=filter(diss_pair_var_avg,window==W))
summary(var_model)
"
Condition fixed-effect is positive but non-sig for all windows. Hmm...
"

pair_var_wide <- diss_pair_var_avg %>% pivot_wider(names_from=condition,
                                                   values_from=c("diss_comb_var","diss_emerge_var"))
head(pair_var_wide)
pair_var_wide$diss_comb_diff <- pair_var_wide$diss_comb_var_coupled-pair_var_wide$`diss_comb_var_one-way`
ggplot(pair_var_wide,aes(x=diss_comb_diff)) + geom_histogram() + facet_wrap(~window) + theme_bw()

diff <- pair_var_wide %>% select(window,diss_comb_diff)
ggplot(diff,aes(x=window,y=diss_comb_diff)) + stat_summary() + theme_bw()
hist(filter(diff,window==20)$diss_comb_diff,breaks=10)
ggplot(diss,aes(x=diss_comb_diff)) + geom_density() #+ facet_wrap(~window) + theme_bw()
"
These plots make it seem like there is indeed more variability in coupled versus one-way trials.
Cool! Encouraging. Although, in actuality there are two outlier coupled duos accounting for much of this.

Between this and the initial analysis, it seems promising that may indeed be more
tonal variability (in terms of Combined Consonance) across pieces in coupled trials.
Keep in mind that CC seems to saturate at low values (maximal high dissonance), so this
variability is mostly due to more cases of extreme high CC in coupled trials.
"
