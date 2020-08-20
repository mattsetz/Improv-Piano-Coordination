library(tidyverse)


# Analyze collapsed onsets-consonance continuous gc --------------------------------------------
indir <- "Pipeline/granger-causality-onsets-consonance/granger-causality-collapsed-onsets-consonance-detrended/hop-6-10sec/fvals/"
files <- list.files(indir)
gc <- NULL
for (f in files) {
  print(f)
  p <- str_extract(str_extract(f,"[a-z]{1,2}[0-9][.]csv"),"[a-z]{1,2}[0-9]")
  order <- as.numeric(str_extract(str_extract(f,"[0-9]{1,2}steps"),"[0-9]{1,2}"))*0.6
  dat <- read.csv(paste(indir,f,sep=""),header = FALSE)
  gc <- rbind(gc, data.frame(player=p,order= order,
                             otherO_playerO= dat[1,3],
                             otherC_playerC= dat[2,4],
                             playerO_otherO= dat[3,1],
                             playerC_otherC= dat[4,2]))
}

head(gc)
summary <- read.csv("summary-individual.csv")
gc <- merge(gc,select(summary,player,condition))

# Analyze continuous GC values
gc_long <- gc %>% pivot_longer(cols = c("otherO_playerO","playerO_otherO","otherC_playerC","playerC_otherC"),
                               names_to = "direction", values_to = "GC")
ggplot(gc_long, aes(x=order,y=GC,color=direction)) + stat_summary() + xlab("order (sec)") + ylab("Granger Causality (collapsed onsets)") + facet_wrap(~condition) + theme_bw()

"
From this plot it appears that:
(1) ghost --> live has more GC than live --> ghost in terms of Collapsed Onsets, but NOT Consonance
(2) GC of consonance is higher in coupled trials compared to one-way trials
(3) GC of onset density is higher in coupled trials than both ghost-->live and live-->ghost

Of course we'll need some statistical tests to verify this
"

# test for asymmetry in onset density of ghost trials (1)
ggplot(filter(gc_long,condition=="one-way",direction=="otherO_playerO"|direction=="playerO_otherO"), aes(x=GC,color=direction,fill=direction)) + geom_density(alpha=.2) + theme_bw() + theme(legend.position = "bottom") + facet_wrap(~order) + xlab("granger causality of collapsed onsets in one-way trials")
t.test(filter(gc,condition=="one-way",round(order,1)==5.4)$otherO_playerO,
       filter(gc,condition=="one-way",round(order,1)==5.4)$playerO_otherO, paired = TRUE)

"
Yeah, this is sig... 
Although the distributions aren't normal, so difference in means is mostly from outliers

	Paired t-test

data:  filter(gc, condition == one-way, round(order, 1) == 5.4)$otherO_playerO and filter(gc, condition == one-way, round(order, 1) == 5.4)$playerO_otherO
t = 3.9327, df = 85, p-value = 0.0001709
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 0.004872506 0.014837406
sample estimates:
mean of the differences 
            0.009854956 
"




# Analyze sig ratio collapsed onsets-consonance ---------------------------
indir <- "Pipeline/granger-causality-onsets-consonance/granger-causality-collapsed-onsets-consonance-detrended/hop-6-10sec/sig/"
files <- list.files(indir)
gc <- NULL
for (f in files) {
  print(f)
  p <- str_extract(str_extract(f,"[a-z]{1,2}[0-9][.]csv"),"[a-z]{1,2}[0-9]")
  order <- as.numeric(str_extract(str_extract(f,"[0-9]{1,2}steps"),"[0-9]{1,2}"))*0.6
  dat <- read.csv(paste(indir,f,sep=""),header = FALSE)
  gc <- rbind(gc, data.frame(player=p,order= order,
                             otherO_playerO= dat[1,3],
                             otherC_playerC= dat[2,4],
                             playerO_otherO= dat[3,1],
                             playerC_otherC= dat[4,2]))
}

head(gc)
summary <- read.csv("summary-individual.csv")
gc <- merge(gc,select(summary,player,condition))

gc_sig_ratio <- gc %>% group_by(condition,order) %>% summarise(num_trials=n(),
                                                               otherO_playerO_sig=sum(otherO_playerO),
                                                               playerO_otherO_sig=sum(playerO_otherO),
                                                               otherC_playerC_sig=sum(otherC_playerC),
                                                               playerC_otherC_sig=sum(playerC_otherC))
gc_sig_ratio$otherO_playerO_sig <- gc_sig_ratio$otherO_playerO_sig/gc_sig_ratio$num_trials
gc_sig_ratio$otherC_playerC_sig <- gc_sig_ratio$otherC_playerC_sig/gc_sig_ratio$num_trials
gc_sig_ratio$playerO_otherO_sig <- gc_sig_ratio$playerO_otherO_sig/gc_sig_ratio$num_trials
gc_sig_ratio$playerC_otherC_sig <- gc_sig_ratio$playerC_otherC_sig/gc_sig_ratio$num_trials
head(gc_sig_ratio)

gc_long <- gc_sig_ratio %>% select(-num_trials) %>% pivot_longer(cols = c("otherO_playerO_sig",
                                                                          "playerO_otherO_sig",
                                                                          "otherC_playerC_sig",
                                                                          "playerC_otherC_sig"),
                                                                names_to = "direction",
                                                                values_to = "sig_ratio")
ggplot(gc_long, aes(x=order,y=sig_ratio,color=direction,fill=direction)) + geom_col(position = "dodge") + facet_wrap(~condition) + theme_bw() + xlab("order (sec)")

"
According to these plots of sig ratio, it looks like:
(1) ghost-->live has higher sig rate for Onset Density
(2) ghost-->live might also have higher sig rate for Consonance, though this is less clear
(3) it isn't as clear that ghost-->live onset GC is higher than live-->live onset GC
"

# Analyze continuous gc collapsed onsets on optimal model orders ---------------------
indir <- "Pipeline/granger-causality-onsets-consonance/granger-causality-collapsed-onsets-consonance-detrended/optimal-model-orders/hop-6-10sec/fvals/"
files <- list.files(indir)
gc <- NULL
for (f in files) {
  print(f)
  p <- str_extract(str_extract(f,"[a-z]{1,2}[0-9][.]csv"),"[a-z]{1,2}[0-9]")
  order <- as.numeric(str_extract(str_extract(f,"[0-9]{1,2}steps"),"[0-9]{1,2}"))*0.6
  dat <- read.csv(paste(indir,f,sep=""),header = FALSE)
  gc <- rbind(gc, data.frame(player=p,order= order,
                             otherO_playerO= dat[1,3],
                             otherC_playerC= dat[2,4],
                             playerO_otherO= dat[3,1],
                             playerC_otherC= dat[4,2]))
}

head(gc)
summary <- read.csv("summary-individual.csv")
gc <- merge(gc,select(summary,player,condition))

head(gc)
ggplot(gc, aes(x=order))+geom_histogram()+theme_bw()+xlab("optimal order (sec)")
"
The optimal orders (as determined by AIC in MVGC toolbox) are generally small compared to the orders
we chose based on musical intuition. Given that time series values come from 2 second windows, it may
be misleading to look at these small orders because there is a lot of overlap in predictors and predicted
values.
"
gc_long <- gc %>% pivot_longer(cols = c("otherO_playerO","playerO_otherO","otherC_playerC","playerC_otherC"),
                               names_to = "direction", values_to = "GC")
ggplot(gc_long, aes(x=direction,y=GC))+stat_summary()+facet_wrap(~condition)+theme_bw()
"
The main result here still looks like: ghost-->live has higher onset density GC than live-->ghost
"

# Analyze sig ratio collapsed onsets-consonance optimal model orders ---------------------------
indir <- "Pipeline/granger-causality-onsets-consonance/granger-causality-collapsed-onsets-consonance-detrended/optimal-model-orders/hop-6-10sec/sig/"
files <- list.files(indir)
gc <- NULL
for (f in files) {
  print(f)
  p <- str_extract(str_extract(f,"[a-z]{1,2}[0-9][.]csv"),"[a-z]{1,2}[0-9]")
  order <- as.numeric(str_extract(str_extract(f,"[0-9]{1,2}steps"),"[0-9]{1,2}"))*0.6
  dat <- read.csv(paste(indir,f,sep=""),header = FALSE)
  gc <- rbind(gc, data.frame(player=p,order= order,
                             otherO_playerO= dat[1,3],
                             otherC_playerC= dat[2,4],
                             playerO_otherO= dat[3,1],
                             playerC_otherC= dat[4,2]))
}

head(gc)
summary <- read.csv("summary-individual.csv")
gc <- merge(gc,select(summary,player,condition))

gc_sig_ratio <- gc %>% group_by(condition,order) %>% summarise(num_trials=n(),
                                                               otherO_playerO_sig=sum(otherO_playerO),
                                                               playerO_otherO_sig=sum(playerO_otherO),
                                                               otherC_playerC_sig=sum(otherC_playerC),
                                                               playerC_otherC_sig=sum(playerC_otherC))
gc_sig_ratio$otherO_playerO_sig <- gc_sig_ratio$otherO_playerO_sig/gc_sig_ratio$num_trials
gc_sig_ratio$otherC_playerC_sig <- gc_sig_ratio$otherC_playerC_sig/gc_sig_ratio$num_trials
gc_sig_ratio$playerO_otherO_sig <- gc_sig_ratio$playerO_otherO_sig/gc_sig_ratio$num_trials
gc_sig_ratio$playerC_otherC_sig <- gc_sig_ratio$playerC_otherC_sig/gc_sig_ratio$num_trials
head(gc_sig_ratio)

gc_long <- gc_sig_ratio %>% select(-num_trials) %>% pivot_longer(cols = c("otherO_playerO_sig",
                                                                          "playerO_otherO_sig",
                                                                          "otherC_playerC_sig",
                                                                          "playerC_otherC_sig"),
                                                                 names_to = "direction",
                                                                 values_to = "sig_ratio")
ggplot(gc_long, aes(x=condition,y=sig_ratio,color=direction,fill=direction)) + geom_col(position = "dodge") + theme_bw() + xlab("order (sec)")

"
This plot seems really weird, I don't believe it.
"

"
All in all, the strongest result by far seems to be that
(1) ghost-->live has more GC of onset density than live-->ghost
(2) some evidence that live-->live GC is higher than ghost-->live, though less compelling

I think its best to put all of this in the SI. I can refer to GC in the main text. But
there is enough massaging of data that I'm not sure this is worthy to be in the main text, or needs to be for that matter.
"





# Analyze raw onsets-consonance continuous gc --------------------------------------------
indir <- "Pipeline/granger-causality-onsets-consonance/granger-causality-raw-onsets-consonance-detrended/hop-6-10sec/fvals/"
files <- list.files(indir)
gc <- NULL
for (f in files) {
  print(f)
  p <- str_extract(str_extract(f,"[a-z]{1,2}[0-9][.]csv"),"[a-z]{1,2}[0-9]")
  order <- as.numeric(str_extract(str_extract(f,"[0-9]{1,2}steps"),"[0-9]{1,2}"))*0.6
  dat <- read.csv(paste(indir,f,sep=""),header = FALSE)
  gc <- rbind(gc, data.frame(player=p,order= order,
                             otherO_playerO= dat[1,3],
                             otherC_playerC= dat[2,4],
                             playerO_otherO= dat[3,1],
                             playerC_otherC= dat[4,2]))
}

head(gc)
summary <- read.csv("summary-individual.csv")
gc <- merge(gc,select(summary,player,condition))

write.csv(gc, "Pipeline/granger-causality-onsets-consonance/gc-raw-onsets-consonance-detrended.csv",row.names = FALSE)
gc <- read.csv("Pipeline/granger-causality-onsets-consonance/gc-raw-onsets-consonance-detrended.csv")
# Analyze continuous GC values
gc_onsets <- gc %>% select(-otherC_playerC,-playerC_otherC) %>% 
  rename(ghost_live=otherO_playerO,live_ghost=playerO_otherO) %>% pivot_longer(cols = c("ghost_live","live_ghost"),
                                                                               names_to = "direction", values_to = "GC")
gc_onsets[gc_onsets$condition=="coupled",]$direction = "live_live"
ggplot(gc_onsets, aes(x=order,y=GC,color=direction)) + stat_summary() + xlab("model order (sec)") + ylab("Granger Causality of Onset Density") + theme_bw() + theme(legend.position = c(2.,8))

gc_consonance <- gc %>% select(-otherO_playerO,-playerO_otherO) %>% 
  rename(ghost_live=otherC_playerC,live_ghost=playerC_otherC) %>% pivot_longer(cols = c("ghost_live","live_ghost"),
                                                                               names_to = "direction", values_to = "GC")
gc_consonance[gc_consonance$condition=="coupled",]$direction = "live_live"
ggplot(gc_consonance, aes(x=order,y=GC,color=direction)) + stat_summary() + xlab("model order (sec)") + ylab("Granger Causality of Tonal Consonance") + theme_bw() + theme(legend.position = c(.2,.8))

"
From this plot it appears that:
(1) ghost --> live has more GC than live --> ghost in terms of Raw Onsets, but NOT Consonance
(2) ghost-->live has equivalent GC compared to live-->live
"

# test for asymmetry in onset density of ghost trials (1)
ggplot(filter(gc,condition=="one-way"), aes(x=otherO_playerO-playerO_otherO)) + geom_density() + facet_wrap(~order)+theme_bw() + theme(legend.position = "bottom") + facet_wrap(~order) + xlab("distribution of onset density gc asymmetries \nwithin overdubbed trials")
t.test(filter(gc,condition=="one-way",round(order,1)==5.4)$otherC_playerC,
       filter(gc,condition=="one-way",round(order,1)==5.4)$playerC_otherC, paired = TRUE)

"
This is sig. But again, these are long-tailed distributions centered at 0.

	Paired t-test

data:  filter(gc, condition == one-way, round(order, 1) == 5.4)$otherO_playerO and filter(gc, condition == one-way, round(order, 1) == 5.4)$playerO_otherO
t = 3.7235, df = 84, p-value = 0.0003547
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 0.004842099 0.015942775
sample estimates:
mean of the differences 
             0.01039244 
"

# ghost-live ~= live->live
t.test(c(filter(gc,condition=="coupled",round(order,1)==5.4)$playerO_otherO,
         filter(gc,condition=="coupled",round(order,1)==5.4)$otherO_playerO),
       filter(gc,condition=="one-way",round(order,1)==5.4)$otherO_playerO,var.equal = TRUE)



# Analyze sig ratio raw onsets-consonance ---------------------------
indir <- "Pipeline/granger-causality-onsets-consonance/granger-causality-raw-onsets-consonance-detrended/hop-6-10sec/sig/"
files <- list.files(indir)
gc_sig <- NULL
for (f in files) {
  print(f)
  p <- str_extract(str_extract(f,"[a-z]{1,2}[0-9][.]csv"),"[a-z]{1,2}[0-9]")
  order <- as.numeric(str_extract(str_extract(f,"[0-9]{1,2}steps"),"[0-9]{1,2}"))*0.6
  dat <- read.csv(paste(indir,f,sep=""),header = FALSE)
  gc_sig <- rbind(gc_sig, data.frame(player=p,order= order,
                             otherO_playerO= dat[1,3],
                             otherC_playerC= dat[2,4],
                             playerO_otherO= dat[3,1],
                             playerC_otherC= dat[4,2]))
}

summary <- read.csv("summary-individual.csv")
gc_sig <- merge(gc_sig,select(summary,player,condition))

gc_sig_ratio <- gc_sig %>% group_by(condition,order) %>% summarise(num_trials=n(),
                                                               otherO_playerO_sig=sum(otherO_playerO),
                                                               playerO_otherO_sig=sum(playerO_otherO),
                                                               otherC_playerC_sig=sum(otherC_playerC),
                                                               playerC_otherC_sig=sum(playerC_otherC))
gc_sig_ratio$otherO_playerO_sig <- gc_sig_ratio$otherO_playerO_sig/gc_sig_ratio$num_trials
gc_sig_ratio$otherC_playerC_sig <- gc_sig_ratio$otherC_playerC_sig/gc_sig_ratio$num_trials
gc_sig_ratio$playerO_otherO_sig <- gc_sig_ratio$playerO_otherO_sig/gc_sig_ratio$num_trials
gc_sig_ratio$playerC_otherC_sig <- gc_sig_ratio$playerC_otherC_sig/gc_sig_ratio$num_trials
head(gc_sig_ratio)

gc_sig_onsets <- gc_sig_ratio %>% select(-otherC_playerC_sig,-playerC_otherC_sig) %>% 
  rename(ghost_live=otherO_playerO_sig,live_ghost=playerO_otherO_sig) %>% pivot_longer(cols = c("ghost_live","live_ghost"),
                                                                               names_to = "direction", values_to = "sig_ratio")
gc_sig_onsets[gc_sig_onsets$condition=="coupled",]$direction = "live_live"
ggplot(gc_sig_onsets, aes(x=order,y=sig_ratio,color=direction,fill=direction)) + geom_col(position = "dodge") + theme_bw() + theme(legend.position = "top") + xlab("order (sec)") + ylab("Significance Ratio\nOnset Density GC")

"
ghost_live > live_ghost
"

gc_sig_consonance <- gc_sig_ratio %>% select(-otherO_playerO_sig,-playerO_otherO_sig) %>% 
  rename(ghost_live=otherC_playerC_sig,live_ghost=playerC_otherC_sig) %>% pivot_longer(cols = c("ghost_live","live_ghost"),
                                                                                       names_to = "direction", values_to = "sig_ratio")
gc_sig_consonance[gc_sig_consonance$condition=="coupled",]$direction = "live_live"
ggplot(gc_sig_consonance, aes(x=order,y=sig_ratio,color=direction,fill=direction)) + geom_col(position = "dodge") + theme_bw() + theme(legend.position = "top") + xlab("model order (sec)") + ylab("Significance Ratio\nTonal Consonance GC")


"
According to these plots of sig ratio, it looks like:
live-live > ghost-live > live-ghost

These sig ratios are far below those of onset density though.
"


"
Overall, looking at collapsed and raw onsets, it appears that ghost-->live
is greater than live-->ghost for GC onsets. ghost-->live is overlapping with
live-->live. These are the only results I would hang my hat on, and even they
come after some good massaging of the data, so I think they should go in the SI.
"