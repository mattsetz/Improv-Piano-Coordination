library(tidyverse)
library(rjags)
library(runjags)

# how does emergent dissonance evolve over the course of a piece as a function of condition?

diss <- read.csv('Pipeline/tonal/master-emergent-dissonance-zeroed.csv')
diss <- diss %>% group_by(pair,window) %>% mutate(n_time50=ntile(time,50),n_time100=ntile(time,100)) %>% ungroup()
head(diss)


# Individual onset density ------------------------------------------------

diss_ind <- diss %>% drop_na() %>% group_by(window,condition,pair) %>% summarise(diss_1=mean(diss.1),diss_2=mean(diss.2))
ggplot(diss_ind, aes(x=-diss_1,color=condition,fill=condition)) + geom_density(alpha=.2) + facet_wrap(~window)
ggplot(diss_ind, aes(x=-diss_2,color=condition,fill=condition)) + geom_density(alpha=.2) + facet_wrap(~window)

diss_comb <- diss %>% drop_na() %>% group_by(window,condition,pair) %>% summarise(diss_comb=mean(diss.comb))
ggplot(diss_comb, aes(x=-diss_comb,color=condition,fill=condition)) + geom_density(alpha=.2) + facet_wrap(~window)

diss_emerge <- diss %>% drop_na() %>% group_by(window,condition,pair) %>% summarise(diss_emerge=mean(diss.emerge))
ggplot(diss_emerge, aes(x=-diss_emerge,color=condition,fill=condition)) + geom_density(alpha=.2) + facet_wrap(~window)


# Visualize ---------------------------------------------------------------

diss_avg <- diss %>% group_by(pair,window,n_time50,condition) %>% summarise(diss.emerge=mean(diss.emerge,na.rm = TRUE))

ggplot(filter(diss,window==5), aes(x=n_time10,y=diss.emerge,color=condition)) + geom_point() + geom_line(alpha=.3, aes(group=pair))
ggplot(filter(diss,window==5), aes(x=n_time10,y=diss.emerge,color=condition)) + stat_summary()
ggplot(filter(diss,window==5), aes(x=n_time20,y=diss.emerge,color=condition)) + stat_summary()

ggplot(filter(diss_avg,window==5), aes(x=2*n_time50,y=1.8-diss.emerge,fill=condition)) + stat_summary(geom="point",aes(color=condition)) + stat_summary(geom="line",aes(color=condition)) + stat_summary(geom="ribbon",alpha=.4) + theme_bw() + theme(legend.position = c(.7,.3)) + xlab("Normalized Time (%)") + ylab("Emergent Consonance")

ggplot(filter(diss,window==5), aes(x=n_time100,y=diss.emerge,color=condition)) + stat_summary()

"
Looking at this summary statistic, it seems that (1) both coupled + one-way trials decrease linearly throughout the first half of
pieces, plateau at some value and then increase in the final segment of a piece. (2) In the first half of pieces they appear to have 
the same level of emergent dissonance, but in the second half there is more emergent consonance in coupled pieces.
"

diss.w <- diss %>% filter(window==5)
ggplot(diss.w, aes(x=diss.emerge,color=condition,fill=condition))+geom_histogram(position="dodge",stat="density",alpha=.3)+facet_wrap(~n_time10)
ggplot(diss.w, aes(x=diss.emerge,color=condition,fill=condition))+geom_histogram(position="dodge",stat="density",alpha=.3)+facet_wrap(~n_time20)

"
Looks like emergent dissonance is long-tailed distributed to the right. In other words, dissonance peaks at some lower bound, but can also
become very high. The observed differences in mean emergent dissonance between conditions happen because emergent dissonance happens more often
in one-way trials, whereas coupled trials obtain emergent consonance more often. What happens when we compare average dissonance within a piece though?
"

diss.avg.10 <- diss %>% filter(window==5) %>% group_by(condition,pair,n_time50) %>% summarise(diss.emerge=mean(diss.emerge,na.rm=TRUE))
ggplot(diss.avg.10, aes(x=diss.emerge,color=condition,fill=condition))+geom_histogram(position="dodge",stat="density",alpha=.3)+facet_wrap(~n_time50)


"
Looks like this long-right-tailed pattern happens across pieces (i.e. not necessarily within pieces). In other words, most pieces are clustered 
around some low dissonance value, but then there are many 'outliers' -- pieces that have higher emergent consonance. There are more such outliers
in one-way trials. How about within a piece -- do we observe this pattern as well, or is emergent consonance normally distributed?
"

pairs = unique(diss$pair)
diss.w <- diss %>% filter(window==20)
p = sample(pairs,1)
ggplot(filter(diss.w,pair==p), aes(x=diss.emerge))+geom_density()+ggtitle(p)#+facet_wrap(~n_time10)

"
Playing around above, it looks like generally there is a long-right-tail distribution throughout individual pieces. As you look
at diss.emerge in smaller windowed units of a given piece this approaches a normal distribution though.
"






# Linear Model Per Piece --------------------------------------------------
diss.w <- diss %>% filter(window==2)

# with linear model
lin.lm.fits <- NULL
for (p in unique(diss.w$pair)) {
  print(p)
  diss.p <- diss.w %>% filter(pair==p)
  lm.p <- lm(diss.emerge ~ n_time10, diss.p)
  lin.lm.fits <- rbind(lin.lm.fits, data.frame(pair=p,intercept=lm.p$coefficients[[1]],linearB=lm.p$coefficients[[2]],condition=diss.p$condition[[1]]))
}

ggplot(lin.lm.fits, aes(x=linearB,color=condition,fill=condition))+geom_histogram(position = "dodge", stat = "density")


# with quad model
diss.w$n_time10_2 <- diss.w$n_time10*diss.w$n_time10
lm.fits <- NULL
for (p in unique(diss.w$pair)) {
  print(p)
  diss.p <- diss.w %>% filter(pair==p)
  lm.p <- lm(diss.emerge ~ n_time10+n_time10_2, diss.p)
  lm.fits <- rbind(lm.fits, data.frame(pair=p,intercept=lm.p$coefficients[[1]],linearB=lm.p$coefficients[[2]],quadB=lm.p$coefficients[[3]],condition=diss.p$condition[[1]]))
}

ggplot(lm.fits, aes(x=linearB,color=condition,fill=condition))+geom_histogram(position = "dodge", stat = "density")

"
Okay, so with just a linear model it appears that emergent dissonance decreases (negative slope) for coupled trials but stays constant
for one-way trials. This trend dissappears when we add a quadratic term.
"
library(brms)
lin.test <- brm(linearB ~ condition, lin.lm.fits)
plot(lin.test)
hypothesis(lin.test, "conditiononeMway > 0",class = "b")
summary(lin.test)

"
According to brms (using default prios -- beware), it looks like we have 2.03 Evidence Ratio in favor of the idea that emergent dissonance decreases more in coupled
trials. In other words, there is evidence to support this idea, but not super strong evidence. Its possible that a hierarchical model will impose shrinkage and reveal
a greater difference between conditions. Or not!
"


# Hierarchical Bayesian Model w BRMS ---------------------------------------------
diss.w.avg <- diss %>% filter(window==2) %>% group_by(pair,n_time10,condition) %>% summarise(diss.emerge=mean(diss.emerge,na.rm = TRUE)) %>% drop_na()
coupled.fit <- brm(diss.emerge ~ n_time10 + (pair|n_time10), filter(diss.w.avg,condition=="coupled"), file="Analysis/brms/diss-emerge-coupled-fit", cores = 2)


# Hierarchical Bayesian Model w JAGS --------------------------------------
diss.w.avg <- diss %>% filter(window==2) %>% group_by(pair,n_time100,condition) %>% summarise(diss.emerge=mean(diss.emerge,na.rm = TRUE)) %>% ungroup() %>% drop_na()
# fit JAGS model
diss.w.avg$condition <- as.numeric(diss.w.avg$condition)
diss.w.avg$pair <- as.numeric(diss.w.avg$pair)
dataList <- list(
  y=diss.w.avg$diss.emerge-mean(diss.w.avg$diss.emerge), # center y vals
  x=diss.w.avg$n_time100-mean(diss.w.avg$n_time100), # center x vals
  s=diss.w.avg$pair,
  c= unique(arrange(select(diss.w.avg,pair,condition),pair))$condition,
  Ntotal=nrow(diss.w.avg),
  Ntrials=length(unique(diss.w.avg$pair)),
  Nconditions=2
)

dissonanceModel = jags.model( file="Analysis/jags/MultiConditionDissonanceModel.txt" , data=dataList ,
                                        n.chains=3 , n.adapt=500 )
update( dissonanceModel , n.iter=500 )
multiConditionCodaSamples = coda.samples( dissonanceModel , 
                                          variable.names = c("beta1_cond1_cond2",
                                                             "sigma", "beta0sigma","beta1sigma",
                                                             "beta1mu", "beta0mu"),
                                          n.iter=3334 )
save(multiConditionCodaSamples,file="Analysis/jags/MultiConditionEmergentDissonanceCodaSamples_ntime100.rds")

samples <- get(load("Analysis/jags/MultiConditionEmergentDissonanceCodaSamples_ntime10.rds"))
gelman.diag(samples)
effectiveSize(multiConditionCodaSamples)
gelman.diag(multiConditionCodaSamples)
summary(multiConditionCodaSamples)
plot(multiConditionCodaSamples)

"
There seems to be some evidence in support of the idea that emergent dissonance 
decreases over time in both conditions, but moreso in coupled trials. This trend seems to be robust to
time granularity -- seems to apply roughly equally to deciles, 50-tiles and 100-tiles.

We can compute BayesFactor on the hypothesis that beta1_cond1_cond2 < 0 to get a numeric sense of this.

Questions:
* how to compute BayesFactor?
* is there good reason to explore an extended model with subjects effects?
"

# BayesFactor for hypothesis that coupled slope is less than 0
samples <- as.matrix(samples,chains=TRUE)
colnames(samples)
slopes = samples[,"beta1_cond1_cond2"]
hist(slopes)
sum(slope_cond1<0)/sum(slope_cond1>0)
slope_cond1 <- samples[,"beta0mu[1]"]
hist(slope_cond1)

# Hierarchical Bayesian Quad Model w JAGS ---------------------------------
diss.w.avg <- diss %>% filter(window==2) %>% group_by(pair,n_time10,condition) %>% summarise(diss.emerge=mean(diss.emerge,na.rm = TRUE)) %>% ungroup() %>% drop_na()
diss.w.avg$n_time10_2 <- diss.w.avg$n_time10*diss.w.avg$n_time10
# fit JAGS model
diss.w.avg$condition <- as.numeric(diss.w.avg$condition)
diss.w.avg$pair <- as.numeric(diss.w.avg$pair)
dataList <- list(
  y=diss.w.avg$diss.emerge-mean(diss.w.avg$diss.emerge), # center y vals
  x=diss.w.avg$n_time10-mean(diss.w.avg$n_time10), # center x vals
  x2=diss.w.avg$n_time10_2-mean(diss.w.avg$n_time10_2),
  s=diss.w.avg$pair,
  c= unique(arrange(select(diss.w.avg,pair,condition),pair))$condition,
  Ntotal=nrow(diss.w.avg),
  Ntrials=length(unique(diss.w.avg$pair)),
  Nconditions=2
)

dissonanceQuadModel = jags.model( file="Analysis/jags/MultiConditionQuadDissonanceModel.txt" , data=dataList ,
                              n.chains=3 , n.adapt=500 )
update( dissonanceQuadModel , n.iter=500 )
multiConditionQuadCodaSamples = coda.samples( dissonanceQuadModel , 
                                              variable.names = c("beta1_cond1_cond2","beta2_cond1_cond2",
                                                             "sigma", "beta0sigma","beta1sigma",
                                                             "beta1mu", "beta0mu","beta2mu"),
                                              n.iter=19994 )asdfdd
save(multiConditionQuadCodaSamples,file="Analysis/jags/MultiConditionQuadEmergentDissonanceCodaSamples_ntime10.rds")

load("Analysis/jags/MultiConditionQuadEmergentDissonanceCodaSamples_ntime100.rds")
effectiveSize(multiConditionQuadCodaSamples)
gelman.diag(multiConditionQuadCodaSamples)
plot(multiConditionQuadCodaSamples)
summary(multiConditionQuadCodaSamples)

"
Now it appears that the difference in linear slope btw conditions is even greater if we include a quadratic term.
It also appears that there may be a difference between the 
"

# Hierarchical Bayesian Model w Subjects ----------------------------------
diss.w <- diss %>% filter(window==2)
diss.w$pair_id <- str_remove_all(diss.w$pair, '[0-9]')
diss.w$pair_id <- diss.w %>% group_indices(pair_id)
diss.w$condition <- as.numeric(diss.w$condition)
diss.w$trial_id <- diss.w %>% group_indices(pair)
diss.w.avg <- diss.w %>% group_by(n_time10,trial_id,pair_id,condition) %>% summarise(diss.emerge=mean(diss.emerge,na.rm = TRUE)) %>% ungroup() %>% drop_na()

dataList = list(
  y=diss.w.avg$diss.emerge-mean(diss.w.avg$diss.emerge), # center y vals
  x=diss.w.avg$n_time10-mean(diss.w.avg$n_time10), # center x vals
  t=diss.w.avg$trial_id,
  p=arrange(unique(select(diss.w.avg,trial_id,pair_id)),trial_id)$pair_id,
  c=arrange(unique(select(diss.w.avg,pair_id,condition)),pair_id)$condition,
  Ntotal=nrow(diss.w.avg),
  Ntrials=length(unique(diss.w.avg$trial_id)),
  Npairs=length(unique(diss.w.avg$pair_id)),
  Nconditions=2
)

# fit JAGS model
dissonanceModelSubject = jags.model( file="Analysis/jags/MultiConditionDissonanceModelBySubject.txt" , data=dataList ,
                              n.chains=3 , n.adapt=500 )
update( dissonanceModelSubject , n.iter=500 )
multiConditionSubjectCodaSamples = coda.samples( dissonanceModelSubject , 
                                          variable.names = c("beta1_cond1_cond2",
                                                             "sigma", "beta0mu_sigma","beta1mu_sigma",
                                                             "beta1mu_mu", "beta0mu_mu"),
                                          n.iter=39994 )
save(multiConditionSubjectCodaSamples,file="Analysis/jags/MultiConditionBySubjectEmergentDissonanceCodaSamples.rds")

effectiveSize(multiConditionSubjectCodaSamples)
gelman.diag(multiConditionSubjectCodaSamples)
summary(multiConditionSubjectCodaSamples)
plot(multiConditionSubjectCodaSamples)

"
For some reason the effective sample size of beta1mu_sigma is very small, even though the other 
parameters have a relatively high ESS. Therefore I'm not sure I trust any of these results. Or would it be
alright to just trust the parameters that are well-behaved? 
One option is to simply run this for more time of course.
"