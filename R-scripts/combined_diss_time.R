library(tidyverse)
library(rjags)
library(runjags)

# how does emergent dissonance evolve over the course of a piece as a function of condition?

diss <- read.csv('Pipeline/tonal/master-emergent-dissonance-zeroed.csv')
diss <- diss %>% group_by(pair,window) %>% mutate(n_time50=ntile(time,50),n_time100=ntile(time,100)) %>% ungroup()
head(diss)

# Visualize Individual Dissonance ---------------------------------------------------------------
diss_ind <- diss %>% select(player,window,condition,time,diss.1,diss.2) %>% pivot_longer(cols=c(diss.1,diss.2),names_to="ind_diss")

diss_avg_w <- diss %>% group_by(window,pair,condition,n_time50) %>% summarise(diss.1=mean(diss.1,na.rm = TRUE),diss.2=mean(diss.2,na.rm=TRUE),diss_comb=mean(diss.comb,na.rm = TRUE)) %>% ungroup()

ggplot(diss.avg.w, aes(x=n_time50,y=diss.1,color=condition)) + stat_summary()
ggplot(diss.avg.w, aes(x=n_time50,y=diss.2,color=condition)) + stat_summary()


# Visualize Combined Dissonance ---------------------------------------------------------------
diss.avg.w <- diss %>% group_by(window,pair,condition) %>% summarise(diss.1=mean(diss.1,na.rm = TRUE),diss.2=mean(diss.2,na.rm=TRUE),diss_comb=mean(diss.comb,na.rm = TRUE)) %>% ungroup()
ggplot(diss.avg.w, aes(x=diss_comb,color=condition,fill=condition))+geom_density(alpha=.2)+facet_wrap(~window)

"
Overall there is no effect of condition on combined dissonance.
"

ggplot(diss.avg.w, aes(x=n_time50,y=diss_comb,color=condition)) + stat_summary()
ggplot(filter(diss,window==5), aes(x=n_time10,y=diss.comb,color=condition)) + stat_summary()
ggplot(filter(diss,window==5), aes(x=n_time20,y=diss.comb,color=condition)) + stat_summary()
ggplot(filter(diss,window==5), aes(x=n_time50,y=diss.comb,color=condition)) + stat_summary()
ggplot(filter(diss,window==5), aes(x=n_time100,y=diss.comb,color=condition)) + stat_summary() + xlab('Normalized Time (%)') + ylab("Emergent Dissonance")

diss.avg.w <- diss %>% filter(window==5) %>% group_by(pair,condition,n_time50) %>% summarise(diss.comb=mean(diss.comb,na.rm = TRUE)) %>% ungroup()
ggplot(diss.avg.w, aes(x=n_time50,y=diss.comb,color=condition)) + stat_summary()



"
Looking at this summary statistic, it seems that (1) both coupled + one-way trials decrease linearly throughout the first half of
pieces, plateau at some value and then increase in the final segment of a piece. (2) In the first half of pieces they appear to have 
the same level of emergent dissonance, but in the second half there is more emergent consonance in coupled pieces.
"

diss.w <- diss %>% filter(window==5)
ggplot(diss.w, aes(x=diss.comb,color=condition,fill=condition))+geom_histogram(position="dodge",stat="density",alpha=.3)+facet_wrap(~n_time10)
ggplot(diss.w, aes(x=diss.comb,color=condition,fill=condition))+geom_histogram(position="dodge",stat="density",alpha=.3)+facet_wrap(~n_time20)

"
Looks like emergent dissonance is long-tailed distributed to the right. In other words, dissonance peaks at some lower bound, but can also
become very high. The observed differences in mean emergent dissonance between conditions happen because emergent dissonance happens more often
in one-way trials, whereas coupled trials obtain emergent consonance more often. What happens when we compare average dissonance within a piece though?
"

diss.avg.10 <- diss %>% filter(window==5) %>% group_by(condition,pair,n_time50) %>% summarise(diss.comb=mean(diss.comb,na.rm=TRUE))
ggplot(diss.avg.10, aes(x=diss.comb,color=condition,fill=condition))+geom_histogram(position="dodge",stat="density",alpha=.3)+facet_wrap(~n_time50)


"
Looks like this long-right-tailed pattern happens across pieces (i.e. not necessarily within pieces). In other words, most pieces are clustered 
around some low dissonance value, but then there are many 'outliers' -- pieces that have higher emergent consonance. There are more such outliers
in one-way trials. How about within a piece -- do we observe this pattern as well, or is emergent consonance normally distributed?
"

pairs = unique(diss$pair)
diss.w <- diss %>% filter(window==20)
p = sample(pairs,1)
ggplot(filter(diss.w,pair==p), aes(x=diss.comb))+geom_density()+ggtitle(p)#+facet_wrap(~n_time10)

"
Playing around above, it looks like generally there is a long-right-tail distribution throughout individual pieces. As you look
at diss.comb in smaller windowed units of a given piece this approaches a normal distribution though.
"


# Fit Hierarchical Bayesian Quad Model w JAGS -- Parameter Sweep ---------------------------------
fit.quad.model <- function(ntime, window_size) {
  diss.w.avg <- diss %>% filter(window==window_size) %>% rename(n_time=ntime) %>% group_by(pair,n_time,condition) %>% summarise(diss.comb=mean(diss.comb,na.rm = TRUE)) %>% ungroup() %>% drop_na()
  diss.w.avg$n_time_2 <- diss.w.avg$n_time*diss.w.avg$n_time
  
  # fit JAGS model
  diss.w.avg$condition <- as.numeric(diss.w.avg$condition)
  diss.w.avg$pair <- as.numeric(diss.w.avg$pair)
  dataList <- list(
    y=diss.w.avg$diss.comb-mean(diss.w.avg$diss.comb), # center y vals
    x=diss.w.avg$n_time-mean(diss.w.avg$n_time), # center x vals
    x2=diss.w.avg$n_time_2-mean(diss.w.avg$n_time_2),
    s=diss.w.avg$pair,
    c= unique(arrange(select(diss.w.avg,pair,condition),pair))$condition,
    Ntotal=nrow(diss.w.avg),
    Ntrials=length(unique(diss.w.avg$pair)),
    Nconditions=2
  )
  
  print("building model")
  dissonanceQuadModel = jags.model( file="Analysis/jags/MultiConditionQuadDissonanceModel.txt" , data=dataList ,
                                    n.chains=3 , n.adapt=500 )
  update( dissonanceQuadModel , n.iter=500 )
  multiConditionQuadCodaSamples = coda.samples( dissonanceQuadModel , 
                                                variable.names = c("beta1_cond1_cond2","beta2_cond1_cond2",
                                                                   "sigma", "beta0sigma","beta1sigma","beta2sigma",
                                                                   "beta1mu", "beta0mu","beta2mu"), n.iter=25000)
  
  save(multiConditionQuadCodaSamples,file=paste("Analysis/jags/quad_fits_max_ess/MultiConditionQuadEmergentDissonanceCodaSamples_window",window_size,"_",ntime,".rds",sep = ""))
}

# fit a few models with high ESS
fit.quad.model(ntime = "n_time50",window_size = 5)
fit.quad.model(ntime = "n_time50",window_size = 2)
fit.quad.model(ntime = "n_time20",window_size = 2)
fit.quad.model(ntime = "n_time20",window_size = 5)

samples <- get(load("Analysis/jags/quad_fits_max_ess/MultiConditionQuadEmergentDissonanceCodaSamples_window2_n_time50.rds"))
effectiveSize(samples)
gelman.diag(samples)

plot(samples)

# parameter sweep
windows <- unique(diss$window)
ntimes <- c("n_time10","n_time20","n_time50","n_time100")
for (w in windows) {
  for (ntime in ntimes) {
    fit.quad.model(ntime,w)
  }
}


# Analyze Quad Model Fits -------------------------------------------------

# Individual fit
# Step 1: Evaluate MCMC chains
samples <- get(load("Analysis/jags/quad_fits_max_ess/MultiConditionQuadEmergentDissonanceCodaSamples_window5_n_time20.rds"))
effectiveSize(samples)
# gelman.diag
g <- matrix(NA, nrow=nvar(samples), ncol=2)
for (v in 1:nvar(samples)) {
      g[v,] <- gelman.diag(samples[,v])$psrf
}
g
# Step 2: analyze posterior
summary(samples)
plot(samples)

samples_m <- as.matrix(samples,chains = TRUE)
sum(samples_m[,"beta1_cond1_cond2"]<0)/sum(samples_m[,"beta1_cond1_cond2"]>0) # EvidenceRatio
sum(samples_m[,"beta2_cond1_cond2"]>0)/sum(samples_m[,"beta2_cond1_cond2"]<0) # EvidenceRatio 
hist(samples_m[,"beta1_cond1_cond2"])
hist(samples_m[,"beta2_cond1_cond2"])
hist(samples_m[,"beta0mu[2]"])

# Parameter Sweep
inDir = 'Analysis/jags/quad_fits'
files = list.files(inDir)

fits <- NULL
for (f in files) {
  print(f)
  samples_df <- as.data.frame(as.matrix(get(load(paste(inDir,f,sep='/'))),chains = TRUE))
  samples_df$ntime=as.numeric(str_extract_all(f,"[0-9]{1,3}")[[1]][[1]])
  samples_df$window_size=as.numeric(str_extract_all(f,"[0-9]{1,3}")[[1]][[2]])
  fits <- rbind(fits,samples_df)
}

fits %>% group_by(window_size,ntime) %>% count()
ggplot(fits, aes(x=beta1_cond1_cond2)) + geom_density() + facet_wrap(~ntime+window_size)

"
From above fits, appears that linear slope is negative for both pieces, but more negative in coupled pieces. Also there is a positive
quad coefficient, which is more positive in coupled trials. This means that coupled trials are more U-shaped (less flat) and that
emergent dissonance decreases more throughout. These effects appear to be robust to time granulity and window size.

For the parameter sweep, ESS values are pretty small -- we didn't run for enough iterations. While I haven't systematically investigated
this, it appears that higher time granularity results in higher ESS for the same # of iterations.
"

" How do we interpret more negative linear coefficient in coupled trials?"
quad <- NULL
for (B in -seq(0,100,30)) {
  x=-100:100
  y=x^2+B*x
  quad <- rbind(quad,data.frame(B=B,x=x,y=y))
}
ggplot(quad,aes(x=x,y=y))+geom_point()+facet_wrap(~B)

"How do we interpret larger x^2 coefficient in coupled trials?"
quad <- NULL
for (A in 1:5) {
  x=-100:100
  y=A*x^2+x
  quad <- rbind(quad,data.frame(A=A,x=x,y=y))
}
ggplot(quad,aes(x=x,y=y))+geom_point()+facet_wrap(~A)
"
Bigger quad coeffients mean steeper curves.
"



# Hierarchical Bayesian Linear Model w JAGS --------------------------------------
diss.w.avg <- diss %>% filter(window==2) %>% group_by(pair,n_time100,condition) %>% summarise(diss.comb=mean(diss.comb,na.rm = TRUE)) %>% ungroup() %>% drop_na()
# fit JAGS model
diss.w.avg$condition <- as.numeric(diss.w.avg$condition)
diss.w.avg$pair <- as.numeric(diss.w.avg$pair)
dataList <- list(
  y=diss.w.avg$diss.comb-mean(diss.w.avg$diss.comb), # center y vals
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


diss.w.avg <- diss %>% filter(window==2) %>% group_by(pair,n_time10,condition) %>% summarise(diss.comb=mean(diss.comb,na.rm = TRUE)) %>% ungroup() %>% drop_na()
diss.w.avg$n_time10_2 <- diss.w.avg$n_time10*diss.w.avg$n_time10
# fit JAGS model
diss.w.avg$condition <- as.numeric(diss.w.avg$condition)
diss.w.avg$pair <- as.numeric(diss.w.avg$pair)
dataList <- list(
  y=diss.w.avg$diss.comb-mean(diss.w.avg$diss.comb), # center y vals
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
                                              n.iter=19994 )
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
diss.w.avg <- diss.w %>% group_by(n_time10,trial_id,pair_id,condition) %>% summarise(diss.comb=mean(diss.comb,na.rm = TRUE)) %>% ungroup() %>% drop_na()

dataList = list(
  y=diss.w.avg$diss.comb-mean(diss.w.avg$diss.comb), # center y vals
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
# Linear Model Per Piece --------------------------------------------------
diss.w <- diss %>% filter(window==2)

# with linear model
lin.lm.fits <- NULL
for (p in unique(diss.w$pair)) {
  print(p)
  diss.p <- diss.w %>% filter(pair==p)
  lm.p <- lm(diss.comb ~ n_time10, diss.p)
  lin.lm.fits <- rbind(lin.lm.fits, data.frame(pair=p,intercept=lm.p$coefficients[[1]],linearB=lm.p$coefficients[[2]],condition=diss.p$condition[[1]]))
}

ggplot(lin.lm.fits, aes(x=linearB,color=condition,fill=condition))+geom_histogram(position = "dodge", stat = "density")


# with quad model
diss.w$n_time10_2 <- diss.w$n_time10*diss.w$n_time10
lm.fits <- NULL
for (p in unique(diss.w$pair)) {
  print(p)
  diss.p <- diss.w %>% filter(pair==p)
  lm.p <- lm(diss.comb ~ n_time10+n_time10_2, diss.p)
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

