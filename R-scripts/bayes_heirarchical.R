library(rjags)
library(tidyverse)

# input data ####
diss <- read.csv('Pipeline/tonal/master-emergent-dissonance-zeroed.csv')

# build JAGS model of emergent dissonance -- coupled ####
W = 5
diss.w = filter(diss,window==5,condition=="one-way") %>% group_by(pair) %>% 
  mutate(n_time100=ntile(time,100)) %>% group_by(pair,n_time100) %>% 
  summarise(emerge.diss=mean(diss.emerge,rm.na=TRUE)) %>% ungroup()
y = diss.w$emerge.diss
subjects = group_indices(diss.w,pair)
Ntotal = length(y)
dataList = list(
  y=y,
  t=diss.w$n_time100,
  pair = subjects,
  Nsubjects=length(unique(diss.w$pair)),
  Ntotal = Ntotal
)

modelString = "
model {
  for (i in 1:Ntotal) {
    y[i] ~ dnorm(beta0[pair[i]]+beta1[pair[i]]*t[i], sdi[pair[i]])
  }
  for (p in 1:Nsubjects) {
    beta0[p] ~ dnorm(muS0,sdS0)
    beta1[p] ~ dnorm(muS1,sdS1)
    sdi[p] ~ dgamma(.01,.01)
  }
  muS0 ~ dunif(-2,2)
  sdS0 ~ dgamma(.01,.01)
  muS1 ~ dunif(-2,2)
  sdS1 ~ dgamma(.01,.01)
}"
writeLines( modelString , con="emerge-diss-single-condition.txt" )

jagsModel = jags.model( file="emerge-diss-single-condition.txt" , data=dataList ,
                        n.chains=3 , n.adapt=500 )
# burn in
update( jagsModel , n.iter=500 )

# sample posterior w/ MCMC
codaSamples = coda.samples( jagsModel , variable.names=c("muS0","muS1") ,
                            n.iter=6334 )

summary(codaSamples)
plot(codaSamples)
coupledSamples <- codaSamples
save(coupledSamples,file="coupledSamples.rda")
effectiveSize(coupledSamples)

# build JAGS model of emergent dissonance ####
modelString = "
model {
  for (i in 1:Ntotal) {
    y[i] ~ dnorm(beta0[pair[i]]+beta1[pair[i]]*t[i], sdi[pair[i]])
  }
  for (p in 1:Nsubjects) {
    beta0[p] ~ dnorm(muS0[condition[p]],sdS0[condition[p]])
    beta1[p] ~ dnorm(muS1[condition[p]],sdS1[condition[p]])
    sdi[p] ~ dgamma(.01,.01)
  }
  for (c in 1:Nconditions) {
    muS0[c] ~ dnorm(muP0, sdP0)
    sdS0[c] ~ dgamma(.01,.01)
    muS1[c] ~ dnorm(muP1, sdP1)
    sdS1[c] ~ dgamma(.01,.01)
  }

  muP0 ~ dunif(-2,2)
  sdP0 ~ dgamma(.01,.01)
  muP1 ~ dunif(-2,2)
  sdP1 ~ dgamma(.01,.01)
}"

writeLines( modelString , con="emerge-diss-combined-conditions.txt" )

W = 2
diss.summary <- diss %>% filter(window==W) %>% group_by(pair,n_time10,window,condition) %>% 
  summarise(diss.emerge = mean(diss.emerge, rm.na=TRUE)) %>% ungroup()
y = diss.summary$diss.emerge
t = diss.summary$n_time10

dataList = list()



# find suitable prior from data ####
max(diss.w$diss.emerge,na.rm = TRUE)
min(diss.w$diss.emerge,na.rm = TRUE)
mean(diss.w$diss.emerge,na.rm = TRUE)
sd(diss.w$diss.emerge)

