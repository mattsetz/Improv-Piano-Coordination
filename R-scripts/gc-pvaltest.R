library(tidyverse)
library(runjags)
library(rjags)

# input data (old) ####
inDir <- 'Pipeline/tonal/gc-detrended-dissonance/'
files <- list.files(inDir)
gc <- NULL
for (f in files) {
  print(f)
  df = read.csv(paste(inDir,f,sep=''))
  gc <- rbind(gc,df)
}

gc.shuffled <- NULL
inDir <- 'Pipeline/tonal/gc-detrended-dissonance/shuffled/'
files <- list.files(inDir)
for (f in files) {
  print(f)
  df = read.csv(paste(inDir,f,sep=''))
  gc.shuffled <- rbind(gc.shuffled,df)
}

gc <- rbind(gc,gc.shuffled)
head(gc)
gc$is.sig <- ifelse(gc$p.val<.05,1,0)

# build sig models for ghost.live ####
O <- 4.8
W <- 2
gc.w <- gc %>% filter(order_sec==O,window==2,direction=="ghost.live")
dataList = list(
  y = gc.w$is.sig,
  Ntotal = nrow(gc.w)
)

modelString = " # open quote for modelString
  model {
    for ( i in 1:Ntotal ) {
      y[i] ~ dbern( theta )
    }
    theta ~ dbeta( 1 , 1 )
  }
" # close quote for modelString
writeLines( modelString , con="gc-sig-model.txt" ) # write to file

# instantiate JAGS model
jagsModel = jags.model( file="gc-sig-model.txt" , data=dataList ,
                        n.chains=3 , n.adapt=500 )
# burn in
update( jagsModel , n.iter=500 )

# sample posterior w/ MCMC
codaSamples_ghost_live = coda.samples( jagsModel , variable.names=c("theta") ,
                            n.iter=3334 )
plot(codaSamples)
summary(codaSamples)

# do the same thing for live.ghost
gc.w <- gc %>% filter(order_sec==O,window==2,direction=="live.ghost")
dataList = list(
  y = gc.w$is.sig,
  Ntotal = nrow(gc.w)
)
jagsModel = jags.model( file="gc-sig-model.txt" , data=dataList ,
                        n.chains=3 , n.adapt=500 )
# burn in
update( jagsModel , n.iter=500 )

# sample posterior w/ MCMC
codaSamplesLiveGhost = coda.samples( jagsModel , variable.names=c("theta") ,
                            n.iter=3334 )

# model combining over condition ####
O = 4.8
W = 2
gc.w <- gc %>% filter(order_sec==O,window==W)

combModelString = " # open quote for modelString
  model {
    for ( i in 1:Ntotal ) {
      y[i] ~ dbern( theta[c[i]] )
    }
    for (c in 1:Nconditions) {
      theta[c] ~ dbeta(1,1)
    }
}
" # close quote for modelString
writeLines( combModelString , con="gc-sig-model-comb.txt" )
gc.w$direction_numeric <- as.numeric(gc.w$direction)
# live.live = 1, ghost.live = 2, live.ghost = 3, shuffled = 4
dataList = list(y=gc.w$is.sig,
                c=gc.w$direction_numeric,
                Ntotal=nrow(gc.w),
                Nconditions=length(unique(gc.w$direction)))

jagsModel = jags.model( file="gc-sig-model-comb.txt" , data=dataList ,
                        n.chains=3 , n.adapt=500 )
# burn in
update( jagsModel , n.iter=500 )

# sample posterior w/ MCMC
codaSamplesCombined = coda.samples( jagsModel , variable.names=c("theta") ,
                                     n.iter=3334 )
summary(codaSamplesCombined)
plot(codaSamples)
save(codaSamples,file="Pipeline/tonal/GcDissPtestSamples.rda")
gelman.plot(codaSamples)
effectiveSize(codaSamples)

summary(codaSamples)
