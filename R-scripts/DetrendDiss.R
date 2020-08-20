library(tidyverse)
library(pracma)

# try to detrend ####
diss <- read.csv('Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-individual.csv')
first.differencing <- function(s) {
  result <- c(NA)
  for (i in 2:length(s)) {
    result = c(result,s[[i]]-s[[i-1]])
  }
  return(result)
}

diss$detrended_diss = c(NA,diff(diss$avg_dist))


ggplot(filter(diss,player=="a1",window==2,time>30,time<40), aes(x=time,y=avg_dist)) +
  geom_point(color="red") +
  geom_smooth(method = "lm") +
  geom_point(aes(y=detrended_diss),color="green")

ggplot(filter(diss,player=="g3",window==2), aes(x=time,y=avg_dist)) +
  geom_point(color="red") +
  geom_smooth(method = "lm") +
  geom_point(aes(y=detrended_diss),color="green")


# check to see if detrending worked ####
Box.test(filter(diss,player=="a1",window==2)$avg_dist,lag=25,type="Ljung-Box")
Box.test(filter(diss,player=="a1",window==2)$detrended_diss,lag=25,type="Ljung-Box")

test$p.value
a1.detrend <- detrend(filter(diss,player=="a1")$avg_dist,tt="linear")

ggplot(a1.detrend)