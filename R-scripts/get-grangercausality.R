library(tidyverse)


# import data ####
diam.ind <- read.csv('Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-individual.csv')
summary <- read.csv("summary-individual.csv")
diss <- merge(diam.ind,summary)
diss$detrended_diss = c(NA,diff(diss$avg_dist))



# Granger Causality ####
library(lmtest)

get.gc.shuffled <- function(diss, p, LAG_ORDER, NUM_STEPS) {
  gc.diss <- NULL
  windows <- c(2)
  for (w in windows) {
    diss.player <- diss %>% filter(player==p,window==w)
    diss.other <- diss %>% filter(player==as.character(diss.player$other[1]),
                                  window==w)
    condition <- as.character(diss.player$condition[1])
    
    gc.shuffled = grangertest(diss.other[sample(nrow(diss.other)),]$detrended_diss,
                              diss.player[sample(nrow(diss.player)),]$detrended_diss, order = LAG_ORDER)
    
    gc.shuffled.df <- data.frame(player=as.character(diss.player$other[1]),
                                 other=p,
                                 condition=as.character(diss.player$condition[1]),
                                 direction='shuffled',
                                 shuffled=TRUE,
                                 order=LAG_ORDER,
                                 order_sec=LAG_ORDER*(.2*NUM_STEPS),
                                 window=w,
                                 p.val=gc.shuffled$`Pr(>F)`[2])
    print("writing shuffled to file")
    write.csv(gc.shuffled.df, paste('Pipeline/tonal/gc-detrended-dissonance/shuffled/',
                                    as.character(LAG_ORDER),'-',
                                    as.character(p),'-shuffled.csv',sep=''))
  }
  return()
}

get.gc <- function(diss, p, LAG_ORDER, NUM_STEPS) {
  gc.diss <- NULL
  windows <- c(2)
  for (w in windows) {
    diss.player <- diss %>% filter(player==p,window==w)
    diss.other <- diss %>% filter(player==as.character(diss.player$other[1]),
                                  window==w)
    condition <- as.character(diss.player$condition[1])
    
    gc = grangertest(diss.player$detrended_diss, 
                     diss.other$detrended_diss, order = LAG_ORDER)
    gc.diss <- rbind(gc.diss, data.frame(player=p,
                                         other=as.character(diss.player$other[1]),
                                         condition=condition,
                                         direction=if_else(condition=="one-way","live.ghost","live.live"),
                                         shuffled=FALSE,
                                         order=LAG_ORDER,
                                         order_sec=LAG_ORDER*(.2*NUM_STEPS),
                                         window=w,
                                         p.val=gc$`Pr(>F)`[2],
                                         f.val=gc$`F`[2]))
    
    if (condition=="one-way") {
      gc.reverse <- grangertest(diss.other$detrended_diss,
                                diss.player$detrended_diss,order = LAG_ORDER)
      gc.diss <- rbind(gc.diss, data.frame(player=as.character(diss.player$other[1]),
                                           other=p,
                                           condition=condition,
                                           direction=if_else(condition=="one-way","ghost.live","live.live"),
                                           shuffled=FALSE,
                                           order=LAG_ORDER,
                                           order_sec=LAG_ORDER*(.2*NUM_STEPS),
                                           window=w,
                                           p.val=gc.reverse$`Pr(>F)`[2],
                                           f.val=gc.reverse$`F`[2]))
    }
    
    print("writing to file")
    write.csv(gc.diss, paste('Pipeline/tonal/gc-detrended-dissonance/',
                             as.character(LAG_ORDER),'-',
                             as.character(p),'.csv',sep=''))
    
    gc.shuffled = grangertest(diss.other[sample(nrow(diss.other)),]$detrended_diss,
                                       diss.player[sample(nrow(diss.player)),]$detrended_diss, order = LAG_ORDER)

    gc.shuffled.df <- data.frame(player=as.character(diss.player$other[1]),
                                         other=p,
                                         condition=as.character(diss.player$condition[1]),
                                         direction='shuffled',
                                         shuffled=TRUE,
                                         order=LAG_ORDER,
                                         order_sec=LAG_ORDER*(.2*NUM_STEPS),
                                         window=w,
                                         p.val=gc.shuffled$`Pr(>F)`[2],
                                         f.val=gc.shuffled$`F`[2])
  print("writing shuffled to file")
  write.csv(gc.shuffled.df, paste('Pipeline/tonal/gc-detrended-dissonance/shuffled/',
                           as.character(LAG_ORDER),'-',
                           as.character(p),'-shuffled.csv',sep=''))
  }
  return()
}

get.gc(diss[seq(1,nrow(diss),STEPS),],"a1",20,STEPS)

STEPS = 3
lags <- seq(6,15,2)
for (LAG_ORDER in lags) {
  for (p in unique(diss$player)) {
    print(p)
    tryCatch(get.gc(diss[seq(1,nrow(diss),STEPS),],p,LAG_ORDER, STEPS),error=function(e) {return(NA)})
  }
}
