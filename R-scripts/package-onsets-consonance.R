library(tidyverse)


# Package collapsed onsets and consonance time series ---------------------

onsets_collapsed <- read.csv("Pipeline/rhythm/onset-density/collapsed/master-onset-density-individual.csv")
consonance <- read.csv("Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-individual.csv")
summary <- read.csv("summary-individual.csv")
onsets_collapsed <- merge(onsets_collapsed, dplyr::select(summary,player,condition))
onsets_consonance <- NULL

for (p in unique(consonance$player)) {
  print(p)
  onsets_p <- onsets_collapsed %>% filter(player==p) %>% rename(player_onsets=num_onsets)
  condition = as.character(onsets_p$condition[1])
  onsets_p$time <- round(onsets_p$time,1)
  onsets_other <- onsets_collapsed %>% filter(player==as.character(onsets_p$other[1])) %>% rename(other_onsets=num_onsets) %>% dplyr::select(time,other_onsets)
  onsets_other$time <- round(onsets_other$time,1)
  consonance_p <- consonance %>% filter(player==p,window==2) %>% dplyr::select(time,avg_dist) %>% rename(player_diss=avg_dist)
  consonance_p$time <- round(consonance_p$time,1)
  consonance_other <- consonance %>% filter(player==as.character(onsets_p$other[1]),window==2) %>% dplyr::select(time,avg_dist) %>% rename(other_diss=avg_dist)
  consonance_other$time <- round(consonance_other$time,1)
  onsets_consonance <- merge(merge(merge(onsets_p,onsets_other,all=TRUE),consonance_p,all=TRUE),consonance_other,all=TRUE) %>% dplyr::select(time,player_onsets,player_diss,other_onsets,other_diss)
  onsets_consonance$player_onsets <- onsets_consonance$player_onsets %>% replace_na(0)
  onsets_consonance$other_onsets <- onsets_consonance$other_onsets %>% replace_na(0)
  onsets_consonance <- onsets_consonance[seq(1,nrow(onsets_consonance),3),]
  onsets_consonance$player_onsets <- c(NA,diff(onsets_consonance$player_onsets))
  onsets_consonance$player_diss <- c(NA,diff(onsets_consonance$player_diss))
  onsets_consonance$other_onsets <- c(NA,diff(onsets_consonance$other_onsets))
  onsets_consonance$other_diss <- c(NA,diff(onsets_consonance$other_diss))
  out_dir <- "Pipeline/detrended-onsets-consonance"
  write.csv(onsets_consonance, paste("Pipeline/detrended-onsets-consonance/collapsed/detrended-onsets-consonance-",p,".csv",sep=""),row.names = FALSE)
}




# Package raw onsets and consonance time series ---------------------------

onsets_raw <- read.csv("Pipeline/rhythm/onset-density/total/master-raw-onset-density-individual.csv")
consonance <- read.csv("Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-individual.csv")
summary <- read.csv("summary-individual.csv")
summary$other <- str_remove(as.character(summary$pair),as.character(summary$player))
onsets_raw <- merge(onsets_raw, dplyr::select(summary,player,other,condition))
onsets_consonance <- NULL

for (p in unique(consonance$player)) {
  print(p)
  onsets_p <- onsets_raw %>% filter(player==p) %>% rename(player_onsets=num_onsets)
  condition = as.character(onsets_p$condition[1])
  onsets_p$time <- round(onsets_p$time,1)
  onsets_other <- onsets_raw %>% filter(player==as.character(onsets_p$other[1])) %>% rename(other_onsets=num_onsets) %>% dplyr::select(time,other_onsets)
  onsets_other$time <- round(onsets_other$time,1)
  consonance_p <- consonance %>% filter(player==p,window==2) %>% dplyr::select(time,avg_dist) %>% rename(player_diss=avg_dist)
  consonance_p$time <- round(consonance_p$time,1)
  consonance_other <- consonance %>% filter(player==as.character(onsets_p$other[1]),window==2) %>% dplyr::select(time,avg_dist) %>% rename(other_diss=avg_dist)
  consonance_other$time <- round(consonance_other$time,1)
  onsets_consonance <- merge(merge(merge(onsets_p,onsets_other,all=TRUE),consonance_p,all=TRUE),consonance_other,all=TRUE) %>% dplyr::select(time,player_onsets,player_diss,other_onsets,other_diss)
  onsets_consonance$player_onsets <- onsets_consonance$player_onsets %>% replace_na(0)
  onsets_consonance$other_onsets <- onsets_consonance$other_onsets %>% replace_na(0)
  onsets_consonance <- onsets_consonance[seq(1,nrow(onsets_consonance),3),]
  onsets_consonance$player_onsets <- c(NA,diff(onsets_consonance$player_onsets))
  onsets_consonance$player_diss <- c(NA,diff(onsets_consonance$player_diss))
  onsets_consonance$other_onsets <- c(NA,diff(onsets_consonance$other_onsets))
  onsets_consonance$other_diss <- c(NA,diff(onsets_consonance$other_diss))
  write.csv(onsets_consonance, paste("Pipeline/detrended-onsets-consonance/raw/detrended-onsets-consonance-",p,".csv",sep=""),row.names = FALSE)
}
