library(tidyverse)

onsets.ind <- read.csv('Pipeline/rhythm/onset-density/collapsed/master-onset-density-individual.csv')
onsets.comb <- read.csv('Pipeline/rhythm/onset-density/collapsed/master-onset-density-combined.csv')

###############################
# UTILITY FUNCTIONS
###############################

pad.onsets.df <- function(df1, df2) {
  if (nrow(df1) > nrow(df2)) {
    t <- df1 %>% filter(time>max(df2$time)) %>% select(time)
    df2.end <- data.frame(time=t)
    df2.end$num_onsets <- 0
    df2.end$player <- df2$player[1]
    df2.end$other <- df2$other[1]
    df2.end$pair <- df2$pair[1]
    df2.end$condition <- df2$condition[1]
    df2 <- rbind(df2,df2.end)
  } else if (nrow(df1) < nrow(df2)) {
    t <- df2 %>% filter(time>max(df1$time)) %>% select(time)
    df1.end <- data.frame(time=t)
    df1.end$num_onsets <- 0
    df1.end$player <- df1$player[1]
    df1.end$other <- df1$other[1]
    df1.end$pair <- df1$pair[1]
    df1.end$condition <- df1$condition[1]
    df1 <- rbind(df1,df1.end)
  }
  return(rbind(df1, df2))
}

#####################################
# Compute variability ratio
#####################################

# DataFrame:
# pair | condition | n_t | ratio
var.ratio <- NULL
for (p in unique(onsets.ind$player)) {
  print(p)
  player <- onsets.ind %>% filter(player==p)
  other.name <- as.character(player$other[1])
  other <- onsets.ind %>% filter(player==other.name)
  individual <- pad.onsets.df(player,other)

  print("wtf1")
  # find ratio of variability
  individual$n_t <- ntile(individual$time,10)
  ind.sum <- individual %>% group_by(player,n_t) %>%
              summarise(v=var(num_onsets)) %>% ungroup()
  ind.sum <- ind.sum %>% group_by(n_t) %>% 
              summarise(ind_var=mean(v)) %>% ungroup()
  
  print("wtf2")
  pair.name <- as.character(player$pair[1])
  combined <- onsets.comb %>% filter(pair==pair.name)
  combined$n_t <- ntile(combined$time,10)
  comb.sum <- combined %>% group_by(n_t) %>% 
              summarise(comb_var=var(num_onsets)) %>% ungroup()
  
  print("wtf3")
  ind.comb <- merge(ind.sum,comb.sum)
  ind.comb$ratio <- ind.comb$ind_var/ind.comb$comb_var
  ind.comb$pair <- as.character(player$pair[1])
  ind.comb$condition <- as.character(player$condition[1])
  
  print("wtf4")
  var.ratio <- rbind(var.ratio,ind.comb)
}

head(var.ratio)
t.test(filter(var.ratio,condition=="coupled")$ratio,
       filter(var.ratio,condition=="one-way")$ratio)

var.ratio.sum <- var.ratio %>% group_by(condition,n_t) %>%
                  summarise(avg_ratio=mean(ratio),
                            se_ratio=sd(ratio)/sqrt(n())) %>% ungroup()

ggplot(var.ratio.sum, aes(x=n_t, y=avg_ratio, color=condition,fill=condition)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=avg_ratio-se_ratio,
                    ymax=avg_ratio+se_ratio),
                color="black",
                position="dodge")


#####################################
# Surrogate analysis
#####################################

surrogate.onsets <- function(onsets1, onsets2) {
  pair.name <- paste(as.character(onsets1$player[1]),
                     as.character(onsets2$player[2],sep=""))
  len <- min(nrow(onsets1),nrow(onsets2))
  num_onsets_ <- onsets1$num_onsets[1:len] + onsets2$num_onsets[1:len]
  surrogate.pair <- data.frame(time = onsets1$time[1:len],
                               num_onsets = num_onsets_,
                               pair = pair.name,
                               condition = "uncoupled")
  
  return(surrogate.pair)
}

coupled.trials <- unique(filter(onsets.ind,condition=="coupled")$player)
for (p in coupled.trials) {
  print(p)
  player <- onsets.ind %>% filter(player==p)
  print('where?')
  
  # get random recording to pair
  other.name <- as.character(sample(coupled.trials,1))
  while ((other.name == as.character(p)) | (other.name == as.character(player$other[1]))) {
    other.name <- sample(coupled.trials,1)
  }
  other <- onsets.ind %>% filter(player==other.name)
  
  print('wtf')
  # trunctate to match shorter dataframe
  len <- min(nrow(player), nrow(other))
  player <- player[1:len,]
  other <- other[1:len,]
  individual <- rbind(player, other)
  
  # get variability ratio
  individual$n_t <- ntile(individual$time, 10)
  ind.sum <- individual %>% group_by(player,n_t) %>%
    summarise(v=var(num_onsets)) %>% ungroup()
  ind.sum <- ind.sum %>% group_by(n_t) %>% 
    summarise(ind_var=mean(v)) %>% ungroup()
  
  print("wtf2")
  combined <- surrogate.onsets(player, other)
  combined$n_t <- ntile(combined$time,10)
  comb.sum <- combined %>% group_by(n_t) %>% 
    summarise(comb_var=var(num_onsets)) %>% ungroup()
  
  print("wtf3")
  ind.comb <- merge(ind.sum,comb.sum)
  ind.comb$ratio <- ind.comb$ind_var/ind.comb$comb_var
  ind.comb$pair <- as.character(player$pair[1])
  ind.comb$condition <- "uncoupled"
  
  print("wtf4")
  var.ratio <- rbind(var.ratio,ind.comb)
}

var.ratio$condition <- factor(var.ratio$condition)
ggplot(filter(var.ratio,n_t<7), aes(x=ratio,fill=condition,color=condition)) +
  geom_density(alpha=0.3)

t.test(filter(var.ratio,n_t<5,condition=="coupled")$ratio,
       filter(var.ratio,n_t<5,condition=="uncoupled")$ratio)
