#################################
# Analysis of tonal entropy
#################################

library(tidyverse)

#####################################
# input data
#####################################
summary <- read.csv("summary.csv")
inDir <- 'Pipeline/tonal-entropy/'
infiles <- list.files(inDir,pattern=".*csv$")
entropy <- NULL
for (f in infiles) {
  print(f)
  
  entropy_i <- read.csv(paste(inDir,f,sep=''))
  
  name <- str_replace(f,'-entropy.csv','')
  this_player <- str_split(name,"[.]")[[1]][[3]]
  entropy_i$this_player <- this_player
  entropy_i$other_player <- str_replace(str_split(name,"[.]")[[1]][[2]], this_player, "")
  entropy <- rbind(entropy, entropy_i)
}
summary$this_player = summary$person_trial
entropy <- merge(entropy, summary)

#####################################
# summary
#####################################
length(unique(entropy$this_player))
entropy.summary <- entropy %>% group_by(this_player) %>% 
          summarise(avg_entropy5=mean(entropy5, na.rm=TRUE)) %>% ungroup()
View(entropy.summary[order(entropy.summary$avg_entropy5),])

# view min/max entropy excerpts
entropy <- entropy %>% group_by(this_player) %>%
            mutate(n_t = ntile(time,10)) %>% ungroup()
entropy.sum <- entropy %>% group_by(this_player, n_t) %>%
                summarise(avg_entropy10=mean(entropy10,na.rm=TRUE))
View(entropy.sum[order(entropy.sum$avg_entropy10),])

# view case studies
n7.entropy <- entropy %>% filter(this_player=='n7')
n7.onsets <- read.csv('Pipeline/onsets/t11.k7n7.n7.csv')
n7.onsets$note_norm <- 3*(n7.onsets$note-min(n7.onsets$note))/
                        (max(n7.onsets$note)-min(n7.onsets$note))

ggplot(n7.entropy,aes(x=time,y=entropy10)) +
  geom_line() +
  geom_point(data = n7.onsets, 
             aes(x=time,y=note_norm),
             shape='.') +
  ggtitle('CaseStudy: Tonal Entropy - n7')

# granger causality

######################################
# cross-correlation
######################################
cc_entropy <- NULL
for (person in unique(entropy$this_player)) {
  print(person)
  person <- entropy %>% filter(this_player==person)
  other <- entropy %>% filter(this_player==person$other_player[[1]])
  
  max.len = max(length(person$entropy10), length(other$entropy10))
  person_entropy = c(person$entropy10, rep(0, max.len-length(person$entropy10)))
  other_entropy = c(other$entropy10, rep(0, max.len-length(other$entropy10)))
  cc <- ccf(person_entropy, other_entropy, plot=FALSE, na.action=na.pass)
  cc_i <- data.frame(cc = cc$acf,
                     lag = cc$lag,
                     person = person$this_player[[1]],
                     condition = person$condition[[1]])
  cc_entropy <- rbind(cc_entropy, cc_i)
}

head(cc_entropy)
ggplot(cc_entropy, aes(x=cc,fill=condition,color=condition)) +
  geom_density(alpha=0.3) +
  theme_classic() +
  xlab("cross correlation") +
  ylab("frequency")
t.test(filter(cc_entropy,condition=="coupled")$cc,
       filter(cc_entropy,condition=="one-way")$cc)

cc.sum <- cc_entropy %>% group_by(condition) %>%
          summarise(mean_entropy=mean(cc,na.rm=TRUE),
                    se=sd(cc,na.rm=TRUE)/sqrt(n()))
ggplot(cc.sum,aes(x=condition,y=mean_entropy)) +
  geom_bar(stat="identity") +
  geom_linerange(aes(ymin=mean_entropy-se,
                     ymax=mean_entropy+se)) +
  theme_classic()+
  ylab("mean cross correlation of entropy")

entropy_lag <- cc_entropy %>% group_by(condition,lag) %>%
                summarize(avg_cc=mean(cc,na.rm=TRUE),
                          error_mean=sd(cc,na.rm=TRUE)/sqrt(n()))
entropy_lag$lag <- .5*entropy_lag$lag
ggplot(filter(entropy_lag,abs(lag)<=10), aes(x=lag,y=avg_cc)) +
  geom_bar(stat="identity",position="identity") +
  xlab("lag (seconds)") +
  ylab("mean cross correlation") +
  theme_classic() +
  facet_wrap(~condition)

# more cc at positive lags in one-way trials
cc_entropy.one_way <- cc_entropy %>% filter(condition=="one-way")
cc_entropy.one_way$direction <- ifelse(cc_entropy.one_way$lag>0,
                                       "ghost_leads",
                                       "live_leads")
cc_entropy.one_way <- cc_entropy.one_way %>% filter(lag!=0)
t.test(filter(cc_entropy.one_way,direction=="live_leads")$cc,
       filter(cc_entropy.one_way,direction=="ghost_leads")$cc)

######################################
# correlation between tonal entropy and onset density?
######################################
onsets <- NULL
inDir <- 'Pipeline/tonal-entropy/onset-density/'
infiles <- list.files(inDir)
for (f in infiles) {
  print(f)
  
  onsets_i <- read.csv(paste(inDir,f,sep=''))
  
  name <- str_replace(f,'-onsets.csv','')
  this_player <- str_split(name,"[.]")[[1]][[3]]
  onsets_i$this_player <- this_player
  onsets <- rbind(onsets, onsets_i)
}

entropy <- merge(entropy,onsets)
entropy.long <- entropy %>% gather(metric,value,-time,
                                   -this_player,-person_trial,-other_player,
                                   -condition)
entropy.long$window <- str_extract(entropy.long$metric,"[0-9]{1,2}$")
entropy.long$metric <- str_replace(entropy.long$metric,"[0-9]{1,2}$","")
entropy.long <- entropy.long %>% spread(metric,value)
entropy.long <- entropy.long %>% filter(!is.na(window))
ggplot(entropy.long, aes(x=onsets,y=entropy)) +
  geom_point(shape=".") +
  geom_smooth(method=lm) +
  facet_wrap(~window) +
  ggtitle("Onsets vs Entropy\nFaceted By Window Length (sec)")

entropy.long.10 <- entropy.long %>% filter(window==10)
lmodel <- lm(entropy ~ onsets, data=entropy.long.10)

# what happens when we divide by the number of note onsets?
entropy$normal_entropy10 <- entropy$entropy10/entropy$onsets10
ggplot(entropy, aes(x=onsets10, y=normal_entropy10)) +
  geom_point(shape='.')

entropy.sum <- entropy %>% group_by(this_player) %>%
                mutate(n_t=ntile(time,10))
entropy.sum <- entropy.sum %>% group_by(this_player,condition,n_t) %>%
                mutate(avg_norm_entropy10_trial=mean(normal_entropy10,na.rm=TRUE),
                       avg_entropy10_trial=mean(entropy10,na.rm=TRUE))
entropy.sum <- entropy.sum %>% ungroup() %>% group_by(condition,n_t) %>%
              summarise(avg_norm_entropy10=mean(avg_norm_entropy10_trial,na.rm=TRUE),
                        se = sd(avg_norm_entropy10_trial,na.rm=TRUE)/sqrt(n()),
                        avg_entropy10=mean(avg_entropy10_trial,na.rm=TRUE),
                        se_abs = sd(avg_entropy10_trial,na.rm=TRUE)/sqrt(n()))
ggplot(entropy.sum, aes(x=n_t,y=avg_norm_entropy10,color=condition,fill=condition)) +
  geom_line()+
  geom_point()

ggplot(entropy.sum, aes(x=n_t,y=avg_entropy10,color=condition,fill=condition)) +
  geom_line()+
  geom_point()

#####################################
# entropy over normalized time
#####################################

