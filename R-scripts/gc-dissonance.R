library(tidyverse)

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
gc %>% group_by(condition,order,shuffled) %>% count() %>% View()

# visualize gc F values ####
head(gc)
ggplot(gc, aes(x=order_sec,y=f.val,color=direction)) + 
  stat_summary() + ggtitle("First Differencing")
ggplot(gc, aes(x=order_sec,y=f.val,color=direction)) + geom_point(alpha=.5)
ggplot(filter(gc,order_sec>2,order_sec<3), aes(x=f.val,color=direction,fill=direction))+geom_density(alpha=.2)


# analyze F vals ####
head(gc)
gc.oneway <- gc %>% filter(condition=="one-way",shuffled==FALSE)
f_diffs <- NULL
for (p in unique(filter(gc.oneway,direction=="ghost.live")$player)) {
  for (o in unique(gc.oneway$order_sec)) {
    ghost.live <- gc.oneway %>% filter(player==p,direction=="ghost.live",order_sec==o)
    live.ghost <- gc.oneway %>% filter(player==ghost.live$other[[1]],direction=="live.ghost",order_sec==o)
    f_diffs <- rbind(f_diffs, data.frame(f_diff=(ghost.live$f.val - live.ghost$f.val),
                                         order_sec=o))
    #f_diffs = c(f_diffs,(ghost.live$f.val - live.ghost$f.val)) 
  }
}
ggplot(filter(f_diffs,order_sec==2.4),aes(x=f_diff))+geom_histogram()
ggplot(f_diffs, aes(x=f_diff)) + geom_histogram() + facet_wrap(~order_sec)



# visualize gc p-values ####
gc.diss <- gc
gc.diss$is.sig <- gc.diss$p.val < 0.05
gc.diss <- gc.diss %>% group_by(direction,window,order_sec) %>% mutate(total=n()) %>% ungroup()
gc.diss.sig <- gc.diss %>% filter(is.sig) %>% group_by(direction,window,order_sec) %>% 
  summarise(total.sig=n()) %>% ungroup()
gc <- merge(gc.diss.sig,gc.diss)
gc <- gc %>% select(direction,window,order_sec,total.sig,total) %>% distinct()
gc$sig.ratio <- gc$total.sig/gc$total
View(gc)

ggplot(gc, aes(x=order_sec,y=100*sig.ratio,color=direction,fill=direction)) +
  geom_bar(position="dodge",stat="identity") + 
  xlab("order (sec)") +
  ylab("significance rate (%)") +
  theme_bw() + theme(legend.position="bottom")


# analyze gc p-values ####
head(gc)
gc.4.ghost.live <- gc %>% filter(order_sec==4.8)
y = rep(0,gc.4)
Ntotal = length(y)  # Compute the total number of flips.
dataList = list(    # Put the information into a list.
  y=y,
  Ntotal = Ntotal
)
modelString = "
model {
  for ( i in 1:Ntotal ) {
    y[i] ~ dbern( theta )  # likelihood
  }
  theta ~ dbeta(1,1) #prior
}
"
writeLines( modelString , con="GcSigmodel.txt" ) # write to file



# how does this correspond to listener ratings? ####
ratings <- read.csv('Participant-Data/subjective-ratings.csv')
head(ratings)
head(gc)
ratings

ratings$player <- paste(ratings$Subject,ratings$Trial,sep='')
ratings$condition <- ifelse(ratings$Condition=="Real","coupled","one-way")
ratings <- ratings %>% select(player,condition,Question,Response)
ratings.wide <- ratings %>% spread(Question,Response)
ggplot(ratings.wide, aes(x=Supporter,color=condition,fill=condition))+
  geom_bar(position="dodge")


# analyze coupled trials
gc.ratings <- merge(gc,ratings.wide)
gc.ratings$is_sig <- gc.ratings$p.val < 0.05
ggplot(filter(gc.ratings,direction=="live.live",order_sec==3.6), 
       aes(x=Supporter,color=is_sig,fill=is_sig)) +
  geom_bar(position="dodge") +
  xlab("Rating: Leader - - Follower") +
  ylab("num significant \nGC player -> other") +
  ggtitle("coupled. order=3.6(sec)")

# analyze one-way trials
gc.reverse <- gc
gc.reverse$player <- gc$other
gc.reverse$other <- gc$player
gc.ratings <- merge(gc.reverse, ratings.wide)
gc.ratings$is_sig <- gc.ratings$p.val < 0.05
ggplot(filter(gc.ratings,direction=="ghost.live",order_sec<10), 
       aes(x=`Easy to Collaborate`,color=is_sig,fill=is_sig)) +
  geom_bar(position="dodge") +
  xlab("Live Rating: Leader - - Follower") +
  ylab("num significant \nGC ghost -> live") +
  ggtitle("one-way")
