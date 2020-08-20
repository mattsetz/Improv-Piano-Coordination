library(tidyverse)

diss_ind <- read.csv("Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-individual.csv")
summary <- read.csv('summary-individual.csv')
diss_ind <- merge(summary,diss_ind)
diss_ind <- diss_ind %>% group_by(window,player) %>% mutate(ntime10=ntile(time,10),ntime20=ntile(time,20),ntime50=ntile(time,50),ntime100=ntile(time,100)) %>% ungroup()


# Is individual consonance higher in coupled trials? ----------------------

ggplot(diss_ind, aes(x=1.8-avg_dist,color=condition,fill=condition)) + geom_density(alpha=.2) +facet_wrap(~window)
diss_avg <- diss_ind %>% group_by(player,condition,window) %>% summarise(diss=mean(avg_dist,na.rm = TRUE))
ggplot(filter(diss_avg,window<10), aes(x=1.8-diss,color=condition,fill=condition)) + geom_density(alpha=.2) + xlab("Mean Individual Consonance Per Piece") + theme_bw()+theme(legend.position="bottom")+facet_wrap(~window)
t.test(filter(diss_avg,condition=="coupled",window==10)$diss,filter(diss_avg,condition=="one-way",window==10)$diss)

"
From the above distributions and t-tests, it appears there is no significant difference in individual
consonance as a function of condition. This is a bit confusing in light of the emergent consonance results though. Given
that there is higher emergent consonance in coupled trials, but no effect on combined consonance, shouldn't it be the 
case that there is no significant difference in Emergent Consonance?
"


# Visualize consonance over time ------------------------------------------

diss.avg <- diss_ind %>% group_by(window,ntime20,player,condition) %>% summarise(diss=mean(avg_dist,na.rm = TRUE)) %>% ungroup()
ggplot(filter(diss.avg,window==5), aes(x=5*ntime20,y=1.8-diss,color=condition,fill=condition)) + stat_summary() + ylab("Individual Consonance") + xlab("normalized time (%)") + theme_bw() + theme(legend.position = c(.75,.75))

diss.avg <- diss_ind %>% filter(window==5) %>% group_by(ntime50,player,condition) %>% summarise(diss=mean(avg_dist,na.rm = TRUE)) %>% ungroup()
ggplot(diss.avg, aes(x=2*ntime50,y=1.8-diss,color=condition,fill=condition)) + stat_summary() + ylab("Consonance") + xlab("normalized time (%)")

ggplot(summarise(group_by(diss,player,condition),diss=mean(avg_dist,na.rm = TRUE)), aes(x=1.8-diss,color=condition,fill=condition)) + geom_density(alpha=.2) + xlab("consonance")
