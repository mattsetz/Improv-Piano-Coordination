library(tidyverse)

# read in zeroed dissonance data frame ####
diss0 <- read.csv('Pipeline/tonal/master-emergent-dissonance-zeroed.csv')
diss0 <- diss0 %>% group_by(pair,window) %>% mutate(n_time50=ntile(time,50),n_time100=ntile(time,100)) %>% ungroup()
head(diss)

# read in non-zeroed dissonance data frame ####
diss.combined <- read.csv('Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-combined.csv')
summary <- read.csv('summary-combined.csv')
diss.combined <- merge(diss.combined, summary)
diss.ind <- read.csv('Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-individual.csv')

diss <- NULL
for (p in unique(diss.combined$pair)) {
  print(p)
  p1 <- str_extract_all(p,'[a-z]{1,2}[0-9]')[[1]][[1]]
  p2 <- str_extract_all(p,'[a-z]{1,2}[0-9]')[[1]][[2]]
  
  p12.diss <- diss.combined %>% filter(pair==p) %>% 
    select(-max_dist) %>% rename(diss.comb=avg_dist)
  p1.diss <- diss.ind %>% filter(player==p1) %>% 
    select(-max_dist,-player,-other) %>% rename(diss.1=avg_dist)
  p2.diss <- diss.ind %>% filter(player==p2) %>% 
    select(-max_dist,-player,-other) %>% rename(diss.2=avg_dist)
  diss.p <- merge(p12.diss, p1.diss, all.x = TRUE)
  diss.p <- merge(diss.p, p2.diss, all.x = TRUE)
  diss.p$diss.emerge <- diss.p$diss.comb - ((diss.p$diss.1+diss.p$diss.2)/2)
  diss.p$diss.emerge2 <- diss.p$diss.comb - (diss.p$diss.1+diss.p$diss.2)
  
  diss <- rbind(diss, diss.p)
}

diss_comb <- read.csv("Pipeline/tonal/cloud-diameter/chew-2-11/master-cloud-diameter-combined.csv")


# Are diss0, diss and diss_comb the same? --------------------------------------------

head(diss0)
head(diss)
mean(filter(diss0,pair=="a2b2")$diss.emerge,na.rm = TRUE)
mean(filter(diss,pair=="a2b2")$diss.emerge,na.rm = TRUE)

c1f1_0 <- diss0 %>% filter(pair=="c1f1",window==2)
c1f1 <- diss %>% filter(pair=="c1f1",window==2)


# Emergent Consonance and Combined Consonance in coupled versus one-way trials -----------------------------------
W = 5
diss_avg <- diss0 %>% filter(window==W) %>% group_by(condition,yolked_id) %>% drop_na() %>%
  summarise(diss_comb = mean(diss.comb,na.rm = TRUE),
            diss_emerge = mean(diss.emerge,na.rm = TRUE),
            diss_ind = mean(diss.1+diss.2,na.rm = TRUE))

diss_wide <- diss_avg %>% pivot_wider(id_cols = yolked_id, names_from = condition,
                                      values_from = c("diss_comb","diss_emerge","diss_ind"))

t.test(-diss_wide$diss_emerge_coupled,-diss_wide$`diss_emerge_one-way`,paired = TRUE) # sig
t.test(-diss_wide$diss_comb_coupled,-diss_wide$`diss_comb_one-way`,paired = TRUE) # not sig

ggplot(diss_avg, aes(x=diss_comb,color=condition,fill=condition)) + geom_density(alpha=.2) + theme_bw()
var.test(filter(diss_avg,condition=="coupled")$diss_comb,
    filter(diss_avg,condition=="one-way")$diss_comb)

# Individual Consonance in coupled versus one-way trials ------------------

W = 5
diss_avg <- diss %>% filter(window==W) %>% group_by(condition,yolked_id) %>%
  summarise(diss_ind = mean(diss.1+diss.2,na.rm = TRUE))
diss_wide <- diss_avg %>% pivot_wider(id_cols = yolked_id, names_from = condition,
                                       values_from = diss_ind)
t.test(-diss_wide$coupled,-diss_wide$`one-way`,paired = TRUE) # not sig


# Variability of Combined Consonance across pieces condition -----------------------
W=5
diss_avg <- diss0 %>% filter(window==W) %>% group_by(pair,condition) %>% drop_na()
  summarise(diss_comb=mean(diss.comb,na.rm = TRUE)) %>% drop_na()
var.test(filter(diss_avg,condition=="coupled")$diss_comb,
         filter(diss_avg,condition=="one-way")$diss_comb)
ggplot(diss_avg, aes(x=diss_comb,color=condition,fill=condition)) + geom_density()
nrow(diss_avg)



# Variability of combined consonance within pieces ------------------------
W = 5
diss_var <- diss0 %>% group_by(pair,window,condition,yolked_id) %>% drop_na() %>%
  summarise(var_comb_cons = var(diss.comb)) %>% drop_na() %>% ungroup() %>% 
  group_by(window, condition, yolked_id) %>% summarise(var_comb_cons=mean(var_comb_cons))
  
diss_var_wide <- diss_var %>% pivot_wider(names_from = c("condition","window"),
                                          values_from = var_comb_cons)
View(diss_var_wide)
t.test(diss_var_wide$coupled_10,diss_var_wide$`one-way_10`,paired = TRUE)
t.test(diss_var_wide$coupled_10,diss_var_wide$`one-way_10`,paired = TRUE)



# 1/17/2020 start here ####

# pre-process data
diss <- read.csv("Pipeline/tonal/master-emergent-dissonance-zeroed.csv")
head(diss)
summary <- read.csv("summary-combined.csv")
diss <- merge(diss, summary)
diss <- diss %>% group_by(pair,window) %>% mutate(ntime50=ntile(time,50), ntime100=ntile(time,100))

# Emergent Consonance -----------------------------------------------------

# paired comparison
diss_avg <- diss %>% ungroup() %>% group_by(window,yolked_id,condition) %>% summarise(diss_cons=mean(1.8-diss.emerge,na.rm = TRUE))
diss_wide <- diss_avg %>% pivot_wider(names_from = c("window","condition"),values_from = diss_cons)
ggplot(diss_wide,aes(x=`2_coupled`-`2_one-way`)) + geom_histogram() + xlab("EC coupled minus EC oneway\nwithin yoked id") + theme_bw()
t.test(diss_wide$`2_coupled`,diss_wide$`2_one-way`,paired = TRUE)

# non-paired comparison
diss_avg <- diss %>% ungroup() %>% group_by(window,pair,condition) %>% summarise(diss_cons=mean(1.8-diss.emerge,na.rm = TRUE))
ggplot(diss_avg, aes(x=diss_cons,color=condition,fill=condition)) + geom_density(alpha=.2) + xlab("Emergent Consonance\nAverage Within Piece") + theme_bw() + facet_wrap(~window)
t.test(filter(diss_avg, condition=="coupled",window==5)$diss_cons,
       filter(diss_avg, condition=="one-way",window==5)$diss_cons,var.equal = TRUE)


# Combined Consonance -----------------------------------------------------

# paired comparison
diss_avg <- diss %>% ungroup() %>% group_by(window,yolked_id,condition) %>% summarise(comb_cons=mean(1.8-diss.comb,na.rm = TRUE))
diss_wide <- diss_avg %>% pivot_wider(names_from = c("window","condition"),values_from = comb_cons)
ggplot(diss_wide,aes(x=`2_coupled`-`2_one-way`)) + geom_histogram() + xlab("Combined Consonance coupled minus Combined Consonance oneway\nwithin yoked id") + theme_bw()
t.test(diss_wide$`5_coupled`,diss_wide$`5_one-way`,paired = TRUE)

# non-paired comparison
diss_avg <- diss %>% ungroup() %>% group_by(window,pair,condition) %>% summarise(diss_cons=mean(1.8-diss.emerge,na.rm = TRUE))
ggplot(diss_avg, aes(x=diss_cons,color=condition,fill=condition)) + geom_density(alpha=.2) + xlab("Emergent Consonance\nAverage Within Piece") + theme_bw() + facet_wrap(~window)
t.test(filter(diss_avg, condition=="coupled",window==5)$diss_cons,
       filter(diss_avg, condition=="one-way",window==5)$diss_cons,var.equal = TRUE)

# Individual Consonance -----------------------------------------------------

diss_ind_one_way <- diss %>% filter(condition=="one-way") %>% 
  select(pair,window,time,n_time20,ntime50,ntime100,diss.1) %>% rename(ind_diss=diss.1)


# paired comparison
diss_avg <- diss %>% ungroup() %>% group_by(window,yolked_id,condition) %>% summarise(cons.1=mean(1.8-diss.1,na.rm = TRUE),cons.2=mean(1.8-diss.2,na.rm = TRUE))
ggplot(filter(diss_avg,condition=="one-way"), aes(x=cons.2-cons.1)) + geom_histogram() + xlab("Individual Consonance\nLive minus Ghost Within Piece") + facet_wrap(~window) + theme_bw()


