library(tidyverse)

a1.onsets <- read.csv("Pipeline/rhythm/onset-density/total/individual/t1.a1b1.a1.csv")
a1.entropy <- read.csv("Pipeline/tonal/tonal-entropy/t1.a1b1.a1-entropy.csv")
a1.dissonance <- read.csv("Pipeline/tonal/cloud-diameter/individual/t1.a1b1.a1-cloud-diameter.csv")

a1.entropy <- a1.entropy %>% gather(window,entropy,-time)
a1.entropy$window <- str_replace(a1.entropy$window, "entropy", "")

### Cloud diameter vs tonal entropy
a1 <- merge(a1.entropy,a1.dissonance) %>% na.omit()
a1.2 <- a1 %>% filter(window==2)
a1.2 <- merge(a1.2,a1.onsets) %>% na.omit()

ggplot(a1.2, aes(x=entropy,y=avg_dist)) +
  geom_point()

### Cloud diameter vs onset density
a1.diss.2 <- a1.dissonance %>% filter(window==2) %>% select(-window,-max_dist)
a1.onsets <- a1.onsets %>% select(-avg_vel)
a1.diss.onset <- merge(a1.diss.2, select(a1.onsets,time,num_onsets),by.x = "time")
ggplot(a1.2, aes(x=num_onsets,y=avg_dist)) +
  geom_point()
