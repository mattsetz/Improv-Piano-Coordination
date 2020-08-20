########################
# MS 5/7/19
# Analyze combined onset density
########################

library(tidyverse)


summary <- read.csv('summary-combined.csv')
inDir <- "Pipeline/onset-novelty/collapsed/combined/"
novelty <- NULL
infiles <- list.files(inDir)
for (f in infiles) {
  print(f)
  df <- read.csv(paste(inDir,f,sep=''))
  df$name <- str_replace(f,'-onset-novelty.csv','')
  novelty <- rbind(novelty, df)
}
novelty <- merge(summary,novelty)
novelty$name <- factor(novelty$name)

novelty.percentile <- novelty %>% group_by(name) %>% mutate(percentile_novelty20=ntile(novelty20,10)) %>% ungroup()
ggplot(filter(novelty.percentile,percentile_novelty20==10), 
       aes(x=novelty20,color=condition,fill=condition))+
  geom_density(alpha=0.3)
