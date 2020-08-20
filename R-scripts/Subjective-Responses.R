#################################
# Analyze subjective responses
#################################

library(tidyverse)

ratings <- read.csv('Participant-Data/subjective-ratings-other.csv')
ratings <- ratings %>% rename(player=person_trial,pair=personCombo,condition=Condition,
                              "High Quality"=High.Quality_Response,
                              "Well Coordinated"=Well.Coordinated_Response,
                              "Leader or Supporter"=Supporter_Response,
                              "Easy to Coordinate"=Easy.to.Collaborate_Response)

# visualize all responses
ratings_long <- ratings %>% select(player,condition,`Easy to Coordinate`,
                                   `High Quality`,`Well Coordinated`,
                                   `Leader or Supporter`) %>% 
  pivot_longer(cols = c("Easy to Coordinate","High Quality",
                        "Well Coordinated","Leader or Supporter"),
               names_to = "Question",values_to = "Response")

ggplot(ratings_long, aes(x=Response,fill=condition)) + geom_bar(position="dodge") + facet_wrap(~Question) + theme_bw() + theme(legend.position = "bottom")
  

# average per individual
ratings_long$subject <- str_extract(ratings_long$player,"[a-z]{1,2}")
ratings_long_avg <- ratings_long %>% group_by(subject,condition,Question) %>% 
  summarise(Response=mean(Response))
ggplot(ratings_long_avg, aes(x=round(Response,0),fill=condition)) + geom_bar(position="dodge") + facet_wrap(~Question) + theme_bw() + theme(legend.position = "bottom") + xlab("Mean Response Per Subject")


"
Claims to test:
(1-3) Participants rate coupled trials to be 
    (1) Higher Quality 
    (2) Better Coordinated 
    (3) Easier to Coordination
(4) Particiapnts feel they play more of a supporter role in one-way trials
"

# Sign test with mean response per subject   ----------------------------------------------------
ratings_avg <- ratings %>% group_by(Subject,condition) %>% summarise('Easy to Coordinate'=mean(`Easy to Coordinate`),
                                                                     'Well Coordinated'=mean(`Well Coordinated`),
                                                                     'High Quality'=mean(`High Quality`),
                                                                     'Leader or Supporter'=mean(`Leader or Supporter`)) %>%
  pivot_wider(names_from = condition, values_from = c('Easy to Coordinate','Well Coordinated',
                                                      'High Quality','Leader or Supporter')) %>% drop_na()


# Easy to Coordinate
ratings_avg$coord_ease = ratings_avg$`Easy to Coordinate_coupled`-ratings_avg$`Easy to Coordinate_one-way`
ratings_avg %>% filter(coord_ease>0) %>% nrow()
ratings_avg %>% nrow()
binom.test(24,26) 
"
	Exact binomial test

data:  24 and 26
number of successes = 24, number of trials = 26, p-value = 1.049e-05
alternative hypothesis: true probability of success is not equal to 0.5
95 percent confidence interval:
 0.7486971 0.9905446
sample estimates:
probability of success 
             0.9230769
"

# well coordinated
ratings_avg$well_coord = ratings_avg$`Well Coordinated_coupled`-ratings_avg$`Easy to Coordinate_one-way`
ratings_avg %>% filter(well_coord>0) %>% nrow()
ratings_avg %>% nrow()
binom.test(23,26)
"
	Exact binomial test

data:  23 and 26
number of successes = 23, number of trials = 26, p-value = 8.798e-05
alternative hypothesis: true probability of success is not equal to 0.5
95 percent confidence interval:
 0.6984596 0.9755419
sample estimates:
probability of success 
             0.8846154 
"

# high quality
ratings_avg$quality = ratings_avg$`High Quality_coupled`-ratings_avg$`High Quality_one-way`
ratings_avg %>% filter(quality>0) %>% nrow()
ratings_avg %>% nrow()
binom.test(21,26)
"
	Exact binomial test

data:  21 and 26
number of successes = 21, number of trials = 26, p-value = 0.002494
alternative hypothesis: true probability of success is not equal to 0.5
95 percent confidence interval:
 0.6064945 0.9344519
sample estimates:
probability of success 
             0.8076923
"

# leader/follower
ratings_avg$supporter = ratings_avg$`Leader or Supporter_coupled`-ratings_avg$`Leader or Supporter_one-way`
ratings_avg %>% filter(supporter<0) %>% nrow()
ratings_avg %>% nrow()
binom.test(18,26)

"
	Exact binomial test

data:  18 and 26
number of successes = 18, number of trials = 26, p-value = 0.07552
alternative hypothesis: true probability of success is not equal to 0.5
95 percent confidence interval:
 0.4821036 0.8567400
sample estimates:
probability of success 
             0.6923077
"

# leader follower with continuous paired t test
t.test(ratings_avg$`Leader or Supporter_coupled`,ratings_avg$`Leader or Supporter_one-way`,paired=TRUE)
"
	Paired t-test

data:  ratings_avg$`Leader or Supporter_coupled` and ratings_avg$`Leader or Supporter_one-way`
t = -3.1662, df = 25, p-value = 0.004036
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -1.1108956 -0.2352582
sample estimates:
mean of the differences 
             -0.6730769
"


#############################
# mixed effects model
#############################
library(lme4)
ratings.quality.model = lmer(e ~ Easty to Collaborate_OtherResponse +
                               
                          gender + (1|subject) + (1|scenario),
                        data=politeness, REML=FALSE)
ratings <- ratings %>% spread(key = Question, value = Response)


#############################
# visualize ratings
#############################
ratings <- read.csv('Participant-Data/subjective-ratings.csv')
ratings$Condition <- ifelse(ratings$Condition=="Real",'coupled',
                            'one-way')

ratings.summary <- ratings %>% group_by(Question,Condition) %>% 
                   summarise(avg_response = mean(Response),
                             sd_response = sd(Response)/sqrt(n()))
ggplot(filter(ratings.summary,Question!="Supporter"), aes(x=Question,y=avg_response,fill=Condition)) +
  geom_bar(stat="identity",position=position_dodge()) +
  geom_errorbar(aes(ymin=avg_response-sd_response,
                    ymax=avg_response+sd_response),
                width=.2,
                position=position_dodge(.9)) +
  ylab('Response') +
  theme_classic()

ggplot(filter(ratings,Question=="Supporter"), aes(x=Response,color=Condition,fill=Condition)) +
  geom_bar(position = position_dodge()) +
  theme_classic()

# summarize data
summary <- ratings %>% group_by(Subject, Trial) %>%
              summarize(condition=unique(Condition))
write.csv(summary, 'summary.csv', row.names = FALSE)


# Fit a mixed linear model
ratings.real <- ratings %>% filter(Condition=="Real")
head(ratings.real)
ratings.real <- ratings.real %>% spread(Question,Response)
