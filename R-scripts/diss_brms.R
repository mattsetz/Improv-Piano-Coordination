library(brms)
library(tidyverse)

##################################
# Hypotheses:
# 1. emergent dissonance decreases over time within coupled pieces
# 2. emergent dissonance stays constant within one-way pieces
# 3. emergent dissonance decreases more in coupled versus one-way pieces
##################################


W = 5
# read-in data ####
diss <- read.csv('Pipeline/tonal/master-emergent-dissonance-zeroed.csv')
diss.summary <- diss %>% group_by(n_time10,pair,condition,window) %>%
                  summarise(diss.emerge=mean(diss.emerge,rm.na=TRUE)) %>% ungroup()
# uncoupled model ####
uncoupled.model <- brm(diss.emerge ~ n_time10 + (1+n_time10|pair),
                       data = filter(diss.summary,window==W,condition=="one-way"),
                       chains = 4, iter = 2000,
                       file = "emerge-diss.oneway.model")

# coupled model ####
coupled.model <- brm(diss.emerge ~ n_time10 + (1+n_time10|pair),
                     data = filter(diss,window==W,condition=="coupled"),
                     chains = 4, iter = 2000,
                     file = "emerge-diss.coupled.model")

# combined model ####
full.model <- brm(diss.emerge ~ condition*n_time10 + (1+n_time10|pair),
                  data = filter(diss.summary,window==W),
                  chains = 4, iter = 2000,
                  file = "emerge-diss.full.model")

summary(full.model)
plot(full.model)

# create dummy dataset ####
x = 1:1000
y = .8*x + rnorm(1000,0,5)
y2 = .5*x + rnorm(1000,0,5)
dat = data.frame(x=x,y=y,c="control")
dat = rbind(dat, data.frame(x=x,y=y2,c="manipulate"))

fit1 <- brm(formula = y ~ x,
            data = dat,
            prior = c(set_prior("normal(0,5)", class="b")),
            iter = 100, chains = 4)

summary(fit1, waic=TRUE)
hypothesis(fit1, "x > 0", class = "b")

plot(fit1)

# example w/ 2 conditions ####
x = 1:1000
y = .8*x + rnorm(1000,0,5)
y2 = .5*x + rnorm(1000,0,5)
dat = data.frame(x=x,y=y,c="control")
dat = rbind(dat, data.frame(x=x,y=y2,c="manipulate"))

fit2 <- brm(formula = y ~ x*c,
            data = dat,
            prior = c(set_prior("normal(0,5)", class="b")),
            iter = 100, chains = 3)
plot(fit2)
hypothesis(fit2, "x>0",class="b")
summary(fit2)


# example repeated measures w/ 2 conditions ####
B1 = .8
B2 = .5
df <- NULL
for (i in 1:10) {
  t = 1:100
  y = rnorm(1,B1,2)*t + rnorm(1000,0,5)
  df <- rbind(df, data.frame(t=t,y=y,c="1",id=as.character(i)))
}
for (i in 11:17) {
  t = 1:100
  y = rnorm(1,B2,2)*t + rnorm(1000,0,5)
  df <- rbind(df, data.frame(t=t,y=y,c="2",id=as.character(i)))
}

fit3 <- brm(y ~ t*c + (t|id),
            data = df,
            prior = c(set_prior("normal(0,5)",class="b")),
            iter = 200, chains = 3, cores = 3)
plot(fit3)

# running on our dataset ####
diss <- read.csv("Pipeline/tonal/master-emergent-dissonance-zeroed.csv")
fit.diss <- brm(diss.emerge ~ n_time10*condition + (1 + n_time10|pair),
                data = filter(diss,window==5),
                iter = 1000,
                chains = 4,
                cores = 4)
fit.diss
