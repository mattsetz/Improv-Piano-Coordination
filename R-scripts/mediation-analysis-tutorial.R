library(tidyverse)
library(mediation)

# MS 12/30/19
# Toy mediation analysis


# Generate toy data -------------------------------------------------------
# Case 1: Dissonance of Live Player is predicted by Density of Ghost Player, mediated by Dissonance of Ghost Player ----
density_ghost <- rnorm(1000)
diss_ghost <- density_ghost + rnorm(1000,sd = .3)
diss_live <- diss_ghost + rnorm(1000,sd=.3)
df1 <- data.frame("density_ghost"=density_ghost, "diss_ghost"= diss_ghost, "diss_live"=diss_live)
write.csv(df1,"toy-mediation-data/mediation.csv",row.names = FALSE)

# Case 2: Dissonance of Live Player is predicted by Density of Ghost Player, NOT MEDIATED by Dissonance of Ghost Player
density_ghost <- rnorm(1000)
diss_ghost <- density_ghost + rnorm(1000,sd = .3)
diss_live <- density_ghost + rnorm(1000,sd=.3)
df1 <- data.frame("density_ghost"=density_ghost, "diss_ghost"= diss_ghost, "diss_live"=diss_live)
write.csv(df1,"toy-mediation-data/no-mediation.csv",row.names = FALSE)

# Case 3: Mediation effect in lagged Time Series Data
density_ghost <- rnorm(1000)
diss_ghost <- density_ghost + rnorm(1000,sd = .3)
diss_live <- c(rnorm(10), diss_ghost[1:990]+rnorm(990,sd=.3))
df1 <- data.frame("density_ghost"=density_ghost, "diss_ghost"= diss_ghost, "diss_live"=diss_live)
write.csv(df1,"toy-mediation-data/mediation-ts.csv",row.names = FALSE)

# Case 4: No Mediation effect in lagged Time Series Data
density_ghost <- rnorm(1000)
diss_ghost <- density_ghost + rnorm(1000,sd = .3)
diss_live <- c(rnorm(10), density_ghost[1:990]+rnorm(990,sd=.3))
df1 <- data.frame("density_ghost"=density_ghost, "diss_ghost"= diss_ghost, "diss_live"=diss_live)
write.csv(df1,"toy-mediation-data/no-mediation-ts.csv",row.names = FALSE)

# Case 1: Dissonance of Live Player is predicted by Density of Ghost Player, mediated by Dissonance of Ghost Player ----

df1 <- data.frame("density_ghost"=density_ghost, "diss_ghost"= diss_ghost, "diss_live"=diss_live)

fit.totaleffect <- lm(diss_live ~ density_ghost, df1)
fit.mediator <- lm(diss_ghost ~ density_ghost, df1)
fit.diss_live <- lm(diss_live ~ diss_ghost + density_ghost, df1)
results = mediate(fit.mediator, fit.diss_live, treat='density_ghost', mediator='diss_ghost')
summary(results)

# Case 2: Dissonance of Live Player is predicted by Density of Ghost Player, NOT MEDIATED by Dissonance of Ghost Playerdensity_ghost <- rnorm(200) ----
density_ghost <- rnorm(200)
diss_ghost <- density_ghost + rnorm(200,sd = .3)
diss_live <- density_ghost + rnorm(200,sd=.3)
df2 <- data.frame("density_ghost"=density_ghost, "diss_ghost"= diss_ghost, "diss_live"=diss_live)

fit.totaleffect <- lm(diss_live ~ density_ghost, df2)
fit.mediator <- lm(diss_ghost ~ density_ghost, df2)
fit.diss_live <- lm(diss_live ~ diss_ghost + density_ghost, df2)
results = mediate(fit.mediator, fit.diss_live, treat='density_ghost', mediator='diss_ghost')
summary(results)

# Case 3: Mediation effect in lagged Time Series Data ----
df1 <- read.csv("toy-mediation-data/mediation-ts.csv")

library(data.table)
fit.mediator <- lm(diss_ghost ~ density_ghost, data = df1)
for (l in seq(-12,12,2)) {
  print(lag)
  df1$diss_live_lead <- shift(df1$diss_live, l)
  fit.diss_player <- lm(diss_live_lead ~ diss_ghost + density_ghost, data = df1)
  results = mediate(fit.mediator, fit.diss_player, treat = 'density_ghost', mediator = 'diss_ghost', dropobs = TRUE)
                    
  save(results, file=paste("Pipeline/mediation-analysis-toy/tonal-mediation-lag",as.character(l),".rds",sep=""))
}

# analyze mediation results
files = list.files("Pipeline/mediation-analysis-toy")
mediation_results <- NULL
for (f in files) {
  lag = str_extract(f,"lag-{0,1}[0-9]{1,2}")
  print(f)
  results = get(load(paste("Pipeline/mediation-analysis-toy/",f,sep="")))
  df <- data.frame("acme_est" = results$d0, 
                   "acme_ci_lower" = results$d0.ci[1][[1]],
                   "acme_ci_upper" = results$d0.ci[2][[1]],
                   "acme_p_val" = results$d0.p,
                   "ade_est" = results$z0,
                   "ade_ci_lower" = results$z0.ci[1][[1]],
                   "ade_ci_upper" = results$z0.ci[2][[1]],
                   "ade_p_val" = results$z0.p,
                   "lag"=lag)
  mediation_results <- rbind(df, mediation_results)
}

View(mediation_results)

"
In this simulated data we should expect significant mediation (ACME) at a lag of 10 but no other lags.
As desired, we do see sig mediation at lag of 10. But we also see sig mediation at lags of 0 and 20. This is weird
and concerning. Not sure why this is the case. Are these simply false positives? It gives me some distrust in the analysis.
"

# Case 4: No Mediation effect in lagged Time Series Data ----