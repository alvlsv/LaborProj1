


# Libraries ----
library(tidyverse)
library(locpol)
library(sampleSelection)
library(stargazer)
library(marginaleffects)
#Data ----
data_raw <- read_csv("data/dataset.csv")

data <- data_raw |> mutate(
  education_f = fct(as.character(diploma)),
  region = fct(as.character(region)),
  relocated = fct(as.character(relocated)),
  other_work = fct(as.character(other_work)),
  health_f = fct(as.character(health)),
  marital_status_f = fct(as.character(marital_status)),
  male = fct(as.character(gender))
)




# Simple Mincer ----
dumb_mincer <-
  lm(
    log(wage) ~ experience * education + I(experience ^ 2) * I(education ^ 2) ,
    data |> filter(wage > 0)
  )
dumb_mincer |> summary()
dumb_mincer |> avg_slopes()

simple_mincer <-
  lm(log(wage) ~ -1 + experience * education_f, data |> filter(wage > 0))
simple_mincer |> summary()





# Correction for Selection  ---

## Participation model ----

data_mod <- data |> mutate(wage_1 = ifelse(wage == 0, NA, wage))

heckmod <-
  heckit(
    has_wage ~  male + NCAT1 + NCAT2 + NCAT3 + NCAT4 + NCAT5 + NCAT6 + total_hh_income  + wealth_ladder + power_ladder + health+experience+education,
    log(wage_1) ~ -1 + experience * education_f ,
    data = data_mod
  )
heckmod |> summary()



stargazer(simple_mincer, heckmod)

## Local Model ----
model <- locpol(y ~ x,
                d,
                deg = 1,
                xeval = c(1:9) / 10,
                kernel = gaussK)
model$lpFit
plot



library("mvtnorm")

nObs <- 1000
sigma <- matrix(c(1,-0.7,-0.7, 1), ncol = 2)
errorTerms <- rmvnorm(nObs, c(0, 0), sigma)
myData <-
  data.frame(
    no = c(1:nObs),
    x1 = rnorm(nObs),
    x2 = rnorm(nObs),
    u1 = errorTerms[, 1],
    u2 =  errorTerms[, 2]
  )
myData$y <- 2 + myData$x1 + myData$u1
myData$s <- (2 * myData$x1 + myData$x2 + myData$u2 - 0.2) > 0
myData$y[!myData$s] <- NA
myOls <- lm(y ~ x1, data = myData)
summary(myOls)
myHeckit <- heckit(s ~ x1 + x2, y ~ x1, myData, print.level = 1)
summary(myHeckit)
