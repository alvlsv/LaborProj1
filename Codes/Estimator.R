

# Libraries ----
library(tidyverse)
library(locpol)

#datafull<-
data <- read_csv("data/dataset.csv")
data_no_na <- data |> na.omit() |> filter(wage > 0)



# Simple Mincer ----
lm(
  log(wage) ~ education + experience * education + I(experience ^ 2) * I(education ^ 2) + region +,
  data_no_na
) |> summary()



# Correction for Selection  ---

## Participation model ----
participation_model <-
  glm(has_wage ~ region + xi1, data, family = binomial(link = "logit"))



## Local Model ----
model <- locpol(y ~ x,
                d,
                deg = 1,
                xeval = c(1:9) / 10,
                kernel = gaussK)
model$lpFit
plot
