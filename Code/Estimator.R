

# Libraries ----
library(tidyverse)
library(locpol)
library(sampleSelection)

#Data ---- 
data <- read_csv("data/dataset.csv")


# Simple Mincer ----
lm(
  log(wage) ~ education + experience * education + I(experience ^ 2) * I(education ^ 2) , data |> filter(wage > 0, wage<1e+05)
) |> summary()


data |> select(wage) 
  
# Correction for Selection  ---

## Participation model ----

selection(has_wage~ education,  wage ~ experience+education, data)


## Local Model ----
model <- locpol(y ~ x,
                d,
                deg = 1,
                xeval = c(1:9) / 10,
                kernel = gaussK)
model$lpFit
plot
