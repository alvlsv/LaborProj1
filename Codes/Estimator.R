library(tidyverse)
library(locpol)


data <- read_csv("data/dataset.csv")
data_no_na <- data|> na.omit()|> filter(wage>0)

## Simple Mincer 
lm(log(wage) ~ education+experience*education+I(experience^2)*I(education^2),data_no_na)|> summary()



## Correction for Selection 

## Participation model 




data |> filter(`_v23`<1e+07)|> select(`_v23`)
data |> filter(xj10<1e+07)|> select(xj10) 


model<-locpol(y~x, d, deg=1, xeval=c(1:9)/10,  kernel = gaussK)
model$lpFit
plot
