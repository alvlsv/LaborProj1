

# Libraries ----
library(tidyverse)
library(locpol)
library(sampleSelection)

#Data ---- 
data <- read_csv("data/dataset.csv")


# Simple Mincer ----
simple_mincer <-
  lm(
  log(wage) ~ education + experience * education + I(experience ^ 2) * I(education ^ 2) , data |> filter(wage>0)
) 
simple_mincer |> summary()


data |> select(wage) 
  
# Correction for Selection  ---

## Participation model ----

data_mod <- data|> na.omit()|> mutate(wage_1= ifelse(wage==0, NA,wage))

heckit(has_wage ~ gender+age+,  log(wage_1) ~ experience , data_mod)




## Local Model ----
model <- locpol(y ~ x,
                d,
                deg = 1,
                xeval = c(1:9) / 10,
                kernel = gaussK)
model$lpFit
plot



library( "mvtnorm" )

nObs <- 1000
sigma <- matrix( c( 1, -0.7, -0.7, 1 ), ncol = 2 )
errorTerms <- rmvnorm( nObs, c( 0, 0 ), sigma )
myData <- data.frame( no = c( 1:nObs ), x1 = rnorm( nObs ), x2 = rnorm( nObs ),
                      u1 = errorTerms[ , 1 ], u2 =  errorTerms[ , 2 ] )
myData$y <- 2 + myData$x1 + myData$u1
myData$s <- ( 2 * myData$x1 + myData$x2 + myData$u2 - 0.2 ) > 0
myData$y[ !myData$s ] <- NA
myOls <- lm( y ~ x1, data = myData)
summary( myOls )
myHeckit <- heckit( s ~ x1 + x2, y ~ x1, myData, print.level = 1 )
summary( myHeckit )
