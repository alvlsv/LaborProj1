library(tidyverse)
library(survival)
library(muhaz)
library(haven)

dataset_raw <- read_dta("Project 2 data after stset.dta",encoding = "latin1")

write.csv(dataset_raw, file="data_raw.csv")

coxph



muhaz

kphaz.fit()
