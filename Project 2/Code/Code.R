library(tidyverse)
library(survival)
library(muhaz)
library(haven)

dataset_raw <- read_dta("Project 2 data after stset.dta",encoding = "latin1")

write.csv(dataset_raw, file = "data_raw.csv")


dataset_male <-
  dataset_raw |>
  filter(gender == 0) |>
  select(no_rab, 
         date_rab, 
         god_roz, 
         semejnoe, 
         ivdiwen, 
         agecat, 
         otn_zan, 
         otrasl_s) |>
  na.omit() |>
  rename(id = no_rab,
         marital_status = semejnoe,
         birth_year = god_roz,
         age_cat = agecat,
         n_dependents = ivdiwen, 
         employment_status = otn_zan,
         sector_of_last_emp = otrasl_s,
         raion = 
         )

dataset_raw$raion |> hist()

muhaz

kphaz.fit()
