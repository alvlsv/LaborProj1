library(tidyverse)
library(survival)
library(muhaz)
library(haven)

dataset_raw <- read_dta("Project 2 data after stset.dta", encoding = "latin1")

write.csv(dataset_raw, file = "data_raw.csv")



dataset_male <-
  dataset_raw |>
  filter(gender == 0) |>
  select(
    no_rab, # id
    date_rab,# origin
    `_t`, #time
    semejnoe,
    status, #type of non-employment (status before got registered) 
    ivdiwen,#number of dependents
    agecat,
    otn_zan,
    otrasl_s,
    p_avar,
    raion,
    status,
    obrzw,
    age,
    p_arch_N
  ) |>
  rename(
    id = no_rab,
    unemployed = unemp_d,
    got_employed = p_arch_N,
    marital_status = semejnoe,
    age_cat = agecat,
    time = `_t`,
    n_dependents = ivdiwen,
    employment_status = otn_zan,
    sector_of_last_emp = otrasl_s,
    chernobyl = p_avar,
    education = obrzw
  ) |> mutate(censored = 1 - got_employed)


Surv(dataset_male$time, 1-dataset_male$censored)|> na.omit()


muhaz

