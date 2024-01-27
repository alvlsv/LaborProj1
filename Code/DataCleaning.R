library(tidyverse)
library(locpol)
library(haven)


data_full <- read_dta("data/Round28WF.dta")



#region	регион (на уровне крупных городов/областей/районов) - вроде получше первого
#status	тип населенного пункта (город/село)
#x_educ	образование (уровень) -- старше 14
#x_age	возраст
#xh5	пол
#xh6	год рождения
#xi1	родился там, где живет (были ли переезды)
#xj10+xj40	Сколько заплатили чистыми за последние 30 дней
#xj32	Есть ли еще работа

choice <-
  data_full |>
  select(
    region,
    xh5,
    xi1,
    `_v23`,
    `_v176`,
    `_v145`,
    x_age,
    xj56,
    xm3,
    x_marst,
    `_v54`,
    xj73,
    xj63,
    xj62, 
    NCAT1,
    NCAT2,
    NCAT3,
    NCAT4,
    NCAT5,
    NCAT6,
    TINCM_N
  ) |>
  rename(
    wage_0 = `_v23`,
    education = `_v145`,
    experience = `_v176`,
    gender = xh5,
    age = x_age,
    relocated = xi1,
    other_work = xj56,
    health = xm3,
    marital_status = x_marst,
    now_in_school=`_v54`,
    pension = xj73,
    power_ladder= xj63,
    wealth_ladder=xj62,
    total_hh_income=TINCM_N,
  ) |>  
  mutate_at(vars(wage_0), ~replace(., is.na(.)| .>1e+07, 0)) |> rename(wage=wage_0) |> filter(now_in_school!=1| is.na(now_in_school))|> select(-now_in_school)|> mutate(has_wage = wage!=0)|> 
  mutate_if(~ >1e+07, NA)


write_csv(choice, "data/dataset.csv")

