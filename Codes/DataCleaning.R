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
    `_v181`,
    `_v145`,
    `_v181`,
    xj85,
    x_age,
    xi2,
    xj56,
    xm80,
    xm3,
    xj1,
    xi3,
    xj2cod08,
    x_marst,
    `_v54`,
    xj90
  ) |>
  rename(
    wage_0 = `_v23`,
    education = `_v145`,
    experience = `_v176`,
    gender = xh5,
    unempl_wage = `_v181`,
    unempl_reg = xj85,
    age = x_age,
    relocated = xi1,
    other_work = xj56,
    drinking = xm80,
    health = xm3,
    born_in_city = xi3,
    republic = xi2,
    profession = xj2cod08,
    marital_status = x_marst,
    now_in_school=`_v54`,
    line = xj90
  ) |>  
  mutate_at(vars(wage_0), ~replace(., is.na(.)| .>10e+07, 0))|> rename(wage=wage_0) |> filter(now_in_school!=1)


write_csv(choice, "data/dataset.csv")
