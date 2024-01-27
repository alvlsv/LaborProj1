library(tidyverse)
library(locpol)
library(haven)


data <- read_dta("data/Round28WF.dta")


data$`_v170`[data$`_v170` < 1e+05] |> na.omit() |> hist()

data$xj81[data$xj81 < 1e+05] |> hist()



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
  data |> 
  select(region, 
         xh5, 
         xi1, 
         `_v23`, 
         `_v176`, 
         `_v181`, 
         `_v145`) |> 
  mutate(wage = `_v23`, 
         education = `_v145`, 
         experience = `_v176`,
         has_wage = is.na(wage)*(wage==0)*1)

write_csv(choice, "data/dataset.csv")
choice$education
