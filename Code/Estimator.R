# Libraries ----
library(tidyverse)
library(locpol)
library(sampleSelection)
library(stargazer)
library(marginaleffects)
library(margins)
library(sandwich)
#Data ----
data_raw <- read_csv("data/dataset.csv")

data <- data_raw |> mutate(
  education_f = fct(as.character(diploma)),
  region = fct(as.character(region)),
  relocated = fct(as.character(relocated)),
  other_work = fct(as.character(other_work)),
  health_f = fct(as.character(health)),
  marital_status_f = fct(as.character(marital_status)),
  male = fct(as.character(gender))
)
sum(data$has_wage==1)




# Simple Mincer ----
dumb_mincer_1 <-
  lm(
    log(wage) ~ experience + education+I(experience^2)  ,
    data_raw |> filter(wage > 0)
  )

dumb_mincer_2 <-
  lm(
    log(wage) ~ experience * education + I(experience ^ 2) * I(education ^ 2) ,
    data_raw |> filter(wage > 0)
  )

dumb_mincer |> summary()
dumb_mincer |> avg_slopes()

mincer <-
  lm(log(wage) ~  -1+experience+I(experience^2) + education_f,
     data |> filter(wage > 0, education_f != 1))
mincer |> summary()






# Participation model ----

data_mod <-
  data |> mutate(wage_1 = ifelse(wage == 0, NA, wage),
                 log_wage = log(wage_1))


dumb_heckmod_1 <-
  heckit(
    has_wage ~  relocated+ marital_status + male + NCAT1 + NCAT2 + NCAT3 + NCAT4 + NCAT5 + NCAT6 + total_hh_income+ wealth_ladder + power_ladder + health ,
    log(wage_1) ~  experience + education+I(experience^2),
    data = data_mod
  )
dumb_heckmod_2 <-
  heckit(
    has_wage ~  age+relocated+marital_status + male + NCAT1 + NCAT2 + NCAT3 + NCAT4 + NCAT5 + NCAT6 + total_hh_income+ wealth_ladder + power_ladder + health ,
    log(wage_1) ~  experience * education+I(experience^2) * I(education^2),
    data = data_mod
  )

avg_slopes(dumb_heckmod_2)

dumb_heckmod_1 |> summary()

heckmod <-
  heckit(
    has_wage ~  age+relocated+marital_status + male + NCAT1 + NCAT2 + NCAT3 + NCAT4 + NCAT5 + NCAT6 + total_hh_income+ wealth_ladder + power_ladder + health,
    log(wage_1) ~ -1+ experience+I(experience^2) + education_f ,
    data = data_mod|> filter( education_f != 1)
  )
heckmod |> summary()

x<-summary(dumb_heckmod_1, vcov.=vcovHC(dumb_heckmod_1, "HC3"))
x$estimate

stargazer(dumb_mincer_1, dumb_heckmod_1, dumb_mincer_2, dumb_heckmod_2) 

stargazer(mincer, heckmod)

# Local Model ----

data_df <-
  data_mod |>
  select(log_wage, experience) |>
  as.data.frame() |> na.omit()

data_df_2 <-
  data_mod |>
  filter(education_f == 2) |>
  select(log_wage, experience) |>
  as.data.frame() |> na.omit()

bwth_2 <-
  thumbBw(data_df_2$experience, data_df_2$log_wage, 1, gaussK)


local_model_2 <-
  locpol(
    log_wage ~ experience,
    data_df_2,
    bw = bwth_2,
    kernel = gaussK,
    deg = 1
  )
confInterval(local_model_2)



data_df_3 <-
  data_mod  |>
  filter(education_f == 3) |>
  select(log_wage, experience) |>
  as.data.frame() |> na.omit()

bwth_3 <-
  thumbBw(data_df_3$experience, data_df_3$log_wage, 1, gaussK)

local_model_3 <-
  locpol(
    log_wage ~ experience,
    data_df_3,
    bw = bwth_3,
    kernel = gaussK,
    deg = 1
  )
confInterval(local_model_3)


data_df_4 <-
  data_mod |>
  filter(education_f == 4, male == 2) |>
  select(log_wage, experience) |>
  as.data.frame() |> na.omit()

bwth_4 <-
  thumbBw(data_df_4$experience, data_df_4$log_wage, 1, gaussK)

local_model_4 <-
  locpol(
    log_wage ~ experience,
    data_df_4,
    bw = bwth_4,
    kernel = gaussK,
    deg = 1
  )
confInterval(local_model_4)



data_df_5 <-
  data_mod |>
  filter(education_f == 5) |>
  select(log_wage, experience) |>
  as.data.frame() |>
  na.omit()

local_model_5 <-
  locpol(
    log_wage ~ experience,
    data_df_5,
    bw = bwth_5,
    kernel = gaussK,
    deg = 1
  )
confInterval(local_model_5)





data_df_6 <-
  data_mod |>
  filter(education_f == 6) |>
  select(log_wage, experience) |>
  as.data.frame() |>
  na.omit()
bwth_6 <-
  thumbBw(data_df_6$experience, data_df_6$log_wage, 1, gaussK)

local_model_6 <-
  locpol(
    log_wage ~ experience,
    data_df_6,
    bw = bwth_6,
    kernel = gaussK,
    deg = 1
  )
confInterval(local_model_6)


plotdata <-
  tibble(
    exp_2 = local_model_2$lpFit$experience,
    wage_2 = local_model_2$lpFit$log_wage,
    exp_3 = local_model_3$lpFit$experience,
    wage_3 = local_model_3$lpFit$log_wage,
    exp_4 = local_model_4$lpFit$experience,
    wage_4 = local_model_4$lpFit$log_wage,
    exp_5 = local_model_5$lpFit$experience,
    wage_5 = local_model_5$lpFit$log_wage,
    exp_6 = local_model_6$lpFit$experience,
    wage_6 = local_model_6$lpFit$log_wage
  ) |> filter_at(vars(starts_with("exp")),  ~ .x < 50)




nprm_plot_full <-
  ggplot(plotdata, aes(x = exp_6, y = wage_6, color = "6")) +
  geom_line() +
  geom_line(aes(x = exp_2, y = wage_2, color = "2")) +
  geom_line(aes(x = exp_3, y = wage_3, color = "3")) +
  geom_line(aes(x = exp_4, y = wage_4, color = "4")) +
  geom_line(aes(x = exp_5, y = wage_5, color = "5")) +
  theme_bw() + labs(color = "Education group",  x = "Experience (yrs)", y = "log(Wage)")


ggsave(
  "fullplot.pdf",
  path = "../Figures",
  width = 170,
  height = 105,
  units = "mm"
)

plotdata |> mutate(across(starts_with("exp")))


##


pred_2 <-
  locpol(
    log_wage ~ experience,
    data_df_2,
    bw = bwth_2,
    kernel = gaussK,
    xeval = 1:38,
    deg = 1
  )
pred_3 <-
  locpol(
    log_wage ~ experience,
    data_df_3,
    bw = bwth_3,
    kernel = gaussK,
    xeval = 1:40,
    deg = 1
  )
pred_4 <-
  locpol(
    log_wage ~ experience,
    data_df_4,
    bw = bwth_4,
    kernel = gaussK,
    xeval = 1:40,
    deg = 1
  )
pred_5 <-
  locpol(
    log_wage ~ experience,
    data_df_5,
    bw = bwth_5,
    kernel = gaussK,
    xeval = 1:40,
    deg = 1
  )
pred_6 <-
  locpol(
    log_wage ~ experience,
    data_df_6,
    bw = bwth_6,
    kernel = gaussK,
    xeval = 1:50,
    deg = 1
  )

library(rootSolve)

diff_2_4 <-
  function(r) {
    s1 = 0
    for (t in 1:length(pred_2$lpFit$log_wage)) {
      s1 <- s1 + (1 + r) ^ (-t - 7.5) * exp(pred_2$lpFit$log_wage)[t]
    }
    s2 = 0
    for (t in 1:length(pred_4$lpFit$log_wage)) {
      s2 <- s2 + (1 + r) ^ (-t - 11) * exp(pred_4$lpFit$log_wage)[t]
    }
    diff <- s1 - s2
    return(diff)
  }
r_2_4 <- uniroot.all(diff_2_4, c(-1, 1))

diff_4_5 <-
  function(r) {
    s1 = 0
    for (t in 1:length(pred_4$lpFit$log_wage)) {
      s1 <- s1 + (1 + r) ^ (-t - 11) * exp(pred_4$lpFit$log_wage)[t]
    }
    s2 = 0
    for (t in 1:length(pred_5$lpFit$log_wage)) {
      s2 <- s2 + (1 + r) ^ (-t - 13) * exp(pred_5$lpFit$log_wage)[t]
    }
    diff <- s1 - s2
    return(diff)
  }
r_4_5 <- uniroot.all(diff_4_5, c(-1, 1))








c(r_2_4, r_4_5, r_4_6)



diff_4_6 <-
  function(r) {
    s1 = 0
    for (t in 1:length(pred_4$lpFit$log_wage)) {
      s1 <- s1 + (1 + r) ^ (-t - 11) * exp(pred_4$lpFit$log_wage)[t]
    }
    s2 = 0
    for (t in 1:length(pred_6$lpFit$log_wage)) {
      s2 <- s2 + (1 + r) ^ (-t - 15) * exp(pred_6$lpFit$log_wage)[t]
    }
    diff <- s1 - s2
    return(diff)
  }
r_4_6 <- uniroot.all(diff_4_6, c(-1, 1))


c(r_2_4, r_4_5, r_4_6)


diff_2_3 <-
  function(r) {
    s1 = 0
    for (t in 1:length(pred_2$lpFit$log_wage)) {
      s1 <- s1 + (1 + r) ^ (-t - 7.5) * exp(pred_2$lpFit$log_wage)[t]
    }
    s2 = 0
    for (t in 1:length(pred_3$lpFit$log_wage)) {
      s2 <- s2 + (1 + r) ^ (-t - 7.5-2) * exp(pred_3$lpFit$log_wage)[t]
    }
    diff <- s1 - s2
    return(diff)
  }
r_2_3 <- uniroot.all(diff_2_3, c(-1, 1))


diff_5_6 <-
  function(r) {
    s1 = 0
    for (t in 1:length(pred_5$lpFit$log_wage)) {
      s1 <- s1 + (1 + r) ^ (-t - 13) * exp(pred_5$lpFit$log_wage)[t]
    }
    s2 = 0
    for (t in 1:length(pred_6$lpFit$log_wage)) {
      s2 <- s2 + (1 + r) ^ (-t - 15) * exp(pred_6$lpFit$log_wage)[t]
    }
    diff <- s1 - s2
    return(diff)
  }
r_5_6 <- uniroot.all(diff_5_6, c(-1, 1))

