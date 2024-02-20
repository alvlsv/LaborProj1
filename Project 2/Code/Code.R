library(tidyverse)
library(survival)
library(muhaz)
library(haven)
library(stargazer)
library(ggsurvfit)
library(survminer)


dataset_raw <-
  read_dta("Project 2 data after stset.dta", encoding = "latin1")

#write.csv(dataset_raw, file = "data_raw.csv")

plot_width = 220 / 1.4
plot_height = 140 / 1.4

dataset_male_raw <-
  dataset_raw |>
  filter(gender == 0) |>
  select(
    no_rab,
    # id
    date_rab,
    # origin
    `_t`,
    #time
    semejnoe,
    # 1 – married; 2 – not married; 3 – divorced/widow
    status,
    #type of non-employment (status before got registered) 1 – redundant; 2 – lost job; 3 – long-term not employed; 4 – never worked before
    ivdiwen,
    #number of dependents
    agecat,
    otrasl_s,
    p_avar,
    obrzw,
    age,
    p_arch_N,
    n_zarpl,
    t_prof,
    n_emp,
    strong,
    hwage,
    hherfin,
    o_stav
  ) |>
  rename(
    id = no_rab,
    got_employed = p_arch_N,
    marital_status = semejnoe,
    age_cat = agecat,
    spell = `_t`,
    n_dependents = ivdiwen,
    sector_of_last_emp = otrasl_s,
    chernobyl = p_avar,
    education = obrzw,
    desired_wage =n_zarpl,
    change_in_prof = t_prof,
    number_of_jobs = n_emp,
    high_income = strong,
    #dummy for regions with higher than average per capita income
    high_wage = hwage ,
    high_concentration = hherfin,
    general_tenure = o_stav
  ) |>
  mutate(
    censored = 1 - got_employed,
    marital_status = factor(marital_status),
    age_cat = factor(age_cat),
    education = factor(education),
    sector_of_last_emp = factor(sector_of_last_emp)
  )

dataset_male <-
  right_join(
    dataset_male_raw |>
      filter(is.na(spell)),
    dataset_male_raw |>
      select(id, spell, censored, got_employed) |>
      filter(!is.na(spell)),
    by = join_by(id == id)
  ) |>
  select(-spell.x,-censored.x,-got_employed.x) |>
  rename(spell = spell.y,
         censored = censored.y) |> filter(education != 7)




Surv(dataset_male$spell, dataset_male$censored)




all_risk <-
  survfit2(Surv(spell, censored) ~ 1, data = dataset_male) |>
  ggsurvfit() +
  labs(x = "Unemployment duration (weeks)") +
  add_confidence_interval() +
  scale_ggsurvfit() +
  xlim(NA, 200)

ggsave(
  "all_risk_plot.pdf",
  path = "../Figures",
  all_risk,
  width = plot_width,
  height = plot_height,
  units = "mm"
)


all_haz <-
  epiR::epi.insthaz(survfit2(Surv(spell, censored) ~ 1, data = dataset_male)) %>%
  ggplot(aes(x = time, y = hest)) +
  geom_smooth(
    method = "loess",
    formula = "y ~ x",
    alpha = 0.2,
    linewidth = 0.6,
    se = F
  ) +
  labs(
    title = "",
    x = "Unemployment duration (weeks)",
    y = "Instantaneous Hazard",
    color = "",
    fill = ""
  ) +
  theme_ggsurvfit_default() +
  scale_ggsurvfit() +
  xlim(NA, 200)


ggsave(
  "all_hazard_plot.pdf",
  path = "../Figures",
  all_haz,
  width = plot_width,
  height = plot_height,
  units = "mm"
)



names_status <-
  c('Redundant',
    'Lost job',
    'Long-term not employed',
    'Never worked before ')


status_risk <-
  survfit2(Surv(spell, censored) ~ status, data = dataset_male) |>
  ggsurvfit() +
  labs(x = "Unemployment duration (weeks)") +
  add_confidence_interval() +
  scale_color_discrete(labels = names_status) +
  scale_fill_discrete(labels = names_status) +
  scale_ggsurvfit() +
  xlim(NA, 200)

ggsave(
  "status_risk_plot.pdf",
  path = "../Figures",
  status_risk,
  width = plot_width,
  height = plot_height,
  units = "mm"
)


status_haz <-
  epiR::epi.insthaz(survfit2(Surv(spell, censored) ~ status, data = dataset_male)) %>%
  ggplot(aes(
    x = time,
    y = hest,
    color = strata,
    fill = strata
  )) +
  geom_smooth(
    method = "loess",
    formula = "y ~ x",
    alpha = 0.2,
    linewidth = 0.6,
    se = F
  ) +
  labs(
    title = "",
    x = "Unemployment duration (weeks)",
    y = "Instantaneous Hazard",
    color = "",
    fill = ""
  ) +
  scale_color_discrete(labels = names_status) +
  scale_fill_discrete(labels = names_status) +
  theme_ggsurvfit_default() +
  scale_ggsurvfit() +
  xlim(NA, 200)

ggsave(
  "status_haz_plot.pdf",
  path = "../Figures",
  status_haz,
  width = plot_width,
  height = plot_height,
  units = "mm"
)



names_education <-
  c(
    "9 Years",
    "General secondary",
    "Junior Prof",
    "Secondary Prof",
    "University",
    "Post-Grad"
  )

educ_risk <-
  survfit2(Surv(spell, censored) ~ education,
           data = dataset_male |> filter(education != 7)) |>
  ggsurvfit() +
  labs(x = "Unemployment duration (weeks)") +
  add_confidence_interval() +
  scale_color_discrete(labels = names_education) +
  scale_fill_discrete(labels = names_education) +
  scale_ggsurvfit() +
  xlim(NA, 200)


ggsave(
  "educ_risk_plot.pdf",
  path = "../Figures",
  educ_risk,
  width = plot_width,
  height = plot_height,
  units = "mm"
)


educ_haz <-
  epiR::epi.insthaz(survfit2(Surv(spell, censored) ~ education, data = dataset_male)) %>%
  ggplot(aes(
    x = time,
    y = hest,
    color = strata,
    fill = strata
  )) +
  geom_smooth(
    method = "loess",
    formula = "y ~ x",
    alpha = 0.2,
    linewidth = 0.6,
    se = F
  ) +
  labs(
    title = "",
    x = "Unemployment duration (weeks)",
    y = "Instantaneous Hazard",
    color = "",
    fill = ""
  ) +
  scale_color_discrete(labels = names_education) +
  scale_fill_discrete(labels = names_education) +
  theme_ggsurvfit_default() +
  scale_ggsurvfit() +
  xlim(NA, 200)


ggsave(
  "educ_haz_plot.pdf",
  path = "../Figures",
  educ_haz,
  width = plot_width,
  height = plot_height,
  units = "mm"
)




chern_risk <-
  survfit2(Surv(spell, censored) ~ chernobyl,
           data = dataset_male) |>
  ggsurvfit() +
  labs(x = "Unemployment duration (weeks)") +
  add_confidence_interval() +
  scale_color_discrete(labels = c("Chernobyl victim status", "Others")) +
  scale_fill_discrete(labels = c("Chernobyl victim status", "Others")) +  scale_ggsurvfit() +
  xlim(NA, 200)


ggsave(
  "chern_risk_plot.pdf",
  path = "../Figures",
  chern_risk,
  width = plot_width,
  height = plot_height,
  units = "mm"
)



chern_haz <-
  epiR::epi.insthaz(survfit2(Surv(spell, censored) ~ chernobyl,
                             data = dataset_male)) |>
  ggplot(aes(
    x = time,
    y = hest,
    color = strata,
    fill = strata
  )) +
  geom_smooth(
    method = "loess",
    formula = "y ~ x",
    alpha = 0.2,
    linewidth = 0.6,
    se = F
  ) +
  labs(
    title = "",
    x = "Unemployment duration (weeks)",
    y = "Instantaneous Hazard",
    color = "",
    fill = ""
  ) +
  scale_color_discrete(labels = c("Chernobyl victim status", "Others")) +
  scale_fill_discrete(labels = c("Chernobyl victim status", "Others")) +
  theme_ggsurvfit_default() +
  scale_ggsurvfit() +
  xlim(NA, 200)


ggsave(
  "chern_haz_plot.pdf",
  path = "../Figures",
  chern_haz,
  width = plot_width,
  height = plot_height,
  units = "mm"
)



change_risk <-
  survfit2(Surv(spell, censored) ~ change_in_prof ,
           data = dataset_male) |>
  ggsurvfit() +
  labs(x = "Unemployment duration (weeks)") +
  scale_ggsurvfit()  +
  add_confidence_interval() +
  scale_color_discrete(labels = c("Profession unchanged", "Profession changed")) +
  scale_fill_discrete(labels = c("Profession unchanged", "Profession changed")) +
  theme_ggsurvfit_default() +
  scale_ggsurvfit() +
  xlim(NA, 200)




ggsave(
  "change_risk_plot.pdf",
  path = "../Figures",
  change_risk,
  width = plot_width,
  height = plot_height,
  units = "mm"
)


change_haz <-
  epiR::epi.insthaz(survfit2(Surv(spell, censored) ~ change_in_prof,
                             data = dataset_male)) |>
  ggplot(aes(
    x = time,
    y = hest,
    color = strata,
    fill = strata
  )) +
  geom_smooth(
    method = "loess",
    formula = "y ~ x",
    alpha = 0.2,
    linewidth = 0.6,
    se = F
  ) +
  labs(
    title = "",
    x = "Unemployment duration (weeks)",
    y = "Instantaneous Hazard",
    color = "",
    fill = ""
  ) +
  scale_color_discrete(labels = c("Profession unchanged", "Profession changed")) +
  scale_fill_discrete(labels = c("Profession unchanged", "Profession changed")) +
  theme_ggsurvfit_default() +
  scale_ggsurvfit() +
  xlim(NA, 200)


ggsave(
  "change_haz_plot.pdf",
  path = "../Figures",
  change_haz,
  width = plot_width,
  height = plot_height,
  units = "mm"
)







names_marital_status <-
  c("Married",
    "Not Married",
    "Divorced/Widowed")


marital_risk <-
  survfit2(Surv(spell, censored) ~ marital_status, data = dataset_male) |>
  ggsurvfit() +
  labs(x = "Unemployment duration (weeks)") +
  add_confidence_interval() +
  scale_color_discrete(labels = names_marital_status) +
  scale_fill_discrete(labels = names_marital_status) +
  scale_ggsurvfit() +
  xlim(NA, 200)

ggsave(
  "marital_risk_plot.pdf",
  path = "../Figures",
  marital_risk,
  width = plot_width,
  height = plot_height,
  units = "mm"
)


marital_haz <-
  epiR::epi.insthaz(survfit2(Surv(spell, censored) ~ marital_status, data = dataset_male)) %>%
  ggplot(aes(
    x = time,
    y = hest,
    color = strata,
    fill = strata
  )) +
  geom_smooth(
    method = "loess",
    formula = "y ~ x",
    alpha = 0.2,
    linewidth = 0.6,
    se = F
  ) +
  labs(
    title = "",
    x = "Unemployment duration (weeks)",
    y = "Instantaneous Hazard",
    color = "",
    fill = ""
  ) +
  scale_color_discrete(labels = names_marital_status) +
  scale_fill_discrete(labels = names_marital_status) +
  theme_ggsurvfit_default() +
  scale_ggsurvfit() +
  xlim(NA, 200)

ggsave(
  "marital_haz_plot.pdf",
  path = "../Figures",
  marital_haz,
  width = plot_width,
  height = plot_height,
  units = "mm"
)




city_income_risk <-
  survfit2(Surv(spell, censored) ~ high_income, data = dataset_male) |>
  ggsurvfit() +
  labs(x = "Unemployment duration (weeks)") +
  add_confidence_interval() +
  scale_color_discrete(labels = c("Region with higher than avg pc income", "Region with higher than avg pc income")) +
  scale_fill_discrete(labels = c("Region with higher than avg pc income", "Region with higher than avg pc income")) +
  scale_ggsurvfit() +
  xlim(NA, 200)

ggsave(
  "city_income_risk_plot.pdf",
  path = "../Figures",
  city_income_risk,
  width = plot_width,
  height = plot_height,
  units = "mm"
)


city_income_haz <-
  epiR::epi.insthaz(survfit2(Surv(spell, censored) ~ high_income, data = dataset_male)) %>%
  ggplot(aes(
    x = time,
    y = hest,
    color = strata,
    fill = strata
  )) +
  geom_smooth(
    method = "loess",
    formula = "y ~ x",
    alpha = 0.2,
    linewidth = 0.6,
    se = F
  ) +
  labs(
    title = "",
    x = "Unemployment duration (weeks)",
    y = "Instantaneous Hazard",
    color = "",
    fill = ""
  ) +
  scale_color_discrete(labels = c("Region with higher than avg pc income", "Region with higher than avg pc income")) +
  scale_fill_discrete(labels = c("Region with higher than avg pc income", "Region with higher than avg pc income")) +
  theme_ggsurvfit_default() +
  scale_ggsurvfit() +
  xlim(NA, 200)

ggsave(
  "city_income_haz_plot.pdf",
  path = "../Figures",
  city_income_haz,
  width = plot_width,
  height = plot_height,
  units = "mm"
)




city_wage_risk <-
  survfit2(Surv(spell, censored) ~ high_wage, data = dataset_male) |>
  ggsurvfit() +
  labs(x = "Unemployment duration (weeks)") +
  add_confidence_interval() +
  scale_color_discrete(labels = c("Region with higher than avg wage", "Region with higher than avg wage")) +
  scale_fill_discrete(labels = c("Region with higher than avg wage", "Region with higher than avg wage")) +
  scale_ggsurvfit() +
  xlim(NA, 200)

ggsave(
  "city_wage_risk_plot.pdf",
  path = "../Figures",
  city_wage_risk,
  width = plot_width,
  height = plot_height,
  units = "mm"
)


city_wage_haz <-
  epiR::epi.insthaz(survfit2(Surv(spell, censored) ~ high_wage, data = dataset_male)) %>%
  ggplot(aes(
    x = time,
    y = hest,
    color = strata,
    fill = strata
  )) +
  geom_smooth(
    method = "loess",
    formula = "y ~ x",
    alpha = 0.2,
    linewidth = 0.6,
    se = F
  ) +
  labs(
    title = "",
    x = "Unemployment duration (weeks)",
    y = "Instantaneous Hazard",
    color = "",
    fill = ""
  ) +
  scale_color_discrete(labels =c("Region with higher than avg wage", "Region with higher than avg wage")) +
  scale_fill_discrete(labels = c("Region with higher than avg wage", "Region with higher than avg wage")) +
  theme_ggsurvfit_default() +
  scale_ggsurvfit() +
  xlim(NA, 200)

ggsave(
  "city_wage_haz_plot.pdf",
  path = "../Figures",
  city_wage_haz,
  width = plot_width,
  height = plot_height,
  units = "mm"
)




city_conc_risk <-
  survfit2(Surv(spell, censored) ~ high_concentration, data = dataset_male) |>
  ggsurvfit() +
  labs(x = "Unemployment duration (weeks)") +
  add_confidence_interval() +
  scale_color_discrete(labels = c("Region with higher than avg concentration ratio", "Region with higher than avg concentration ratio")) +
  scale_fill_discrete(labels = c("Region with higher than avg concentration ratio", "Region with higher than avg concentration ratio")) +
  scale_ggsurvfit() +
  xlim(NA, 200)

ggsave(
  "city_conc_risk_plot.pdf",
  path = "../Figures",
  city_conc_risk,
  width = plot_width,
  height = plot_height,
  units = "mm"
)


city_conc_haz <-
  epiR::epi.insthaz(survfit2(Surv(spell, censored) ~ high_concentration, data = dataset_male)) %>%
  ggplot(aes(
    x = time,
    y = hest,
    color = strata,
    fill = strata
  )) +
  geom_smooth(
    method = "loess",
    formula = "y ~ x",
    alpha = 0.2,
    linewidth = 0.6,
    se = F
  ) +
  labs(
    title = "",
    x = "Unemployment duration (weeks)",
    y = "Instantaneous Hazard",
    color = "",
    fill = ""
  ) +
  scale_color_discrete(labels = c("Region with higher than avg concentration ratio", "Region with higher than avg concentration ratio")) +
  scale_fill_discrete(labels = c("Region with higher than avg concentration ratio", "Region with higher than avg concentration ratio")) +
  theme_ggsurvfit_default() +
  scale_ggsurvfit() +
  xlim(NA, 200)

ggsave(
  "city_conc_haz_plot.pdf",
  path = "../Figures",
  city_conc_haz,
  width = plot_width,
  height = plot_height,
  units = "mm"
)




# Cox -----

cox_mod <-
  coxph(
    Surv(spell, censored) ~  marital_status + age_cat + education + n_dependents +
      chernobyl + desired_wage + sector_of_last_emp + change_in_prof + general_tenure + number_of_jobs +
      high_income + high_wage + high_concentration,
    data = dataset_male, 
  )
cox_mod |> summary()


cox_mod_1 <-
  coxph(
    Surv(spell, censored) ~  marital_status + age_cat + education + n_dependents +
      chernobyl +desired_wage + sector_of_last_emp + change_in_prof + general_tenure + number_of_jobs +
      high_income + high_wage + high_concentration,
    data = dataset_male |> filter(status == 1) , 
  )
cox_mod_1 |> summary()


cox_mod_2 <-
  coxph(
    Surv(spell, censored) ~  marital_status + age_cat + education + n_dependents +
      chernobyl +desired_wage + sector_of_last_emp + change_in_prof + general_tenure + number_of_jobs +
      high_income + high_wage + high_concentration,
    data = dataset_male |> filter(sector_of_last_emp == 2) ,
    robust = T
  )
cox_mod_2 |> summary()

cox_mod_3 <-
  coxph(
    Surv(spell, censored) ~  marital_status + age_cat + education + n_dependents +
      chernobyl +desired_wage + sector_of_last_emp + change_in_prof + general_tenure + number_of_jobs +
      high_income + high_wage + high_concentration,
    data = dataset_male |> filter(status == 3) ,
    robust = T
  )
cox_mod_3 |> summary()




library(survminer)

m0<-cox_mod |> cox.zph()
m1<-cox_mod_1 |> cox.zph()
m2<-cox_mod_2 |> cox.zph()
m3<-cox_mod_3 |> cox.zph()
stargazer(m0$table)
stargazer(m1$table[,1])
stargazer(m2$table[,1])
stargazer(m3$table[,1])




stargazer(cox_mod,
          cox_mod_1,
          cox_mod_2,
          cox_mod_3,
          type = "latex",
          df = F)





survfit(cox_mod) |> autoplot() + theme_ggsurvfit_default()



names(cox_mod$model)

# Summary Stat ----
library(Hmisc)
ggplot(dataset_male, aes(x=spell))+geom_histogram()



dataset_male |> as.data.frame() |> stargazer(summary = T)

#marital_status + age_cat + education + n_dependents + chernobyl +desired_wage + sector_of_last_emp + change_in_prof + general_tenure + number_of_jobs + high_income + high_wage + high_concentration

cox_mod_strat <-
  coxph(
    Surv(spell, censored) ~ chernobyl + marital_status + change_in_prof+ strata(education, status, age_cat, high_income, high_concentration, sector_of_last_emp, general_tenure, n_dependents, high_wage),
    data = dataset_male, robust=T
  )

cox_mod_strat |> summary()
zph_strat <- cox_mod_strat |> cox.zph()
cox_mod_strat |> stargazer(df = F)

test_plot <- ggcoxzph(zph_strat)
test_plot_1 <-test_plot[1]
test_plot_2 <-test_plot[2]
test_plot_3 <-test_plot[3]
ggsave(
  "test_plot_1.pdf",
  path = "../Figures",
  test_plot_1$`1`,
  width = plot_height,
  height =plot_width/3,
  units = "mm"
)

ggsave(
  "test_plot_2.pdf",
  path = "../Figures",
  test_plot_2$`2`,
  width = plot_height,
  height =plot_width/3,
  units = "mm"
)
ggsave(
  "test_plot_3.pdf",
  path = "../Figures",
  test_plot_3$`3`,
  width = plot_height,
  height =plot_width/3,
  units = "mm"
)

