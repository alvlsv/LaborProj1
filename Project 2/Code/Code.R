library(tidyverse)
library(survival)
library(muhaz)
library(haven)
library(stargazer)
library(ggsurvfit)


dataset_raw <-
  read_dta("Project 2 data after stset.dta", encoding = "latin1")

write.csv(dataset_raw, file = "data_raw.csv")



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
    zarplat,
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
    desired_wage = n_zarpl,
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
  select(-spell.x, -censored.x, -got_employed.x) |>
  rename(spell = spell.y,
         censored = censored.y) |> filter(education != 7)




Surv(dataset_male$spell, dataset_male$censored)




all_risk <-
  survfit2(Surv(spell, censored) ~ 1, data = dataset_male) |>
  ggsurvfit("risk") +
  labs(x = "Unemployment duration (weeks)") +
  add_confidence_interval() +
  scale_color_discrete(labels = names_status) +
  scale_fill_discrete(labels = names_status) +
  scale_ggsurvfit() +
  xlim(NA, 200)

ggsave(
  "all_risk_plot.pdf",
  path = "../Figures",
  all_risk,
  width = 220/2,
  height = 140/2,
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
  ) + scale_color_discrete(labels = names_status) + scale_fill_discrete(labels = names_status) + theme_ggsurvfit_default() + scale_ggsurvfit()+  xlim(NA, 200)


ggsave(
  "all_hazard_plot.pdf",
  path = "../Figures",
  all_haz,
  width = 220/2,
  height = 140/2,
  units = "mm"
)



names_status <-
  c('Redundant',
    'Lost job',
    'Long-term not employed',
    'Never worked before ')


status_risk <-
  survfit2(Surv(spell, censored) ~ status, data = dataset_male) |>
  ggsurvfit("risk") +
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
  width = 220/2,
  height = 140/2,
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
  scale_ggsurvfit()+ 
  xlim(NA, 200)

ggsave(
  "status_haz_plot.pdf",
  path = "../Figures",
  status_haz,
  width = 220/2,
  height = 140/2,
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
  ggsurvfit("risk") +
  labs(x = "Unemployment duration (weeks)") +
  add_confidence_interval() +
  scale_color_discrete(labels = names_education) +
  scale_fill_discrete(labels = names_education) +  scale_ggsurvfit()


ggsave(
  "educ_risk_plot.pdf",
  path = "../Figures",
  educ_risk,
  width = 220/2,
  height = 140/2,
  units = "mm"
)



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
  ) + scale_color_discrete(labels = names_education) + scale_fill_discrete(labels = names_education) + theme_ggsurvfit_default() + scale_ggsurvfit()



survfit2(Surv(spell, censored) ~ chernobyl,
         data = dataset_male) |>
  ggsurvfit() +
  labs(x = "Unemployment duration (weeks)") +
  add_confidence_interval() +
  scale_color_discrete(labels = c("Chernobyl victim status", "Others")) +
  scale_fill_discrete(labels = c("Chernobyl victim status", "Others")) +  scale_ggsurvfit()




survfit2(Surv(spell, censored) ~ change_in_prof ,
         data = dataset_male) |>
  ggsurvfit() +
  labs(x = "Unemployment duration (weeks)") +
  scale_ggsurvfit()  +
  add_confidence_interval() +
  scale_color_discrete(labels = c("Profession unchanged", "Profession changed")) +
  scale_fill_discrete(labels = c("Profession unchanged", "Profession changed"))








cox_mod <-
  coxph(
    Surv(spell, censored) ~ marital_status + age_cat + education + n_dependents +
      chernobyl + desired_wage + sector_of_last_emp + change_in_prof + general_tenure + number_of_jobs +
      high_income + high_wage + high_concentration,
    data = dataset_male ,
    robust = T,
    model = T
  )



cox_mod |> summary()

cox_mod_1 <-
  coxph(
    Surv(spell, censored) ~  marital_status + age_cat + education + n_dependents +
      chernobyl + desired_wage + sector_of_last_emp + change_in_prof + general_tenure + number_of_jobs +
      high_income + high_wage + high_concentration,
    data = dataset_male |> filter(status == 1) ,
    robust = T
  )
cox_mod_1 |> summary()


cox_mod_2 <-
  coxph(
    Surv(spell, censored) ~  marital_status + age_cat + education + n_dependents +
      chernobyl + desired_wage + sector_of_last_emp + change_in_prof + general_tenure + number_of_jobs +
      high_income + high_wage + high_concentration,
    data = dataset_male |> filter(status == 2) ,
    robust = T
  )
cox_mod_2 |> summary()

cox_mod_3 <-
  coxph(
    Surv(spell, censored) ~  marital_status + age_cat + education + n_dependents +
      chernobyl + desired_wage + sector_of_last_emp + change_in_prof + general_tenure + number_of_jobs +
      high_income + high_wage + high_concentration,
    data = dataset_male |> filter(status == 3) ,
    robust = T
  )
cox_mod_3 |> summary()




library(survminer)

cox_mod |> cox.zph()
cox_mod_1 |> cox.zph()
cox_mod_2 |> cox.zph()
cox_mod_3 |> cox.zph()


survminer::ggcoxzph(cox_mod |> cox.zph())


stargazer(cox_mod, cox_mod_1, cox_mod_2, cox_mod_3, type="text", df=F)

survreg(
  Surv(spell, censored) ~ marital_status + age_cat + education + n_dependents +
    chernobyl + desired_wage + change_in_prof + number_of_jobs,
  data = dataset_male,
  
) |> plot()


survfit(cox_mod) |> autoplot() + theme_ggsurvfit_default()






GGally::ggpairs(dataset_male|> select(-sector_of_last_emp),
                ggplot2::aes(), cardinality_threshold = 20)
