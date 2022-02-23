library(MASS)
library(caret)
library(tidyverse)
library(here)
library(broom)
install.packages("skimr")
library(skimr)
library(psych)


regression_clean <- read_csv(here("cleandata","perenial_complete_for_analysis.csv"))

regression_clean <- regression_clean %>% 
  mutate(sr_12a_actions_contacted_officials_binary = 
           case_when(
             sr_12a_actions_contacted_officials == "morethanonce" ~ 1,
             sr_12a_actions_contacted_officials == "once" ~ 1,
             sr_12a_actions_contacted_officials == "nocontact" ~ 0,
             TRUE ~ -1 # Just to be careful
           ))

# table(regression_clean$sr_12a_actions_contacted_officials_binary)

# set reference groups for regression
regression_clean <- regression_clean %>% 
  mutate(race_white_dumvar = fct_relevel(race_white_dumvar,"race_white"),
         children_dumvar = fct_relevel(children_dumvar,"children"),
         sr_75_religion_dumvar = fct_relevel(sr_75_religion_dumvar, "religious"),
         gender_dumvar = fct_relevel(gender_dumvar, "male"),
         sr_56_marital_status = fct_relevel(sr_56_marital_status, "married_partner"),
         sr_61_education = fct_relevel(sr_61_education, "assocdeg_orlower"),
         sr_71_employment_status = fct_relevel(sr_71_employment_status, "workfull"),
         sr_72_income = fct_relevel(sr_72_income, "under100k"),
         sr_79_political_leaning = fct_relevel(sr_79_political_leaning, "liberal"),
         sr_7_believe_about_climate_change = fct_relevel(sr_7_believe_about_climate_change,
                                                        "excl_human"))
         


# Cut down for model

data_for_regression <- regression_clean %>% 
  select(sr_12a_actions_contacted_officials_binary,
    age_true,
    race_white_dumvar,
    gender_dumvar,
    children_dumvar,
    sr_75_religion_dumvar,
    sr_56_marital_status,
    sr_61_education,
    sr_71_employment_status,
    sr_72_income,
    sr_79_political_leaning,
    sr_7_believe_about_climate_change,
    descdynamicnorms_comp,
    desccontactnorms_all_comp,
    descrolemodelnorms_all_comp,
    injunctcontactnorms_all_comp,
    injunctmotivation_all_comp,
    cimbenefits_comp,
    cimperceivedrisk_comp,
    sr_10_harm_you_personally_reversed,
    sr_11_harm_future_generations_reversed,
    sr_21a_effective_actions_contacting_officials,
    efficacy_effectiveness_all_comp,
    efficacy_competresp_all_comp,
    behatt_admirablegood_comp,
    behatt_usefulpleasantsensible_comp,
    behatt_coolexcitingeasy_comp,
    sr_30_easy_to_call,
    sr_31_able_to_call,
    sr_41a_right_to_modify,
    sr_41b_laws_of_nature,
    sr_41c_ingenuity,
    sr_41d_impotent,
    sr_41e_govt_do_more,
    sr_41f_equity)

set.seed(19881)

# ensures even split on response variable. 
default_idx = caret::createDataPartition(as.factor(data_for_regression$sr_12a_actions_contacted_officials_binary), p = 0.75, list = FALSE)
default_train = data_for_regression[default_idx, ]
default_test = data_for_regression[-default_idx, ]



logit_contacted <- glm(sr_12a_actions_contacted_officials_binary ~
                         age_true + race_white_dumvar + gender_dumvar + children_dumvar +
                         sr_75_religion_dumvar + sr_56_marital_status + sr_61_education +
                         sr_71_employment_status + sr_72_income +
                         sr_79_political_leaning + sr_7_believe_about_climate_change +
                         descdynamicnorms_comp + desccontactnorms_all_comp +
                         descrolemodelnorms_all_comp + injunctcontactnorms_all_comp +
                         injunctmotivation_all_comp + cimbenefits_comp +
                         cimperceivedrisk_comp + sr_10_harm_you_personally_reversed +
                         sr_11_harm_future_generations_reversed +
                         sr_21a_effective_actions_contacting_officials +
                         efficacy_effectiveness_all_comp + efficacy_competresp_all_comp +
                         behatt_admirablegood_comp + behatt_usefulpleasantsensible_comp +
                         behatt_coolexcitingeasy_comp +
                         sr_30_easy_to_call + sr_31_able_to_call + sr_41a_right_to_modify +
                         sr_41b_laws_of_nature + sr_41c_ingenuity + sr_41d_impotent +
                         sr_41e_govt_do_more + sr_41f_equity, 
                       data = default_train, 
                       family=binomial)


logit_contacted_aic <- stepAIC(logit_contacted, direction="backward")

tidy(logit_contacted_aic)

summary(logit_contacted_aic)
