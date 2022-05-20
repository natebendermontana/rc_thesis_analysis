# Strive for just the libraries you need for the task at hand. All at the top. 
# idea is for every line to work immediately on my machine, other than installing
# packages I don't have. 

library(here)
library(dplyr)
library(tidyverse)

set.seed(19881)

# read in and clean data
regression_clean <- read_csv(here("cleandata","perennial_analysis_Apr22.csv")) %>% 
  select(-`...1`) # weird column is sneaking in

regression_clean <- regression_clean %>% 
  mutate(sr_12a_actions_contacted_officials_binary = 
           case_when(
             sr_12a_actions_contacted_officials == "morethanonce" ~ 1,
             sr_12a_actions_contacted_officials == "once" ~ 1,
             sr_12a_actions_contacted_officials == "nocontact" ~ 0,
             TRUE ~ -1 # Just to be careful
           ))

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

# Subset to only data for regression
data_for_regression <- regression_clean %>% 
  dplyr::select(sr_12a_actions_contacted_officials_binary,
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

# 

evs_forclustering <- regression_clean %>% 
  select(cimbenefits_comp,
         desccontactnorms_all_comp,
         cimperceivedrisk_comp,
         sr_31_able_to_call,
         sr_41c_ingenuity,
         behatt_usefulpleasantsensible_comp,
         sr_11_harm_future_generations_reversed,
         injunctcontactnorms_all_comp,
         age_true)

# Standardize variables
scaled_clean <- scale(evs_forclustering)

kmean_3 <- kmeans(scaled_clean, 3)

clust.centers <- kmean_3$centers

scaled_check <- cbind(scaled_clean, cluster = kmean_3$cluster)
# Nate: manually check the distance between these three centers and 
# one or two random rows just to make sure all the crap I do below
# is right. 

evs.mat <- as.matrix(scaled_clean)
dist.holder <- data.frame(clust_dist_1 = rep(0.0,nrow(scaled_clean)),
                      clust_dist_2 = 0.0,
                      clust_dist_3 = 0.0)

for(i in 1:nrow(evs.mat)){
  
  data.row <- evs.mat[i,]
  for.dist <- matrix(data.row,nrow=3,ncol=length(data.row),byrow = T)
  
  dists <- sqrt(rowSums((clust.centers - for.dist)^2))
  
  dist.holder[i,] <- dists
  
}

exemplars <- c(which.min(dist.holder[,1]),
               which.min(dist.holder[,2]),
               which.min(dist.holder[,3]))

exemplars # these are the indices

# if we've kept the rows the same, this should work
regression_clean %>% 
  slice(exemplars) %>% 
  data.frame() %>% 
  t() # transpose just to make it print better

# Let's display the top 5-ish by cluster
percentile <- 5/nrow(regression_clean)

for(i in 1:3){
  
  cutoff <- quantile(dist.holder[,i],probs=percentile)

  print(paste("Cluster Number ",i))
  print("Regression Clean")
  print(
    regression_clean[dist.holder[,i] <= cutoff,] %>% 
      data.frame()
  )
  
  print("Scaled Clean")
  print(
    scaled_clean[dist.holder[,i] <= cutoff,]
  )
  
  print("----------------------------------------------------")
    
}





