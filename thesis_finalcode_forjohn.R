# Regression ####
### Setup data ####
library(ggplot2)
library(GGally)
library(ggfortify)
library(broom)
library(here)
library(ModelMetrics) # JC: for "kappa"
library(dplyr)
library(tidyverse)
library(psych)

#regression_clean <- read.csv("/Users/natebender/Desktop/Repo/RCthesisanalysis/cleandata/perenial_complete_for_analysis.csv", header=TRUE, stringsAsFactors = TRUE)

# JC: get on board with readr and here!
regression_clean <- read_csv(here("cleandata","perennial_analysis_Apr22.csv"))

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

n.runs <- 100
set.seed(19881)

if(exists("results")){
  rm(results)
}

for(i in 1:n.runs){
  # ensures even split on response variable. 
  default_idx <- caret::createDataPartition(as.factor(data_for_regression$sr_12a_actions_contacted_officials_binary), p = 0.75, list = FALSE)
  default_train <- data_for_regression[default_idx, ]
  default_test <- data_for_regression[-default_idx, ]
  
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
  
  if (i == 1) {
    results <- tidy(logit_contacted_aic) %>% 
      mutate(simulation=i)
    
  } else {
    results <- results %>% 
      bind_rows(tidy(logit_contacted_aic) %>% 
                  mutate(simulation=i))
    
  }
}

df <- results %>% 
  dplyr::group_by(term) %>% 
  dplyr::summarize(n = n(),
                   mean_est = mean(estimate),
                   min_est = min(estimate),
                   max_est = max(estimate),
                   mean_sd = mean(std.error)) %>% 
  mutate(fraction_of_models = n/n.runs) %>%
  arrange(desc(n))

print(df, n = Inf)
write.csv(df,"/Users/natebender/Desktop/repo/RCthesisanalysis/output_tables/ThesisPastactionregmodel_simulations_Apr22.csv", row.names = TRUE)

# JC: stick with "<-" for assignment in R
default_idx <- caret::createDataPartition(as.factor(data_for_regression$sr_12a_actions_contacted_officials_binary), p = 0.75, list = FALSE)
default_train <- data_for_regression[default_idx, ]
default_test <- data_for_regression[-default_idx, ]

### Create models ####

# # Grabbing the 9 EVs that appear in half or more of the simulations
# TRAINING DATA
logit_final <- glm(sr_12a_actions_contacted_officials_binary~
                     cimbenefits_comp+
                     desccontactnorms_all_comp+
                     age_true+
                     cimperceivedrisk_comp+
                     sr_31_able_to_call+
                     sr_41c_ingenuity+
                     behatt_usefulpleasantsensible_comp+
                     sr_11_harm_future_generations_reversed+
                     injunctcontactnorms_all_comp,
                   data=default_train,family=binomial)

# JC: just doing the final one
#library(MASS)
#logit_final_aic <- stepAIC(logit_final, direction="backward")

summary(logit_final)  # skipping the final AIC pass
#summary(logit_final_aic) # age gets dropped from the model

# JC 3/14: Here's a quick coef plot

pretty.names <- tibble(
  term = c("(Intercept)","cimbenefits_comp","desccontactnorms_all_comp",
           "age_true","cimperceivedrisk_comp","sr_31_able_to_call",
           "sr_41c_ingenuity", "behatt_usefulpleasantsensible_comp",
           "sr_11_harm_future_generations_reversed",
           "injunctcontactnorms_all_comp"), 
  pretty_term = c("Intercept","Interpersonal Discussion \n& Media Exposure","Descriptive Norms",
                  "Age", "Perceived Risk", "PBC: Calling Ability", "Worldview: Ingenuity", 
                  "Attitude: Useful/Pleasant/Sensible", "Future Generations Harm", "Injunctive Norms"),
  paste(1:10)
)


logit_final_results <- tidy(logit_final) %>% 
  mutate(exp_est = exp(estimate),
         lb = exp(estimate - 2*std.error),
         ub = exp(estimate + 2*std.error)) 

logit_final_results <- logit_final_results %>% 
  left_join(pretty.names,by="term") %>% 
  mutate(term = fct_reorder(term,estimate),
         pretty_term = fct_reorder(pretty_term,estimate))

# Example that won't really work till you fill in the names
png("Thesis_oddsratio_chart.png") # starts writing a png to file
ggplot(logit_final_results %>% 
         dplyr::filter(term != "(Intercept)"),
       aes(x=exp_est,y=pretty_term)) + 
  geom_vline(xintercept = 1,color="gray80") + 
  geom_point() + 
  theme_minimal(base_size = 30) + 
  labs(x="\nOdds Multiplier",y="") + 
  geom_errorbarh(aes(y=pretty_term,xmin=lb,xmax=ub),height=0.1) 
dev.off()

for_csv <- tidy(logit_final)
write.csv(for_csv,"/Users/natebender/Desktop/repo//RCthesisanalysis/output_tables/thesis_pastactionregmodel_Apr22.csv", row.names = TRUE)

# JC: Based on some looking below, I think our model will perform better
# if we pick a cutoff other than 0.5. Let's see which cutoff gives
# the best performance on the kappa stat

default_train <- default_train %>% 
  mutate(contacted_prob = predict(logit_final,newdata=default_train,type="response"),
         contacted = sr_12a_actions_contacted_officials_binary) 
  # Your tolerance of incredibly long column names mystifies me. 

prob.cutoff <- 0.5

default_train <- default_train %>% 
  mutate(contacted_est = if_else(contacted_prob <= prob.cutoff,0,1))

# Training confusion matrix
table("actual"=default_train$contacted,"estimated"=default_train$contacted_est)

kappa(default_train$contacted,default_train$contacted_est)
# Appears that kappa takes a cutoff, so we don't need the above. Leaving
# for posterity. Let's test

kappa(default_train$contacted,default_train$contacted_prob,cutoff = 0.5)
kappa(default_train$contacted,default_train$contacted_prob,cutoff = 0.4)
kappa(default_train$contacted,default_train$contacted_prob,cutoff = 0.6)

kappa.test <- tibble(cutoff=seq(0.05,0.75,by=0.01),
                     kappa=0.0)

for(i in 1:nrow(kappa.test)){
  kappa.test$kappa[i] <- kappa(default_train$contacted,
                               default_train$contacted_prob,
                               cutoff = kappa.test$cutoff[i])
}

ggplot(kappa.test,
       aes(x=cutoff,y=kappa)) + 
  geom_line() + 
  stat_smooth() + 
  theme_minimal()

# Okay, looks like 0.4 is maybe optimal
kappa.test[which.max(kappa.test$kappa),]
# Alright, now I'm going to jump back down to the 
# work I was doing evaluating the test data.

# JC: my stuff starts here
default_test <- default_test %>% 
  mutate(contacted_prob = predict(logit_final,newdata=default_test,type="response"))

# Let's look at some results
default_test %>% 
  dplyr::select(contacted_prob, sr_12a_actions_contacted_officials_binary) %>% 
  slice_sample(n=10) %>% 
  arrange(contacted_prob)

# Seems like we're going to need a custom probability cutoff (0.5 might not
# be optimal). Going back up to add that in where we do training.

# Using a cutoff of 0.4
default_test <- default_test %>% 
  mutate(contacted_est = if_else(contacted_prob <= 0.4,0,1),
         contacted = sr_12a_actions_contacted_officials_binary)

# Testing confusion matrix
table("actual"=default_test$contacted,"estimated"=default_test$contacted_est)

# Accuracy is

86+12  # accurately predicted
98/(98+14)  # 87.5% accuracy

kappa(default_test$contacted,default_test$contacted_est)
# 0.52 Kappa. Also not bad.

# Making a quick chart for Alex. Only 21 contacts, so just doing quintiles
default_test <- default_test %>% 
  mutate(prob_tier = cut(contacted_prob,
                         breaks=quantile(contacted_prob,
                                         probs=0:5/5),
                         include.lowest=T))

png("Thesis_liftchart.png") # starts writing a png to file
default_test %>% 
  dplyr::group_by(prob_tier) %>% 
  dplyr::summarize(frac_contacted = mean(contacted)) %>%
  ggplot(aes(x=prob_tier, y=frac_contacted)) +
  geom_col() + 
  geom_text(aes(label = scales::percent(frac_contacted)), vjust = -0.5) +
  theme_minimal(base_size = 15) + 
  labs(x="\nProbability Tier by Quintile\n",y="\nFraction in Tier Contacting\n") + 
  scale_x_discrete(labels=c("[0.00295,0.0134]" = "1", "(0.0134,0.0308]" = "2", 
  "(0.0308,0.0697]" = "3", "(0.0697,0.234]" = "4", "(0.234,0.919]" = "5")) +
  scale_y_continuous(label=scales::percent_format())
dev.off()

# end of eval stuff for JC

# Showing fraction of models plot that each variable appeared in in order to defend simulations approach / cutoff
prettynames_full <- tibble(
  term = c("(Intercept)","desccontactnorms_all_comp","cimbenefits_comp",
           "age_true","cimperceivedrisk_comp", "injunctcontactnorms_all_comp",
           "sr_41c_ingenuity","behatt_usefulpleasantsensible_comp", "sr_31_able_to_call",
           "sr_11_harm_future_generations_reversed","behatt_coolexcitingeasy_comp","descdynamicnorms_comp",
           "behatt_admirablegood_comp","injunctmotivation_all_comp","sr_10_harm_you_personally_reversed",
           "sr_71_employment_statusother","sr_71_employment_statusretired", "efficacy_competresp_all_comp",
           "children_dumvarnochildren","sr_30_easy_to_call","sr_41a_right_to_modify",
           "sr_79_political_leaningconservative","sr_79_political_leaningmoderate",
           "sr_72_income100_150k","sr_72_incomeover150k","sr_41e_govt_do_more",
           "sr_41f_equity", "sr_41d_impotent","gender_dumvarfemale",
           "sr_21a_effective_actions_contacting_officials",
           "efficacy_effectiveness_all_comp","race_white_dumvarrace_other",
           "sr_41b_laws_of_nature","descrolemodelnorms_all_comp",
           "sr_61_educationbachelordeg","sr_61_educationgraduatedeg",
           "sr_56_marital_statusdivorce_widow","sr_56_marital_statussingle",
           "sr_75_religion_dumvarnotreligious","sr_7_believe_about_climate_changemost_human",
           "sr_7_believe_about_climate_changemost_natural","sr_7_believe_about_climate_changenatural_human",
           "sr_7_believe_about_climate_changenotchanging"
             ), 
  pretty_term = c("Intercept","Descriptive Norms","Interpersonal Discussion & Media Exposure",
                  "Age", "Perceived Risk","Injunctive Norms","Worldview: Ingenuity",
                  "Behavioral Attitude: Useful/Pleasant/Sensible","PBC: Calling Ability",
                  "Future Generations Harm","Behavioral Attitude: Cool/Exciting/Easy",
                  "Dynamic Descriptive Norms","Behavioral Attitude: Admirable/Good",
                  "Injunctive Norms: Motivation to Comply","Personal Harm",
                  "Employment: Other","Employment: Retired","Group Efficacy: Inst. Competency/Responsiveness",
                  "Children: None","PBC: Easy to Call","Worldview: Right to Modify",
                  "Ideology: Conservative","Ideology: Moderate","Income: 100-150k",
                  "Income: 150k+","Worldview: Gov Should Do More For People","Worldview: Need More Equity",
                  "Worldview: Impotent","Gender: Female","Contacting Officials is Effective Climate Action",
                  "Group Efficacy: Institutional Effectiveness","Race: Other",
                  "Worldview: Subject to Laws of Nature", "Descriptive Norms: Role Models",
                  "Education: Bachelor Degree","Education: Graduate Degree",
                  "Marital: Divorced/Widow","Marital: Single","Not Religious",
                  "CC Mostly Human Caused", "CC Mostly Natural", "CC Natural & Human",
                  "CC is Not Changing"
                    ),
  paste(1:43)
)

df <- df %>% 
  left_join(prettynames_full,by="term")
write.csv(df,"/Users/natebender/Desktop/repo/RCthesisanalysis/output_tables/ThesisPastactionregmodel_simulations_Apr22.csv", row.names = TRUE)


ggplot(df, aes(x=fraction_of_models, y=reorder(pretty_term, fraction_of_models))) +
  geom_point() +
  theme_minimal(base_size = 15) +
  scale_x_continuous(labels = scales::percent) +
  labs(x="\nFraction of Models That Include Variable", y="Variable\n")

# probabilities by tier
default_test %>% 
  dplyr::group_by(prob_tier) %>% 
  dplyr::summarize(frac_contacted = mean(contacted))

### Validation tests  ####
# the kappa statistic is a measure of how closely the instances classified by the machine learning classifier matched the data labeled as ground truth, controlling for the accuracy of a random classifier as measured by the expected accuracy
library(blorr)
blr_model_fit_stats(logit_final)

# Variable inflation factors (rule of thumb: under 5 shows no worrisome collinearity)
vif(logit_final)

library(lmtest)
# lrtest(logit_final_aic, logit_final) #not sig different than saturated model, all else being equal -- go with simpler model
lrtest(logit_final) #sig diff from null model, which is good

#Hosmer-Lemeshow GOF test --> sensitive to group number, not good for binary predictors
#https://stats.stackexchange.com/questions/186219/how-many-groups-to-use-in-hosmer-and-lemeshow-test
# no sig values --> safe to say that there is no evidence of poor model fit
library(ResourceSelection)
for (i in 4:15) {
  print(hoslem.test(default_train$sr_12a_actions_contacted_officials_binary, fitted(logit_final), g=i) $p.value)
} 

# Via Cook's Distance - no evidence of influential points 
plot(logit_final)

### Visualizing model #### 
# JC - should I be visualizing this using the training or testing model? My feeling is visualize using the training model, 
# and only use the testing model to talk about accuracy score via cross-validation. Correct?

# Exponentiated coefficients - showing in terms of odds ratios, not log odds
# library(broom)
# m1_log_preds = tidy(logit_final, conf.int = T, exponentiate = T) %>%
# mutate(Model = "Past contact")
# m1_log_predstest <- subset(m1_log_preds, term !="(Intercept)") #& p.value < 0.05)  # remove intercept
# m1_log_predstest <- m1_log_predstest %>%
#   mutate(term = c("CIM benefits", "Descriptive norms", "Perceived risk", "Efficacy: able to call", "Human ingenuity", "CC personal harm",  "CC harm future generations", "Injunctive norms"))
# write.csv(m1_log_predstest,"/Users/natebender/Desktop/repo//RCthesisanalysis/output_tables/thesis_EXPpastactionregmodel.csv", row.names = TRUE)
# 
# 
# # Plot
# #pdf("ThesisPastaction_oddsratio_plot.pdf") # starts writing a PDF to file
# # png("ThesisPastaction_oddsratio_plot.png") # starts writing a PDF to file
# zp1 <- ggplot(m1_log_predstest, aes(colour = Model))
# zp1 <- zp1 + geom_hline(yintercept = 1, colour = gray(1/2), lty = 2)
# zp1 <- zp1 + geom_linerange(aes(x = term, ymin = conf.low,
#                                 ymax = conf.high),
#                             lwd = 1, position = position_dodge(width = 1/2))
# zp1 <- zp1 + geom_pointrange(aes(x = term, y = estimate, ymin = conf.low,
#                                  ymax = conf.high),
#                              lwd = 1/2, position = position_dodge(width = 1/2),
#                              shape = 21, fill = "WHITE")
# zp1 <- zp1 + coord_flip() + theme_bw()
# INTzp1_log <- zp1 + ggtitle("What influences past representative contact?") + ylab("Odds Ratio") + xlab("Variable")
# print(INTzp1_log)  # The trick to these is position_dodge().
# dev.off()


# Cluster analysis ####
# Subset to just the eight predictive variables
# "CIM benefits", "Descriptive norms", "Perceived risk", "Efficacy: able to call", "Human ingenuity", "CC personal harm",  "CC harm future generations", "Injunctive norms"))
evs_forclustering <- regression_clean %>% 
  dplyr::select(cimbenefits_comp,
         desccontactnorms_all_comp,
         cimperceivedrisk_comp,
         sr_31_able_to_call,
         sr_41c_ingenuity,
         behatt_usefulpleasantsensible_comp,
         sr_11_harm_future_generations_reversed,
         injunctcontactnorms_all_comp,
         age_true
         )

# Standardize variables
scaled_clean <- scale(evs_forclustering)

# Check correlations — no issues
res <- cor(scaled_clean)
round(res, 2)

### Validation tests for number of clusters ####
# elbow method suggests 3 clusters.
library(factoextra)
fviz_nbclust(scaled_clean, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

# Average silhouette method - suggests 2
fviz_nbclust(scaled_clean, kmeans, method = "silhouette")

# Gap statistic - suggests 3 clusters
# nstart option attempts multiple initial configurations and reports on the best one. 
# For example, adding nstart=25 will generate 25 initial random centroids and choose the best one for the algorithm.
# kmax sets upper limit of clusters to test against
# B is the number of bootstrapped samples the function uses to test against. The reference dataset is generated using Monte Carlo simulations of the sampling process
library(cluster)
gap_stat <- clusGap(scaled_clean, FUN = kmeans, nstart = 10, d.power = 2,
                    K.max = 20, B = 300)
fviz_gap_stat(gap_stat)

# 30 indices method - majority rule recommendation is 3 clusters
library(NbClust)
nb <- NbClust(scaled_clean, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")
fviz_nbclust(nb)

# Based on these tests, going with 3 clusters
kmean_3 <- kmeans(scaled_clean, 3)
kmean_3$centers
kmean_3
autoplot(kmean_3, scaled_clean, frame = TRUE)  #pca viz


# Silhouette plot
library(cluster)
library(factoextra)
sil <- silhouette(kmean_3$cluster, dist(scaled_clean))
fviz_silhouette(sil)

### Visualizing clusters ####
library(GGally)
library(plotly)

# Group cluster results by mean for each variable in regression_clean dataframe
regression_clean$cluster <- as.factor(kmean_3$cluster)  # add cluster values back into dataframe

# creating dataframe of segmentation variable means but only for actual ppl who contacted. 
actual_contact <- regression_clean %>%  # Desc stats on the total actual ppl who contacted, not grouped by cluster
  dplyr::select(
    cimbenefits_comp,
    desccontactnorms_all_comp,
    cimperceivedrisk_comp,
    sr_31_able_to_call,
    sr_41c_ingenuity,
    behatt_usefulpleasantsensible_comp,
    sr_11_harm_future_generations_reversed,
    injunctcontactnorms_all_comp,
    sr_12a_actions_contacted_officials_binary) %>%
  filter(sr_12a_actions_contacted_officials_binary==1) %>%
  summarize_if(is.numeric, mean) %>% 
  mutate(Cluster = as.factor(c(0))) %>% #0 is a placeholder for this total group of actual contacters. 
  select(-sr_12a_actions_contacted_officials_binary) %>% 
  setNames(c("Interpersonal Discussion \n& Media Exposure","Descriptive Norms",
             "Perceived Risk", "PBC: Calling Ability", "Worldview: Ingenuity", 
             "Attitude: Useful/Pleasant/Sensible", "Future Generations Harm", "Injunctive Norms", "Cluster"))
  

df3_clus_avg <- regression_clean %>%
  group_by(cluster) %>%
  summarize_if(is.numeric, mean)

scaled_avg <- regression_clean %>% 
  select(cimbenefits_comp,
         desccontactnorms_all_comp,
         age_true,
         cimperceivedrisk_comp,
         sr_31_able_to_call,
         sr_41c_ingenuity,
         behatt_usefulpleasantsensible_comp,
         sr_11_harm_future_generations_reversed,
         injunctcontactnorms_all_comp,
         cluster) %>% 
  setNames(c("Interpersonal Discussion & Media Exposure","Descriptive Norms",
             "Age", "Perceived Risk", "PBC: Calling Ability", "Worldview: Ingenuity", 
             "Attitude: Useful/Pleasant/Sensible", "Future Generations Harm", "Injunctive Norms", "Cluster"))

data.frame(colnames(scaled_avg))

scaled_avg <- scaled_avg %>% 
  mutate_at(c(1:9), funs(c(scale(.)))) %>% 
  group_by(Cluster) %>%
  summarize_if(is.numeric, mean)


cluster_means <- df3_clus_avg %>% 
  dplyr::select(cimbenefits_comp,
                desccontactnorms_all_comp,
                age_true,
                cimperceivedrisk_comp,
                sr_31_able_to_call,
                sr_41c_ingenuity,
                behatt_usefulpleasantsensible_comp,
                sr_11_harm_future_generations_reversed,
                injunctcontactnorms_all_comp,
                cluster
                ) %>% 
  setNames(c("Interpersonal Discussion \n& Media Exposure","Descriptive Norms",
             "Age", "Perceived Risk", "PBC: Calling Ability", "Worldview: Ingenuity", 
             "Attitude: Useful/Pleasant/Sensible", "Future Generations Harm", "Injunctive Norms", "Cluster"))
#write.csv(cluster_means,"/Users/natebender/Desktop/repo/RCthesisanalysis/output_tables/meanscores_clusters_Apr22.csv", row.names = TRUE)

# Final EV list and indices
# 6"CIM benefits", 8"Descriptive contact norms", , 11"Perceived risk", 22 (but for some reason it's 21 below)"Efficacy: able to call", 12"CC personal harm",  13"CC harm future generations" 24"sr_41c_ingenuity "injunctcontactnorms_all_comp

library(data.table)

meanscores_all <- bind_rows(cluster_means, actual_contact)
setDT(meanscores_all)
setDT(cluster_means)

forplot <- cluster_means %>% 
  dplyr::select(-Age) 

forplot_long <- melt(data = forplot,
                     id.vars = c("Cluster"),# "respondent_id"),
                     variable.name = "variable",
                     value.name = "mean_value")
  
forplot_all <- meanscores_all %>% 
  dplyr::select(-Age)

forplot_all_long <- melt(data = forplot_all,
                     id.vars = c("Cluster"),# "respondent_id"),
                     variable.name = "variable",
                     value.name = "mean_value")

# scaled data for plot
setDT(scaled_avg)

forplot_long <- melt(data = scaled_avg,
                     id.vars = c("Cluster"),# "respondent_id"),
                     variable.name = "variable",
                     value.name = "mean_value")



library("ggsci")
# Plot segmentation variables means by cluster
p <- forplot_long %>%
  ggplot(aes(x=variable, y=mean_value, shape=Cluster, color=Cluster)) +
  geom_point(size=9) +
  theme_minimal(base_size = 30)+
  theme(axis.text.x = element_text(angle=35, hjust=.95, size=25),
        axis.text.y = element_text(size=25),
        legend.key.size = unit(2.0, "cm"),
        legend.key = element_rect(color = NA, fill = NA),
        legend.title.align = 0.5,
        legend.text = element_text(size=25),
        legend.title = element_text(size=25)) +
  scale_x_discrete(limits = c("Age","Worldview: Ingenuity","Future Generations Harm",
                              "Perceived Risk", "PBC: Calling Ability","Interpersonal Discussion & Media Exposure","Attitude: Useful/Pleasant/Sensible",
                              "Injunctive Norms","Descriptive Norms")) +
  scale_color_npg(name="Clusters",
                  labels=c("Unlikely to Act", "Ready to Go", "Just Add Norms")) +
  scale_shape_manual(name="Clusters",
                     labels=c("Unlikely to Act", "Ready to Go", "Just Add Norms"),
                     values = c(15, 16, 17)) +
  scale_fill_discrete(labels = c("Unlikely to Act", "Ready to Go", "Just Add Norms")) + #Rename the legend title and text labels.
  labs(x="Variable\n", y="\nStandardized\nMean Value\n")
p

###
###
### Descriptive stats on clusters ####

# number of respondents in each cluster
counts <- regression_clean %>% 
  dplyr::group_by(cluster) %>% 
  dplyr::summarize(n = n(),
            actual_contacted = sum(sr_12a_actions_contacted_officials_binary))

counts <- counts %>% 
  left_join(df3_clus_avg,by="cluster")

counts <- counts %>% 
  dplyr::select(cluster,
                n,
                actual_contacted,
                age_true,
                cimperceivedrisk_comp,
                sr_11_harm_future_generations_reversed,
                sr_41c_ingenuity,
                sr_31_able_to_call,
                behatt_usefulpleasantsensible_comp,
                cimbenefits_comp,
                injunctcontactnorms_all_comp,
                desccontactnorms_all_comp) %>% 
  setNames(c("Cluster", "n", "Actual Contacted","Age","Perceived Risk","Future Generations Harm","Worldview: Ingenuity",
             "PBC: Calling Ability","Behatt: Useful/Pleasant/Sensible","Interpersonal Discussion \n& Media Exposure",
             "Injunctive Norms","Descriptive Norms")) 

write.csv(counts, "/Users/natebender/Desktop/repo/RCthesisanalysis/output_tables/meanscores_clusters_Apr22.csv", row.names = TRUE)

library(purrr)
regression_clean %>%  # for categorical vars desc stats by cluster. List is in-progress, just experimenting at the moment. 
  dplyr::select(
    cluster, 
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
  ) %>% 
  filter(cluster==3) %>% 
  describe()

# Determine if there are any differences between the 453 resp in Regression_Clean and the incomplete resp in original dataset
#####
#####
regression_clean %>%
  select(race_white_dumvar,
                 gender_dumvar,
                 children_dumvar,
                 sr_75_religion_dumvar,
                 sr_56_marital_status,
                 sr_61_education,
                 sr_71_employment_status,
                 sr_72_income,
                 sr_79_political_leaning,
         age_true,
         sr_7_believe_about_climate_change) %>% 
  map(summary)

nonaccepted <- d_accept %>% 
  anti_join(regression_clean, by="respondent_id")  # anti-join grabs all of "d_accept" except those resp_ids that match in "regression_clean". So we're looking at only those in d_accept who were not chosen
nonaccepted %>%
  select(race_white_dumvar,
         gender_dumvar,
         children_dumvar,
         sr_75_religion_dumvar,
         sr_56_marital_status,
         sr_61_education,
         sr_71_employment_status,
         sr_72_income,
         sr_79_political_leaning,
         sr_12a_actions_contacted_officials,
         sr_7_believe_about_climate_change) %>%
  mutate(n = n()) %>% 
  map(summary)

nonaccepted %>%
  select(age_true,
         cimperceivedrisk_comp,
         sr_11_harm_future_generations_reversed,
         sr_41c_ingenuity,
         sr_31_able_to_call,
         behatt_usefulpleasantsensible_comp,
         cimbenefits_comp,
         injunctcontactnorms_all_comp,
         desccontactnorms_all_comp) %>% 
  mutate(n = n()) %>% 
  describe()

###
### Messing with chart sizing for thesis presentation
### 
default_test %>% 
  dplyr::group_by(prob_tier) %>% 
  dplyr::summarize(frac_contacted = mean(contacted)) %>%
  ggplot(aes(x=prob_tier, y=frac_contacted)) +
  geom_col() + 
  geom_text(aes(label = scales::percent(frac_contacted)), size=8, vjust = -0.5) +
  theme_minimal(base_size = 30) + 
  labs(x="\nProbability Tier by Quintile\n",y="\nFraction in Tier Contacting\n") + 
  scale_x_discrete(labels=c("[0.00295,0.0134]" = "1", "(0.0134,0.0308]" = "2", 
                            "(0.0308,0.0697]" = "3", "(0.0697,0.234]" = "4", "(0.234,0.919]" = "5")) +
  scale_y_continuous(label=scales::percent_format())


# age
exp(10*-0.03) # 0.74 for every decade
exp(10*-0.03+2*0.01)  # .73 - .76

ggplot(logit_final_results %>% 
         dplyr::filter(term != "(Intercept)"),
       aes(x=exp_est,y=pretty_term)) + 
  geom_vline(xintercept = 1,color="gray80") + 
  geom_point(size = 5) + 
  theme_minimal(base_size = 30) + 
  labs(x="\nOdds Multiplier",y="") + 
  geom_errorbarh(aes(y=pretty_term,xmin=lb,xmax=ub),height=0.1, size=1) 

# ###
# # Checking group efficacy against personal efficacy for full sample per Alex request
# ggplot(regression_clean,
#        aes(x=sr_21a_effective_actions_contacting_officials,y=efficacy_effectiveness_all_comp)) + 
#   scale_color_npg(name="Clusters",
#   labels=c("Unlikely to Act", "Ready to Go", "Just Add Norms")) +
#   scale_shape_manual(name="Clusters",
#                      labels=c("Unlikely to Act", "Ready to Go", "Just Add Norms"),
#                      values = c(15, 16, 17)) +
#   scale_fill_discrete(labels = c("Unlikely to Act", "Ready to Go", "Just Add Norms")) + 
#   geom_jitter(aes(color = cluster, size = 3), width = .25)+
#   theme_minimal(base_size = 30) + 
#   theme(axis.text.x = element_text(size=25),
#         axis.text.y = element_text(size=25),
#         legend.key.size = unit(2.0, "cm"),
#         legend.key = element_rect(color = NA, fill = NA),
#         legend.title.align = 0.5,
#         legend.text = element_text(size=25),
#         legend.title = element_text(size=25)) +
#   guides(colour = guide_legend(override.aes = list(size=8)))+
#   labs(x="\nPersonal Efficacy",y="Grp Eff: Effectiveness")
# 
# ggplot(regression_clean,
#        aes(x=sr_21a_effective_actions_contacting_officials,y=efficacy_competresp_all_comp)) + 
#   scale_color_npg(name="Clusters",
#                   labels=c("Unlikely to Act", "Ready to Go", "Just Add Norms")) +
#   scale_shape_manual(name="Clusters",
#                      labels=c("Unlikely to Act", "Ready to Go", "Just Add Norms"),
#                      values = c(15, 16, 17)) +
#   scale_fill_discrete(labels = c("Unlikely to Act", "Ready to Go", "Just Add Norms")) + 
#   geom_jitter(aes(color = cluster, size = 3), width = .25)+
#   theme_minimal(base_size = 30) + 
#   theme(axis.text.x = element_text(size=25),
#         axis.text.y = element_text(size=25),
#         legend.key.size = unit(2.0, "cm"),
#         legend.key = element_rect(color = NA, fill = NA),
#         legend.title.align = 0.5,
#         legend.text = element_text(size=25),
#         legend.title = element_text(size=25)) +
#   guides(colour = guide_legend(override.aes = list(size=8)))+
#   labs(x="\nPersonal Efficacy",y="Grp Eff: Competency/Responsiveness")
# 
# 
# res <- cor.test(regression_clean$sr_21a_effective_actions_contacting_officials, 
#                 regression_clean$efficacy_effectiveness_all_comp, method = "spearman")
# res
# 
# res <- cor.test(regression_clean$sr_21a_effective_actions_contacting_officials, 
#                 regression_clean$efficacy_competresp_all_comp, method = "spearman")
# res
# 
# test <- lm(sr_21a_effective_actions_contacting_officials ~
#                          I(efficacy_competresp_all_comp^2),# + efficacy_competresp_all_comp,
#                        data = regression_clean)
# summary(test)
# #install.packages("ggiraphExtra")
# library(ggiraphExtra)
# ggPredict(test, interactive=T)
# 
# plot(sr_21a_effective_actions_contacting_officials ~
#        I(efficacy_competresp_all_comp^2), data=regression_clean)
# abline(test)


###
###
### messing with kmeans centroids
regression_clean$cluster <- recode_factor(regression_clean$cluster, "1"="Unlikely to Act", "2"="Ready to Go", "3"="Just Add Norms") 

# Iris dataset just to show example of this code to representative points of each cluster
library(datasets)
data(iris)
k=3 #number of centroids
model <- kmeans(iris[,1:4],k) #use only first 4 columns of iris data
index <- c()

#calculate indices of closest instance to centroid
for (i in 1:k){
  rowsum <- rowSums(abs(iris[which(model$cluster==i),1:4] - model$centers[i,]))
  index[i] <- as.numeric(names(which.min(rowsum)))
}
index

# my data 
index=c()

for (i in 1:k){
  rowsum <- rowSums(abs(scaled_clean[which(kmean_3$cluster==i),1:9] - kmean_3$centers[i,]))
  index[i] <- as.numeric(names(which.min(rowsum)))
}
index
# Not working because I need to scale this data, perhaps? To match the scaled data that the clustering analysis was run on originally?

### 
### 
### PAMM approach
### 
### 
library(cluster)

pam_evs <- regression_clean %>% 
  select(respondent_id,
                cimbenefits_comp,
                desccontactnorms_all_comp,
                cimperceivedrisk_comp,
                sr_31_able_to_call,
                sr_41c_ingenuity,
                behatt_usefulpleasantsensible_comp,
                sr_11_harm_future_generations_reversed,
                injunctcontactnorms_all_comp,
                age_true
  )

# Standardize variables
pam_scaled <- pam_evs %>% 
  mutate_at(c(2:10), funs(c(scale(.))))  # scale vars except for resp_id

pamresult <- pam(pam_scaled, k = 3)  # run pam clustering algorithm
pamresult$medoids  # tells us repondent_ids for the three medoids that we can filter by below

regression_clean$pamcluster = pamresult$cluster  # add cluster membership back into full dataframe

# looking at the three pam medoids. Doesn't quite line up with the clusters we found using k-means; cluster 2 from k-means is represented twice when using pam. 
# "cluster" var in regression_clean is the result of the k-means approach. "pamcluster" is the result of this pam approach. Added them side-by-side to check for discrepancy. 
# I assume this is an issue?? 
cluster_reps <- regression_clean %>%  
  filter(respondent_id %in%
           c("11865426794","11870150283","11877343624"))
