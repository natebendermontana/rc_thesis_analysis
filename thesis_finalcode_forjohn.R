# Regression ####
### Setup data ####
library(tidyverse)
library(ggplot2)
library(GGally)
library(ggfortify)
library(broom)
library(here)
library(ModelMetrics) # JC: for "kappa"
library(dplyr)

#regression_clean <- read.csv("/Users/natebender/Desktop/Repo/RCthesisanalysis/cleandata/perenial_complete_for_analysis.csv", header=TRUE, stringsAsFactors = TRUE)

# JC: get on board with readr and here!
regression_clean <- read_csv(here("cleandata","perenial_complete_for_analysis.csv"))

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

set.seed(19881)

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
                     sr_10_harm_you_personally_reversed+
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
           "sr_41c_ingenuity","sr_10_harm_you_personally_reversed",
           "sr_11_harm_future_generations_reversed",
           "injunctcontactnorms_all_comp"), 
  pretty_term = c("Intercept","Interpersonal Discussion \n& Media Exposure","Descriptive Norms",
                  "Age", "Perceived Risk", "PBC: Calling Ability", "Worldview: Ingenuity", 
                  "Personal Harm", "Future Generations Harm", "Injunctive Norms"),
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

ggplot(logit_final_results %>% 
         filter(term != "(Intercept)"),
       aes(x=exp_est,y=term)) + 
  geom_vline(xintercept = 1,color="gray80") + 
  geom_point() + 
  theme_minimal() + 
  labs(x="Odds Multiplier",y="") + 
  geom_errorbarh(aes(y=term,xmin=lb,xmax=ub),height=0.1) 

# Example that won't really work till you fill in the names
png("Thesis_oddsratio_chart.png") # starts writing a png to file
ggplot(logit_final_results %>% 
         dplyr::filter(term != "(Intercept)"),
       aes(x=exp_est,y=pretty_term)) + 
  geom_vline(xintercept = 1,color="gray80") + 
  geom_point() + 
  theme_minimal() + 
  labs(x="Odds Multiplier",y="") + 
  geom_errorbarh(aes(y=pretty_term,xmin=lb,xmax=ub),height=0.1) 
dev.off()

for_csv <- tidy(logit_final)
write.csv(for_csv,"/Users/natebender/Desktop/repo//RCthesisanalysis/output_tables/thesis_pastactionregmodel.csv", row.names = TRUE)

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


# JC: Okay, this is where trouble starts. Your testing data should *only* be
# used for evaluation. You're grading your own homework by refitting the model 
# on the testing data. I'm going to comment out the bad steps


# TESTING DATA
# logit_final_test <- glm(sr_12a_actions_contacted_officials_binary~
#                           cimbenefits_comp+
#                           desccontactnorms_all_comp+
#                           cimperceivedrisk_comp+
#                           sr_31_able_to_call+
#                           sr_41c_ingenuity+
#                           sr_10_harm_you_personally_reversed+
#                           sr_11_harm_future_generations_reversed+
#                           injunctcontactnorms_all_comp,
#                         data=default_test,family=binomial)
# summary(logit_final_test)

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
111/(111+15) # pretty legit

95+12
107/(107+19)

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
  theme_minimal() + 
  labs(x="\nProbability Tier\n",y="\nFraction in Tier Contacting\n") + 
  scale_y_continuous(label=scales::percent_format())
dev.off()

# end of eval stuff for JC

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
         sr_10_harm_you_personally_reversed,
         sr_11_harm_future_generations_reversed,
         injunctcontactnorms_all_comp,
         age_true)

# Standardize variables
scaled_clean <- scale(evs_forclustering)

# Check correlations — no issues
res <- cor(scaled_clean)
round(res, 2)

### Validation tests for number of clusters ####
# elbow method suggests 2 or 3 clusters.
library(factoextra)
fviz_nbclust(scaled_clean, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

# Average silhouette method - suggests 3
fviz_nbclust(scaled_clean, kmeans, method = "silhouette")

# Gap statistic - suggests 12 clusters - need to figure out why this is suggesting wildly higher # of clusters than other validation tests. 
# nstart option attempts multiple initial configurations and reports on the best one. 
# For example, adding nstart=25 will generate 25 initial random centroids and choose the best one for the algorithm.
# kmax sets upper limit of clusters to test against
# B is the number of bootstrapped samples the function uses to test against. The reference dataset is generated using Monte Carlo simulations of the sampling process
library(cluster)
gap_stat <- clusGap(scaled_clean, FUN = kmeans, nstart = 10, d.power = 2,
                    K.max = 20, B = 500)
fviz_gap_stat(gap_stat)

# 30 indices method - majority rule recommendation is 3 clusters
library(NbClust)
nb <- NbClust(scaled_clean, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")
fviz_nbclust(nb)

# Based on these tests, going with 3 clusters for now. # 
kmean_3 <- kmeans(scaled_clean, 3)
kmean_3$centers
kmean_3
autoplot(kmean_3, scaled_clean, frame = TRUE)


### Visualizing clusters ####
library(GGally)
library(plotly)

# Group cluster results by mean for each variable in regression_clean dataframe
regression_clean$cluster <- as.factor(kmean_3$cluster)

df3_clus_avg <- regression_clean %>%
  group_by(cluster) %>%
  summarize_if(is.numeric, mean)


testtable <- df3_clus_avg %>% 
  dplyr::select(cimbenefits_comp,
                desccontactnorms_all_comp,
                age_true,
                cimperceivedrisk_comp,
                sr_31_able_to_call,
                sr_41c_ingenuity,
                sr_10_harm_you_personally_reversed,
                sr_11_harm_future_generations_reversed,
                injunctcontactnorms_all_comp,
                cluster,
                respondent_id) %>% 
  setNames(c("Interpersonal Discussion \n& Media Exposure","Descriptive Norms",
             "Age", "Perceived Risk", "PBC: Calling Ability", "Worldview: Ingenuity", 
             "Personal Harm", "Future Generations Harm", "Injunctive Norms", "Cluster", "respondent_id"))
write.csv(testtable,"/Users/natebender/Desktop/repo/RCthesisanalysis/output_tables/meanscores_clusters.csv", row.names = TRUE)

# Final EV list and indices
# 6"CIM benefits", 8"Descriptive contact norms", , 11"Perceived risk", 22 (but for some reason it's 21 below)"Efficacy: able to call", 12"CC personal harm",  13"CC harm future generations" 24"sr_41c_ingenuity "injunctcontactnorms_all_comp

library(data.table)
setDT(testtable)

forplot <- testtable %>% 
  dplyr::select(-Age)

forplot_long <- melt(data = forplot,
                     id.vars = c("Cluster", "respondent_id"),
                     variable.name = "variable",
                     value.name = "mean_value")
  
library("ggsci")
p <- forplot_long %>%
  ggplot(aes(x=fct_reorder(variable, mean_value, .desc=T), y=mean_value, shape=Cluster, color=Cluster)) +
  geom_point(size=5) +
  theme_minimal(base_size = 15)+
  theme(axis.text.x = element_text(angle=12, hjust=1, size=15),
        legend.key.size = unit(1.0, "cm"),
        legend.key = element_rect(color = NA, fill = NA),
        legend.title.align = 0.5) +
  scale_color_npg() +
  labs(x="\nVariable\n", y="\nMean Value\n")
p

forplotage <- testtable %>% 
  dplyr::select(Age, Cluster, respondent_id)
forplotage_long <- melt(data = forplotage,
                     id.vars = c("Cluster", "respondent_id"),
                     variable.name = "variable",
                     value.name = "mean_value")

age_plot <- forplotage_long %>%
  ggplot(aes(x=fct_reorder(variable, mean_value, .desc=T), y=mean_value, shape=Cluster, color=Cluster)) +
  geom_point(size=5) +
  theme_minimal(base_size = 15)+
  theme(legend.key.size = unit(1.0, "cm"),
        legend.key = element_rect(color = NA, fill = NA),
        legend.title.align = 0.5) +
  scale_color_npg() +
  labs(x="\nVariable\n", y="\nMean Value\n")
age_plot

# library(ggpubr)
# ggarrange(p, age_plot,
#           labels = c("A", "B"),
#           align = "h",
#           common.legend = T, 
#           legend = "right")

# Visualize clusters by raw numbers of responses
plot_1 <- c(6, 8, 10, 11)
regression_clean %>% 
  pivot_longer(cols = plot_1,
               names_to = "question", 
               values_to = "response") %>% 
  ggplot(aes(x = response, colour = cluster)) +
  facet_wrap(vars(question), ncol = 1) +
  geom_point(stat = "count") +
  geom_line(stat = "count") +
  labs(x = "Response", y = "Number of respondents") 

plot_2 <- c(12, 13, 22, 25)  # JC - low priority question but why in the hell aren't last two indices the same as above?!?!?? Gah. 
regression_clean %>% 
  pivot_longer(cols = plot_2,
               names_to = "question", 
               values_to = "response") %>% 
  ggplot(aes(x = response, colour = cluster)) +
  facet_wrap(vars(question), ncol = 1) +
  geom_point(stat = "count") +
  geom_line(stat = "count") +
  labs(x = "Response", y = "Number of respondents")


### Descriptive stats on clusters ####

# number of respondents in each cluster
counts <- regression_clean %>% 
  group_by(cluster) %>% 
  summarize(n = n(),
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
                sr_10_harm_you_personally_reversed,
                cimbenefits_comp,
                injunctcontactnorms_all_comp,
                desccontactnorms_all_comp) %>% 
  setNames(c("Cluster", "n", "Actual Contacted","Age","Perceived Risk","Future Generations Harm","Worldview: Ingenuity",
             "PBC: Calling Ability","Personal Harm","Interpersonal Discussion \n& Media Exposure",
             "Injunctive Norms","Descriptive Norms")) 

write.csv(counts, "/Users/natebender/Desktop/repo/RCthesisanalysis/output_tables/meanscores_clusters.csv", row.names = TRUE)

df3_clus_avg  # for means of continuous data grouped by cluster

library(purrr)
regression_clean %>%  # for categorical vars desc stats. List is in-progress, just experimenting at the moment. 
  dplyr::select(
    age_true,
    sr_72_income,
    sr_71_employment_status
  ) %>% 
  split(regression_clean$cluster) %>% 
  map(summary)

