# Regression ####
### Setup data ####
library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)
library(ggfortify)
library(here)

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
default_idx = caret::createDataPartition(as.factor(data_for_regression$sr_12a_actions_contacted_officials_binary), p = 0.75, list = FALSE)
default_train = data_for_regression[default_idx, ]
default_test = data_for_regression[-default_idx, ]

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
library(MASS)
logit_final_aic <- stepAIC(logit_final, direction="backward")

# age gets dropped from the model
summary(logit_final_aic)
for_csv <- tidy(logit_final_aic)
write.csv(for_csv,"/Users/natebender/Desktop/repo//RCthesisanalysis/output_tables/thesis_pastactionregmodel.csv", row.names = TRUE)

# TESTING DATA
logit_final_test <- glm(sr_12a_actions_contacted_officials_binary~
                          cimbenefits_comp+
                          desccontactnorms_all_comp+
                          cimperceivedrisk_comp+
                          sr_31_able_to_call+
                          sr_41c_ingenuity+
                          sr_10_harm_you_personally_reversed+
                          sr_11_harm_future_generations_reversed+
                          injunctcontactnorms_all_comp,
                        data=default_test,family=binomial)
summary(logit_final_test)

### Validation tests  ####
# the kappa statistic is a measure of how closely the instances classified by the machine learning classifier matched the data labeled as ground truth, controlling for the accuracy of a random classifier as measured by the expected accuracy
library(blorr)
blr_confusion_matrix(logit_final_aic)
blr_confusion_matrix(logit_final_test)


blr_model_fit_stats(logit_final_aic)

# Variable inflation factors (rule of thumb: under 5 shows no worrisome collinearity)
vif(logit_final_aic)

library(lmtest)
lrtest(logit_final_aic, logit_final) #not sig different than saturated model, all else being equal -- go with simpler model
lrtest(logit_final_aic) #sig diff from null model, which is good

#Hosmer-Lemeshow GOF test --> sensitive to group number, not good for binary predictors
#https://stats.stackexchange.com/questions/186219/how-many-groups-to-use-in-hosmer-and-lemeshow-test
# no sig values --> safe to say that there is no evidence of poor model fit
library(ResourceSelection)
for (i in 4:15) {
  print(hoslem.test(default_train$sr_12a_actions_contacted_officials_binary, fitted(logit_final_aic), g=i) $p.value)
} 

# Via Cook's Distance - no evidence of influential points 
plot(logit_final_aic)

### Visualizing model #### 
# JC - should I be visualizing this using the training or testing model? My feeling is visualize using the training model, 
# and only use the testing model to talk about accuracy score via cross-validation. Correct?

# Exponentiated coefficients - showing in terms of odds ratios, not log odds
library(broom)
m1_log_preds = tidy(logit_final_aic, conf.int = T, exponentiate = T) %>%
mutate(Model = "Past contact")
m1_log_predstest <- subset(m1_log_preds, term !="(Intercept)") #& p.value < 0.05)  # remove intercept
m1_log_predstest <- m1_log_predstest %>%
  mutate(term = c("CIM benefits", "Descriptive norms", "Perceived risk", "Efficacy: able to call", "Human ingenuity", "CC personal harm",  "CC harm future generations", "Injunctive norms"))
write.csv(m1_log_predstest,"/Users/natebender/Desktop/repo//RCthesisanalysis/output_tables/thesis_EXPpastactionregmodel.csv", row.names = TRUE)


# Plot
#pdf("ThesisPastaction_oddsratio_plot.pdf") # starts writing a PDF to file
png("ThesisPastaction_oddsratio_plot.png") # starts writing a PDF to file
zp1 <- ggplot(m1_log_predstest, aes(colour = Model))
zp1 <- zp1 + geom_hline(yintercept = 1, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes(x = term, ymin = conf.low,
                                ymax = conf.high),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes(x = term, y = estimate, ymin = conf.low,
                                 ymax = conf.high),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + coord_flip() + theme_bw()
INTzp1_log <- zp1 + ggtitle("What influences past representative contact?") + ylab("Odds Ratio") + xlab("Variable")
print(INTzp1_log)  # The trick to these is position_dodge().
dev.off()


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
         injunctcontactnorms_all_comp)

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

# Average silhouette method - suggests 2
fviz_nbclust(scaled_clean, kmeans, method = "silhouette")

# Gap statistic - suggests 11 clusters - need to figure out why this is suggesting wildly higher # of clusters than other validation tests. 
# nstart option attempts multiple initial configurations and reports on the best one. 
# For example, adding nstart=25 will generate 25 initial random centroids and choose the best one for the algorithm.
# kmax sets upper limit of clusters to test against
# B is the number of bootstrapped samples the function uses to test against. The reference dataset is generated using Monte Carlo simulations of the sampling process
library(cluster)
gap_stat <- clusGap(scaled_clean, FUN = kmeans, nstart = 10,
                    K.max = 20, B = 20)
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

# Final EV list and indices
# 6"CIM benefits", 8"Descriptive contact norms", , 11"Perceived risk", 22 (but for some reason it's 21 below)"Efficacy: able to call", 12"CC personal harm",  13"CC harm future generations" 24"sr_41c_ingenuity "injunctcontactnorms_all_comp

p <- ggparcoord(
  data = df3_clus_avg, 
  columns = c(6, 8, 10, 11, 12, 13, 21, 24), # If I could find a way to call these by name instead of index that would be great
  groupColumn = "cluster", 
  scale = "std", # univariately, subtract mean and divide by standard deviation
  order = c(24, 6, 8, 10, 21, 11, 12, 13)) + 
  labs(x = "Predictive variables", 
       y = "Mean value", 
       title = "Mean scores by cluster for\nthe regression predictive variables") +
  theme(axis.text.x=element_text(angle=7, hjust=1))
ggplotly(p)

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

plot_2 <- c(12, 13, 22, 25)  # why in the hell aren't last two indices the same as above?!?!?? Gah. 
regression_clean %>% 
  pivot_longer(cols = plot_2,
               names_to = "question", 
               values_to = "response") %>% 
  ggplot(aes(x = response, colour = cluster)) +
  facet_wrap(vars(question), ncol = 1) +
  geom_point(stat = "count") +
  geom_line(stat = "count") +
  labs(x = "Response", y = "Number of respondents")



