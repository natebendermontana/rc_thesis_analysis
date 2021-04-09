# Thesis data analysis ####
# National Survey; Aug 3-9, 2020

# Load data, packages, libraries  ####

# Packages / libraries
install.packages("psych")
install.packages("GPArotation")
install.packages("factoextra")
library(psych)
library(GPArotation)
library(tidyverse)
library(dplyr)
library(factoextra)
library(ggfortify)

# Load data
setwd("/Users/natebender/Desktop/Repo")
d <- read.csv("/Users/natebender/Desktop/Graduate_School/Thesis/All-for-1.5/Analysis/dyn_national_20200828 - results-20200930-082116_TEST.csv")

d_accept <- d %>% 
  filter(reject==0)

# EDA ####
# Number of respondents / rejects
respondents <- d %>% 
  filter(reject==0) %>% 
  count(respondent_id)

rejected <- d %>% 
  filter(reject==1) %>% 
  count(respondent_id)

# EDA --IN PROGRESS--
d %>% 
  filter(reject==0) %>%
  count(age) 
  
  
  summarize(meanage = mean(age),
            minage = min(age),
            maxage = max(age),
            total = n())


# PCA Principal Components Analysis ####
# PCA - DYNAMIC NORMS
d_pca_dynamicnorms <- d_accept %>% 
  select(
    matches("sr_24"),
    matches("sr_25"))

fa.parallel(d_pca_dynamicnorms, fm='minres', fa='fa')
pca1 <- fa(d_pca_dynamicnorms, nfactors = 1, rotate = "Varimax", fm="minres") 
print(pca1)
print(pca1$loadings,cutoff = 0.3)
fa.diagram(pca1)

# PCA - DESCRIPTIVE NORMS
d_pca_descriptivenorms <- d_accept %>% 
  select(
    matches("sr_19a"),
    matches("sr_19b"),
    matches("sr_19c"),
    matches("sr_19d"),
    matches("sr_19e"),
    matches("sr_19f"),
    matches("sr_19g"),
    matches("sr_19h"),
    matches("sr_20a"),
    matches("sr_20b"),
    matches("sr_20c"),
    matches("sr_20d"),
    matches("sr_20e"),
    matches("sr_20f"),
    matches("sr_20g"),
    matches("sr_20h"))
    
fa.parallel(d_pca_descriptivenorms, fm='minres', fa='fa')
pca1 <- fa(d_pca_descriptivenorms, nfactors = 5, rotate = "Varimax", fm="minres") 
print(pca1)
print(pca1$loadings,cutoff = 0.3)

# Biplot of loadings
autoplot(pca1,x=1,y=2,alpha=0.1)
fa.diagram(pca1)

# PCA - INJUNCTIVE NORMS
d_pca_injunctivenorms <- d_accept %>% 
  select(
    matches("sr_22a"),
    matches("sr_22b"),
    matches("sr_22c"),
    matches("sr_22d"),
    matches("sr_22e"),
    matches("sr_22f"),
    matches("sr_22g"),
    matches("sr_22h"),
    matches("sr_23a"),
    matches("sr_23b"),
    matches("sr_23c"),
    matches("sr_23d"),
    matches("sr_23e"),
    matches("sr_23f"),
    matches("sr_23g"),
    matches("sr_23h"))

fa.parallel(d_pca_injunctivenorms, fm='minres', fa='fa')
pca1 <- fa(d_pca_injunctivenorms, nfactors = 5, rotate = "Varimax", fm="minres") 
print(pca1)
print(pca1$loadings,cutoff = 0.3)

# Biplot of loadings
autoplot(pca1,x=1,y=2,alpha=0.1)
fa.diagram(pca1)







