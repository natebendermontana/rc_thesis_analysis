

### linear regression with future contact intentions
```{r, eval=T, echo=F, include=F}

lm_contacting <- glm(sr_13a_takeactions_contact_officials ~ 
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
                       sr_41e_govt_do_more + sr_41f_equity, data = train)

lm_contacting_aic <- stepAIC(lm_contacting, direction="backward")
```

```{r}
summary(lm_contacting_aic)
```

```{r}
# Need to research validation testing for linear regression. Lift curve via ROCR only works with logistic. 
```


```{r, eval=T, echo=F, include=T}
# model significance
## liklihood ratio tests
lrtest(lm_contacting_aic, lm_contacting) #not sig different than saturated model, which is good. All else equal -- go with simpler model
lrtest(lm_contacting_aic) #sig diff from null model, which is good

# model diagnostics
par(mfrow=c(2,2)) #this command creates a plot grid with 2 rows and 2 columns to view the diagnostic plots all at once
plot(lm_contacting_aic)
# Residuals vs fitted: Checks linear relationship. Want a horizontal line w/ no distinct patterns. VIOLATED.
# Q-Q plot: Checks if residuals are normally distributed. VIOLATED. 
# Scale-Location: Checks homogeneity of variance. Want a horizontal line w/ no distinct patterns. VIOLATED. 
# Residuals vs leverage: Checks influential cases via Cook's D. It plots Cook's D as a dotted line (if you can't see a dotted line that's great!). Any points outside of the Cook's D dotted line might be high leverage points. THIS IS GOOD. 

dev.off() #this turns the plot grid off

# Linear relationship is violated. 
# normality is violated
# heteroscedastic
# no influential points though


# Add observations indices and
# drop some columns (.se.fit, .sigma) for simplification
model.diag.metrics <- augment(lm_contacting_aic)

model.diag.metrics <- model.diag.metrics %>%
  mutate(index = 1:nrow(model.diag.metrics)) %>%
  select(index, everything(), -.sigma)
# Inspect the data
head(model.diag.metrics, 4)

# check specific points according to Cook's D 
model.diag.metrics %>%
  top_n(3, wt = .cooksd)

plot(lm_contacting_aic, 4)


## normality
sf.test(lm_contacting_aic$resid) #non significant p-value means normality assumption is not violated. 

## model gof
#Hosmer-Lemeshow GOF test --> sensitive to group number, not good for binary predictors
#https://stats.stackexchange.com/questions/186219/how-many-groups-to-use-in-hosmer-and-lemeshow-test
for (i in 4:15) {
  print(hoslem.test(regression_clean$sr_13a_takeactions_contact_officials, fitted(lm_contacting_aic), g=i) $p.value)
} #no sig values --> shows no evidence of poor model fit
```

### Graph the Intentions regression

```{r}
m1_preds = tidy(lm_contacting_aic, conf.int = T) %>%
  mutate(Model = "Future intentions")
m1_predstest <- subset(m1_preds, term !="(Intercept)" & p.value < 0.05)  # remove intercept
#m1_predstest <- m1_predstest %>% 
#  mutate(term2 = c("Group efficacy", "Personal efficacy", "Descriptive norms: CC advocates", "Place attachment", "CC concern"))

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot
pdf("Thesisfutureintentions_plot_p_05.pdf") # starts writing a PDF to file
png("Thesisfutureintentions_plot_p_05.png") # starts writing a PDF to file
zp1 <- ggplot(m1_predstest, aes(colour = Model))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes(x = term, ymin = conf.low,
                                ymax = conf.high),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes(x = term, y = estimate, ymin = conf.low,
                                 ymax = conf.high),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + coord_flip() + theme_bw()
INTzp1 <- zp1 + ggtitle("What influences future climate action intentions?") + ylab("Association") + xlab("Variable")
print(INTzp1)  # The trick to these is position_dodge().
dev.off()

write.csv(m1_predstest,"/Users/natebender/Desktop/ThesisIntentionsregmodels.csv", row.names = TRUE)


print(INTzp1) # prints plot to screen after it's been saved to file
```





























### Ordinal logistic regression. model selection using AIC w/ backwards selection
```{r, eval=T, echo=F, include=F}
# Build initial ordinal logistic regression model with all potential variables included
OLR_saturated <- polr(sr_12a_actions_contacted_officials ~ 
                        age_true + race_white_dumvar + gender_dumvar + children_dumvar + 
                        sr_75_religion_dumvar + sr_56_marital_status + sr_61_education + 
                        sr_71_employment_status + sr_72_income + sr_78_political_affiliation + 
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
                        sr_41e_govt_do_more + sr_41f_equity,data = OLR_complete_cases, Hess = T)

OLR_aic <- stepAIC(OLR_saturated, direction="backward")
```

$$\\[.05in]$$
  #### Break out final model for readability
  ```{r, eval=T, echo=F, include=T}
summary(OLR_aic)

olr_aic2 <- polr(sr_12a_actions_contacted_officials ~ 
                   age_true +
                   #desccontactnorms_all_comp +
                   descrolemodelnorms_all_comp + 
                   injunctcontactnorms_all_comp +
                   cimbenefits_comp +
                   cimperceivedrisk_comp +
                   sr_10_harm_you_personally_reversed +
                   sr_11_harm_future_generations_reversed + 
                   sr_41c_ingenuity,
                 data = OLR_complete_cases,
                 Hess = T)

brant(olr_aic2)
```

$$\\[.05in]$$
  #### Interpreting final model coefficients and intercepts
  ```{r, eval=F, echo=F, include=T}
# get the p-values from the final model and store the coefficient table
ctable <- round(coef(summary(OLR_aic)), 4)
# calculate and store p-values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = F) * 2
# combine coefficient table and p-values table
(ctable <- cbind(ctable, "p value" = round(p, 4)))
```

#### Interpreting proportional odds ratios and CIs
```{r, eval=T, echo=F, include=T}
# # Add CIs for variables and for interpreting the proportional odds ratios. 
# (ci <- confint(fit1_OLR)) # default method gives profiled CIs
# confint.default(fit1_OLR) # CIs assuming normality
# exp(coef(fit1_OLR))
# exp(cbind(OR = coef(fit1_OLR), ci))

# get confidence intervals
# profiled CIs
ci <- round(confint(OLR_aic), 4)
# log odd coefficients
or <- round(coef(OLR_aic), 4)
# convert coefficients into odds ratio, combine with CIs
round(exp(cbind(OR = or, ci)), 4)

# Visualizing the variable coefficients
install.packages("jtools")
install.packages("ggstance")
library(ggstance)
library(jtools)
jtools::plot_summs(OLR_aic, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
```
*Non-significant predictors:*
  Age
Descriptive norms related to role models
Belief on whether CC will harm you personally (sr_10 harm you personally)

*Significant predictors, coefficients, and proportional odds ratios, in descending order of leverage:*
  Descriptive Contact Norms: 0.77
For a one unit increase in the composite variable of descriptive contact norms, the odds of moving from "no contact" to "one contact" or "more than one contact" (in the last twelve months) are 2.17 times greater, given that the other variables in the model are held constant.

CIM Benefits (interpersonal discussion / hearing about CC): 0.58
For a one unit increase in composite variable of CIM Benefits, the odds of moving from "no contact" to "one contact" or "more than one contact" (in the last twelve months) are 1.78 times greater, given that the other variables in the model are held constant.

Perceived Risk: 0.54
For a one unit increase in composite variable of CIM Benefits, the odds of moving from "no contact" to "one contact" or "more than one contact" (in the last twelve months) are 1.72 times greater, given that the other variables in the model are held constant.

sr_11 How much will CC harm future generations?: -0.49
For a one unit increase in composite variable of CIM Benefits, the odds of moving from "no contact" to "one contact" or "more than one contact" (in the last twelve months) are 0.61 times greater, given that the other variables in the model are held constant.

Injunctive Contact Norms: 0.47
For a one unit increase in composite variable of CIM Benefits, the odds of moving from "no contact" to "one contact" or "more than one contact" (in the last twelve months) are 1.60 times greater, given that the other variables in the model are held constant.

sr_41 Human ingenuity will ensure we do not make the Earth unlivable: -0.35
For a one unit increase in composite variable of CIM Benefits, the odds of moving from "no contact" to "one contact" or "more than one contact" (in the last twelve months) are 0.70 times greater, given that the other variables in the model are held constant.

*Discussion*
  Descriptive Contact Norms, CIM Benefits, Perceived Risk, Injunctive Contact Norms all have a positive effect on propensity to contact officials. The more a person perceives positive descriptive norms related to contacting officials; perceives positive injunctive norms related to contacting officials; hears about CC from friends, media, & public figures; or perceives CC as a risk, the more likely they are to contact officials. It's interesting that hearing about CC more often (through interpersonal discussion, mass media, or from public figures) is predictive of contacting officials along with the other more intuitive factors. 

Conversely, the more a person believes CC will harm future generations or the more they agree with the statement that "human ingenuity will ensure we do not make the Earth unlivable", the less likely they are to contact officials. The first could be explained by the person believing that CC is more an issue for the distant future; if CC will only harm future generations, why bother contacting officials now? Additionally, the ecological worldview that human ingenuity will solve CC may align with a person thinking CC is more a technological problem than a social one, and this worldview may align with the person perceiving less risk from CC since they see CC as a problem that will inevitably be solved by human innovation. 

$$\\[.05in]$$
#### Multi-collinearity
```{r, eval=T, echo=F, include=T}
# social norms factors
ggpairs(OLR_complete_cases[, c(2:6)], title = "Correlation Plot between social norms variables")

# cimbenefits, perceivedrisk, personal harm, future generations harm, efficacy - contacting officials, efficacy - effectiveness orgs, efficacy - competency/responsiveness orgs
ggpairs(OLR_complete_cases[, c(7:13)], title = "1/3 vars Correlation Plot")

#  behavioral attitudes, easy/able to call
ggpairs(OLR_complete_cases[, c(14:19)], title = "2/3 vars Correlation Plot")

# worldviews
ggpairs(OLR_complete_cases[, c(20:25)], title = "3/3 vars Correlation Plot")

# Variable inflation factor test. Need to double-check that the "No intercept: vifs may not be sensible" warning msg isn't throwing things off. 
# Below 2 should be the cutoff for showing no evidence of collinearity. This is violated. 
car::vif(OLR_aic)
```


#### Parallel regression assumption via Brant's test 
```{r, eval=T, echo=F, include=T}
library(brant)
# Testing parallel regression assumption
# If the relationship between all pairs of groups is the same, then there is only one set of coefficients, which means that there is only one model. If this assumption is violated, different models are needed to describe the relationship between each pair of outcome groups.
# A p-value of less than 0.05 on this test, particularly on the Omnibus plus at least one of the variables, should be interpreted as a failure of the proportional odds assumption.
brant(OLR_aic)

# If the proportional odds assumption is not met, one can use a multinomial logistic regression model, an adjacent-categories logistic model, or a partial proportional odds model.


# Second approach to test the parallel lines assumption visually. 
# Code from the web. Needs tweaking. 

# # Predict the probability (p) of diabete positivity
# probabilities <- predict(fit1_OLR, type = "response")
# predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# head(predicted.classes)
# 
# mydata <- PimaIndiansDiabetes2 %>%
#   dplyr::select_if(is.numeric) 
# predictors <- colnames(mydata)
# 
# # Bind the logit and tidying the data for plot
# mydata <- mydata %>%
#   mutate(logit = log(probabilities/(1-probabilities))) %>%
#   gather(key = "predictors", value = "predictor.value", -logit)
# Create the scatter plots:
# ggplot(mydata, aes(logit, predictor.value))+
#   geom_point(size = 0.5, alpha = 0.5) +
#   geom_smooth(method = "loess") + 
#   theme_bw() + 
#   facet_wrap(~predictors, scales = "free_y") 
```
The Omnibus shows a probability of 0, along with desccontactnorms_all_comp (p=.02), failing the Brant test / proportional odds assumption. This means that something other than the ordinal linear regression approach must be used. 

#### Model goodness-of-fit (Lipsitz & McFadden's pseudo-R^2)
```{r, eval=F, echo=F, include=T}
# The Lipsitz test is a goodness of fit test for ordinal response logistic regression models.
# Since the null hypothesis is a good model fit, low p-values indicate potential problems with the model.
generalhoslem::lipsitz.test(OLR_aic)

# values of .2 to .4 for the McF's pseudo-R^2 represent an excellent fit.
PseudoR2(OLR_aic)

# Need to work on the vector of categorical variables somehow
# cats <- c("race_white_dumvar", "gender_dumvar", "children_dumvar", "sr_75_religion_dumvar", "sr_56_marital_status", "sr_61_education", "sr_71_employment_status", "sr_72_income","sr_78_political_affiliation","sr_79_political_leaning","sr_7_believe_about_climate_change")

# generalhoslem::pulkrob.chisq(fit1_OLR, catvars = c("race_white_dumvar", "gender_dumvar", "children_dumvar", "sr_75_religion_dumvar", "sr_56_marital_status", "sr_61_education", "sr_71_employment_status", "sr_72_income","sr_78_political_affiliation","sr_79_political_leaning","sr_7_believe_about_climate_change"))
```
Lipsitz test of p=.03; low p-value indicate likely problems with the model. However a McFadden's pseudo R^2 indicates great model fit? Don't know how to interpret that wrinkle in light of the model failing the Lipsitz test and the Brant test. 


#### Influential factors / outliers via Cook's Distance
```{r, eval=F, echo=F, include=T}
# Cook's Distance test that I haven't figured out yet for OLR. 
# plot(fit1_OLR, which = 4, id.n = 3)
# 
# model.data <- augment(fit1_OLR) %>% 
#   mutate(index = 1:n())
# 
# model.data %>% top_n(3, .cooksd)
```
