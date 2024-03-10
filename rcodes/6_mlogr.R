#========================================================================#
# Title: Multiple logistic regression
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: March11, 2024
#========================================================================#

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation
library(summarytools) #exploration
library(readstata13) #read stata data
library(QuantPsyc) #classification table
library(ResourceSelection) #hosmer-lemeshow test
library(pROC) #roc-auc


# Data --------------------------------------------------------------------

log_data <- read.dta13("data/logistic_regression.dta")

log_data <- 
  log_data %>% 
  mutate(admit = as.factor(admit),
         rank = as.factor(rank))

# Explore -----------------------------------------------------------------

skimr::skim(log_data)
freq(log_data)
descr(log_data)

# Univariable analysis ----------------------------------------------------

zero_mod <- glm(admit ~ 1, family = binomial(), data = log_data)
add1(zero_mod, scope = ~ gre + gpa + rank, test = "F")

# prefinal_mod <- glm(admit ~ gpa, family = binomial(), data = log_data)
# summary(prefinal_mod)


# Multivariable analysis --------------------------------------------------

# Variable selection
full_mod <- glm(admit ~., family = binomial(), data = log_data)

#1) Forward
forw <- step(zero_mod, scope = ~ gre + gpa + rank, direction = "forward")

#2) Backward
back <- step(full_mod, direction = "backward")

#3) Stepwise
# both = step(full_mod, direction = "both")
# summary(both)

AIC(forw, back) #both and back has the lowest AIC

prefinal_mod <- back
summary(prefinal_mod)


# Interaction -------------------------------------------------------------

int_mod1 <- update(prefinal_mod, .~. + gre * gpa)
summary(int_mod1) 

int_mod2 <- update(prefinal_mod, .~. + gre * rank)
summary(int_mod2) 

int_mod3 <- update(prefinal_mod, .~. + gpa * rank)
summary(int_mod3) 

# All are not significant


# Assumptions -------------------------------------------------------------

# 1) Linearity of cont IV with logit transformed DV

log_data <- 
  log_data %>% 
  mutate(LNgpa = log(gpa) * gpa,
         LNgre = log(gre) * gre)

update(prefinal_mod, .~. + LNgpa + LNgre) %>% 
  summary()


# Outliers and influential observations -----------------------------------

out_data <- cooks.distance(prefinal_mod)  
sum(out_data > 1)


# Multicollinearity -------------------------------------------------------

car::vif(prefinal_mod) #all aGSIF should be < 3.16 


# Model fit ---------------------------------------------------------------

# 1) Classification table
ClassLog(prefinal_mod, log_data$admit) 

# 2) Hosmer-lemeshow test
hoslem.test(prefinal_mod$y, fitted(prefinal_mod), g=10)

# 3) ROC-AUC
roc_curve <- roc(prefinal_mod$y, fitted(prefinal_mod), ci = T, percent = T)
plot(roc_curve, print.auc = T)

# 4) Pseudo R^2
PseudoR2(prefinal_mod, which = c("Nagel", "CoxSnell"))

# Final model -------------------------------------------------------------

final_mod <- prefinal_mod 
summary(final_mod)


# More advanced -----------------------------------------------------------

# Table presentation with gtsummary
library(gtsummary)

final_mod %>% 
  tbl_regression(
    label = list(gre ~ "GRE score",
                 gpa ~ "GPA",
                 rank ~ "Rank of university"),
    exponentiate = T,
    pvalue_fun = function (x) style_pvalue(x, digits = 3)
  )
