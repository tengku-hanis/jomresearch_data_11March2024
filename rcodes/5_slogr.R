#========================================================================#
# Title: Simple logistic regression
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
library(DescTools) #pseudoR2


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

slogr_mod <- glm(admit ~ gpa, family = binomial(), data = log_data)
summary(slogr_mod)


# Assumptions -------------------------------------------------------------

# 1) Linearity of cont IV with logit transformed DV

log_data <- 
  log_data %>% 
  mutate(LNgpa = log(gpa) * gpa)

update(slogr_mod, .~. + LNgpa) %>% 
  summary()


# Model fit ---------------------------------------------------------------

# 1) Classification table
ClassLog(slogr_mod, slogr_mod$y) 

# 2) Hosmer-lemeshow test
hoslem.test(slogr_mod$y, fitted(slogr_mod), g=10)

# 3) ROC-AUC
roc_curve <- roc(slogr_mod$y, fitted(slogr_mod), ci = T, percent = T)
plot(roc_curve, print.auc = T)

# 4) Pseudo R^2
PseudoR2(slogr_mod, which = c("CoxSnell", "Nagel"))

# Final model -------------------------------------------------------------

summary(slogr_mod)
cbind(exp(slogr_mod$coefficients), exp(confint(slogr_mod))) %>% 
  round(digits = 2)


# More advanced -----------------------------------------------------------

# Table presentation with gtsummary
library(gtsummary)

slogr_mod %>% 
  tbl_regression(
    label = gpa ~ "GPA",
    exponentiate = T,
    pvalue_fun = function (x) style_pvalue(x, digits = 3)
  )

