#========================================================================#
# Title: Simple linear regression
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: March11, 2024
#========================================================================#

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation
library(summarytools) #exploration


# Data --------------------------------------------------------------------

lr_data <- read.csv("data/linear_regression.csv")


# Explore -----------------------------------------------------------------

skimr::skim(lr_data)
freq(lr_data$species)
descr(lr_data %>% select(-species))

# Univariable analysis ----------------------------------------------------

slr_mod <- lm(sepal_length ~ sepal_width, data = lr_data)
summary(slr_mod)


# Assumptions -------------------------------------------------------------

lr_data$res <- resid(slr_mod)
lr_data$pred <- fitted.values(slr_mod)

# 1) Normality
lr_data %>% 
  ggplot(aes(res)) +
  geom_histogram() 

lr_data %>% 
  ggplot(aes(sample = res)) +
  geom_qq() +
  geom_qq_line() 

# 2) Overall linearity
# 3) Equal variance
ggplot(lr_data, aes(pred, res)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2)

# 4) Linearity of cont IVs 
ggplot(lr_data, aes(sepal_width, res)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2)


# Model fit ---------------------------------------------------------------

# Adjusted R^2
summary(slr_mod)
# The model explain 0.5% of the variance of the outcome


# Final model -------------------------------------------------------------

summary(slr_mod)
confint(slr_mod)


# More advanced -----------------------------------------------------------

# Table presentation with gtsummary
library(gtsummary)

slr_mod %>% 
  tbl_regression(
    label = sepal_width ~ "Sepal Width",
    pvalue_fun = function (x) style_pvalue(x, digits = 3)
  )

