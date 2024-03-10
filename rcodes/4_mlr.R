#========================================================================#
# Title: Multiple linear regression
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: March11, 2024
#========================================================================#

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation
library(summarytools) #exploration


# Data --------------------------------------------------------------------

lr_data <- read.csv("data/linear_regression.csv")

lr_data <- 
  lr_data %>% 
  mutate(species = as.factor(species),
         species = fct_recode(species, 
                              Setosa = "0",
                              Versicolor = "1",
                              Virginica = "2"))


# Explore -----------------------------------------------------------------

skimr::skim(lr_data)
freq(lr_data$species)
descr(lr_data %>% select(-species))

# Univariable analysis ----------------------------------------------------

zero_mod <- lm(sepal_length ~ 1, data = lr_data)
add1(zero_mod, scope = ~ sepal_width + petal_width + species, test = "F")

# slr_mod <- lm(sepal_length ~ sepal_width, data = lr_data)
# summary(slr_mod)


# Multivariable analysis --------------------------------------------------

# Variable selection
full_mod <- lm(sepal_length ~., data = lr_data)

#1) Forward
forw <- step(zero_mod, scope =~ sepal_width + petal_width + species, direction = "forward")

#2) Backward
back <- step(full_mod, direction = "backward")

#3) Stepwise
# both = step(full_mod, direction = "both")
# summary(both)

AIC(forw, back) #both and back has the lowest AIC, also can look at adjusted R^2

prefinal_mod <- back
summary(prefinal_mod)


# Interaction -------------------------------------------------------------

int_mod1 <- update(prefinal_mod, .~. + sepal_width*petal_width)
summary(int_mod1) 

int_mod2 <- update(prefinal_mod, .~. + sepal_width*species)
summary(int_mod2) 

int_mod3 <- update(prefinal_mod, .~. + petal_width*species)
summary(int_mod3) 

# All are not significant

# Assumptions -------------------------------------------------------------

lr_data$res <- resid(prefinal_mod)
lr_data$pred <- fitted.values(prefinal_mod)

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

ggplot(lr_data, aes(petal_width, res)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2)


# Outliers and influential observations -----------------------------------

out_data <- cooks.distance(prefinal_mod)  
sum(out_data > 1)


# Multicollinearity -------------------------------------------------------

car::vif(prefinal_mod) #all aGSIF should be < 3.16 


# Model fit ---------------------------------------------------------------

# Adjusted R^2
summary(prefinal_mod)
# The model explain 73.2% of the variance of the outcome


# Final model -------------------------------------------------------------

final_mod <- prefinal_mod
summary(final_mod)


# More advanced -----------------------------------------------------------

# Table presentation with gtsummary
library(gtsummary)

final_mod %>% 
  tbl_regression(
    label = list(sepal_width ~ "Sepal Width",
                 petal_width ~ "Petal Width",
                 species ~ "Species"),
    estimate_fun = purrr::partial(style_ratio, digits = 2),
    pvalue_fun = function (x) style_pvalue(x, digits = 3)
  )

