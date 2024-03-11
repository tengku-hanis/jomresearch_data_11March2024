#========================================================================#
# Title: Pearson correlation
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: March11, 2024
#========================================================================#

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation
library(DescTools) #lillie test


# Data --------------------------------------------------------------------

pearson_data <- read.csv("data/correlation.csv")


# Assumptions ---------------------------------------------------------------

## Normality ----
# 1. Visually
pearson_data %>% 
  ggplot(aes(Murder)) +
  geom_histogram() 

pearson_data %>% 
  ggplot(aes(sample = Murder)) +
  geom_qq() +
  geom_qq_line() 

pearson_data %>% 
  ggplot(aes(Assault)) +
  geom_histogram() 

pearson_data %>% 
  ggplot(aes(sample = Assault)) +
  geom_qq() +
  geom_qq_line() 

# 2. Statistical tests
shapiro.test(pearson_data$Murder)
LillieTest(pearson_data$Murder)

shapiro.test(pearson_data$Assault)
LillieTest(pearson_data$Assault)

## Linearity ----

pearson_data %>% 
  ggplot(aes(Murder, Assault)) +
  geom_point()

## Outliers ----

pearson_data %>% 
  ggplot(aes(Murder)) +
  geom_boxplot()

pearson_data %>% 
  ggplot(aes(Assault)) +
  geom_boxplot()


# Pearson correlation -----------------------------------------------------

cor.test(pearson_data$Murder, pearson_data$Assault, method = "pearson")





 
