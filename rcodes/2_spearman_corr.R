#========================================================================#
# Title: Spearman correlation
# Author: Jom Research (https://jomresearch.netlify.app/)
# Date: March11, 2024
#========================================================================#

# Packages ----------------------------------------------------------------

library(tidyverse) #data manipulation
library(DescTools) #lillie test


# Data --------------------------------------------------------------------

spearman_data <- read.csv("data/correlation.csv")


# Normality ---------------------------------------------------------------

## Normality ----
# 1. Visually
spearman_data %>% 
  ggplot(aes(Rape)) +
  geom_histogram() 

spearman_data %>% 
  ggplot(aes(sample = Rape)) +
  geom_qq() +
  geom_qq_line() 

spearman_data %>% 
  ggplot(aes(Assault)) +
  geom_histogram() 

spearman_data %>% 
  ggplot(aes(sample = Assault)) +
  geom_qq() +
  geom_qq_line() 

# 2. Statistical tests
shapiro.test(spearman_data$Rape)
LillieTest(spearman_data$Rape)


# Pearson correlation -----------------------------------------------------

cor.test(spearman_data$Rape, spearman_data$Assault, method = "spearman")






