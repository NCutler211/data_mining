#__author__ = "Nate Cutler"
#__email__ = "ncutler211@gmail.com"

# Load necessary libraries
library(tidyverse)
library(caret)

# Read the parking data from a CSV file
parking <- read_csv("https://docs.google.com/spreadsheets/d/11ahddH6snm10AuxF51MlOISx2yCsJ2WmPKFdfRFpInU/gviz/tq?tqx=out:csv")

# Summarize and glimpse the parking dataset
summary(parking)
glimpse(parking)

# Summarize factors in parking dataset
summary(factor(parking$body_style))
summary(factor(parking$make))
summary(factor(parking$color))
summary(factor(parking$agency))
summary(factor(parking$plate))
summary(factor(parking$violation_code))

# Capturing top 4 body styles and filtering dataframe
top_bodys <- parking %>%  
  group_by(body_style) %>%  
  summarize(total_obs = n()) %>%
  top_n(4)
parking <- parking %>%
  filter(body_style %in% top_bodys$body_style)

# Corrections in 'make' and 'color' columns
parking$make  = ifelse(parking$make == 'MERC', "MEBZ", parking$make)
parking$make  = ifelse(parking$make == 'TOYD', "TOYT", parking$make)
parking$color  = ifelse(parking$color == 'SI', "SL", parking$color)
parking$color  = ifelse(parking$color == 'WI', "WT", parking$color)

# Data cleaning and manipulation for 'fine' column
parking$fine = parking$fine %>% str_replace('\\$','')
parking$fine = as.integer(parking$fine)
parking$fine <- ifelse(is.na(parking$fine), median(parking$fine, na.rm=TRUE), parking$fine)

# Data cleaning and formatting for 'violation' column
parking$violation = parking$violation %>%
  str_replace_all('\\s','_') %>%
  str_replace_all('\\|','-') %>%
  str_remove('[.]')

# Read the 'costs' data from a CSV file
costs <- read_csv("https://docs.google.com/spreadsheets/d/1WUD22BH836zFaNp7RM5YlNVgSLzo6E_t-sznxzjVf9E/gviz/tq?tqx=out:csv")

# Visualize relationship between 'bodyfat' and 'bmi'
ggplot(costs, aes(x = costs$bodyfat, y = costs$bmi)) +
  geom_point() +
  geom_smooth(method = 'lm')

# Data preprocessing for 'region' in 'costs' dataset
costs = costs %>%
  select(-bodyfat)
my_dummies <- dummyVars(~ region, data = costs, fullRank = TRUE)
my_dummies_pred <- predict(my_dummies, newdata = costs)
costs <- cbind(costs, my_dummies_pred)
costs = costs %>%
  select(-region)

# Data preprocessing for 'children', 'age', and 'bmi' in 'costs' dataset
costs$children <- ifelse(costs$children >= 1, "yes", "no")
costs$age <- scale(costs$age)
costs$bmi <- scale(costs$bmi)

# Fit a linear regression model
charge_mod <- lm(charges ~ age + sex + bmi + children + smoker + regionnorth_west + regionsouth_east + regionsouth_west, data = costs)
summary(charge_mod)
