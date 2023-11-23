#Nathaniel Cutler
#_credits_ Nick DiRienzo contributed the structure and some of the code

install.packages("rpart.plot")

library(tidyverse)
library(rpart.plot)
library(caret)

air <- read_csv("https://docs.google.com/spreadsheets/d/1Lk1v7oXp5B_jK2R84a5PtUAGkdNYQb_Tekl9PkKi7p8/gviz/tq?tqx=out:csv")
air <- air %>% mutate_if(is.character, as.factor)
summary(air)[,2:7] 

# Testing with a subset before full execution
air <- sample_n(air, 5000)

# Splitting into training & test sets because random forest models take the form of feature ~ target I utilize a single data frame
split_index <- createDataPartition(air$price, p = 0.8, list = F)

# use index to split data
training <- air[split_index,]
features_test <- air[-split_index, !(colnames(air) %in% c('price'))]
target_test <- air[-split_index, 'price']

# Creating a single training data tree
air_tree <- rpart(price ~ . ,data = training)
air_tree$variable.importance

#Plotting tree
rpart.plot(air_tree)

