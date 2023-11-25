#Nathaniel Cutler


library(tidyverse)
library(rpart.plot)
library(caret)
library(randomForest)

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

#Checking test error
tree_preds <- predict(air_tree, newdata = features_test)
tree_mse <- mean((tree_preds - target_test$price)^2)
sqrt(tree_mse) #decision tree is off an average of $102.07

#Pruning tree to address overfitting
tree_pruned <- prune(air_tree, cp = 0.045)
rpart.plot(tree_pruned)

#Rule of thumb formula m=p/3
#Rule of thumb formula for classification trees m=sqrt(p)
#m is number of features available for each split
#p is total number of features in dataset


#fitting a random forest model
rf_train <- randomForest(price ~ ., data = training, mtry = 6)
#Checking how much a given feature reduces the RSS on average across all trees in forest
importance(rf_train)

#Checking test error of random forest
rf_preds <- predict(rf_train, newdata = features_test)
rf_mse <- mean((rf_preds - target_test$price)^2)
sqrt(rf_mse) #now its only off an average of $86.81 much better







