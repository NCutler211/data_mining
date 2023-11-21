#__author__ = "Nate Cutler"
#__email__ = "ncutler211@gmail.com"

# Load necessary libraries
library(tidyverse)
library(caret)
library(ggplot2)

# Read the credit data from a CSV file
credit <- read_csv("https://docs.google.com/spreadsheets/d/1jFkOKgD5NGeD8mDj_42oBNJfFVK42-1cMKk0JxVFxeA/gviz/tq?tqx=out:csv")

# Create dummy variables for credit_score and preprocess the data
cr_dummy = dummyVars(credit_score ~ ., data = credit, fullRank = TRUE)
cr = predict(cr_dummy, newdata = credit) 
cr = data.frame(cr)
credit_vals = credit %>% select(credit_score)
cr = cbind(cr, credit_vals) 
cr = cr %>% mutate(credit_score = factor(ifelse(cr$credit_score == 'good', 1, 0))) 
glimpse(cr) 

# Function to calculate test error using k-fold cross-validation
error_calc = function(split_pro, folds, kn) {
  split_index = createDataPartition(cr$credit_score, p = split_pro, list = FALSE, times = folds)
  
  error_df = data.frame(matrix(ncol = 2, nrow = ncol(split_index)))
  colnames(error_df) = c('test_error', 'fold')
  
  for(i in 1:nrow(error_df)){
    features_train = cr[ split_index[,i], !(names(cr) %in% c('credit_score'))] 
    features_test  = cr[-split_index[,i], !(names(cr) %in% c('credit_score'))]
    target_train = cr[ split_index[,i], "credit_score"]
    target_test = cr[-split_index[,i], "credit_score"]
    
    preprocess_object = preProcess(features_train, 
                                   method = c('scale', 'center', 'knnImpute'))
    features_train = predict(preprocess_object, features_train)
    features_test = predict(preprocess_object, features_test)
    
    knn_fit = knn3(features_train, target_train, k = kn)
    knn_pred = predict(knn_fit, features_test, type = 'class' )
    
    error = mean(ifelse(target_test != knn_pred, 1, 0))
    error_df[i,'test_error'] = error
    error_df[i, 'fold'] = i
  }
  return(error_df)
}

# Calculate test error for different values of k using k-fold cross-validation
k10 = error_calc(.7, 10, 10)
k1 = error_calc(.7, 10, 1)
k11 = error_calc(.7, 10, 11)
k50 = error_calc(.7, 10, 50)

# Plotting test error for different values of k
ggplot(k50,
       aes(x = fold, y = test_error)) +
  geom_line(color = 'red') +
  geom_line(aes(x = k1$fold, y = k1$test_error), color = 'blue') +
  geom_line(aes(x = k10$fold, y = k10$test_error), color = 'green') +
  geom_line(aes(x = k11$fold, y = k11$test_error), color = 'orange')

# Calculate mean test error for different values of k
k10m = mean(k10$test_error)
k1m = mean(k1$test_error)
k11m = mean(k11$test_error)
k50m = mean(k50$test_error)
