#__author__ = "Nate Cutler"
#__email__ = "ncutler211@gmail.com"

# Load necessary libraries
library(tidyverse)
library(caret)
library(MASS)
library(ISLR)
library(RANN)
library(e1071)

# Read the diabetes data from a CSV file
diabetes_df <- read_csv("https://docs.google.com/spreadsheets/d/1nOExf5tmEUHsD27-leG3PNQ7el4Y5eRm5dfFfj7W6WM/gviz/tq?tqx=out:csv")

# Summarize and glimpse the diabetes dataset
glimpse(diabetes_df)
summary(diabetes_df)

# Visualize histograms of selected columns in the diabetes dataset
hist(diabetes_df$n_pregnant)
hist(diabetes_df$blood_glucose)
hist(diabetes_df$diastolic_bp)
hist(diabetes_df$skinfold_thickness)
hist(diabetes_df$serum_insulin)
hist(diabetes_df$bmi)
hist(diabetes_df$pedigree)
hist(diabetes_df$age)
hist(diabetes_df$positive_test)

# Convert the 'positive_test' column to factor
diabetes_df$positive_test = as.factor(diabetes_df$positive_test)
glimpse(diabetes_df)

# Create a copy of the dataset for imputation
diabetes_df_imp = diabetes_df

# Data imputation for certain columns with missing or zero values
diabetes_df_imp$blood_glucose[diabetes_df_imp$blood_glucose == 0] = NA
diabetes_df_imp$blood_glucose <- ifelse(is.na(diabetes_df_imp$blood_glucose), median(diabetes_df_imp$blood_glucose, na.rm=TRUE), diabetes_df_imp$blood_glucose)

diabetes_df_imp$diastolic_bp[diabetes_df_imp$diastolic_bp == 0] = NA
diabetes_df_imp$diastolic_bp <- ifelse(is.na(diabetes_df_imp$diastolic_bp), median(diabetes_df_imp$diastolic_bp, na.rm=TRUE), diabetes_df_imp$diastolic_bp)

diabetes_df_imp$skinfold_thickness[diabetes_df_imp$skinfold_thickness == 0] = NA
diabetes_df_imp$skinfold_thickness <- ifelse(is.na(diabetes_df_imp$skinfold_thickness), median(diabetes_df_imp$skinfold_thickness, na.rm=TRUE), diabetes_df_imp$skinfold_thickness)

diabetes_df_imp$bmi[diabetes_df_imp$bmi == 0] = NA
diabetes_df_imp$bmi <- ifelse(is.na(diabetes_df_imp$bmi), median(diabetes_df_imp$bmi, na.rm=TRUE), diabetes_df_imp$bmi)

# Summary of the imputed dataset
summary(diabetes_df_imp)

# Split the dataset into training and test sets
df_split <- createDataPartition(diabetes_df_imp$positive_test, p = 0.8, list = FALSE)
features_train <- diabetes_df_imp[df_split, !(names(diabetes_df_imp) %in% c('positive_test'))] 
features_test  <- diabetes_df_imp[-df_split, !(names(diabetes_df_imp) %in% c('positive_test'))]
target_train <- diabetes_df_imp[df_split, "positive_test"]
target_test <- diabetes_df_imp[-df_split, "positive_test"]

# Preprocess the training features using centering, scaling, and KNN imputation
preprocess_object = preProcess(features_train, method = c('center', 'scale', 'knnImpute'))
preprocess_object

# Determine the value for 'k' in KNN imputation
k_form = sqrt(length(target_test$positive_test))
k_form
k_form = 13

# Fit KNN model
knn_fit = knn3(features_train, target_train$positive_test, k = k_form)
knn_fit

# Predict using KNN model
knn_pred <- predict(knn_fit, features_test, type = 'class')

# Create predictions dataframe for KNN
predictions <- cbind(data.frame(target_test$positive_test, knn_pred))
summary(predictions)

# Fit logistic regression model
full_train <- cbind(features_train, target_train)
log_train <- glm(positive_test ~ ., family = 'binomial', data = full_train)
summary(log_train)

# Predict using logistic regression model
log_pred <- predict(log_train, newdata = features_test, type = 'response')
log_pred <- ifelse(log_pred >= 0.5, 1, 0)

# Update predictions dataframe with logistic regression predictions
predictions$log_pred <- factor(log_pred)
summary(predictions)

# Calculate errors for KNN and logistic regression predictions
predictions$knn_error <- ifelse(predictions$target_test != predictions$knn_pred, 1, 0)
predictions$log_error <- ifelse(predictions$target_test != predictions$log_pred, 1, 0)
summary(predictions)
