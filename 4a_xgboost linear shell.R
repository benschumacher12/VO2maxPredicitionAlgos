# Clear environment and console
rm(list = ls())
cat("\014")

# Load all needed packages
library(tidyverse)
library(Matrix)
library(xgboost)

# Change the NA action for this script, will reset at end of script
previous_na_action <- options('na.action')
options(na.action = 'na.pass')

# Disable scientific notation
options("scipen" = 100, "digits" = 4)

# Load the data
load("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Data/blsa.RData")
dat <- first_vo2_surv; rm(first_vo2_surv)

# Remove the ID variable
dat <- dat[ , !(colnames(dat) %in% c("idno"))]

# Make VO2max the first column in the dataframe
dat <- dat %>%
  relocate(vo2_max)

# The following variables are excluded in OPACH 
drops <- c("rgspeed", "ldcw47b", "uw150meters", "c400secs", "wkindex", "uw150speed",
           "mdcw21", "mdcw26", "hrstart", "hrend", "hakcal")
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### All obs, all preds ####
# Create data structure for xgboost
xgb_dat <- xgb.DMatrix(data = sparse.model.matrix(vo2_max ~ ., data = dat)[, -1],
                       label = dat$vo2_max)

# Create a grid of hyperparameters that we will search through
hyper_grid = expand.grid(
  nrounds = seq(370, 450, 20), # Boosting Iterations
  lambda = 0, # L2, Ridge Regularization
  alpha = seq(0, 3, 1), # L1, LASSO Regularization
  eta = seq(1, 2, 0.25) # Learning Rate
)

# Run the 10-fold CV with grid search
j <- "All obs, all preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/4b_xgboost linear source.R")

# Print the test and train RMSEs
allobs_allpreds_test_RMSE <- paste0("Test RMSE: ",
                               format(round(final_eval_log$test_rmse_mean[1], 2), nsmall = 2),
                               " +/- ",
                               format(round(final_eval_log$test_rmse_std[1], 2), nsmall = 2))

allobs_allpreds_train_RMSE <- paste0("Train RMSE: ",
                               format(round(final_eval_log$train_rmse_mean[1], 2), nsmall = 2),
                               " +/- ",
                               format(round(final_eval_log$train_rmse_std[1], 2), nsmall = 2))

# Save the logged information
allobs_allpreds_all_eval_log <- eval_log_individual_models; rm(eval_log_individual_models)
allobs_allpreds_min_eval_log <- min_eval_log; rm(min_eval_log)
allobs_allpreds_final_eval_log <- final_eval_log; rm(final_eval_log)
allobs_allpreds_final_xgb_linear_model <- final_xgb_linear; rm(final_xgb_linear)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### All obs, OPACH preds #### 
dat_opach <- dat[ , !(colnames(dat) %in% drops)]

xgb_dat <- xgb.DMatrix(data = sparse.model.matrix(vo2_max ~ ., data = dat_opach)[, -1],
                             label = dat_opach$vo2_max)

# Create a grid of hyperparameters that we will search through
hyper_grid = expand.grid(
  nrounds = seq(350, 430, 20), # Boosting Iterations
  lambda = 0, # L2, Ridge Regularization
  alpha = seq(0, 3, 1), # L1, LASSO Regularization
  eta = seq(1, 2, 0.25) # Learning Rate
)

# Run the 10-fold CV with grid search
j <- "All obs, OPACH preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/4b_xgboost linear source.R")

# Print the test and train RMSEs
allobs_opachpreds_test_RMSE <- paste0("Test RMSE: ",
                                    format(round(final_eval_log$test_rmse_mean[1], 2), nsmall = 2),
                                    " +/- ",
                                    format(round(final_eval_log$test_rmse_std[1], 2), nsmall = 2))

allobs_opachpreds_train_RMSE <- paste0("Train RMSE: ",
                                     format(round(final_eval_log$train_rmse_mean[1], 2), nsmall = 2),
                                     " +/- ",
                                     format(round(final_eval_log$train_rmse_std[1], 2), nsmall = 2))

# Save the logged information
allobs_opachpreds_all_eval_log <- eval_log_individual_models; rm(eval_log_individual_models)
allobs_opachpreds_min_eval_log <- min_eval_log; rm(min_eval_log)
allobs_opachpreds_final_eval_log <- final_eval_log; rm(final_eval_log)
allobs_opachpreds_final_xgb_linear_model <- final_xgb_linear; rm(final_xgb_linear)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Men, all preds #### 
dat_men <- dat[dat$sex_num_m1f0 == 1, ]

xgb_dat <- xgb.DMatrix(data = sparse.model.matrix(vo2_max ~ ., data = dat_men)[, -1],
                           label = dat_men$vo2_max)

hyper_grid = expand.grid(
  nrounds = seq(500, 700, 25), # Boosting Iterations
  lambda = 0, # L2, Ridge Regularization
  alpha = seq(0, 3, 1), # L1, LASSO Regularization
  eta = seq(1, 2, 0.25) # Learning Rate
)

# Run the 10-fold CV with grid search
j <- "Men, all preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/4b_xgboost linear source.R")

# Print the test and train RMSEs
men_allpreds_test_RMSE <- paste0("Test RMSE: ",
                                    format(round(final_eval_log$test_rmse_mean[1], 2), nsmall = 2),
                                    " +/- ",
                                    format(round(final_eval_log$test_rmse_std[1], 2), nsmall = 2))

men_allpreds_train_RMSE <- paste0("Train RMSE: ",
                                     format(round(final_eval_log$train_rmse_mean[1], 2), nsmall = 2),
                                     " +/- ",
                                     format(round(final_eval_log$train_rmse_std[1], 2), nsmall = 2))

# Save the logged information
men_allpreds_all_eval_log <- eval_log_individual_models; rm(eval_log_individual_models)
men_allpreds_min_eval_log <- min_eval_log; rm(min_eval_log)
men_allpreds_final_eval_log <- final_eval_log; rm(final_eval_log)
men_allpreds_final_xgb_linear_model <- final_xgb_linear; rm(final_xgb_linear)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Men, OPACH preds #### 
dat_men_opach <- dat[dat$sex_num_m1f0 == 1, !(colnames(dat) %in% drops)]

xgb_dat <- xgb.DMatrix(data = sparse.model.matrix(vo2_max ~ ., data = dat_men_opach)[, -1],
                                 label = dat_men_opach$vo2_max)

hyper_grid = expand.grid(
  nrounds = seq(500, 625, 25), # Boosting Iterations
  lambda = 0, # L2, Ridge Regularization
  alpha = seq(0, 3, 1), # L1, LASSO Regularization
  eta = seq(1, 2, 0.25) # Learning Rate
)

# Run the 10-fold CV with grid search
j <- "Men, OPACH preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/4b_xgboost linear source.R")

# Print the test and train RMSEs
men_opachpreds_test_RMSE <- paste0("Test RMSE: ",
                                 format(round(final_eval_log$test_rmse_mean[1], 2), nsmall = 2),
                                 " +/- ",
                                 format(round(final_eval_log$test_rmse_std[1], 2), nsmall = 2))

men_opachpreds_train_RMSE <- paste0("Train RMSE: ",
                                  format(round(final_eval_log$train_rmse_mean[1], 2), nsmall = 2),
                                  " +/- ",
                                  format(round(final_eval_log$train_rmse_std[1], 2), nsmall = 2))

# Save the logged information
men_opachpreds_opach_eval_log <- eval_log_individual_models; rm(eval_log_individual_models)
men_opachpreds_min_eval_log <- min_eval_log; rm(min_eval_log)
men_opachpreds_final_eval_log <- final_eval_log; rm(final_eval_log)
men_opachpreds_final_xgb_linear_model <- final_xgb_linear; rm(final_xgb_linear)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Women, all preds #### 
dat_women <- dat[dat$sex_num_m1f0 == 0, ]

xgb_dat <- xgb.DMatrix(data = sparse.model.matrix(vo2_max ~ ., data = dat_women)[, -1],
                             label = dat_women$vo2_max)

hyper_grid = expand.grid(
  nrounds = seq(175, 275, 25), # Boosting Iterations
  lambda = 0, # L2, Ridge Regularization
  alpha = seq(0, 3, 1), # L1, LASSO Regularization
  eta = seq(1, 2, 0.25) # Learning Rate
)

# Run the 10-fold CV with grid search
j <- "Women, all preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/4b_xgboost linear source.R")

# Print the test and train RMSEs
women_allpreds_test_RMSE <- paste0("Test RMSE: ",
                                 format(round(final_eval_log$test_rmse_mean[1], 2), nsmall = 2),
                                 " +/- ",
                                 format(round(final_eval_log$test_rmse_std[1], 2), nsmall = 2))

women_allpreds_train_RMSE <- paste0("Train RMSE: ",
                                  format(round(final_eval_log$train_rmse_mean[1], 2), nsmall = 2),
                                  " +/- ",
                                  format(round(final_eval_log$train_rmse_std[1], 2), nsmall = 2))

# Save the logged information
women_allpreds_all_eval_log <- eval_log_individual_models; rm(eval_log_individual_models)
women_allpreds_min_eval_log <- min_eval_log; rm(min_eval_log)
women_allpreds_final_eval_log <- final_eval_log; rm(final_eval_log)
women_allpreds_final_xgb_linear_model <- final_xgb_linear; rm(final_xgb_linear)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Women, OPACH preds #### 
dat_women_opach <- dat[dat$sex_num_m1f0 == 0, !(colnames(dat) %in% drops)]

xgb_dat <- xgb.DMatrix(data = sparse.model.matrix(vo2_max ~ ., data = dat_women_opach)[, -1],
                                   label = dat_women_opach$vo2_max)

hyper_grid = expand.grid(
  nrounds = seq(175, 300, 25), # Boosting Iterations
  lambda = 0, # L2, Ridge Regularization
  alpha = seq(0, 3, 1), # L1, LASSO Regularization
  eta = seq(1, 2, 0.25) # Learning Rate
)

# Run the 10-fold CV with grid search
j <- "Women, OPACH preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/4b_xgboost linear source.R")

# Print the test and train RMSEs
women_opachpreds_test_RMSE <- paste0("Test RMSE: ",
                                   format(round(final_eval_log$test_rmse_mean[1], 2), nsmall = 2),
                                   " +/- ",
                                   format(round(final_eval_log$test_rmse_std[1], 2), nsmall = 2))

women_opachpreds_train_RMSE <- paste0("Train RMSE: ",
                                    format(round(final_eval_log$train_rmse_mean[1], 2), nsmall = 2),
                                    " +/- ",
                                    format(round(final_eval_log$train_rmse_std[1], 2), nsmall = 2))

# Save the logged information
women_opachpreds_opach_eval_log <- eval_log_individual_models; rm(eval_log_individual_models)
women_opachpreds_min_eval_log <- min_eval_log; rm(min_eval_log)
women_opachpreds_final_eval_log <- final_eval_log; rm(final_eval_log)
women_opachpreds_final_xgb_linear_model <- final_xgb_linear; rm(final_xgb_linear)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
results <- data.frame(matrix(ncol = 3, nrow = 7))
colnames(results) <- c("Algorithm",
                       "Test RMSE",
                       "Train RMSE")

results$Algorithm[1] <- "Sample, Universe of Predictors"
results$Algorithm[2] <- "Total BLSA, All BLSA Predictors"
results$Algorithm[3] <- "Total BLSA, OPACH Predictors"
results$Algorithm[4] <- "BLSA Men, All BLSA Predictors"
results$Algorithm[5] <- "BLSA Men, OPACH Predictors"
results$Algorithm[6] <- "BLSA Women, All BLSA Predictors"
results$Algorithm[7] <- "BLSA Women, OPACH Predictors"

results$`Test RMSE`[2] <- allobs_allpreds_test_RMSE
results$`Test RMSE`[3] <- allobs_opachpreds_test_RMSE
results$`Test RMSE`[4] <- men_allpreds_test_RMSE
results$`Test RMSE`[5] <- men_opachpreds_test_RMSE
results$`Test RMSE`[6] <- women_allpreds_test_RMSE
results$`Test RMSE`[7] <- women_opachpreds_test_RMSE

results$`Train RMSE`[2] <- allobs_allpreds_train_RMSE
results$`Train RMSE`[3] <- allobs_opachpreds_train_RMSE
results$`Train RMSE`[4] <- men_allpreds_train_RMSE
results$`Train RMSE`[5] <- men_opachpreds_train_RMSE
results$`Train RMSE`[6] <- women_allpreds_train_RMSE
results$`Train RMSE`[7] <- women_opachpreds_train_RMSE

results$`Test RMSE` <- gsub("Test RMSE: ", "", results$`Test RMSE`)
results$`Train RMSE` <- gsub("Train RMSE: ", "", results$`Train RMSE`)

write.csv(results,
          "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/RMSEs/xgblinear_results.csv",
          row.names = FALSE)

#### Get variable importance scores #### 
x <- as.data.frame(xgb.importance(model = allobs_allpreds_final_xgb_linear_model))

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/xgblinear_allobs_allpred.csv",
          row.names = FALSE)

x <- as.data.frame(xgb.importance(model = allobs_opachpreds_final_xgb_linear_model))

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/xgblinear_allobs_opachpred.csv",
          row.names = FALSE)

x <- as.data.frame(xgb.importance(model = men_allpreds_final_xgb_linear_model))

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/xgblinear_men_allpred.csv",
          row.names = FALSE)

x <- as.data.frame(xgb.importance(model = men_opachpreds_final_xgb_linear_model))

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/xgblinear_men_opachpred.csv",
          row.names = FALSE)

x <- as.data.frame(xgb.importance(model = women_allpreds_final_xgb_linear_model))

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/xgblinear_women_allpred.csv",
          row.names = FALSE)

x <- as.data.frame(xgb.importance(model = women_opachpreds_final_xgb_linear_model))

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/xgblinear_women_opachpred.csv",
          row.names = FALSE)

#### Save the image ####
save.image("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Workspaces/ML Results/xgblinear.RData")
# load("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Workspaces/ML Results/xgblinear.RData")

#### Clear environment and console ####
rm(list = ls())
cat("\014")
