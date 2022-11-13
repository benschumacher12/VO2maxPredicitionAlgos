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
  nrounds = seq(90, 110, 20),
  max_depth = seq(2, 4, 1),
  eta = seq(0.0, .2, .1),
  alpha = seq(4, 6, 1),
  subsample = seq(.60, .80, .10),
  colsample_bytree = seq(.8, 1, .1))

# Run the 10-fold CV with grid search
j <- "All obs, all preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/5b_xgboost tree source.R")

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
allobs_allpreds_final_xgb_tree_model <- final_xgb_tree; rm(final_xgb_tree)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### All obs, OPACH preds #### 
dat_opach <- dat[ , !(colnames(dat) %in% drops)]

xgb_dat <- xgb.DMatrix(data = sparse.model.matrix(vo2_max ~ ., data = dat_opach)[, -1],
                             label = dat_opach$vo2_max)

# Create a grid of hyperparameters that we will search through
hyper_grid = expand.grid(
  nrounds = seq(80, 100, 20),
  max_depth = seq(2, 6, 1),
  eta = seq(0.0, .2, .1),
  alpha = seq(0, 4, 1),
  subsample = seq(.70, .90, .10),
  colsample_bytree = seq(.75, 1, .25))

# Run the 10-fold CV with grid search
j <- "All obs, OPACH preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/5b_xgboost tree source.R")

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
allobs_opachpreds_final_xgb_tree_model <- final_xgb_tree; rm(final_xgb_tree)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Men, all preds #### 
dat_men <- dat[dat$sex_num_m1f0 == 1, ]

xgb_dat <- xgb.DMatrix(data = sparse.model.matrix(vo2_max ~ ., data = dat_men)[, -1],
                           label = dat_men$vo2_max)

hyper_grid = expand.grid(
  nrounds = seq(80, 100, 20),
  max_depth = seq(2, 6, 1),
  eta = seq(0.0, .2, .1),
  alpha = seq(2, 4, 1),
  subsample = seq(.60, .80, .10),
  colsample_bytree = seq(.75, 1, .25))

# Run the 10-fold CV with grid search
j <- "Men, all preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/5b_xgboost tree source.R")

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
men_allpreds_final_xgb_tree_model <- final_xgb_tree; rm(final_xgb_tree)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Men, OPACH preds #### 
dat_men_opach <- dat[dat$sex_num_m1f0 == 1, !(colnames(dat) %in% drops)]

xgb_dat <- xgb.DMatrix(data = sparse.model.matrix(vo2_max ~ ., data = dat_men_opach)[, -1],
                                 label = dat_men_opach$vo2_max)

hyper_grid = expand.grid(
  nrounds = seq(40, 80, 20),
  max_depth = 3,
  eta = seq(0.0, .2, .1),
  alpha = 4,
  subsample = seq(.60, .80, .10),
  colsample_bytree = seq(.75, 1, .25))

# Run the 10-fold CV with grid search
j <- "Men, OPACH preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/5b_xgboost tree source.R")

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
men_opachpreds_all_eval_log <- eval_log_individual_models; rm(eval_log_individual_models)
men_opachpreds_min_eval_log <- min_eval_log; rm(min_eval_log)
men_opachpreds_final_eval_log <- final_eval_log; rm(final_eval_log)
men_opachpreds_final_xgb_tree_model <- final_xgb_tree; rm(final_xgb_tree)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Women, all preds #### 
dat_women <- dat[dat$sex_num_m1f0 == 0, ]

xgb_dat <- xgb.DMatrix(data = sparse.model.matrix(vo2_max ~ ., data = dat_women)[, -1],
                             label = dat_women$vo2_max)

hyper_grid = expand.grid(
  nrounds = seq(80, 120, 20),
  max_depth = seq(1, 2, 1),
  eta = seq(0.0, .2, .1),
  alpha = seq(1, 2, 1),
  subsample = seq(.50, .70, .10),
  colsample_bytree = seq(.75, 1, .25))

# Run the 10-fold CV with grid search
j <- "Women, all preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/5b_xgboost tree source.R")

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
women_allpreds_final_xgb_tree_model <- final_xgb_tree; rm(final_xgb_tree)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Women, OPACH preds #### 
dat_women_opach <- dat[dat$sex_num_m1f0 == 0, !(colnames(dat) %in% drops)]

xgb_dat <- xgb.DMatrix(data = sparse.model.matrix(vo2_max ~ ., data = dat_women_opach)[, -1],
                                   label = dat_women_opach$vo2_max)

hyper_grid = expand.grid(
  nrounds = seq(100, 140, 20),
  max_depth = seq(1, 2, 1),
  eta = seq(0.0, .2, .1),
  alpha = seq(3, 6, 1),
  subsample = seq(.50, .80, .10),
  colsample_bytree = seq(.75, 1, .25))

# Run the 10-fold CV with grid search
j <- "Women, OPACH preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/5b_xgboost tree source.R")

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
women_opachpreds_all_eval_log <- eval_log_individual_models; rm(eval_log_individual_models)
women_opachpreds_min_eval_log <- min_eval_log; rm(min_eval_log)
women_opachpreds_final_eval_log <- final_eval_log; rm(final_eval_log)
women_opachpreds_final_xgb_tree_model <- final_xgb_tree; rm(final_xgb_tree)
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
          "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/RMSEs/xgbtree_results.csv",
          row.names = FALSE)

#### Get variable importance scores #### 
x <- as.data.frame(xgb.importance(model = allobs_allpreds_final_xgb_tree_model))
x <- x[ , c("Feature", "Gain")]
x <- x %>% arrange(desc(abs(Gain)))

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/xgbtree_allobs_allpred.csv",
          row.names = FALSE)

x <- as.data.frame(xgb.importance(model = allobs_opachpreds_final_xgb_tree_model))
x <- x[ , c("Feature", "Gain")]
x <- x %>% arrange(desc(abs(Gain)))

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/xgbtree_allobs_opachpred.csv",
          row.names = FALSE)

x <- as.data.frame(xgb.importance(model = men_allpreds_final_xgb_tree_model))
x <- x[ , c("Feature", "Gain")]
x <- x %>% arrange(desc(abs(Gain)))

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/xgbtree_men_allpred.csv",
          row.names = FALSE)

x <- as.data.frame(xgb.importance(model = men_opachpreds_final_xgb_tree_model))
x <- x[ , c("Feature", "Gain")]
x <- x %>% arrange(desc(abs(Gain)))

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/xgbtree_men_opachpred.csv",
          row.names = FALSE)

x <- as.data.frame(xgb.importance(model = women_allpreds_final_xgb_tree_model))
x <- x[ , c("Feature", "Gain")]
x <- x %>% arrange(desc(abs(Gain)))

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/xgbtree_women_allpred.csv",
          row.names = FALSE)

x <- as.data.frame(xgb.importance(model = women_opachpreds_final_xgb_tree_model))
x <- x[ , c("Feature", "Gain")]
x <- x %>% arrange(desc(abs(Gain)))

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/xgbtree_women_opachpred.csv",
          row.names = FALSE)

#### Save the image ####
save.image("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Workspaces/ML Results/xgbtree.RData")
# load("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Workspaces/ML Results/xgbtree.RData")

# Reset NA action
options(na.action = previous_na_action$na.action)

#### Clear environment and console ####
rm(list = ls())
cat("\014")
