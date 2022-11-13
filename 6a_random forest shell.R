# Clear environment and console
rm(list = ls())
cat("\014")

# Load all needed packages
library(tidyverse)
library(randomForest)
library(Metrics)
library(varImp)

library(devtools)
# install_github('araastat/reprtree')
library(reprtree)

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
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#### All obs, all preds ####
dat_rf <- dat

# Establish a grid of hyperparameters
hyper_grid <- expand.grid(ntrees = seq(400, 700, 100),
                          mtry = seq(2, (ncol(dat_rf) - 1), length.out = 5),
                          sampsize = seq(0.6, 0.8, by = 0.1),
                          node_size = seq(5, 10),
                          OOB_RMSE = 0)

j <- "All obs, all preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/6b_random forest insert.R")

final_rf_allobs_allpreds <- final_rf_model
allobs_allpreds_total_RMSE <- rmse
allobs_allpreds_minOOB_RMSE <- min(hyper_grid$OOB_RMSE)

hyper_grid <- hyper_grid %>% arrange(OOB_RMSE)
hyper_grid_allobs_allpreds <- hyper_grid; rm(hyper_grid)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### All obs, OPACH preds #### 
dat_opach <- dat[ , !(colnames(dat) %in% drops)]
dat_rf <- dat_opach

# Establish a grid of hyperparameters
hyper_grid <- expand.grid(ntrees = seq(700, 1000, 100),
                          mtry = seq(2, (ncol(dat_rf) - 1), length.out = 5),
                          sampsize = seq(0.6, 0.8, by = 0.1),
                          node_size = seq(5, 10),
                          OOB_RMSE = 0)

j <- "All obs, OPACH preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/6b_random forest insert.R")

final_rf_allobs_opachpreds <- final_rf_model
allobs_opachpreds_total_RMSE <- rmse
allobs_opachpreds_minOOB_RMSE <- min(hyper_grid$OOB_RMSE)

hyper_grid <- hyper_grid %>% arrange(OOB_RMSE)
hyper_grid_allobs_opachpreds <- hyper_grid; rm(hyper_grid)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Men, all preds #### 
dat_men <- dat[dat$sex_num_m1f0 == 1, ]
dat_rf <- dat_men

# Establish a grid of hyperparameters
hyper_grid <- expand.grid(ntrees = seq(500, 800, 100),
                          mtry = seq(2, (ncol(dat_rf) - 1), length.out = 5),
                          sampsize = seq(0.6, 0.8, by = 0.1),
                          node_size = seq(5, 10),
                          OOB_RMSE = 0)
j <- "Men, all preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/6b_random forest insert.R")

final_rf_men_allpreds <- final_rf_model
men_allpreds_total_RMSE <- rmse
men_allpreds_minOOB_RMSE <- min(hyper_grid$OOB_RMSE)

hyper_grid <- hyper_grid %>% arrange(OOB_RMSE)
hyper_grid_men_allpreds <- hyper_grid; rm(hyper_grid)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Men, OPACH preds #### 
dat_men_opach <- dat[dat$sex_num_m1f0 == 1, !(colnames(dat) %in% drops)]
dat_rf <- dat_men_opach

# Establish a grid of hyperparameters
hyper_grid <- expand.grid(ntrees = seq(500, 800, 100),
                          mtry = seq(2, (ncol(dat_rf) - 1), length.out = 5),
                          sampsize = seq(0.6, 0.8, by = 0.1),
                          node_size = seq(5, 10),
                          OOB_RMSE = 0)

j <- "Men, OPACH preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/6b_random forest insert.R")

final_rf_men_opachpreds <- final_rf_model
men_opachpreds_total_RMSE <- rmse
men_opachpreds_minOOB_RMSE <- min(hyper_grid$OOB_RMSE)

hyper_grid <- hyper_grid %>% arrange(OOB_RMSE)
hyper_grid_men_opachpreds <- hyper_grid; rm(hyper_grid)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Women, all preds #### 
dat_women <- dat[dat$sex_num_m1f0 == 0, ]
dat_rf <- dat_women

# Establish a grid of hyperparameters
hyper_grid <- expand.grid(ntrees = seq(700, 1000, 100),
                          mtry = seq(2, (ncol(dat_rf) - 1), length.out = 5),
                          sampsize = seq(0.6, 0.8, by = 0.1),
                          node_size = seq(5, 10),
                          OOB_RMSE = 0)
j <- "Women, all preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/6b_random forest insert.R")

final_rf_women_allpreds <- final_rf_model
women_allpreds_total_RMSE <- rmse
women_allpreds_minOOB_RMSE <- min(hyper_grid$OOB_RMSE)

hyper_grid <- hyper_grid %>% arrange(OOB_RMSE)
hyper_grid_women_allpreds <- hyper_grid; rm(hyper_grid)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Women, OPACH preds #### 
dat_women_opach <- dat[dat$sex_num_m1f0 == 0, !(colnames(dat) %in% drops)]
dat_rf <- dat_women_opach

# Establish a grid of hyperparameters
hyper_grid <- expand.grid(ntrees = seq(700, 1000, 100),
                          mtry = seq(2, (ncol(dat_rf) - 1), length.out = 5),
                          sampsize = seq(0.6, 0.8, by = 0.1),
                          node_size = seq(5, 10),
                          OOB_RMSE = 0)

j <- "Women, OPACH preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/6b_random forest insert.R")

final_rf_women_opachpreds <- final_rf_model
women_opachpreds_total_RMSE <- rmse
women_opachpreds_minOOB_RMSE <- min(hyper_grid$OOB_RMSE)

hyper_grid <- hyper_grid %>% arrange(OOB_RMSE)
hyper_grid_women_opachpreds <- hyper_grid; rm(hyper_grid)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Get and export feature importance scores ####
x <- as.data.frame(final_rf_allobs_allpreds$importance)

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/randomforest_allobs_allpreds.csv",
          row.names = TRUE)

x <- as.data.frame(final_rf_allobs_opachpreds$importance)

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/randomforest_allobs_opachpreds.csv",
          row.names = TRUE)

x <- as.data.frame(final_rf_men_allpreds$importance)

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/randomforest_men_allpreds.csv",
          row.names = TRUE)

x <- as.data.frame(final_rf_men_opachpreds$importance)

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/randomforest_men_opachpreds.csv",
          row.names = TRUE)

x <- as.data.frame(final_rf_women_allpreds$importance)

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/randomforest_women_allpreds.csv",
          row.names = TRUE)

x <- as.data.frame(final_rf_women_opachpreds$importance)

write.csv(x,
          file = "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/Importance Scores/randomforest_women_opachpreds.csv",
          row.names = TRUE)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Store all results ####
results <- data.frame(matrix(ncol = 2, nrow = 7))
colnames(results) <- c("Algorithm",
                       "OOB RMSE")

results$Algorithm[1] <- "Sample, Universe of Predictors"
results$Algorithm[2] <- "Total BLSA, All BLSA Predictors"
results$Algorithm[3] <- "Total BLSA, OPACH Predictors"
results$Algorithm[4] <- "BLSA Men, All BLSA Predictors"
results$Algorithm[5] <- "BLSA Men, OPACH Predictors"
results$Algorithm[6] <- "BLSA Women, All BLSA Predictors"
results$Algorithm[7] <- "BLSA Women, OPACH Predictors"

results$`OOB RMSE`[2] <- allobs_allpreds_minOOB_RMSE
results$`OOB RMSE`[3] <- allobs_opachpreds_minOOB_RMSE
results$`OOB RMSE`[4] <- men_allpreds_minOOB_RMSE
results$`OOB RMSE`[5] <- men_opachpreds_minOOB_RMSE
results$`OOB RMSE`[6] <- women_allpreds_minOOB_RMSE
results$`OOB RMSE`[7] <- women_opachpreds_minOOB_RMSE

write.csv(results,
          "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/RMSEs/randomforest_results.csv",
          row.names = FALSE)

#### Save the image ####
save.image("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Workspaces/ML Results/randomforest.RData")
# load("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Workspaces/ML Results/randomforest.RData")

#### Clear environment and console ####
rm(list = ls())
cat("\014")
