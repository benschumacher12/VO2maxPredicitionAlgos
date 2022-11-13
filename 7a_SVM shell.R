# Clear environment and console
rm(list = ls())
cat("\014")

# Load all needed packages
library(tidyverse)
library(e1071)
library(Metrics)
library(caret)
library(kernlab)

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
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#### All obs, all preds ####
dat_svm <- dat

# Tune gamma and cost parameters in the SVM models
hyper_grid = expand.grid(
  g = seq(0, 0.020, .002),
  c = seq(2, 4.0, 0.2),
  train_rmse_fold1 = 0,
  train_rmse_fold2 = 0,
  train_rmse_fold3 = 0,
  train_rmse_fold4 = 0,
  train_rmse_fold5 = 0,
  train_rmse_fold6 = 0,
  train_rmse_fold7 = 0,
  train_rmse_fold8 = 0,
  train_rmse_fold9 = 0,
  train_rmse_fold10 = 0,
  test_rmse_fold1 = 0,
  test_rmse_fold2 = 0,
  test_rmse_fold3 = 0,
  test_rmse_fold4 = 0,
  test_rmse_fold5 = 0,
  test_rmse_fold6 = 0,
  test_rmse_fold7 = 0,
  test_rmse_fold8 = 0,
  test_rmse_fold9 = 0,
  test_rmse_fold10 = 0)

j <- "All obs, all preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/7b_SVM insert.R")

hyper_grid <- hyper_grid %>% mutate(test_rmse_mean = select(., c("test_rmse_fold1",
                                                                "test_rmse_fold2",
                                                                "test_rmse_fold3",
                                                                "test_rmse_fold4",
                                                                "test_rmse_fold5",
                                                                "test_rmse_fold6",
                                                                "test_rmse_fold7",
                                                                "test_rmse_fold8",
                                                                "test_rmse_fold9",
                                                                "test_rmse_fold10")) %>% rowMeans())

hyper_grid <- hyper_grid %>% mutate(train_rmse_mean = select(., c("train_rmse_fold1",
                                                                 "train_rmse_fold2",
                                                                 "train_rmse_fold3",
                                                                 "train_rmse_fold4",
                                                                 "train_rmse_fold5",
                                                                 "train_rmse_fold6",
                                                                 "train_rmse_fold7",
                                                                 "train_rmse_fold8",
                                                                 "train_rmse_fold9",
                                                                 "train_rmse_fold10")) %>% rowMeans())

hyper_grid <- hyper_grid %>% select(-contains(c("fold")))
hyper_grid <- hyper_grid %>% arrange(test_rmse_mean)

svm_model <- svm(vo2_max ~ .,
                 data = dat_svm,
                 kernal = "radial",
                 gamma = hyper_grid$g[1],
                 cost = hyper_grid$c[1],
                 na.action = na.omit)

final_svm_allobs_allpreds <- svm_model
allobs_allpreds_all_eval_log <- hyper_grid

hyper_grid$g[1]
hyper_grid$c[1]
sort(unique(hyper_grid$g))
sort(unique(hyper_grid$c))
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### All obs, OPACH preds #### 
dat_opach <- dat[ , !(colnames(dat) %in% drops)]
dat_svm <- dat_opach

# Tune gamma and cost parameters in the SVM models
hyper_grid = expand.grid(
  g = seq(0, 0.020, .002),
  c = seq(3.0, 5.4, 0.2),
  train_rmse_fold1 = 0,
  train_rmse_fold2 = 0,
  train_rmse_fold3 = 0,
  train_rmse_fold4 = 0,
  train_rmse_fold5 = 0,
  train_rmse_fold6 = 0,
  train_rmse_fold7 = 0,
  train_rmse_fold8 = 0,
  train_rmse_fold9 = 0,
  train_rmse_fold10 = 0,
  test_rmse_fold1 = 0,
  test_rmse_fold2 = 0,
  test_rmse_fold3 = 0,
  test_rmse_fold4 = 0,
  test_rmse_fold5 = 0,
  test_rmse_fold6 = 0,
  test_rmse_fold7 = 0,
  test_rmse_fold8 = 0,
  test_rmse_fold9 = 0,
  test_rmse_fold10 = 0)

j <- "All obs, OPACH preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/7b_SVM insert.R")

hyper_grid <- hyper_grid %>% mutate(test_rmse_mean = select(., c("test_rmse_fold1",
                                                                 "test_rmse_fold2",
                                                                 "test_rmse_fold3",
                                                                 "test_rmse_fold4",
                                                                 "test_rmse_fold5",
                                                                 "test_rmse_fold6",
                                                                 "test_rmse_fold7",
                                                                 "test_rmse_fold8",
                                                                 "test_rmse_fold9",
                                                                 "test_rmse_fold10")) %>% rowMeans())

hyper_grid <- hyper_grid %>% mutate(train_rmse_mean = select(., c("train_rmse_fold1",
                                                                  "train_rmse_fold2",
                                                                  "train_rmse_fold3",
                                                                  "train_rmse_fold4",
                                                                  "train_rmse_fold5",
                                                                  "train_rmse_fold6",
                                                                  "train_rmse_fold7",
                                                                  "train_rmse_fold8",
                                                                  "train_rmse_fold9",
                                                                  "train_rmse_fold10")) %>% rowMeans())

hyper_grid <- hyper_grid %>% select(-contains(c("fold")))
hyper_grid <- hyper_grid %>% arrange(test_rmse_mean)

svm_model <- svm(vo2_max ~ .,
                 data = dat_svm,
                 kernal = "radial",
                 gamma = hyper_grid$g[1],
                 cost = hyper_grid$c[1],
                 na.action = na.omit)

final_svm_allobs_opachpreds <- svm_model
allobs_opachpreds_all_eval_log <- hyper_grid

hyper_grid$g[1]
hyper_grid$c[1]
sort(unique(hyper_grid$g))
sort(unique(hyper_grid$c))
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Men, all preds #### 
dat_men <- dat[dat$sex_num_m1f0 == 1, ]
dat_svm <- dat_men

# Tune gamma and cost parameters in the SVM models
hyper_grid = expand.grid(
  g = seq(0.0001, 0.05, 0.01),
  c = seq(2, 4.00, 0.05),
  train_rmse_fold1 = 0,
  train_rmse_fold2 = 0,
  train_rmse_fold3 = 0,
  train_rmse_fold4 = 0,
  train_rmse_fold5 = 0,
  train_rmse_fold6 = 0,
  train_rmse_fold7 = 0,
  train_rmse_fold8 = 0,
  train_rmse_fold9 = 0,
  train_rmse_fold10 = 0,
  test_rmse_fold1 = 0,
  test_rmse_fold2 = 0,
  test_rmse_fold3 = 0,
  test_rmse_fold4 = 0,
  test_rmse_fold5 = 0,
  test_rmse_fold6 = 0,
  test_rmse_fold7 = 0,
  test_rmse_fold8 = 0,
  test_rmse_fold9 = 0,
  test_rmse_fold10 = 0)

j <- "Men, all preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/7b_SVM insert.R")

hyper_grid <- hyper_grid %>% mutate(test_rmse_mean = select(., c("test_rmse_fold1",
                                                                 "test_rmse_fold2",
                                                                 "test_rmse_fold3",
                                                                 "test_rmse_fold4",
                                                                 "test_rmse_fold5",
                                                                 "test_rmse_fold6",
                                                                 "test_rmse_fold7",
                                                                 "test_rmse_fold8",
                                                                 "test_rmse_fold9",
                                                                 "test_rmse_fold10")) %>% rowMeans())

hyper_grid <- hyper_grid %>% mutate(train_rmse_mean = select(., c("train_rmse_fold1",
                                                                  "train_rmse_fold2",
                                                                  "train_rmse_fold3",
                                                                  "train_rmse_fold4",
                                                                  "train_rmse_fold5",
                                                                  "train_rmse_fold6",
                                                                  "train_rmse_fold7",
                                                                  "train_rmse_fold8",
                                                                  "train_rmse_fold9",
                                                                  "train_rmse_fold10")) %>% rowMeans())

hyper_grid <- hyper_grid %>% select(-contains(c("fold")))
hyper_grid <- hyper_grid %>% arrange(test_rmse_mean)

svm_model <- svm(vo2_max ~ .,
                 data = dat_svm,
                 kernal = "radial",
                 gamma = hyper_grid$g[1],
                 cost = hyper_grid$c[1],
                 na.action = na.omit)

final_svm_men_allpreds <- svm_model
men_allpreds_all_eval_log <- hyper_grid

hyper_grid$g[1]
hyper_grid$c[1]
sort(unique(hyper_grid$g))
sort(unique(hyper_grid$c))
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Men, OPACH preds #### 
dat_men_opach <- dat[dat$sex_num_m1f0 == 1, !(colnames(dat) %in% drops)]
dat_svm <- dat_men_opach

# Tune gamma and cost parameters in the SVM models
hyper_grid = expand.grid(
  g = seq(0.0001, 0.05, 0.01),
  c = seq(1.0, 3.00, 0.05),
  train_rmse_fold1 = 0,
  train_rmse_fold2 = 0,
  train_rmse_fold3 = 0,
  train_rmse_fold4 = 0,
  train_rmse_fold5 = 0,
  train_rmse_fold6 = 0,
  train_rmse_fold7 = 0,
  train_rmse_fold8 = 0,
  train_rmse_fold9 = 0,
  train_rmse_fold10 = 0,
  test_rmse_fold1 = 0,
  test_rmse_fold2 = 0,
  test_rmse_fold3 = 0,
  test_rmse_fold4 = 0,
  test_rmse_fold5 = 0,
  test_rmse_fold6 = 0,
  test_rmse_fold7 = 0,
  test_rmse_fold8 = 0,
  test_rmse_fold9 = 0,
  test_rmse_fold10 = 0)

j <- "Men, OPACH preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/7b_SVM insert.R")

hyper_grid <- hyper_grid %>% mutate(test_rmse_mean = select(., c("test_rmse_fold1",
                                                                 "test_rmse_fold2",
                                                                 "test_rmse_fold3",
                                                                 "test_rmse_fold4",
                                                                 "test_rmse_fold5",
                                                                 "test_rmse_fold6",
                                                                 "test_rmse_fold7",
                                                                 "test_rmse_fold8",
                                                                 "test_rmse_fold9",
                                                                 "test_rmse_fold10")) %>% rowMeans())

hyper_grid <- hyper_grid %>% mutate(train_rmse_mean = select(., c("train_rmse_fold1",
                                                                  "train_rmse_fold2",
                                                                  "train_rmse_fold3",
                                                                  "train_rmse_fold4",
                                                                  "train_rmse_fold5",
                                                                  "train_rmse_fold6",
                                                                  "train_rmse_fold7",
                                                                  "train_rmse_fold8",
                                                                  "train_rmse_fold9",
                                                                  "train_rmse_fold10")) %>% rowMeans())

hyper_grid <- hyper_grid %>% select(-contains(c("fold")))
hyper_grid <- hyper_grid %>% arrange(test_rmse_mean)

svm_model <- svm(vo2_max ~ .,
                 data = dat_svm,
                 kernal = "radial",
                 gamma = hyper_grid$g[1],
                 cost = hyper_grid$c[1],
                 na.action = na.omit)

final_svm_men_opachpreds <- svm_model
men_opachpreds_all_eval_log <- hyper_grid

hyper_grid$g[1]
hyper_grid$c[1]
sort(unique(hyper_grid$g))
sort(unique(hyper_grid$c))
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Women, all preds #### 
dat_women <- dat[dat$sex_num_m1f0 == 0, ]
dat_svm <- dat_women

# Tune gamma and cost parameters in the SVM models
hyper_grid = expand.grid(
  g = seq(0.001, 0.015, 0.001),
  c = seq(2, 6, 0.25),
  train_rmse_fold1 = 0,
  train_rmse_fold2 = 0,
  train_rmse_fold3 = 0,
  train_rmse_fold4 = 0,
  train_rmse_fold5 = 0,
  train_rmse_fold6 = 0,
  train_rmse_fold7 = 0,
  train_rmse_fold8 = 0,
  train_rmse_fold9 = 0,
  train_rmse_fold10 = 0,
  test_rmse_fold1 = 0,
  test_rmse_fold2 = 0,
  test_rmse_fold3 = 0,
  test_rmse_fold4 = 0,
  test_rmse_fold5 = 0,
  test_rmse_fold6 = 0,
  test_rmse_fold7 = 0,
  test_rmse_fold8 = 0,
  test_rmse_fold9 = 0,
  test_rmse_fold10 = 0)

j <- "Women, all preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/7b_SVM insert.R")

hyper_grid <- hyper_grid %>% mutate(test_rmse_mean = select(., c("test_rmse_fold1",
                                                                 "test_rmse_fold2",
                                                                 "test_rmse_fold3",
                                                                 "test_rmse_fold4",
                                                                 "test_rmse_fold5",
                                                                 "test_rmse_fold6",
                                                                 "test_rmse_fold7",
                                                                 "test_rmse_fold8",
                                                                 "test_rmse_fold9",
                                                                 "test_rmse_fold10")) %>% rowMeans())

hyper_grid <- hyper_grid %>% mutate(train_rmse_mean = select(., c("train_rmse_fold1",
                                                                  "train_rmse_fold2",
                                                                  "train_rmse_fold3",
                                                                  "train_rmse_fold4",
                                                                  "train_rmse_fold5",
                                                                  "train_rmse_fold6",
                                                                  "train_rmse_fold7",
                                                                  "train_rmse_fold8",
                                                                  "train_rmse_fold9",
                                                                  "train_rmse_fold10")) %>% rowMeans())

hyper_grid <- hyper_grid %>% select(-contains(c("fold")))
hyper_grid <- hyper_grid %>% arrange(test_rmse_mean)

svm_model <- svm(vo2_max ~ .,
                 data = dat_svm,
                 kernal = "radial",
                 gamma = hyper_grid$g[1],
                 cost = hyper_grid$c[1],
                 na.action = na.omit)

final_svm_women_allpreds <- svm_model
women_allpreds_all_eval_log <- hyper_grid

hyper_grid$g[1]
hyper_grid$c[1]
sort(unique(hyper_grid$g))
sort(unique(hyper_grid$c))
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Women, OPACH preds #### 
dat_women_opach <- dat[dat$sex_num_m1f0 == 0, !(colnames(dat) %in% drops)]
dat_svm <- dat_women_opach

# Tune gamma and cost parameters in the SVM models
hyper_grid = expand.grid(
  g = seq(0.001, 0.015, 0.001),
  c = seq(1, 3, 0.25),
  train_rmse_fold1 = 0,
  train_rmse_fold2 = 0,
  train_rmse_fold3 = 0,
  train_rmse_fold4 = 0,
  train_rmse_fold5 = 0,
  train_rmse_fold6 = 0,
  train_rmse_fold7 = 0,
  train_rmse_fold8 = 0,
  train_rmse_fold9 = 0,
  train_rmse_fold10 = 0,
  test_rmse_fold1 = 0,
  test_rmse_fold2 = 0,
  test_rmse_fold3 = 0,
  test_rmse_fold4 = 0,
  test_rmse_fold5 = 0,
  test_rmse_fold6 = 0,
  test_rmse_fold7 = 0,
  test_rmse_fold8 = 0,
  test_rmse_fold9 = 0,
  test_rmse_fold10 = 0)

j <- "Women, OPACH preds"
source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/7b_SVM insert.R")

hyper_grid <- hyper_grid %>% mutate(test_rmse_mean = select(., c("test_rmse_fold1",
                                                                 "test_rmse_fold2",
                                                                 "test_rmse_fold3",
                                                                 "test_rmse_fold4",
                                                                 "test_rmse_fold5",
                                                                 "test_rmse_fold6",
                                                                 "test_rmse_fold7",
                                                                 "test_rmse_fold8",
                                                                 "test_rmse_fold9",
                                                                 "test_rmse_fold10")) %>% rowMeans())

hyper_grid <- hyper_grid %>% mutate(train_rmse_mean = select(., c("train_rmse_fold1",
                                                                  "train_rmse_fold2",
                                                                  "train_rmse_fold3",
                                                                  "train_rmse_fold4",
                                                                  "train_rmse_fold5",
                                                                  "train_rmse_fold6",
                                                                  "train_rmse_fold7",
                                                                  "train_rmse_fold8",
                                                                  "train_rmse_fold9",
                                                                  "train_rmse_fold10")) %>% rowMeans())

hyper_grid <- hyper_grid %>% select(-contains(c("fold")))
hyper_grid <- hyper_grid %>% arrange(test_rmse_mean)

svm_model <- svm(vo2_max ~ .,
                 data = dat_svm,
                 kernal = "radial",
                 gamma = hyper_grid$g[1],
                 cost = hyper_grid$c[1],
                 na.action = na.omit)

final_svm_women_opachpreds <- svm_model
women_opachpreds_all_eval_log <- hyper_grid

hyper_grid$g[1]
hyper_grid$c[1]
sort(unique(hyper_grid$g))
sort(unique(hyper_grid$c))
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Store all results ####
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

results$`Test RMSE`[2] <- allobs_allpreds_all_eval_log$test_rmse_mean[1]
results$`Test RMSE`[3] <- allobs_opachpreds_all_eval_log$test_rmse_mean[1]
results$`Test RMSE`[4] <- men_allpreds_all_eval_log$test_rmse_mean[1]
results$`Test RMSE`[5] <- men_opachpreds_all_eval_log$test_rmse_mean[1]
results$`Test RMSE`[6] <- women_allpreds_all_eval_log$test_rmse_mean[1]
results$`Test RMSE`[7] <- women_opachpreds_all_eval_log$test_rmse_mean[1]

results$`Train RMSE`[2] <- allobs_allpreds_all_eval_log$train_rmse_mean[1]
results$`Train RMSE`[3] <- allobs_opachpreds_all_eval_log$train_rmse_mean[1]
results$`Train RMSE`[4] <- men_allpreds_all_eval_log$train_rmse_mean[1]
results$`Train RMSE`[5] <- men_opachpreds_all_eval_log$train_rmse_mean[1]
results$`Train RMSE`[6] <- women_allpreds_all_eval_log$train_rmse_mean[1]
results$`Train RMSE`[7] <- women_opachpreds_all_eval_log$train_rmse_mean[1]

write.csv(results,
          "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/RMSEs/svm_results.csv",
          row.names = FALSE)

#### Save the image ####
save.image("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Workspaces/ML Results/svm.RData")
# load("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Workspaces/ML Results/svm.RData")

#### Clear environment and console ####
rm(list = ls())
cat("\014")
