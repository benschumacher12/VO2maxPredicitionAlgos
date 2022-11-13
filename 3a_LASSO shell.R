#### Clear environment and console ####
rm(list = ls())
cat("\014")

#### Load all needed packages ####
library(tidyverse)
library(glmnet)
library(caret)

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

# Subset to complete cases
dat <- dat[complete.cases(dat), ]

# The following variables are excluded in OPACH 
drops <- c("rgspeed", "ldcw47b", "uw150meters", "c400secs", "wkindex", "uw150speed",
           "mdcw21", "mdcw26", "hrstart", "hrend", "hakcal")
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### All obs, all preds ####
# glmnet needs the IVs as matrix with dummy vars and the DV as a vector
# glmnet does the scaling by default
x <- model.matrix(vo2_max ~ ., data = dat)[, -1]
y <- dat$vo2_max

# When lambda = 0, no parameters are eliminated.
# As lambda increases, more and more coefficients are set to zero and eliminated & bias increases.
lambdas <- 10^seq(4, -6, by = -.01)

source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/3b_LASSO insert.R")
best_lam_allobs_allpreds <- best_lam
coeffs_allobs_allpreds <- lasso_coeffs
model_allobs_allpreds <- lasso_model
rmse_allobs_allpreds <- rmse
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### All obs, OPACH preds #### 
dat_opach <- dat[ , !(colnames(dat) %in% drops)]
x <- model.matrix(vo2_max ~ ., data = dat_opach)[, -1]
y <- dat_opach$vo2_max

# When lambda = 0, no parameters are eliminated.
# As lambda increases, more and more coefficients are set to zero and eliminated & bias increases.
lambdas <- 10^seq(4, -6, by = -.01)

source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/3b_LASSO insert.R")
best_lam_allobs_opachpreds <- best_lam
coeffs_allobs_opachpreds <- lasso_coeffs
model_allobs_opachpreds <- lasso_model
rmse_allobs_opachpreds <- rmse
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Men, all preds #### 
dat_men <- dat[dat$sex_num_m1f0 == 1, ]
x <- model.matrix(vo2_max ~ ., data = dat_men)[, -1]
y <- dat_men$vo2_max

# When lambda = 0, no parameters are eliminated.
# As lambda increases, more and more coefficients are set to zero and eliminated & bias increases.
lambdas <- 10^seq(4, -6, by = -.01)

source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/3b_LASSO insert.R")
best_lam_men_allpreds <- best_lam
coeffs_men_allpreds <- lasso_coeffs
model_men_allpreds <- lasso_model
rmse_men_allpreds <- rmse
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Men, OPACH preds #### 
dat_men_opach <- dat[dat$sex_num_m1f0 == 1, !(colnames(dat) %in% drops)]
x <- model.matrix(vo2_max ~ ., data = dat_men_opach)[, -1]
y <- dat_men_opach$vo2_max

# When lambda = 0, no parameters are eliminated.
# As lambda increases, more and more coefficients are set to zero and eliminated & bias increases.
lambdas <- 10^seq(4, -6, by = -.01)

source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/3b_LASSO insert.R")
best_lam_men_opachpreds <- best_lam
coeffs_men_opachpreds <- lasso_coeffs
model_men_opachpreds <- lasso_model
rmse_men_opachpreds <- rmse
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Women, all preds #### 
dat_women <- dat[dat$sex_num_m1f0 == 0, ]
x <- model.matrix(vo2_max ~ ., data = dat_women)[, -1]
y <- dat_women$vo2_max

# When lambda = 0, no parameters are eliminated.
# As lambda increases, more and more coefficients are set to zero and eliminated & bias increases.
lambdas <- 10^seq(4, -6, by = -.01)

source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/3b_LASSO insert.R")
best_lam_women_allpreds <- best_lam
coeffs_women_allpreds <- lasso_coeffs
model_women_allpreds <- lasso_model
rmse_women_allpreds <- rmse
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Women, OPACH preds #### 
dat_women_opach <- dat[dat$sex_num_m1f0 == 0, !(colnames(dat) %in% drops)]
x <- model.matrix(vo2_max ~ ., data = dat_women_opach)[, -1]
y <- dat_women_opach$vo2_max

# When lambda = 0, no parameters are eliminated.
# As lambda increases, more and more coefficients are set to zero and eliminated & bias increases.
lambdas <- 10^seq(4, -6, by = -.01)

source("/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/3b_LASSO insert.R")
best_lam_women_opachpreds <- best_lam
coeffs_women_opachpreds <- lasso_coeffs
model_women_opachpreds <- lasso_model
rmse_women_opachpreds <- rmse
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#### Save, export, and clean #### 
write.csv(coeffs_allobs_allpreds,
          "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/LASSO Coeffs/allobs_allpreds_lasso_coeffs.csv",
          row.names = FALSE)

write.csv(coeffs_allobs_opachpreds,
          "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/LASSO Coeffs/allobs_opachpreds_lasso_coeffs.csv",
          row.names = FALSE)

write.csv(coeffs_men_allpreds,
          "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/LASSO Coeffs/men_allpreds_lasso_coeffs.csv",
          row.names = FALSE)

write.csv(coeffs_men_opachpreds,
          "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/LASSO Coeffs/men_opachpreds_lasso_coeffs.csv",
          row.names = FALSE)

write.csv(coeffs_women_allpreds,
          "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/LASSO Coeffs/women_allpreds_lasso_coeffs.csv",
          row.names = FALSE)

write.csv(coeffs_women_opachpreds,
          "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/LASSO Coeffs/women_opachpreds_lasso_coeffs.csv",
          row.names = FALSE)
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
results <- data.frame(matrix(ncol = 2, nrow = 7))
colnames(results) <- c("Algorithm",
                       "RMSE")

results$Algorithm[1] <- "Sample, Universe of Predictors"
results$Algorithm[2] <- "Total BLSA, All BLSA Predictors"
results$Algorithm[3] <- "Total BLSA, OPACH Predictors"
results$Algorithm[4] <- "BLSA Men, All BLSA Predictors"
results$Algorithm[5] <- "BLSA Men, OPACH Predictors"
results$Algorithm[6] <- "BLSA Women, All BLSA Predictors"
results$Algorithm[7] <- "BLSA Women, OPACH Predictors"

results$`RMSE`[2] <- rmse_allobs_allpreds
results$`RMSE`[3] <- rmse_allobs_opachpreds
results$`RMSE`[4] <- rmse_men_allpreds
results$`RMSE`[5] <- rmse_men_opachpreds
results$`RMSE`[6] <- rmse_women_allpreds
results$`RMSE`[7] <- rmse_women_opachpreds

write.csv(results,
          "/Users/benschumacher/Documents/Dissertation/6_Analyses/_Paper 2 Analyses/Output/CSV Output/RMSEs/lasso_results.csv",
          row.names = FALSE)

#### Clear environment and console ####
rm(list = ls())
cat("\014")
