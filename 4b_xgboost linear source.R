# Begin by removing rows from the grid that won't contribute information

# Remove rows where nrounds = 0
hyper_grid <- hyper_grid[!(hyper_grid$nrounds == 0), ]

# Remove rows where everything else = 0, these will all give the same RMSE despite the
# value for nrounds
hyper_grid <- hyper_grid[!(hyper_grid$lambda == 0 &
                             hyper_grid$alpha == 0 &
                             hyper_grid$eta == 0), ]

# Reset row names
row.names(hyper_grid) <- NULL
# The grid is now finalized and ready for training
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Create an empty DF to house final results
min_eval_log <- data.frame(iter = numeric(), # the nrounds that yield min test RMSE
                       train_rmse_mean = numeric(), # training RMSE
                       train_rmse_std = numeric(), # training RMSE std
                       test_rmse_mean = numeric(), # test RMSE
                       test_rmse_std = numeric(), # test RMSE std
                       nrow_hyper_grid = numeric()) # the row from the grid of params

# Create an empty list of DFs to house the evaluation log for each result
eval_log_individual_models <- list()
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Iterate through every row in hyper_grid
for (i in 1:nrow(hyper_grid)){

# Set the temporary parameters to the values ith row of the hyperparameter grid
tmp_nrounds <- hyper_grid$nrounds[i]
tmp_lambda <- hyper_grid$lambda[i]
tmp_alpha <- hyper_grid$alpha[i]
tmp_eta <- hyper_grid$eta[i]

# Pack the list of params to feed to xgboost
  param <- list(booster = "gblinear",
                objective = "reg:squarederror",
                eval_metric = "rmse",
                lambda = tmp_lambda,
                alpha = tmp_alpha,
                eta = tmp_eta)

# Initiate the 10-fold CV xgboost training
  xgb_linear_cv <- xgb.cv(data = xgb_dat,
                 params = param,
                 nfold = 10,
                 nrounds = tmp_nrounds,
                 verbose = F,
                 set.seed(02082022))

# Save the model evaluation log to eval_log_individual_models
  eval_log_individual_models[[i]] <- as.data.frame(xgb_linear_cv$evaluation_log)
  
# Get the row with the minimum test RMSE value
  row_num = which.min(xgb_linear_cv$evaluation_log$test_rmse_mean)

# Create a tmp dataframe to save statistics and the nrow from the grid
  x <- as.data.frame(cbind(xgb_linear_cv$evaluation_log[row_num], i))
  colnames(x)[colnames(x) == "i"] <- "nrow_hyper_grid"
  min_eval_log <- rbind(min_eval_log, x)

# This prints the progress as a percentage of nrow(hyper_grid)
  print(paste0(j, ": ", round((i/nrow(hyper_grid))*100, 2), "%"))
  
  rm(x)
}
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------
# Name the elements of the eval_log_individual_models
names(eval_log_individual_models) <- paste0("rownum_", seq(1, nrow(hyper_grid)))

# Merge the hyperparameters values from the grid into the eval_log dataframe
hyper_grid$nrow_hyper_grid <- row.names(hyper_grid)
min_eval_log <- merge(min_eval_log, hyper_grid, by = c("nrow_hyper_grid"))
min_eval_log <- min_eval_log %>% arrange(test_rmse_mean)
min_eval_log <- min_eval_log %>% relocate(nrow_hyper_grid, .before = nrounds)

# Save final model with the selected params
param <- list(booster = "gblinear",
              objective = "reg:squarederror",
              eval_metric = "rmse",
              lambda =  min_eval_log$lambda[1],
              alpha = min_eval_log$alpha[1],
              eta = min_eval_log$eta[1])

final_xgb_linear <- xgb.cv(data = xgb_dat,
                           params = param,
                           nfold = 10,
                           nrounds = min_eval_log$iter[1],
                           verbose = F,
                           set.seed(02082022))
final_eval_log <- as.data.frame(final_xgb_linear$evaluation_log)
final_eval_log <- final_eval_log %>% arrange(test_rmse_mean)

final_xgb_linear <- xgb.train(data = xgb_dat,
                              params = param,
                              nrounds = min_eval_log$iter[1],
                              set.seed(02082022))
