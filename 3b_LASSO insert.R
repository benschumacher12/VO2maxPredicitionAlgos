# Run the model using CV to find value of lambda that minimizes training MSE
# Alpha = 1 = LASSO; Alpha = 0 = Ridge
lasso_model_cv <- cv.glmnet(x,
                            y,
                            alpha = 1,
                            nfolds = 10,
                            set.seed(02082022),
                            lambda = lambdas)

# Check the lambda value that minimizes error
best_lam <- lasso_model_cv$lambda.min

# Fit the final model on the training data
lasso_model <- glmnet(x,
                      y,
                      alpha = 1,
                      lambda = best_lam)

rm(lasso_model_cv)

# Get the performance metrics against the test data
predictions <- lasso_model %>% predict(x) %>% as.vector()

rmse <- RMSE(y, predictions)

lasso_coeffs <- as.data.frame(as.matrix(coef(lasso_model)))
lasso_coeffs$variable <- row.names(lasso_coeffs)
lasso_coeffs <- lasso_coeffs[lasso_coeffs$s0 !=0, c(2,1)]; row.names(lasso_coeffs) <- NULL

lasso_coeffs <- lasso_coeffs %>% 
  arrange(desc(abs(s0)))
