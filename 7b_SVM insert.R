# Subset to complete case
dat_svm <- dat_svm[complete.cases(dat_svm), ]

# Randomly shuffle the data
set.seed(02082022); dat_svm <- dat_svm[sample(nrow(dat_svm)), ]

# Create 5 equally sized folds
folds <- cut(seq(1, nrow(dat_svm)), breaks = 10, labels = FALSE)

# Iterate through nrow of hypergrid for parameter tuning
for (k in 1:nrow(hyper_grid)){
# Perform 10 fold cross validation
for (i in 1:10){

  # Segment your data by fold using the which() function 
  testIndexes <- which(folds == i, arr.ind = TRUE)
  trainData <- dat_svm[-testIndexes, ]
  testData <- dat_svm[testIndexes, ]

  testData = rbind(trainData[1,], testData)
  testData = testData[-1,]
  
  svm_model <- svm(vo2_max ~ .,
                   data = trainData,
                   kernal = "radial",
                   gamma = hyper_grid$g[k],
                   cost = hyper_grid$c[k])
  
  test_pred_y <- predict(svm_model, testData)
  train_pred_y <- predict(svm_model, trainData)
  
  if (i == 1){hyper_grid$test_rmse_fold1[k] <- RMSE(test_pred_y, testData$vo2_max)}
  if (i == 2){hyper_grid$test_rmse_fold2[k] <- RMSE(test_pred_y, testData$vo2_max)}
  if (i == 3){hyper_grid$test_rmse_fold3[k] <- RMSE(test_pred_y, testData$vo2_max)}
  if (i == 4){hyper_grid$test_rmse_fold4[k] <- RMSE(test_pred_y, testData$vo2_max)}
  if (i == 5){hyper_grid$test_rmse_fold5[k] <- RMSE(test_pred_y, testData$vo2_max)}
  if (i == 6){hyper_grid$test_rmse_fold6[k] <- RMSE(test_pred_y, testData$vo2_max)}
  if (i == 7){hyper_grid$test_rmse_fold7[k] <- RMSE(test_pred_y, testData$vo2_max)}
  if (i == 8){hyper_grid$test_rmse_fold8[k] <- RMSE(test_pred_y, testData$vo2_max)}
  if (i == 9){hyper_grid$test_rmse_fold9[k] <- RMSE(test_pred_y, testData$vo2_max)}
  if (i == 10){hyper_grid$test_rmse_fold10[k] <- RMSE(test_pred_y, testData$vo2_max)}
  
  if (i == 1){hyper_grid$train_rmse_fold1[k] <- RMSE(train_pred_y, trainData$vo2_max)}
  if (i == 2){hyper_grid$train_rmse_fold2[k] <- RMSE(train_pred_y, trainData$vo2_max)}
  if (i == 3){hyper_grid$train_rmse_fold3[k] <- RMSE(train_pred_y, trainData$vo2_max)}
  if (i == 4){hyper_grid$train_rmse_fold4[k] <- RMSE(train_pred_y, trainData$vo2_max)}
  if (i == 5){hyper_grid$train_rmse_fold5[k] <- RMSE(train_pred_y, trainData$vo2_max)}
  if (i == 6){hyper_grid$train_rmse_fold6[k] <- RMSE(train_pred_y, trainData$vo2_max)}
  if (i == 7){hyper_grid$train_rmse_fold7[k] <- RMSE(train_pred_y, trainData$vo2_max)}
  if (i == 8){hyper_grid$train_rmse_fold8[k] <- RMSE(train_pred_y, trainData$vo2_max)}
  if (i == 9){hyper_grid$train_rmse_fold9[k] <- RMSE(train_pred_y, trainData$vo2_max)}
  if (i == 10){hyper_grid$train_rmse_fold10[k] <- RMSE(train_pred_y, trainData$vo2_max)}
  
  # This prints the progress as a percentage of nrow(hyper_grid)
  if (i == 10){print(paste0(j, ": ", round((k/nrow(hyper_grid))*100, 2), "%"))}
}}
