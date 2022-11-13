for (i in 1:nrow(hyper_grid)){
  
  model <- randomForest(
    formula = vo2_max ~ .,
    data = dat_rf,
    ntree = hyper_grid$ntrees[i],
    mtry = hyper_grid$mtry[i],
    sample.fraction = hyper_grid$sampsize[i],
    min.node.size = hyper_grid$node_size[i],
    seed = 02082022,
    na.action = na.omit,
    importance = TRUE)
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- round(sqrt(model$mse[which.min(model$mse)]), 3)
  
  print(paste0(j, ": ", round((i/nrow(hyper_grid))*100, 2), "%"))
}

final_ntrees <- hyper_grid$ntrees[which.min(hyper_grid$OOB_RMSE)]
final_mtry <- hyper_grid$mtry[which.min(hyper_grid$OOB_RMSE)]
final_sampfrac <- hyper_grid$sampsize[which.min(hyper_grid$OOB_RMSE)]
final_nodesize <- hyper_grid$node_size[which.min(hyper_grid$OOB_RMSE)]

final_rf_model <- randomForest(
  formula = vo2_max ~ .,
  data = dat_rf,
  ntrees = final_ntrees,
  mtry = final_mtry,
  sample.fraction = final_sampfrac,
  min.node.size = final_nodesize,
  seed = 02082022,
  na.action = na.omit,
  importance = TRUE)

pred_values_rf <- predict(final_rf_model, dat)
actual_values <- dat$vo2_max
rmse <- mean((pred_values_rf - actual_values)^2, na.rm = TRUE)^0.5
