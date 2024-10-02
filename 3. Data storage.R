# Create data frames for output storage

###### DF1 - Return values ########
start_time <- Sys.time()
pb <- txtProgressBar(min = 0, max = nrow(parameter_grid), style = 3)
### Create table for Scenario 1
# Add in-sample values
Return_values <- data.frame(Scenario = paste("SimulatedNAGARCH",
                                             parameter_grid[1,1],parameter_grid[1,2],parameter_grid[1,3],parameter_grid[1,4],
                                             sep = "_"),
                            Index = 1:length(NA_GARCH_output[[1]]$ActualReturnsInSample),
                            Type = "In-Sample",
                            Returns = NA_GARCH_output[[1]]$ActualReturnsInSample) %>% 
  # Add out-sample values
  bind_rows(
    data.frame(Scenario = paste("SimulatedNAGARCH",
                                parameter_grid[1,1],parameter_grid[1,2],parameter_grid[1,3],parameter_grid[1,4],
                                sep = "_"),
               Index = (length(NA_GARCH_output[[1]]$ActualReturnsInSample)+1):(length(NA_GARCH_output[[1]]$ActualReturnsInSample)+length(NA_GARCH_output[[1]]$ActualReturnsOutSample)),
               Type = "Out-Sample",
               Returns = NA_GARCH_output[[1]]$ActualReturnsOutSample)
  ) %>% 
  # Add forecasted values
  bind_rows(
    data.frame(Scenario = paste("SimulatedNAGARCH",
                                parameter_grid[1,1],parameter_grid[1,2],parameter_grid[1,3],parameter_grid[1,4],
                                sep = "_"),
               Index = (length(NA_GARCH_output[[1]]$ActualReturnsInSample)+1):(length(NA_GARCH_output[[1]]$ActualReturnsInSample)+length(NA_GARCH_output[[1]]$ForecastedReturns)),
               Type = "Forecast",
               Returns = NA_GARCH_output[[1]]$ForecastedReturns)
  ) %>% 
  # Add sigma2 values
  bind_rows(
    data.frame(Scenario = paste("SimulatedNAGARCH",
                                parameter_grid[1,1],parameter_grid[1,2],parameter_grid[1,3],parameter_grid[1,4],
                                sep = "_"),
               Index = (length(NA_GARCH_output[[1]]$ActualReturnsInSample)+1):(length(NA_GARCH_output[[1]]$ActualReturnsInSample)+length(NA_GARCH_output[[1]]$Sigma2)),
               Type = "Sigma2",
               Returns = NA_GARCH_output[[1]]$Sigma2)
  ) %>% 
  # Add f values
  bind_rows(
    data.frame(Scenario = paste("SimulatedNAGARCH",
                                parameter_grid[1,1],parameter_grid[1,2],parameter_grid[1,3],parameter_grid[1,4],
                                sep = "_"),
               Index = (1:(length(NA_GARCH_output[[1]]$f))),
               Type = "f",
               Returns = NA_GARCH_output[[1]]$f
    )
  )

### Create loop for further scenarios

for (i in 2:nrow(parameter_grid)){
  
  Return_values <- Return_values %>% 
    bind_rows(
      data.frame(Scenario = paste("SimulatedNAGARCH",
                                  parameter_grid[i,1],parameter_grid[i,2],parameter_grid[i,3],parameter_grid[i,4],
                                  sep = "_"),
                 Index = 1:length(NA_GARCH_output[[i]]$ActualReturnsInSample),
                 Type = "In-Sample",
                 Returns = NA_GARCH_output[[i]]$ActualReturnsInSample)
    ) %>% 
    bind_rows(
      data.frame(Scenario = paste("SimulatedNAGARCH",
                                  parameter_grid[i,1],parameter_grid[i,2],parameter_grid[i,3],parameter_grid[i,4],
                                  sep = "_"),
                 Index = (length(NA_GARCH_output[[i]]$ActualReturnsInSample)+1):(length(NA_GARCH_output[[i]]$ActualReturnsInSample)+length(NA_GARCH_output[[i]]$ActualReturnsOutSample)),
                 Type = "Out-Sample",
                 Returns = NA_GARCH_output[[i]]$ActualReturnsOutSample)
    ) %>% 
    bind_rows(
      data.frame(Scenario = paste("SimulatedNAGARCH",
                                  parameter_grid[i,1],parameter_grid[i,2],parameter_grid[i,3],parameter_grid[i,4],
                                  sep = "_"),
                 Index = (length(NA_GARCH_output[[i]]$ActualReturnsInSample)+1):(length(NA_GARCH_output[[i]]$ActualReturnsInSample)+length(NA_GARCH_output[[i]]$ForecastedReturns)),
                 Type = "Forecast",
                 Returns = NA_GARCH_output[[i]]$ForecastedReturns)
    ) %>% 
    bind_rows(
      data.frame(Scenario = paste("SimulatedNAGARCH",
                                  parameter_grid[i,1],parameter_grid[i,2],parameter_grid[i,3],parameter_grid[i,4],
                                  sep = "_"),
                 Index = (length(NA_GARCH_output[[i]]$ActualReturnsInSample)+1):(length(NA_GARCH_output[[i]]$ActualReturnsInSample)+length(NA_GARCH_output[[i]]$Sigma2)),
                 Type = "Sigma2",
                 Returns = NA_GARCH_output[[i]]$Sigma2)
    )  %>% 
    bind_rows(
      data.frame(Scenario = paste("SimulatedNAGARCH",
                                  parameter_grid[i,1],parameter_grid[i,2],parameter_grid[i,3],parameter_grid[i,4],
                                  sep = "_"),
                 Index = (1:length(NA_GARCH_output[[i]]$f)),
                 Type = "f",
                 Returns = NA_GARCH_output[[i]]$f)
    ) 
  setTxtProgressBar(pb, i)
}

close(pb)
end_time <- Sys.time()
total_runtime <- end_time - start_time
total_runtime

###### DF2 - Parameter values ########
start_time <- Sys.time()
pb <- txtProgressBar(min = 0, max = nrow(parameter_grid), style = 3)
### Create table for Scenario 1

Parameter_values <- data.frame(Scenario = paste("SimulatedNAGARCH",
                            parameter_grid[1,1],parameter_grid[1,2],parameter_grid[1,3],parameter_grid[1,4],
                            sep = "_"),
           Parameter = c("a", "b", "kappa", "gamma", 
                         "mu", "omega", "alpha", "beta", 
                         "mu_p", "omega_p", "alpha_p", "beta_p",
                         "mu_sign", "omega_sign", "alpha_sign", "beta_sign",
                         "AIC", "BIC", "RMSE", "MAE", "ARCH_Test"),
           Value = c(parameter_grid[1,1],parameter_grid[1,2],parameter_grid[1,3],parameter_grid[1,4],
                      NA_GARCH_output[[1]]$Parameters[1], NA_GARCH_output[[1]]$Parameters[2], NA_GARCH_output[[1]]$Parameters[3], NA_GARCH_output[[1]]$Parameters[4],
                      NA_GARCH_output[[1]]$PValues[1], NA_GARCH_output[[1]]$PValues[2], NA_GARCH_output[[1]]$PValues[3], NA_GARCH_output[[1]]$PValues[4],
                      NA_GARCH_output[[1]]$PValues[1] < 0.05, NA_GARCH_output[[1]]$PValues[2] < 0.05, NA_GARCH_output[[1]]$PValues[3] < 0.05, NA_GARCH_output[[1]]$PValues[4] < 0.05,
                      NA_GARCH_output[[1]]$AIC, NA_GARCH_output[[1]]$BIC, NA_GARCH_output[[1]]$RMSE, NA_GARCH_output[[1]]$MAE, NA_GARCH_output[[1]]$ARCH_Test) )

### Create loop for further scenarios

for (i in 2:length(NA_GARCH_output)){ 
  Parameter_values <- Parameter_values %>% 
    bind_rows(
      data.frame(Scenario = paste("SimulatedNAGARCH",parameter_grid[i,1],parameter_grid[i,2],parameter_grid[i,3],parameter_grid[i,4],
                                                              sep = "_"),
                                             Parameter = c("a", "b", "kappa", "gamma", 
                                                           "mu", "omega", "alpha", "beta", 
                                                           "mu_p", "omega_p", "alpha_p", "beta_p",
                                                           "mu_sign", "omega_sign", "alpha_sign", "beta_sign",
                                                           "AIC", "BIC", "RMSE", "MAE","ARCH_Test"),
                                             Value = c( parameter_grid[i,1],parameter_grid[i,2],parameter_grid[i,3],parameter_grid[i,4],
                                                        NA_GARCH_output[[i]]$Parameters[1], NA_GARCH_output[[i]]$Parameters[2], NA_GARCH_output[[i]]$Parameters[3], NA_GARCH_output[[i]]$Parameters[4],
                                                        NA_GARCH_output[[i]]$PValues[1], NA_GARCH_output[[i]]$PValues[2], NA_GARCH_output[[i]]$PValues[3], NA_GARCH_output[[i]]$PValues[4],
                                                        NA_GARCH_output[[i]]$PValues[1] < 0.05, NA_GARCH_output[[i]]$PValues[2] < 0.05, NA_GARCH_output[[i]]$PValues[3] < 0.05, NA_GARCH_output[[i]]$PValues[4] < 0.05,
                                                        NA_GARCH_output[[i]]$AIC, NA_GARCH_output[[i]]$BIC, NA_GARCH_output[[i]]$RMSE, NA_GARCH_output[[i]]$MAE, NA_GARCH_output[[i]]$ARCH_Test) )
    )
  setTxtProgressBar(pb, i)
}

close(pb)
end_time <- Sys.time()
total_runtime <- end_time - start_time
total_runtime




Parameter_values_df <- Parameter_values %>%
  tidyr::pivot_wider(names_from = Parameter, values_from = Value) %>% 
  mutate(ab = a+b)

### Save cases
Parameter_values_Aux <- Parameter_values
Parameter_values_df_Aux <- Parameter_values_df
Return_values_Aux <- Return_values

