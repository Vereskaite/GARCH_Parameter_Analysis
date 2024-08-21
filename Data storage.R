# Create data frames for output storage

###### DF1 - Return values ########

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
}

Return_values

###### DF2 - Parameter values ########

