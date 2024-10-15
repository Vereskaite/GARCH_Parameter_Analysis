set.seed(123)
options(scipen = 9999)
# install.packages("FinTS", lib = "R")
# install.packages("tidyr", lib = "R")
# install.packages("parallel", lib = "R")

library(dplyr, lib.loc = "R")
library(zoo, lib.loc = "R")
library(FinTS, lib.loc = "R")
library(parallel, lib.loc = "R")

start_time_total <- Sys.time()

Data_Simulation_function <- function(kappa, gamma) {
  # Parameters
  n <- 5000
  mu <- 0
  omega <- 0.1
  alpha <- 0.1
  beta <- 0.1
  a <- 0.8
  b <- 0.8
  kappa <- kappa
  gamma <- gamma
  
  # Generate data for Half-Normal distribution
  sigma <- 0.5
  
  ### Positive
  generate_half_normal <- function(n, sigma) {
    data <- abs(rnorm(n, mean = 0, sd = sigma))
    
    min_val <- min(data)
    max_val <- max(data)
    standardized_data <- (data - min_val) / (max_val - min_val) * (1 - 0.01) + 0.01
    
    return(standardized_data)
  }
  
  standardized_data <- c()
  
  while(length(standardized_data) < n) {
    additional_data <- generate_half_normal(n, sigma)  # Generate n samples each time
    standardized_data <- c(standardized_data, additional_data)
  }
  
  P_t <- standardized_data[1:n]
  
  ### Negative
  
  generate_half_normal_negative <- function(n, sigma) {
    data <- -abs(rnorm(n, mean = 0, sd = sigma))  # Generate negative half-normal values
    
    min_val <- min(data)
    max_val <- max(data)
    standardized_data <- (data - min_val) / (max_val - min_val) * (-0.01 + 1) - 1
    
    return(standardized_data)
  }
  
  standardized_data <- c()
  
  while(length(standardized_data) < n) {
    additional_data <- generate_half_normal_negative(n, sigma)  # Generate n samples each time
    standardized_data <- c(standardized_data, additional_data)
  }
  
  N_t <- standardized_data[1:n]
  
  # Simulate NA-GARCH(1,1) Process
  
  r_t <- numeric(n)
  sigma2_t <- numeric(n)
  f_t <- numeric(n)
  epsilon_t <- numeric(n)
  
  # Initial values
  sigma2_t[1] <- omega / (1 - alpha - beta)  # Set initial variance
  epsilon_t[1] <- rnorm(1, mean = 0, sd = sqrt(sigma2_t[1]))  # First shock
  r_t[1] <- mu + epsilon_t[1]  # First return
  
  for (t in 2:n) {
    
    f_t[t-1] <- a + 0.5 * b * ((exp(kappa * P_t[t-1]) - 1) / (exp(kappa * P_t[t-1]) + 1) - 
                                 (exp(gamma * N_t[t-1]) - 1) / (exp(gamma * N_t[t-1]) + 1))
    
    sigma2_t[t] <- f_t[t-1] * (omega + alpha * epsilon_t[t-1]^2 + beta * sigma2_t[t-1])
    
    z_t <- rnorm(1)
    
    epsilon_t[t] <- z_t * sqrt(sigma2_t[t])
    
    r_t[t] <- mu + epsilon_t[t]
  }
  
  r <- r_t
  
  simulated_data <- data.frame(
    Time = 1:n,
    Returns = r_t,
    Volatility = sqrt(sigma2_t),
    Positive_Sentiment = P_t,
    Negative_Sentiment = N_t,
    f_t = f_t
  ) 
  
}

Data_k4_g4 <- Data_Simulation_function(kappa = 4, gamma = 4)

r <- Data_k4_g4$Returns
P_t <- Data_k4_g4$Positive_Sentiment
N_t <- Data_k4_g4$Negative_Sentiment

# Define function for estimation and forecasting

NA_GARCH_Evaluation <- function(r, P_t, N_t, a, b, kappa, gamma, split_ratio, params) {
  
  # Split data into in-out sample
  split_ratio <- 0.8
  n <- length(r)
  split_point <- floor(split_ratio * n)
  r_in <- r[1:split_point]
  r_out <- r[(split_point + 1):n]
  P_t_in <- P_t[1:split_point]
  P_t_out <- P_t[(split_point + 1):n]
  N_t_in <- N_t[1:split_point]
  N_t_out <- N_t[(split_point + 1):n]
  
  sigma2 <- numeric(length(r))
  epsilon <- numeric(length(r))
  f <- numeric(length(r))
  
  log_likelihood <- function(r, P_t, N_t, a, b, kappa, gamma, split_ratio, params) {
    split_ratio <- 0.8
    n <- length(r)
    split_point <- floor(split_ratio * n)
    r_in <- r[1:split_point]
    r_out <- r[(split_point + 1):n]
    P_t_in <- P_t[1:split_point]
    P_t_out <- P_t[(split_point + 1):n]
    N_t_in <- N_t[1:split_point]
    N_t_out <- N_t[(split_point + 1):n]
    
    mu <- params[1]
    omega <- params[2]
    alpha <- params[3]
    beta <- params[4]
    
    if (omega <= 0 || alpha < 0 || beta < 0 || (alpha + beta)*(a+b) >= 1) {
      return("Parameters out of range")  # Return a large number to indicate an invalid parameter set
    }
    
    n <- length(r_in)
    
    f[1] <- a + 0.5 * b * ((exp(kappa * P_t_in[1]) - 1) / (exp(kappa * P_t_in[1]) + 1) - (exp(gamma * N_t_in[1]) - 1) / (exp(gamma * N_t_in[1]) + 1))
    
    # paskaiciuoja initial variance ir uzdeda max, kad nebutu variance neigiamas
    sigma2[1] <- max((omega *(a+b) / (1 - (alpha + beta)*(a+b))), 1e-6)
    epsilon[1] <- r_in[1] - mu
    logL <- -0.5 * (log(2 * pi) + log(sigma2[1]) + (epsilon[1]^2 / sigma2[1])) #ar tikrai teisingai?
    
    #skaiciuoja tolimesnius t visom sudedamosiom dalim (f, epsilon, sigma)
    for (t in 2:n) {
      f[t] <- a + 0.5 * b * ((exp(kappa * P_t_in[t]) - 1) / (exp(kappa * P_t_in[t]) + 1) - (exp(gamma * N_t_in[t]) - 1) / (exp(gamma * N_t_in[t]) + 1))
      # Adding f corrections
      # if (P_t[t] == 0 && N_t[t] == 0) {
      #   f_t[t] <- 1
      # }
      #NA-GARCH
      sigma2[t] <- max(f[t-1] * (omega + alpha * epsilon[t-1]^2 + beta * sigma2[t-1]), 1e-6)
      epsilon[t] <- r_in[t] - mu
      logL <- logL - 0.5 * (log(2 * pi) + log(sigma2[t]) + (epsilon[t]^2 / sigma2[t]))
    }
    -logL
  }
  
  
  # Initial parameter values (Check which ones it is the best to use)!!!
  start_params <- params
  
  # Parameter estimation using optim. adds what to output if there is an error  ir nekonverguoja.
  
  fit <- tryCatch({
    # First attempt using "BFGS"
    optim(start_params, log_likelihood,
          r = r, P_t = P_t, N_t = N_t, a = a, b = b, kappa = kappa, gamma = gamma, split_ratio = 0.8,
          method = "BFGS", hessian = TRUE, control = list(maxit = 500, reltol = 1e-8))
  }, error = function(e) {
    # If "BFGS" does not converge, try "Nelder-Mead"
    tryCatch({
      optim(start_params, log_likelihood,
            r = r, P_t = P_t, N_t = N_t, a = a, b = b, kappa = kappa, gamma = gamma, split_ratio = 0.8,
            method = "Nelder-Mead", hessian = TRUE, control = list(maxit = 500, reltol = 1e-8))
    }, error = function(e) {
      # If "Nelder-Mead" does not converge, try "L-BFGS-B"
      tryCatch({
        optim(start_params, log_likelihood,
              r = r, P_t = P_t, N_t = N_t, a = a, b = b, kappa = kappa, gamma = gamma, split_ratio = 0.8,
              method = "L-BFGS-B", hessian = TRUE, control = list(maxit = 500, reltol = 1e-8))
      }, error = function(e) {
        # If "L-BFGS-B" does not converge, try "CG"
        tryCatch({
          optim(start_params, log_likelihood,
                r = r, P_t = P_t, N_t = N_t, a = a, b = b, kappa = kappa, gamma = gamma, split_ratio = 0.8,
                method = "CG", hessian = TRUE, control = list(maxit = 500, reltol = 1e-8))
        }, error = function(e) {
          # If "CG" does not converge, try "SANN"
          tryCatch({
            optim(start_params, log_likelihood,
                  r = r, P_t = P_t, N_t = N_t, a = a, b = b, kappa = kappa, gamma = gamma, split_ratio = 0.8,
                  method = "SANN", hessian = TRUE, control = list(maxit = 500, reltol = 1e-8))
          }, error = function(e) {
            # If all methods fail, return the initial parameters with a "Do not converge" message
            return(list(par = start_params, value = "Do not converge", hessian = matrix(NA, 4, 4)))
          })
        })
      })
    })
  })
  
  # Getting parameters
  est_params <- fit$par
  se_params <- sqrt(diag(solve(fit$hessian)))
  p_values <- 2 * (1 - pnorm(abs(est_params / se_params)))
  n_in <- length(r_in)
  logL <- fit$value
  k <- length(est_params)
  AIC <- if(is.numeric(logL)) {2 * k - 2 * logL} else {NA}
  BIC <- if(is.numeric(logL)) {k * log(n) - 2 * logL} else {NA}
  n_out <- length(r_out)
  
  # Residuals
  # Residuals <- (r_in - est_params[1])/sigma2
  
  # Pradeda forecastinti. imama out-of sample dalis
  
  # Define parameters
  mu <- est_params[1]
  omega <- est_params[2]
  alpha <- est_params[3]
  beta <- est_params[4]
  
  # Check p-values and adjust parameters accordingly
  if (p_values[3] > 0.05 | is.na(p_values[3])) {
    alpha <- 0
  }
  if (p_values[4] > 0.05 | is.na(p_values[4])) {
    beta <- 0
  }
  if (p_values[1] > 0.05 | is.na(p_values[1])) {
    mu <- 0
  }
  
  forecasted_volatility <- numeric(n_out)
  
  for (t in 1:n_out) {
    f[t + split_point] <- a + 0.5 * b * ((exp(kappa * P_t_out[t]) - 1) / (exp(kappa * P_t_out[t]) + 1) - (exp(gamma * N_t_out[t]) - 1) / (exp(gamma * N_t_out[t]) + 1))
    
    sigma2[t + split_point] <- max(f[t + split_point - 1] * (omega + alpha * epsilon[t+ split_point - 1]^2 + beta * sigma2[t+ split_point - 1]), 1e-6)
    epsilon[t+ split_point] <- r_out[t] - est_params[1]
    z <- rnorm(n_out, mean = 0, sd = 1)
    
    forecasted_volatility[t] <- sigma2[t + split_point]
    # forecasted_returns <- est_params[1] + z[t] * sqrt(sigma2[t])
  }
  
  # realized_volatility <- sqrt(sum(epsilon^2)/((n_out-1)))
  realized_volatility <- epsilon[(split_point+1):(split_point+n_out)]
  
  
  # ARCH <- FinTS::ArchTest(r)
  # ARCH_Test <- as.numeric(ARCH$p.value)
  RMSE <- sqrt(mean((forecasted_volatility - abs(realized_volatility))^2, na.rm = TRUE))
  MAE <- mean(abs(forecasted_volatility - abs(realized_volatility)), na.rm = TRUE)
  
  ##### result list
  result_list <- list(
    optim_fit = fit,
    Parameters = est_params,
    StdErrors = se_params,
    PValues = p_values,
    AIC = AIC,
    BIC = BIC,
    RMSE = RMSE,
    MAE = MAE,
    # ARCH_Test = ARCH_Test,
    ActualReturnsInSample = r_in,
    ActualReturnsOutSample = r_out,
    ForecastedReturns = forecasted_volatility,
    Sigma2 = sigma2,
    f = f
    # , Residuals = epsilon
    # z=z
  )
  
  return(result_list)
  
}

# Define function for grid creation

generate_parameter_grid <- function(a_range, b_range, kappa_range, gamma_range,
                                    a_step, b_step, kappa_step, gamma_step) {
  a_values <- seq(a_range[1], a_range[2], by = a_step)
  b_values <- seq(b_range[1], b_range[2], by = b_step)
  kappa_values <- seq(kappa_range[1], kappa_range[2], by = kappa_step)
  gamma_values <- seq(gamma_range[1], gamma_range[2], by = gamma_step)
  
  expand.grid(a = a_values, b = b_values, kappa = kappa_values, gamma = gamma_values)
}


# Define parameter grid

parameter_grid <- generate_parameter_grid(a_range = c(0.5, 2), 
                                          a_step = 1,
                                          b_range = c(0.5, 2),
                                          b_step = 1,
                                          kappa_range = c(2, 4), 
                                          kappa_step = 1,
                                          gamma_range = c(2, 4), 
                                          gamma_step = 1)
# Loop scenarios
# start_time <- Sys.time()
# pb <- txtProgressBar(min = 0, max = nrow(parameter_grid), style = 3)
# 
# NA_GARCH_output <- list()
# 
# for (i in 1:nrow(parameter_grid)) {
#   NA_GARCH_output[[i]] <- NA_GARCH_Evaluation(r,P_t,N_t,a = parameter_grid[i, 1],b = parameter_grid[i, 2],
#                                               kappa = parameter_grid[i, 3], gamma = parameter_grid[i, 4],
#                                               params = c(0.1,0.1,0.1,0.1))
#   # Update progress bar
#   setTxtProgressBar(pb, i)
# 
# }
# close(pb)
# end_time <- Sys.time()
# total_runtime <- end_time - start_time
# total_runtime


### Parallelized running through parameter grid
## added for parallelization
n_cores <- detectCores() - 1  # Leave 1 core for the OS
n_cores

start_time <- Sys.time()
NA_GARCH_output <- mclapply(1:nrow(parameter_grid), function(i) {
  NA_GARCH_Evaluation(r, P_t, N_t,
                      a = parameter_grid[i, 1],
                      b = parameter_grid[i, 2],
                      kappa = parameter_grid[i, 3],
                      gamma = parameter_grid[i, 4],
                      params = c(0.1, 0.1, 0.1, 0.1))
}, mc.cores = 2)  # Set the number of cores
end_time <- Sys.time()
time_taken <- end_time - start_time
print(paste("Time taken: ", time_taken))


# Data storage

# start_time <- Sys.time()
# pb <- txtProgressBar(min = 0, max = nrow(parameter_grid), style = 3)

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
  # setTxtProgressBar(pb, i)
}
# 
# close(pb)
# end_time <- Sys.time()
# total_runtime <- end_time - start_time
# total_runtime


Parameter_values <- data.frame(Scenario = paste("SimulatedNAGARCH",
                                                parameter_grid[1,1],parameter_grid[1,2],parameter_grid[1,3],parameter_grid[1,4],
                                                sep = "_"),
                               Parameter = c("a", "b", "kappa", "gamma", 
                                             "mu", "omega", "alpha", "beta", 
                                             "mu_p", "omega_p", "alpha_p", "beta_p",
                                             "mu_sign", "omega_sign", "alpha_sign", "beta_sign",
                                             "AIC", "BIC", "RMSE", "MAE"), #"ARCH_Test"
                               Value = c(parameter_grid[1,1],parameter_grid[1,2],parameter_grid[1,3],parameter_grid[1,4],
                                         NA_GARCH_output[[1]]$Parameters[1], NA_GARCH_output[[1]]$Parameters[2], NA_GARCH_output[[1]]$Parameters[3], NA_GARCH_output[[1]]$Parameters[4],
                                         NA_GARCH_output[[1]]$PValues[1], NA_GARCH_output[[1]]$PValues[2], NA_GARCH_output[[1]]$PValues[3], NA_GARCH_output[[1]]$PValues[4],
                                         NA_GARCH_output[[1]]$PValues[1] < 0.05, NA_GARCH_output[[1]]$PValues[2] < 0.05, NA_GARCH_output[[1]]$PValues[3] < 0.05, NA_GARCH_output[[1]]$PValues[4] < 0.05,
                                         NA_GARCH_output[[1]]$AIC, NA_GARCH_output[[1]]$BIC, NA_GARCH_output[[1]]$RMSE, NA_GARCH_output[[1]]$MAE) ) #NA_GARCH_output[[1]]$ARCH_Test

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
                               "AIC", "BIC", "RMSE", "MAE"), #"ARCH_Test"
                 Value = c( parameter_grid[i,1],parameter_grid[i,2],parameter_grid[i,3],parameter_grid[i,4],
                            NA_GARCH_output[[i]]$Parameters[1], NA_GARCH_output[[i]]$Parameters[2], NA_GARCH_output[[i]]$Parameters[3], NA_GARCH_output[[i]]$Parameters[4],
                            NA_GARCH_output[[i]]$PValues[1], NA_GARCH_output[[i]]$PValues[2], NA_GARCH_output[[i]]$PValues[3], NA_GARCH_output[[i]]$PValues[4],
                            NA_GARCH_output[[i]]$PValues[1] < 0.05, NA_GARCH_output[[i]]$PValues[2] < 0.05, NA_GARCH_output[[i]]$PValues[3] < 0.05, NA_GARCH_output[[i]]$PValues[4] < 0.05,
                            NA_GARCH_output[[i]]$AIC, NA_GARCH_output[[i]]$BIC, NA_GARCH_output[[i]]$RMSE, NA_GARCH_output[[i]]$MAE) ) #NA_GARCH_output[[i]]$ARCH_Test
    )
}


# Parameter_values_df <- Parameter_values %>%
#   tidyr::pivot_wider(names_from = Parameter, values_from = Value) %>% 
#   mutate(ab = a+b)

### Save data to csv output

write.csv(Parameter_values, "Parameter_values_v1.csv")
# write.csv(Parameter_values_df, "Parameter_values_df.csv")
write.csv(Return_values, "Return_values_v1.csv")

end_time_total <- Sys.time()
runtime_all <- end_time_total - start_time_total
runtime_all
