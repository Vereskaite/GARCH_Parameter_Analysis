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

# Define functions

Data_Simulation_function <- function(a_true, b_true, kappa_true, gamma_true) {
  # Parameters
  days <- 2500
  # nr of 5 minute intervals
  intraday_steps <- 78
  n <- days*intraday_steps
  mu <- 0
  omega <- 0.1
  alpha <- 0.1
  beta <- 0.1
  a <- a_true
  b <- b_true
  kappa <- kappa_true
  gamma <- gamma_true
  
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

NA_GARCH_Evaluation <- function(r, r_intraday, P_t, N_t, a, b, kappa, gamma, split_ratio, params) {
  
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
  
  log_likelihood <- function(r, P_t, N_t, a, b, kappa, gamma, split_ratio, params, return_full) {
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
    if (return_full) {
      return(list(log_l = -logL, f = f, sigma2 = sigma2, epsilon = epsilon))
    } else {
      return(-logL)
    }
  }
  
  
  # Initial parameter values (Check which ones it is the best to use)!!!
  start_params <- params
  
  # Parameter estimation using optim. adds what to output if there is an error  ir nekonverguoja.
  
  fit <- tryCatch({
    # First attempt using "BFGS"
    optim(start_params, log_likelihood,
          r = r, P_t = P_t, N_t = N_t, a = a, b = b, kappa = kappa, gamma = gamma, split_ratio = 0.8,return_full = FALSE,
          method = "BFGS", hessian = TRUE, control = list(maxit = 500, reltol = 1e-8))
  }, error = function(e) {
    # If "BFGS" does not converge, try "Nelder-Mead"
    tryCatch({
      optim(start_params, log_likelihood,
            r = r, P_t = P_t, N_t = N_t, a = a, b = b, kappa = kappa, gamma = gamma, split_ratio = 0.8,return_full = FALSE,
            method = "Nelder-Mead", hessian = TRUE, control = list(maxit = 500, reltol = 1e-8))
    }, error = function(e) {
      # If "Nelder-Mead" does not converge, try "L-BFGS-B"
      tryCatch({
        optim(start_params, log_likelihood,
              r = r, P_t = P_t, N_t = N_t, a = a, b = b, kappa = kappa, gamma = gamma, split_ratio = 0.8,return_full = FALSE,
              method = "L-BFGS-B", hessian = TRUE, control = list(maxit = 500, reltol = 1e-8))
      }, error = function(e) {
        # If "L-BFGS-B" does not converge, try "CG"
        tryCatch({
          optim(start_params, log_likelihood,
                r = r, P_t = P_t, N_t = N_t, a = a, b = b, kappa = kappa, gamma = gamma, split_ratio = 0.8,return_full = FALSE,
                method = "CG", hessian = TRUE, control = list(maxit = 500, reltol = 1e-8))
        }, error = function(e) {
          # If "CG" does not converge, try "SANN"
          tryCatch({
            optim(start_params, log_likelihood,
                  r = r, P_t = P_t, N_t = N_t, a = a, b = b, kappa = kappa, gamma = gamma, split_ratio = 0.8,return_full = FALSE,
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
  forecasted_returns <- numeric(n_out)
  
  Logl_output <-  log_likelihood(r, P_t, N_t, a, b, kappa, gamma, split_ratio = 0.8, params, return_full = TRUE)  
  
  f <- Logl_output$f
  sigma2 <- Logl_output$sigma2
  epsilon  <- Logl_output$epsilon
  
  for (t in 1:n_out) {
    f[t + split_point] <- a + 0.5 * b * ((exp(kappa * P_t_out[t]) - 1) / (exp(kappa * P_t_out[t]) + 1) - (exp(gamma * N_t_out[t]) - 1) / (exp(gamma * N_t_out[t]) + 1))
    
    sigma2[t + split_point] <- max(f[t + split_point - 1] * (omega + alpha * epsilon[t+ split_point - 1]^2 + beta * sigma2[t+ split_point - 1]), 1e-6)
    epsilon[t+ split_point] <- r_out[t] - est_params[1]
    z <- rnorm(n_out, mean = 0, sd = 1)
    
    forecasted_volatility[t] <- sqrt(sigma2[t + split_point])
    forecasted_returns[t] <- est_params[1] + z[t] * sqrt(sigma2[t+split_point])
  }
  
  # realized_volatility <- sqrt(sum(epsilon^2)/((n_out-1)))
  # realized_volatility <- epsilon[(split_point+1):(split_point+n_out)]
  # realized_volatility <- STDEV(epsilon)
  
  

  # realisedVolatility <- function(returns) {
  #   # Create a vector of zeros for RV, with the same length as returns
  #   RV <- numeric(length(returns))
  #   
  #   # Loop through the returns to calculate squared returns for RV
  #   for (i in 1:length(returns)) {
  #     RV[i] <- returns[i]^2
  #   }
  #   
  #   return(RV)
  # }
  # 
  # RV <- realisedVolatility(r)
  
  # realisedVolatility <- function(returns, RVstepSize) {
  #   RV <- rep(NA, length(returns))  # Initialize realized volatility vector
  #   mean_returns <- mean(returns)  # Calculate mean of returns
  #   centered_returns <- returns - mean_returns  # Center returns around zero
  #   
  #   for (i in 1:(length(centered_returns) - RVstepSize)) {
  #     RV[i + RVstepSize - 1] <- sd(centered_returns[i:(i + RVstepSize - 1)])  # Calculate standard deviation of centered returns
  #   }
  #   return(RV)
  # }
  
  calculate_rmse <- function(realized_volatility, forecasted_volatility) {
    # Remove NA values for the out-of-sample period
    valid_indices <- !is.na(realized_volatility[(length(realized_volatility) - length(forecasted_volatility) + 1):length(realized_volatility)]) & 
      !is.na(forecasted_volatility)
    squared_errors <- (forecasted_volatility[valid_indices] - realized_volatility[(length(realized_volatility) - length(forecasted_volatility) + 1):length(realized_volatility)][valid_indices])^2
    rmse <- sqrt(mean(squared_errors))
    return(rmse)
  }
  
  # Function to calculate MAE
  calculate_mae <- function(realized_volatility, forecasted_volatility) {
    # Remove NA values for the out-of-sample period
    valid_indices <- !is.na(realized_volatility[(length(realized_volatility) - length(forecasted_volatility) + 1):length(realized_volatility)]) & 
      !is.na(forecasted_volatility)
    absolute_errors <- abs(forecasted_volatility[valid_indices] - realized_volatility[(length(realized_volatility) - length(forecasted_volatility) + 1):length(realized_volatility)][valid_indices])
    mae <- mean(absolute_errors)
    return(mae)
  }
  
  # realized_volatility <- realisedVolatility(r, 5)
  historical_volatility <- sqrt((r-mean(r))^2)
  realized_volatility <- sqrt(rollapply((r_intraday-mean(r_intraday))^2, width = 78, by = 78, FUN = sum, align = "right", partial = FALSE))
  
  
  RMSE_RV <- calculate_rmse(realized_volatility, forecasted_volatility)
  RMSE_HV <- calculate_rmse(historical_volatility, forecasted_volatility)
  
  MAE_RV <- calculate_mae(realized_volatility, forecasted_volatility)
  MAE_HV <- calculate_mae(historical_volatility, forecasted_volatility)


  # ARCH <- FinTS::ArchTest(r)
  # ARCH_Test <- as.numeric(ARCH$p.value)
  # RMSE <- sqrt(mean((forecasted_volatility - abs(realized_volatility))^2, na.rm = TRUE))
  # MAE <- mean(abs(forecasted_volatility - abs(realized_volatility)), na.rm = TRUE)
  
  ##### result list
  result_list <- list(
    optim_fit = fit,
    Parameters = est_params,
    StdErrors = se_params,
    PValues = p_values,
    AIC = AIC,
    BIC = BIC,
    RMSE_HV = RMSE_HV,
    RMSE_RV = RMSE_RV,
    MAE_HV = MAE_HV,
    MAE_RV = MAE_RV,
    # ARCH_Test = ARCH_Test,
    ActualReturnsInSample = r_in,
    ActualReturnsOutSample = r_out,
    ForecastedVolatility = forecasted_volatility,
    ForecastedReturns = forecasted_returns,
    RealizedVolatility = realized_volatility,
    Sigma2 = sigma2,
    f = f
    # , Residuals = epsilon
    # z=z
  )
  
  return(result_list)
  
}

generate_parameter_grid <- function(a_range, b_range, kappa_range, gamma_range,
                                    a_step, b_step, kappa_step, gamma_step) {
  a_values <- seq(a_range[1], a_range[2], by = a_step)
  b_values <- seq(b_range[1], b_range[2], by = b_step)
  kappa_values <- seq(kappa_range[1], kappa_range[2], by = kappa_step)
  gamma_values <- seq(gamma_range[1], gamma_range[2], by = gamma_step)
  
  expand.grid(a = a_values, b = b_values, kappa = kappa_values, gamma = gamma_values)
}

# Create my own function that will run the grid for different parameter selection 
# and reruns with different epsilons

# Define needed parameters
sim <- 2
NA_GARCH_output <- list()

# Test parameter grid
parameter_grid <- generate_parameter_grid(a_range = c(0.5, 2), 
                                          a_step = 1,
                                          b_range = c(0.5, 2),
                                          b_step = 1,
                                          kappa_range = c(2, 4), 
                                          kappa_step = 1,
                                          gamma_range = c(2, 4), 
                                          gamma_step = 1)
parameter_grid <- parameter_grid[1:2, ]

# Selected parameter grid
# a_values <- c(0.1, 0.2, 0.4, 0.6, 0.8, 1, 1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3)
# b_values <- c(0.1, 0.2, 0.4, 0.6, 0.8, 1, 1.2,1.4,1.6,1.8,2,2.2,2.4,2.6,2.8,3)
# kappa_values <- c(0.25,0.5,0.75,1,2,3,4,5,6,7,8,9,10,12,14,16,18,20)
# gamma_values <- c(0.25,0.5,0.75,1,2,3,4,5,6,7,8,9,10,12,14,16,18,20)
# 
# parameter_grid <- expand.grid(a_values, b_values, kappa_values, gamma_values)

  true_parameters_grid <- data.frame(Name = c("Baseline", "Kappa=5", "Kappa=6", "Kappa=7"),
                                   a_true = c(0.8,0.8,0.8,0.8),
                                   b_true = c(0.8,0.8,0.8,0.8),
                                   kappa_true = c(4,5,6,7),
                                   gamma_true = c(4,4,4,4))

true_parameters_grid <- true_parameters_grid[1:2, ]

Parameter_values <- data.frame(Scenario = character(),
                               # Scenario_nr = numeric(),
                               a = numeric(),
                               b = numeric(),
                               kappa = numeric(),
                               gamma = numeric(),
                               RMSE_HV = numeric(),
                               RMSE_RV = numeric(),
                               MAE_HV = numeric(),
                               MAE_RV = numeric(),
                               AIC = numeric(),
                               BIC = numeric())

results <- data.frame()

# Main function

Initial_function <- function(parameter_grid,a_true,b_true,kappa_true,gamma_true) {
  
  Data_k4_g4 <- Data_Simulation_function(a_true,b_true,kappa_true, gamma_true)
  
  # Intra-day split
  r_intraday <- Data_k4_g4$Returns
  P_t_intraday <- Data_k4_g4$Positive_Sentiment
  N_t_intraday <- Data_k4_g4$Negative_Sentiment
  
  # r <- sqrt(rollapply(r_intraday^2, width = 78, by = 78, FUN = sum, align = "right", partial = FALSE))
  r <- rollapply(r_intraday, width = 78, by = 78, FUN = sum, align = "right", partial = FALSE)
  P_t <- P_t_intraday[seq(1, length(P_t_intraday), by = 78)]
  N_t <- N_t_intraday[seq(1, length(N_t_intraday), by = 78)]
  

  for (i in 1:nrow(parameter_grid)) {
    NA_GARCH_output[[i]] <- NA_GARCH_Evaluation(r,r_intraday, P_t,N_t,a = parameter_grid[i, 1],b = parameter_grid[i, 2],
                                                kappa = parameter_grid[i, 3], gamma = parameter_grid[i, 4],
                                                params = c(0.1,0.1,0.1,0.1))
  }
  
  for (i in 1:length(NA_GARCH_output)){ 
    Parameter_values <- Parameter_values %>% 
      bind_rows(
        data.frame(Scenario = paste(a_true,b_true,kappa_true,gamma_true, sep = "_"),
                   a = parameter_grid[i,1],
                   b = parameter_grid[i,2],
                   kappa = parameter_grid[i,3],
                   gamma = parameter_grid[i,4],
                   RMSE_HV = NA_GARCH_output[[i]]$RMSE_HV,
                   RMSE_RV = NA_GARCH_output[[i]]$RMSE_RV,
                   MAE_HV =  NA_GARCH_output[[i]]$MAE_HV,
                   MAE_RV =  NA_GARCH_output[[i]]$MAE_RV,
                   AIC = NA_GARCH_output[[i]]$AIC,
                   BIC = NA_GARCH_output[[i]]$BIC
                         ))
  }

  Parameter_values
}


### No parallelization

start_time <- Sys.time()
# pb_max <- nrow(true_parameters_grid)*sim
# pb <- txtProgressBar(min = 0, max = pb_max, style = 3)

for (j in 1:nrow(true_parameters_grid)){
  for (i in 1:sim) {
    temp_result <- Initial_function(parameter_grid,a_true = true_parameters_grid[j, 2],
                                    b_true = true_parameters_grid[j, 3],
                                    kappa_true = true_parameters_grid[j, 4],
                                    gamma_true = true_parameters_grid[j, 5])
    temp_result <- temp_result %>% mutate(Simulation_nr = paste0(i),
                                          Name = true_parameters_grid[j, 1])
    results <- rbind(results, temp_result)

    # setTxtProgressBar(pb, i)

  }
}

close(pb)
end_time <- Sys.time()
total_runtime <- end_time - start_time
total_runtime


### With parallelization

write.csv(results, "results.csv")

end_time_total <- Sys.time()
runtime_all <- end_time_total - start_time_total
runtime_all

