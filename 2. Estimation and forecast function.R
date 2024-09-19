# Load necessary libraries
library(ggplot2)
library(dplyr)
options(scipen = 9999)

# Define function 
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
    
    # sukuria tuscius ir veliau uzpildo
    n <- length(r_in)
    sigma2 <- numeric(n)
    epsilon <- numeric(n)
    f <- numeric(n)
    
    # Suskaiciuoja pirmine value
    ## naujienu itakos forma, cia imama initial value
    
    f[1] <- a + 0.5 * b * ((exp(kappa * P_t_in[1]) - 1) / (exp(kappa * P_t_in[1]) + 1) - (exp(gamma * N_t_in[1]) - 1) / (exp(gamma * N_t_in[1]) + 1))
    # Adding f corrections
    if (P_t[1] == 0 && N_t[1] == 0) {
      f_t[1] <- 1
    }
    
    # paskaiciuoja initial variance ir uzdeda max, kad nebutu variance neigiamas
    sigma2[1] <- max((omega *(a+b) / (1 - (alpha + beta)*(a+b))), 1e-6)
    epsilon[1] <- r_in[1] - mu
    logL <- -0.5 * (log(2 * pi) + log(sigma2[1]) + (epsilon[1]^2 / sigma2[1])) #ar tikrai teisingai?
    
    #skaiciuoja tolimesnius t visom sudedamosiom dalim (f, epsilon, sigma)
    for (t in 2:n) {
      f[t] <- a + 0.5 * b * ((exp(kappa * P_t_in[t]) - 1) / (exp(kappa * P_t_in[t]) + 1) - (exp(gamma * N_t_in[t]) - 1) / (exp(gamma * N_t_in[t]) + 1))
      # Adding f corrections
      if (P_t[t] == 0 && N_t[t] == 0) {
        f_t[t] <- 1
      }
      #NA-GARCH
      sigma2[t] <- max(f[t-1] * (omega + alpha * epsilon[t-1]^2 + beta * sigma2[t-1]), 1e-6)
      epsilon[t] <- r_in[t] - mu
      # mano irasytas, bet lyg ir neveiekia ,hessianas nekonverguoja
      #logL <- - n/2 * (log(2 * pi)) - 0.5*sum(log(sigma2)) - 0.5*(sum(epsilon^2)/sum(sigma2))
      #GPT, esme, kad su kiekviena iteracija logL dideja (ta pati suma, tik kitokia forma)
      logL <- logL - 0.5 * (log(2 * pi) + log(sigma2[t]) + (epsilon[t]^2 / sigma2[t]))
    }
    -logL  # Return negative log-likelihood
  }
  
  
  # Initial parameter values (Check which ones it is the best to use)!!!
  start_params <- params
  
  # Parameter estimation using optim. adds what to output if there is an error ir nekonverguoja.
  # daugiau pasidometi del nekonvergavimo, galbut kazka pakeitus konverguotu, reiktu pagerinti
  
  # old 
  
  # fit <- tryCatch({
  #   optim(start_params, log_likelihood,
  #         r=r,P_t=P_t,N_t=N_t,a = a,b = b, kappa = kappa, gamma = gamma, split_ratio = 0.8,
  #         method = "BFGS", hessian = TRUE, control = list(maxit = 10000, reltol = 1e-8))
  # }, error = function(e) {
  #   return(list(par = start_params, value = "Do not converge", hessian = matrix(NA, 4, 4)))
  # })
  
  fit <- tryCatch({
    # First attempt using "BFGS"
    optim(start_params, log_likelihood,
          r = r, P_t = P_t, N_t = N_t, a = a, b = b, kappa = kappa, gamma = gamma, split_ratio = 0.8,
          method = "BFGS", hessian = TRUE, control = list(maxit = 10000, reltol = 1e-8))
  }, error = function(e) {
    # If "BFGS" does not converge, try "Nelder-Mead"
    tryCatch({
      optim(start_params, log_likelihood,
            r = r, P_t = P_t, N_t = N_t, a = a, b = b, kappa = kappa, gamma = gamma, split_ratio = 0.8,
            method = "Nelder-Mead", hessian = TRUE, control = list(maxit = 10000, reltol = 1e-8))
    }, error = function(e) {
      # If "Nelder-Mead" does not converge, try "L-BFGS-B"
      tryCatch({
        optim(start_params, log_likelihood,
              r = r, P_t = P_t, N_t = N_t, a = a, b = b, kappa = kappa, gamma = gamma, split_ratio = 0.8,
              method = "L-BFGS-B", hessian = TRUE, control = list(maxit = 10000, reltol = 1e-8))
      }, error = function(e) {
        # If "L-BFGS-B" does not converge, try "CG"
        tryCatch({
          optim(start_params, log_likelihood,
                r = r, P_t = P_t, N_t = N_t, a = a, b = b, kappa = kappa, gamma = gamma, split_ratio = 0.8,
                method = "CG", hessian = TRUE, control = list(maxit = 10000, reltol = 1e-8))
        }, error = function(e) {
          # If "CG" does not converge, try "SANN"
          tryCatch({
            optim(start_params, log_likelihood,
                  r = r, P_t = P_t, N_t = N_t, a = a, b = b, kappa = kappa, gamma = gamma, split_ratio = 0.8,
                  method = "SANN", hessian = TRUE, control = list(maxit = 10000, reltol = 1e-8))
          }, error = function(e) {
            # If all methods fail, return the initial parameters with a "Do not converge" message
            return(list(par = start_params, value = "Do not converge", hessian = matrix(NA, 4, 4)))
          })
        })
      })
    })
  })
  
  
  ### Optim methods: "Nelder-Mead", "BFGS"
  
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
  # Initialize values for forecasting
  sigma2 <- numeric(n_out)
  epsilon <- numeric(n_out)
  f <- numeric(n_out)
  
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
  
  # Calculate initial conditions for forecasting (this is still in-sample)
  f[1] <- a + 0.5 * b * ((exp(kappa * P_t_in[split_point]) - 1) / (exp(kappa * P_t_in[split_point]) + 1) - (exp(gamma * N_t_in[split_point]) - 1) / (exp(gamma * N_t_in[split_point]) + 1))
  sigma2[1] <- max(f[1] * (omega + alpha * (r_in[split_point] - est_params[1])^2 + beta * var(r_in)), 1e-6)
  epsilon[1] <- r_in[split_point]
  
  forecasted_returns <- numeric(n_out)
  
  
  # 
  for (t in 1:n_out) {
    # Forecast next value
    if (t == 1) {
      f[t + split_point] <- a + 0.5 * b * ((exp(kappa * P_t_out[t]) - 1) / (exp(kappa * P_t_out[t]) + 1) - (exp(gamma * N_t_out[t]) - 1) / (exp(gamma * N_t_out[t]) + 1))
    } else {
      f[t + split_point] <- a + 0.5 * b * ((exp(kappa * P_t_out[t - 1]) - 1) / (exp(kappa * P_t_out[t - 1]) + 1) - (exp(gamma * N_t_out[t - 1]) - 1) / (exp(gamma * N_t_out[t - 1]) + 1))
    }
    sigma2[t] <- max(f[t + split_point - 1] * (omega + alpha * epsilon[t]^2 + beta * sigma2[t]), 1e-6)
    epsilon[t] <- r_out[t] - est_params[1]
    z <- rnorm(1000, mean = 0, sd = 1)
    forecasted_returns[t] <- est_params[1] + z[t] * sqrt(sigma2[t])
  }
  
  ARCH <- FinTS::ArchTest(r)
  ARCH_Test <- ARCH$p.value
  RMSE <- sqrt(mean((r_out - forecasted_returns)^2, na.rm = TRUE))
  MAE <- mean(abs(r_out - forecasted_returns), na.rm = TRUE)
  
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
    ARCH_Test = ARCH_Test,
    ActualReturnsInSample = r_in,
    ActualReturnsOutSample = r_out,
    ForecastedReturns = forecasted_returns,
    Sigma2 = sigma2,
    f = f
    # , Residuals = epsilon
    # f = f,
    # sigma2 = sigma2,
    # z=z
  )
  
  return(result_list)
  
}

# Run function 
  # NA_GARCH_simulation_v1 <- NA_GARCH_Evaluation(r,P_t,N_t,a = 0.5,b = 1, kappa = 3, gamma = 3, params = c(0.1,0.1,0.1,0.1))
  # 
  # NA_GARCH_simulation_v1$Parameters
  # NA_GARCH_simulation_v1$f
  # NA_GARCH_simulation_v1$Sigma2
  # 
  # # Plot check
  # plot(NA_GARCH_simulation_v1$ActualReturnsOutSample, type = "l")
  # lines(NA_GARCH_simulation_v1$ForecastedReturns, col = "red", lwd = 2)

# Function to generate parameter grid
generate_parameter_grid <- function(a_range, b_range, kappa_range, gamma_range,
                                    a_step, b_step, kappa_step, gamma_step) {
  a_values <- seq(a_range[1], a_range[2], by = a_step)
  b_values <- seq(b_range[1], b_range[2], by = b_step)
  kappa_values <- seq(kappa_range[1], kappa_range[2], by = kappa_step)
  gamma_values <- seq(gamma_range[1], gamma_range[2], by = gamma_step)
  
  expand.grid(a = a_values, b = b_values, kappa = kappa_values, gamma = gamma_values)
}


# Define parameter ranges and generate the grid
parameter_grid <- generate_parameter_grid(a_range = c(0.2, 3), 
                                          a_step = 0.5,
                                          b_range = c(0.5, 2),
                                          b_step = 1,
                                          kappa_range = c(2, 4), 
                                          kappa_step = 1,
                                          gamma_range = c(2, 4), 
                                          gamma_step = 1)

# Create a loop to run all scenarios - WITHOUT STATUS BAR
NA_GARCH_output <- list()

for (i in 1:nrow(parameter_grid)) {
  NA_GARCH_output[[i]] <- NA_GARCH_Evaluation(r,P_t,N_t,a = parameter_grid[i, 1],b = parameter_grid[i, 2], 
                                              kappa = parameter_grid[i, 3], gamma = parameter_grid[i, 4], 
                                              params = c(0.1,0.1,0.1,0.1))
}

NA_GARCH_output

# parameter_grid <- parameter_grid[1:408,]


