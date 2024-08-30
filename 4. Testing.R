
library(vars)
install.packages("FinTS")
library(FinTS)

parameter_grid <- data.frame(a = c(0.5),
                             b = c(1.5),
                             kappa = c(4),
                             gamma = c(4))

# Create a loop to run all scenarios
NA_GARCH_output <- list()

for (i in 1:nrow(parameter_grid)) {
  NA_GARCH_output[[i]] <- NA_GARCH_Evaluation(r,P_t,N_t,a = parameter_grid[i, 1],b = parameter_grid[i, 2], 
                                              kappa = parameter_grid[i, 3], gamma = parameter_grid[i, 4], 
                                              params = c(0.1,0.1,0.1,0.1))
}

NA_GARCH_output

NA_GARCH_output[[1]]$optim_fit$par
NA_GARCH_output[[1]]$PValues

Box.test(r, lag = 100, type = "Ljung-Box")
acf(r)

FinTS::ArchTest(r)
