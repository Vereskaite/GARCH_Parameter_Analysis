# Define parameters
parameter_grid <- generate_parameter_grid(a_range = c(0.5, 1.5), 
                                          a_step = 1,
                                          b_range = c(0.5, 2),
                                          b_step = 1,
                                          kappa_range = c(2, 4), 
                                          kappa_step = 1,
                                          gamma_range = c(2, 4), 
                                          gamma_step = 1)

# Initialize progress bar
pb <- txtProgressBar(min = 0, max = nrow(parameter_grid), style = 3)

# Initialize output list
NA_GARCH_output <- list()

# Loop through the parameter grid with progress bar
for (i in 1:nrow(parameter_grid)) {
  
  # Perform your evaluation
  NA_GARCH_output[[i]] <- NA_GARCH_Evaluation(r, P_t, N_t, 
                                              a = parameter_grid[i, 1], 
                                              b = parameter_grid[i, 2], 
                                              kappa = parameter_grid[i, 3], 
                                              gamma = parameter_grid[i, 4], 
                                              params = c(0.1, 0.1, 0.1, 0.1))
  
  # Update progress bar
  setTxtProgressBar(pb, i)
}

# Close the progress bar when done
close(pb)
