library(ggplot2)
library(dplyr)
# Table 1 - all scenarios, simulations and grid
# Get all objects that start with "results_parallel"
# List all CSV files in the working directory that start with "results_parallel"
csv_files <- list.files(pattern = "^results_parallel.*\\.csv$")

# Read all CSV files into a list of data frames
df_list <- lapply(csv_files, read.csv)

# Combine all data frames by rows
combined_results <- bind_rows(df_list)

# View the combined data
print(combined_results)


nrow(combined_results)
nrow(results)
results <- combined_results %>% 
  na.omit()

head(results)

# Table 2 - aggregated grid for each scenario and simulation
# creating "all else equal environment"

Grid_aggregate_Aux1 <- results %>%
  mutate(Combination_a = interaction(b, kappa, gamma, drop = TRUE),
         Combination_b = interaction(a, kappa, gamma, drop = TRUE),
         Combination_kappa = interaction(a, b, gamma, drop = TRUE),
         Combination_gamma = interaction(a, b, kappa, drop = TRUE))

Grid_aggregate_Aux2 <- Grid_aggregate_Aux1 %>%
  tidyr::pivot_longer(cols = c(a, b, kappa, gamma), names_to = "Parameter", values_to = "Value") %>%
  tidyr::pivot_longer(cols = starts_with("Combination"), names_to = "Combination_type", values_to = "Combination") %>%
  mutate(Combination = factor(Combination))  # Ensure Combination is a factor for smooths


Grid_aggregate <- Grid_aggregate_Aux2 %>%
  group_by(Name, Simulation_nr, Parameter, Value) %>%
  summarize(RMSE_HV = mean(RMSE_HV, na.rm = TRUE),
            # RMSE_HV.stdev = sd(RMSE_HV, na.rm = TRUE),
            MAE_HV = mean(MAE_HV, na.rm = TRUE),
            # MAE_HV.stdev = sd(MAE_HV, na.rm = TRUE),
            RMSE_RV = mean(RMSE_RV, na.rm = TRUE),
            MAE_RV = mean(MAE_RV, na.rm = TRUE),
            AIC = mean(AIC, na.rm = TRUE),
            BIC = mean(BIC, na.rm = TRUE))  # Calculate mean RMSE for each parameter value

# Table 3 - aggregate of simulations for each scenario

Simulation_aggregate <- Grid_aggregate %>% 
  group_by(Name, Parameter, Value) %>%
  summarize(RMSE_HV = mean(RMSE_HV, na.rm = TRUE),
            MAE_HV = mean(MAE_HV, na.rm = TRUE),
            RMSE_RV = mean(RMSE_RV, na.rm = TRUE),
            MAE_RV = mean(MAE_RV, na.rm = TRUE),
            AIC = mean(AIC, na.rm = TRUE),
            BIC = mean(BIC, na.rm = TRUE))  # Calculate mean RMSE for each parameter value

Simulation_aggregate <- Grid_aggregate %>%  
  group_by(Name, Parameter, Value) %>%  
  summarize(RMSE_HV = mean(RMSE_HV, na.rm = TRUE),  
            RMSE_HV.stdev = sd(RMSE_HV, na.rm = TRUE),  
            MAE_HV = mean(MAE_HV, na.rm = TRUE),  
            MAE_HV.stdev = sd(MAE_HV, na.rm = TRUE),  
            RMSE_RV = mean(RMSE_RV, na.rm = TRUE),  
            RMSE_RV.stdev = sd(RMSE_RV, na.rm = TRUE),  
            MAE_RV = mean(MAE_RV, na.rm = TRUE),  
            MAE_RV.stdev = sd(MAE_RV, na.rm = TRUE),  
            AIC = mean(AIC, na.rm = TRUE),  
            BIC = mean(BIC, na.rm = TRUE))

### Plots of metrics

ggplot(Simulation_aggregate, aes(x = Value, y = RMSE_RV)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +  # Smooth with linear regression
  geom_vline(aes(xintercept = ifelse(Parameter == "a", 0.7, 
                                     ifelse(Parameter == "b", 0.7, 
                                            ifelse(Parameter == "kappa", 4, 
                                                   ifelse(Parameter == "gamma", 4, NA))))), 
             linetype = "dashed", color = "red") +  # Dashed red vertical lines  
  geom_text(aes(x = ifelse(Parameter == "a", 0.7, 
                           ifelse(Parameter == "b", 0.7, 
                                  ifelse(Parameter == "kappa", 4, 
                                         ifelse(Parameter == "gamma", 4, NA)))),
                y = max(RMSE_RV, na.rm = TRUE) * 0.95,  # Adjust vertical position
                label = "actual value"),
            color = "red", hjust = -0.1, size = 2.5) + 
  facet_wrap(~ Parameter, scales = "free_x") +  # Facet by parameter
  labs(title = "Mean RMSE RV for Each Parameter Value (Grouped by Name and Simulation)",
       x = "Parameter Value",
       y = "Mean RMSE") +
  theme_minimal()



ggplot(Simulation_aggregate, aes(x = Value, y = RMSE_HV)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +  # Smooth with linear regression
  geom_vline(aes(xintercept = ifelse(Parameter == "a", 0.7, 
                                     ifelse(Parameter == "b", 0.7, 
                                            ifelse(Parameter == "kappa", 4, 
                                                   ifelse(Parameter == "gamma", 4, NA))))), 
             linetype = "dashed", color = "red") +  # Dashed red vertical lines  
  geom_text(aes(x = ifelse(Parameter == "a", 0.7, 
                           ifelse(Parameter == "b", 0.7, 
                                  ifelse(Parameter == "kappa", 4, 
                                         ifelse(Parameter == "gamma", 4, NA)))),
                y = max(RMSE_RV, na.rm = TRUE) * 0.95,  # Adjust vertical position
                label = "actual value"),
            color = "red", hjust = -0.1, size = 2.5) + 
  facet_wrap(~ Parameter, scales = "free_x") +  # Facet by parameter
  labs(title = "Mean RMSE HV for Each Parameter Value (Grouped by Name and Simulation)",
       x = "Parameter Value",
       y = "Mean RMSE") +
  theme_minimal()


ggplot(Simulation_aggregate, aes(x = Value, y = AIC)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +  # Smooth with linear regression
  geom_vline(aes(xintercept = ifelse(Parameter == "a", 0.7, 
                                     ifelse(Parameter == "b", 0.7, 
                                            ifelse(Parameter == "kappa", 4, 
                                                   ifelse(Parameter == "gamma", 4, NA))))), 
             linetype = "dashed", color = "red") +  # Dashed red vertical lines  
  geom_text(aes(x = ifelse(Parameter == "a", 0.7, 
                           ifelse(Parameter == "b", 0.7, 
                                  ifelse(Parameter == "kappa", 4, 
                                         ifelse(Parameter == "gamma", 4, NA)))),
                y = max(RMSE_RV, na.rm = TRUE) * 0.95,  # Adjust vertical position
                label = "actual value"),
            color = "red", hjust = -0.1, size = 2.5) + 
  facet_wrap(~ Parameter, scales = "free_x") +  # Facet by parameter
  labs(title = "Mean AIC for Each Parameter Value (Grouped by Name and Simulation)",
       x = "Parameter Value",
       y = "Mean AIC") +
  theme_minimal()

ggplot(Simulation_aggregate, aes(x = Value, y = BIC)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +  # Smooth with linear regression
  geom_vline(aes(xintercept = ifelse(Parameter == "a", 0.7, 
                                     ifelse(Parameter == "b", 0.7, 
                                            ifelse(Parameter == "kappa", 4, 
                                                   ifelse(Parameter == "gamma", 4, NA))))), 
             linetype = "dashed", color = "red") +  # Dashed red vertical lines  
  geom_text(aes(x = ifelse(Parameter == "a", 0.7, 
                           ifelse(Parameter == "b", 0.7, 
                                  ifelse(Parameter == "kappa", 4, 
                                         ifelse(Parameter == "gamma", 4, NA)))),
                y = max(RMSE_RV, na.rm = TRUE) * 0.95,  # Adjust vertical position
                label = "actual value"),
            color = "red", hjust = -0.1, size = 2.5) + 
  facet_wrap(~ Parameter, scales = "free_x") +  # Facet by parameter
  labs(title = "Mean BIC for Each Parameter Value (Grouped by Name and Simulation)",
       x = "Parameter Value",
       y = "Mean AIC") +
  theme_minimal()


### Relationship between parameters
Simulation_aggregate
results

ggplot(results, aes(x = a, y = b, fill = RMSE_RV)) +
  geom_tile() +  # Create the heatmap tiles
  scale_fill_gradient(low = "dark blue", high = " light blue") +  # Color gradient for AIC values
  labs(title = "Heatmap of RMSE_RV for a vs b", x = "Parameter a", y = "Parameter b", fill = "RMSE_RV") +
  theme_minimal()  # Clean theme for better visualization


results %>% select(RMSE_RV) %>% summarise(min = min(RMSE_RV), max=max(RMSE_RV))
  