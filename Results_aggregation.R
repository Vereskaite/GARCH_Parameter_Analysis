library(ggplot2)
library(dplyr)
library(tidyr)
library(pat)
library(grid)
library(ggcorrplot)
library(corrplot)
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

# RMSE_RV

# Create a list to store the plots
plots_list <- list()

# Define the parameter combinations
param_combinations <- list(
  c("a", "b"),
  c("a", "kappa"),
  c("a", "gamma"),
  c("b", "kappa"),
  c("b", "gamma"),
  c("kappa", "gamma")
)

# Loop through each parameter combination and create the heatmap
for (params in param_combinations) {
  
  # Aggregate the data for each combination of parameters
  aggregated_data <- results %>%
    group_by(across(all_of(params))) %>%
    summarize(mean_RMSE_RV = mean(RMSE_RV, na.rm = TRUE), .groups = 'drop')
  
  # Create the plot for each parameter combination and store it in the list
  plot <- ggplot(aggregated_data, aes(x = .data[[params[1]]], y = .data[[params[2]]], fill = mean_RMSE_RV)) +  
    geom_tile(width = 1, height = 1) +  
    scale_fill_gradient(low = "blue", high = "red") +  
    labs(title = paste(params[1], "and", params[2]), 
         x = params[1], y = params[2], fill = "Mean RMSE RV") + 
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.key.size = unit(0.2, "cm"),  # Adjust the size of legend keys
          legend.text = element_text(size = 4),  # Adjust the text size in the legend
          legend.title = element_text(size = 5))  # Adjust the title size in the legend
  
  # Add the plot to the list
  plots_list[[paste(params[1], params[2], sep = "_vs_")]] <- plot
}

title <- textGrob("RMSE_RV", gp = gpar(fontsize = 16, fontface = "bold"))
grid.arrange(grobs = plots_list, ncol = 3, top = title) 

# RMSE_HV

# Create a list to store the plots
plots_list <- list()

# Define the parameter combinations
param_combinations <- list(
  c("a", "b"),
  c("a", "kappa"),
  c("a", "gamma"),
  c("b", "kappa"),
  c("b", "gamma"),
  c("kappa", "gamma")
)

# Metrics you want to plot
metrics <- c("RMSE_RV", "RMSE_HV", "AIC", "BIC")

# Loop through each metric
for (metric in metrics) {
  
  # Create a list to store plots for each metric
  metric_plots <- list()
  
  for (params in param_combinations) {
    
    # Aggregate the data for each combination of parameters
    aggregated_data <- results %>%
      group_by(across(all_of(params))) %>%
      summarize(mean_value = mean(.data[[metric]], na.rm = TRUE), .groups = 'drop')
    
    # Create the plot for each parameter combination
    plot <- ggplot(aggregated_data, aes(x = .data[[params[1]]], y = .data[[params[2]]], fill = mean_value)) +  
      geom_tile(width = 1, height = 1) +  
      scale_fill_gradient(low = "blue", high = "red") +  
      labs(title = paste(params[1], "and", params[2]), 
           x = params[1], y = params[2], fill = paste("Mean", metric)) + 
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.key.size = unit(0.2, "cm"),  # Adjust the size of legend keys
            legend.text = element_text(size = 4),  # Adjust the text size in the legend
            legend.title = element_text(size = 5))  # Adjust the title size in the legend
    
    # Add the plot to the metric's plot list
    metric_plots[[paste(params[1], params[2], sep = "_vs_")]] <- plot
  }
  
  # Create the title for the current metric
  title <- textGrob(paste(metric), gp = gpar(fontsize = 16, fontface = "bold"))
  
  # Arrange the 6 subplots for the current metric in a 2x3 layout
  plots_list[[metric]] <- grid.arrange(grobs = metric_plots, ncol = 3, top = title)
}

grid.arrange(grobs = plots_list, ncol = 2, nrow = 2)


### Correlation 

cor_matrix <- results %>% 
  select(-c("Name", "X", "Scenario", "Simulation_nr")) %>% 
  cor(use = "pairwise.complete.obs")

p_matrix <- cor.mtest(results %>% 
                        select(-c("Name", "X", "Scenario", "Simulation_nr")) )$p  # Get p-values

# Plot with significance level
ggcorrplot(cor_matrix, 
           method = "square", 
           type = "lower", 
           p.mat = p_matrix,  # Add p-values
           sig.level = 0.05,  # Mark correlations with p < 0.05
           insig = "blank",  # Hide non-significant values
           lab = TRUE, 
           tl.cex = 10, 
           colors = c("red", "white", "blue"))
t


