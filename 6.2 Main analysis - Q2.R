
##### Scatter plot - to observe general trend #######

### AIC and BIC
Parameter_values_long <- Parameter_values_df %>%
  tidyr::pivot_longer(cols = c(a, b, kappa, gamma), names_to = "parameter", values_to = "value") %>%
  tidyr::pivot_longer(cols = c(AIC, BIC), names_to = "metric", values_to = "metric_value")

Parameter_values_long$variable_pair <- paste(Parameter_values_long$parameter, "vs", Parameter_values_long$metric)

ggplot(Parameter_values_long, aes(x = value, y = metric_value, color = factor(value))) +
  geom_point(size = 0.5, alpha = 0.5) +
  # geom_jitter(size = 0.5, width = 0.1, height = 0.1, alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "light blue", alpha = 0.3) + 
  facet_wrap(~ variable_pair, scales = "free", nrow = 4, ncol = 2) +
  labs(title = "Change of Model Metrics with respect to Parameters",
       x = "Parameter Value",
       y = "Model Performance Metric") +
  theme_minimal() +
  theme(legend.position = "none")

### RMSE and MAE
Parameter_values_long <- Parameter_values_df %>%
  tidyr::pivot_longer(cols = c(a, b, kappa, gamma), names_to = "parameter", values_to = "value") %>%
  tidyr::pivot_longer(cols = c(RMSE, MAE), names_to = "metric", values_to = "metric_value")

Parameter_values_long$variable_pair <- paste(Parameter_values_long$parameter, "vs", Parameter_values_long$metric)

ggplot(Parameter_values_long, aes(x = value, y = metric_value, color = Scenario)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "light blue", alpha = 0.3) + 
  facet_wrap(~ variable_pair, scales = "free", nrow = 4, ncol = 2) +
  labs(title = "Change of Model Metrics with respect to Parameters",
       x = "Parameter Value",
       y = "Model Performance Metric") +
  theme_minimal() +
  theme(legend.position = "none")


#### Run LM (probably not needed) ######

summary(lm(MAE ~ a, data = Parameter_values_df))

#### Sobol indices #######


############ All else equal graphs each case (AIC only) #####

valid_combinations <- Parameter_values_df %>% 
  dplyr::select(b, kappa, gamma) %>% 
  group_by(b, kappa, gamma) %>% 
  unique()

filtered_data <- Parameter_values_df %>%
  semi_join(valid_combinations[1, ], by = c("b", "kappa", "gamma"))

# Plot the relationship between 'a' and 'AIC' for filtered scenarios
ggplot(filtered_data, aes(x = a, y = AIC, color = as.factor(Scenario))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "light blue") +
  labs(title = "Change of AIC with respect to 'a' (All Else Equal)",
       x = "Value of a",
       y = "Value of AIC") +
  theme_minimal() +
  theme(legend.position = "none")


############ All else equal graphs - general case #####

### AIC 
Parameter_values_df_comb <- Parameter_values_df %>%
  mutate(Combination_a = interaction(b, kappa, gamma, drop = TRUE),
         Combination_b = interaction(a, kappa, gamma, drop = TRUE),
         Combination_kappa = interaction(a, b, gamma, drop = TRUE),
         Combination_gamma = interaction(a, b, kappa, drop = TRUE))

long_df <- Parameter_values_df_comb %>%
  tidyr::pivot_longer(cols = c(a, b, kappa, gamma), names_to = "Parameter", values_to = "Value") %>%
  tidyr::pivot_longer(cols = starts_with("Combination"), names_to = "Combination_type", values_to = "Combination") %>%
  mutate(Combination = factor(Combination))  # Ensure Combination is a factor for smooths

ggplot(long_df, aes(x = Value, y = AIC, color = Combination)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE,) +
  facet_wrap(~ Parameter, scales = "free_x") +
  labs(title = "Interaction between Parameters and AIC",
       x = "Parameter Value",
       y = "AIC") +
  theme_minimal() +
  theme(legend.position = "none")  # Adjust position to handle many legend entries


### AIC, BIC, RMSE, MAE
create_plot <- function(data, y_metric) {
  ggplot(data, aes(x = Value, y = !!sym(y_metric), color = Combination)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ Parameter, scales = "free_x") +
    labs(title = paste("Interaction between Parameters and", y_metric),
         x = "Parameter Value",
         y = y_metric) +
    theme_minimal() +
    theme(legend.position = "none")  # Adjust position to handle many legend entries
}

# Create plots for each metric
plot_AIC <- create_plot(long_df, "AIC")
plot_BIC <- create_plot(long_df, "BIC")
plot_RMSE <- create_plot(long_df, "RMSE")
plot_MAE <- create_plot(long_df, "MAE")

# Arrange the plots in a grid
grid.arrange(plot_AIC, plot_BIC, plot_RMSE, plot_MAE, ncol = 2, nrow = 2)


# Example code for only a and AIC

# ggplot(long_df, aes(x = Value, y = AIC, color = factor(Combination))) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE, aes(group = Combination), color = "lightblue", alpha = 0.3) +
#   facet_wrap(~ Parameter, scales = "free_x") +
#   labs(title = "Interaction between Parameters and AIC",
#        x = "Parameter Value",
#        y = "AIC") +
#   theme_minimal() +
#   theme(legend.position = "none")

############ All else equal graphs - adding 3rd dimension (AIC only) #####
# there are visibly seen groups. Need to investigate which parameter drives it

create_plot <- function(data, x, y, color_var, title) {
  ggplot(data, aes_string(x = x, y = y, color = color_var)) +
    geom_point(size = 3) +
    # Uncomment if you want to add a smooth line
    # geom_smooth(method = "lm", se = FALSE, color = "light blue", alpha = 0.3) +
    labs(title = title,
         x = "Parameter a",
         y = "AIC") +
    theme_minimal() +
    theme(legend.position = "right")
}

# a
a_AIC_b <- create_plot(Parameter_values_df_comb, "a", "AIC", "b", "Interaction between Parameter a and b on AIC")
a_AIC_kappa <- create_plot(Parameter_values_df_comb, "a", "AIC", "kappa", "Interaction between Parameter a and kappa on AIC")
a_AIC_gamma <- create_plot(Parameter_values_df_comb, "a", "AIC", "gamma", "Interaction between Parameter a and gamma on AIC")

grid.arrange(a_AIC_b, a_AIC_kappa, a_AIC_gamma, nrow = 3, top = "Interaction between Parameter a and Various Model Parameters on AIC")


# b
b_AIC_a <- create_plot(Parameter_values_df_comb, "b", "AIC", "a", "Interaction between Parameter a and b on AIC")
b_AIC_kappa <- create_plot(Parameter_values_df_comb, "b", "AIC", "kappa", "Interaction between Parameter a and kappa on AIC")
b_AIC_gamma <- create_plot(Parameter_values_df_comb, "b", "AIC", "gamma", "Interaction between Parameter a and gamma on AIC")

grid.arrange(b_AIC_a, b_AIC_kappa, b_AIC_gamma, nrow = 3, top = "Interaction between Parameter a and Various Model Parameters on AIC")

# kappa
kappa_AIC_a <- create_plot(Parameter_values_df_comb, "kappa", "AIC", "a", "Interaction between Parameter a and b on AIC")
kappa_AIC_b <- create_plot(Parameter_values_df_comb, "kappa", "AIC", "b", "Interaction between Parameter a and kappa on AIC")
kappa_AIC_gamma <- create_plot(Parameter_values_df_comb, "kappa", "AIC", "gamma", "Interaction between Parameter a and gamma on AIC")

grid.arrange(kappa_AIC_a, kappa_AIC_b, kappa_AIC_gamma, nrow = 3, top = "Interaction between Parameter a and Various Model Parameters on AIC")

# gamma
gamma_AIC_a <- create_plot(Parameter_values_df_comb, "gamma", "AIC", "a", "Interaction between Parameter a and b on AIC")
gamma_AIC_b <- create_plot(Parameter_values_df_comb, "gamma", "AIC", "b", "Interaction between Parameter a and kappa on AIC")
gamma_AIC_kappa <- create_plot(Parameter_values_df_comb, "gamma", "AIC", "kappa", "Interaction between Parameter a and gamma on AIC")

grid.arrange(gamma_AIC_a, gamma_AIC_b, gamma_AIC_kappa, nrow = 3, top = "Interaction between Parameter a and Various Model Parameters on AIC")
