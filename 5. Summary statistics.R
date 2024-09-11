# Data pool ########
Return_values
Parameter_values
Parameter_values_df

# Problematic scenarios ##########
Parameter_values %>% 
  mutate(ARCH_Test = ifelse(Parameter == "ARCH_Test" & Value > 0.05, "No_ARCH", "ARCH"))


Parameter_values %>% 
  group_by(Scenario) %>% 
  summarise(Convergence = min(Value))


Not_converged <- Parameter_values %>% 
  filter(Parameter == "AIC") %>%
  dplyr::select(Value) %>% 
  is.na() %>% 
  sum()

No_ARCH <- Parameter_values %>% 
  filter(Parameter == "ARCH_Test",
         Value > 0.05) %>%
  dplyr::select(Value) %>% 
  is.na() %>% 
  sum()

No_beta <- Parameter_values %>% 
  filter(Parameter == "beta_sign",
         Value == 0) %>%
  dplyr::select(Value) %>% 
  is.na() %>% 
  sum()

NA_output <- Parameter_values %>% 
  dplyr::filter(Parameter == c("AIC", "BIC", "RMSE", "MAE")) %>% 
  is.na() %>% 
  sum()

Not_converged
No_ARCH
No_beta
NA_output

# Check stability condition 

Par_out_of_range <- Parameter_values %>%
  tidyr::pivot_wider(names_from = Parameter, values_from = Value) %>%
  mutate(
    condition_check = (a + b) * (alpha + beta) < 1
  ) %>%
  filter(condition_check == "FALSE")


# Descriptive statistics ########
summary(Parameter_values %>%
          tidyr::pivot_wider(names_from = Parameter, values_from = Value))

# Check distributions
Parameter_values_Aux1 <- Parameter_values_df %>%
  tidyr::pivot_longer(cols = c(AIC, BIC, RMSE, MAE), names_to = "Metric", values_to = "Value")

ggplot(Parameter_values_Aux1 %>% filter(Metric == c("AIC","BIC")), aes(x = Value)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +  # Histogram
  geom_density(color = "red", size = 1, alpha = 0.7) +  # Density plot
  labs(title = "Histogram with Density Plot for AIC and BIC", x = "Value", y = "Density") +
  facet_wrap(~ Metric, scales = "free") +  # Facet for each metric
  theme_minimal()

ggplot(Parameter_values_Aux1 %>% filter(Metric == c("RMSE","MAE")), aes(x = Value)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.01, fill = "blue", color = "black", alpha = 0.7) +  # Histogram
  geom_density(color = "red", size = 1, alpha = 0.7) +  # Density plot
  labs(title = "Histogram with Density Plot for RMSE and MAE", x = "Value", y = "Density") +
  facet_wrap(~ Metric, scales = "free") +  # Facet for each metric
  theme_minimal()
