# Data pool ########
Return_values_Aux <- na.omit(Return_values_Aux)
Parameter_values_Aux <- na.omit(Parameter_values_Aux)
Parameter_values_df_Aux <- na.omit(Parameter_values_df_Aux)

# Problematic scenarios ##########
Parameter_values_Aux %>% 
  mutate(ARCH_Test = ifelse(Parameter == "ARCH_Test" & Value > 0.05, "No_ARCH", "ARCH")) %>% 
  filter(ARCH_Test == "No_ARCH")


Parameter_values_Aux %>% 
  group_by(Scenario) %>% 
  summarise(Convergence = min(Value))


Not_converged <- Parameter_values_Aux %>% 
  filter(Parameter == "AIC") %>%
  dplyr::select(Value) %>% 
  is.na() %>% 
  sum() %>% 
  print()

No_ARCH <- Parameter_values_Aux %>% 
  filter(Parameter == "ARCH_Test",
         Value > 0.05) %>%
  dplyr::select(Value) %>% 
  is.na() %>% 
  sum() %>% 
  print()

No_beta <- Parameter_values_Aux %>% 
  filter(Parameter == "beta_sign",
         Value == 0) %>%
  dplyr::select(Value) %>% 
  is.na() %>% 
  sum() %>% 
  print()

NA_output <- Parameter_values_Aux %>% 
  dplyr::filter(Parameter == c("AIC", "BIC", "RMSE", "MAE")) %>% 
  is.na() %>% 
  sum() %>% 
  print()

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
  filter(condition_check == "FALSE") %>% 
  print()


# Descriptive statistics ########
summary(Parameter_values_Aux %>%
          tidyr::pivot_wider(names_from = Parameter, values_from = Value))

summary(Parameter_values_df_Aux)

# Check distributions
Parameter_values_Aux1 <- Parameter_values_df_Aux %>%
  tidyr::pivot_longer(cols = c(AIC, BIC, RMSE, MAE,a,b,alpha,beta,kappa,gamma,ab), names_to = "Metric", values_to = "Value")
  
hist_AIC_BIC <- ggplot(Parameter_values_Aux1 %>% filter(Metric == c("AIC","BIC")), aes(x = Value)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +  # Histogram
  # geom_density(color = "red", size = 1, alpha = 0.7) +  # Density plot
  # labs(title = "Histogram with Density Plot for AIC and BIC", x = "Value", y = "Density") +
  facet_wrap(~ Metric, scales = "free") +  # Facet for each metric
  theme_minimal()


Parameter_values_Aux1 <- Parameter_values_Aux1 %>%
  filter(Metric %in% c("AIC", "BIC")) %>%
  group_by(Metric) %>%
  mutate(Standardized_Value = (Value - mean(Value)) / sd(Value))

# Create a plot comparing standardized AIC and BIC distributions
hist_AIC_BIC_Standardized <- ggplot(Parameter_values_Aux1, aes(x = Standardized_Value, fill = Metric, color = Metric)) + 
  # geom_histogram(aes(y = ..density..), binwidth = 0.5, position = "identity", alpha = 0.5, color = "black") +  # Histogram with transparency
  geom_density(size = 1, alpha = 0.5) +  # Density plot with transparency
  labs(title = "Comparison of Standardized AIC and BIC Distributions", x = "Standardized Value", y = "Density") +
  scale_fill_manual(values = c("AIC" = "blue", "BIC" = "red")) +  # Colors for histograms
  scale_color_manual(values = c("AIC" = "blue", "BIC" = "red")) +  # Colors for density lines
  theme_minimal() +
  theme(legend.position = "top")

print(hist_AIC_BIC_Standardized)


hist_RMSE_MAE <- ggplot(Parameter_values_Aux1 %>% filter(Metric == c("RMSE","MAE")), aes(x = Value)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.005, fill = "blue", color = "black", alpha = 0.7) +  # Histogram
  geom_density(color = "red", size = 1, alpha = 0.7) +  # Density plot
  # labs(title = "Histogram with Density Plot for RMSE and MAE", x = "Value", y = "Density") +
  facet_wrap(~ Metric, scales = "free") +  # Facet for each metric
  theme_minimal()
  


Parameter_values_Aux1 <- Parameter_values_Aux1 %>%
  group_by(Metric) %>%
  mutate(Standardized_Value = (Value - mean(Value)) / sd(Value))

# Create a plot comparing standardized RMSE and MAE distributions
hist_RMSE_MAE_standardized <- ggplot(Parameter_values_Aux1 %>% filter(Metric %in% c("RMSE", "MAE")), aes(x = Standardized_Value, fill = Metric, color = Metric)) + 
  # geom_histogram(aes(y = ..density..), binwidth = 0.5, position = "identity", alpha = 0.5, color = "black") +  # Histogram with transparency
  geom_density(size = 1, alpha = 0.5) +  # Density plot with transparency
  labs(title = "Comparison of Standardized RMSE and MAE Distributions", x = "Standardized Value", y = "Density") +
  scale_fill_manual(values = c("RMSE" = "blue", "MAE" = "green")) +  # Colors for histograms
  scale_color_manual(values = c("RMSE" = "blue", "MAE" = "green")) +  # Colors for density lines
  theme_minimal() +
  theme(legend.position = "top")

print(hist_RMSE_MAE_standardized)

hist_a_b <- ggplot(Parameter_values_Aux1 %>% filter(Metric == c("a","b")), aes(x = Value)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +  # Histogram
  # geom_density(color = "red", size = 1, alpha = 0.7) +  # Density plot
  # labs(title = "Histogram with Density Plot for RMSE and MAE", x = "Value", y = "Density") +
  facet_wrap(~ Metric, scales = "free") +  # Facet for each metric
  theme_minimal()

hist_ab <- ggplot(Parameter_values_Aux1 %>% filter(Metric == c("ab")), aes(x = Value)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +  # Histogram
  # geom_density(color = "red", size = 1, alpha = 0.7) +  # Density plot
  # labs(title = "Histogram with Density Plot for RMSE and MAE", x = "Value", y = "Density") +
  facet_wrap(~ Metric, scales = "free") +  # Facet for each metric
  theme_minimal()

hist_kappa_gamma <- ggplot(Parameter_values_Aux1 %>% filter(Metric == c("kappa","gamma")), aes(x = Value)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +  # Histogram
  # geom_density(color = "red", size = 1, alpha = 0.7) +  # Density plot
  # labs(title = "Histogram with Density Plot for RMSE and MAE", x = "Value", y = "Density") +
  facet_wrap(~ Metric, scales = "free") +  # Facet for each metric
  theme_minimal()

hist_alpha_beta <- ggplot(Parameter_values_Aux1 %>% filter(Metric == c("alpha","beta")), aes(x = Value)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +  # Histogram
  # geom_density(color = "red", size = 1, alpha = 0.7) +  # Density plot
  # labs(title = "Histogram with Density Plot for RMSE and MAE", x = "Value", y = "Density") +
  facet_wrap(~ Metric, scales = "free") +  # Facet for each metric
  theme_minimal()


grid.arrange(hist_AIC_BIC,hist_RMSE_MAE,hist_a_b,hist_ab,hist_kappa_gamma,hist_alpha_beta,
             hist_AIC_BIC_Standardized, hist_RMSE_MAE_standardized, nrow=4,ncol=2,
             name="histograms of parameters and metrics")

#Standardizes show that we can use just AIC and RMSE.

#### plot a few TS

example_TS <- Return_values_Aux %>% 
  filter(Scenario == "SimulatedNAGARCH_0.5_0.5_1_1")

ggplot(example_TS %>% filter(Type %in% c("In-Sample","Forecast","Out-Sample")), 
       aes(x=Index, y =Returns, color = Type, alpha = Type))+
  geom_line() +
  scale_alpha_manual(values = c("In-Sample" = 1, "Forecast" = 1, "Out-Sample" = 0.3))+
  # geom_line(data = subset(example_TS, Type == "In-Sample"), color = "grey", alpha = 1) +  # In-Sample lines
  # geom_line(data = subset(example_TS, Type == "Forecast"), color = "blue", alpha = 1) +     # Forecast lines
  # geom_line(data = subset(example_TS, Type == "Out-Sample"), color = "red", alpha = 0.3) +   # Out-Sample lines
  # scale_color_manual(values = c("In-Sample" = "black", "Forecast" = "blue", "Out-Sample" = "red")) +  # Colors for legend
  # scale_alpha_manual(values = c("In-Sample" = 1, "Forecast" = 1, "Out-Sample" = 0.3)) +  # Alpha for legend
  labs(title = "Example Time Series Plot", x = "Index", y = "Returns", color = "Type", alpha = "Type") +
  theme_minimal()+ 
  theme(
    legend.position = "bottom",  # Position legend at the bottom
    legend.justification = "center" 
  )

plot.ts(example_TS %>% filter(Type == "f" & Index >2000) %>% dplyr::select(Returns))
# ALL VALUES BELOW 1, ALWAYS MINIMIZING


## checking f values, are they higher or lower than 1
Return_values_Aux %>% 
  mutate(Returns = ifelse(Index < 2000 & Returns== 0, 1, Returns)) %>% 
  filter(Type == "f" & Returns < 1) %>% 
  nrow()

Return_values_Aux %>% 
  filter(Type == "f" & Returns == 1) 
  
Return_values_Aux %>% 
  filter(Type == "f" & Returns > 1) %>% 
  nrow()

f_data <- Return_values_Aux %>% 
  filter(Type == "f")

ggplot(f_data, aes(x = Returns)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Histogram of f_t with Threshold at 1", x = "f_t", y = "Frequency") +
  theme_minimal()


df_summary <- f_data %>%
  mutate(Category = case_when(
    Returns < 1 ~ "Below 1",
    Returns == 1 ~ "Equal to 1",
    TRUE ~ "Above 1"
  )) %>%
  count(Category) %>%
  mutate(Percentage = n / sum(n) * 100)  # Calculate percentages

# Plot bar plot
ggplot(df_summary, aes(x = Category, y = Percentage, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of f_t Categories", x = "Category", y = "Percentage") +
  scale_fill_manual(values = c("Equal to 1" = "lightgreen", "Above 1" = "brown", "Below 1" = "lightblue")) +
  theme_minimal()
