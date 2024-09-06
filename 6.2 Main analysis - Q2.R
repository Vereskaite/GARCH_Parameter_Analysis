
##### Scatter plot

# Create facetwrap for scatterplots for each metric
# Add LM and it's parameters to the graph

data_a <- Parameter_values %>% filter(Parameter == "gamma") %>% dplyr::select(Scenario, a = Value)
data_AIC <- Parameter_values %>% filter(Parameter == "alpha") %>% dplyr::select(Scenario, AIC = Value)

# Join data on Scenario
data_combined <- left_join(data_a, data_AIC, by = "Scenario")

# Plot kappa vs. a
# neteisingas nes scenarijuose keiciasi ir kiti parametrai. jie turi islikti
ggplot(data_combined, aes(x = a, y = AIC, color = Scenario)) +
  geom_point(size = 3) +
  labs(title = "Change of AIC with respect to a",
       x = "Value of a",
       y = "Value of AIC") +
  theme_minimal() +
  theme(legend.position = "none")

#### Run LM

#### Sobol indices


############ All else equal graphs


