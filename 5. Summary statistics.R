Return_values
Parameter_values

Parameter_values %>% 
  mutate(ARCH_Test = ifelse(Parameter == "ARCH_Test" & Value > 0.05, "No_ARCH", "ARCH"))


Parameter_values %>% 
  group_by(Scenario) %>% 
  summarise(Convergence = min(Value))


Parameter_values %>% 
  group_by(Parameter) %>% 
  summarise()


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

### Check stability condition 

Par_out_of_range <- Parameter_values %>%
  tidyr::pivot_wider(names_from = Parameter, values_from = Value) %>%
  mutate(
    condition_check = (a + b) * (alpha + beta) < 1
  ) %>%
  filter(condition_check == "FALSE")