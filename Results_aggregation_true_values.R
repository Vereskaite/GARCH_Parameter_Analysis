# Collect all files
csv_files_true <- list.files(pattern = "^small_grid.*$")
df_list_true <- lapply(csv_files_true, read.csv)
combined_results_true <- bind_rows(df_list_true)
print(combined_results_true)

results_true <- combined_results_true %>% 
  na.omit()

head(results_true)

# Aggregation

true_data_frame_Aux1 <- results_true %>% 
  group_by(Name, Scenario) %>% 
  summarise(RMSE_RV_mean = mean(RMSE_RV),
            RMSE_RV_stdev = sd(RMSE_RV),
            RMSE_HV_mean = mean(RMSE_HV),
            RMSE_HV_stdev = sd(RMSE_HV),
            MAE_RV_mean = mean(MAE_RV),
            MAE_RV_stdev = sd(MAE_RV),
            MAE_HV_mean = mean(MAE_HV),
            MAE_HV_stdev = sd(MAE_HV),
            AIC_mean = mean(AIC),
            AIC_stdev = sd(AIC),
            BIC_mean = mean(BIC),
            BIC_stdev = sd(BIC))

# addcorresponding st.devs (?) 
true_data_frame_Aux2 <- true_data_frame_Aux1 %>% 
  group_by(Name) %>% 
  summarise(min_RMSE_RV = min(RMSE_RV_mean),
            min_RMSE_HV = min(RMSE_HV_mean),
            min_MAE_RV = min(MAE_RV_mean),
            min_MAE_HV = min(MAE_HV_mean),
            min_AIC = min(AIC_mean),
            min_BIC = min(BIC_mean))

# Difference to first row

true_values_errors < - true_data_frame_Aux2 %>%
  mutate(across(where(is.numeric), ~ . - first(.)))
