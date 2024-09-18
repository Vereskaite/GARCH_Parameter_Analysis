# Interaction between two parameters, e.g., a and b
ggplot(Parameter_values_df, aes(x = a, y = AIC, color = factor(b))) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = b)) +
  labs(title = "Interaction between Parameter a and b on AIC",
       x = "Parameter a",
       y = "AIC") +
  theme_minimal()

pairs(Parameter_values_df_Aux[c("a","b", "AIC", "RMSE")], main = "Pairs Plot of Input Variables")

####. Heatmap


### Regression with interaction terms ######
# try to run taking out least significant one by one. 
# Amount of insignificants  might be masking some of the effects

lm_interactions <- lm(AIC ~ a*b*kappa*gamma, data = Parameter_values_df_Aux)
summary(lm_interactions)

lm_interactions <- lm(RMSE ~ a*b*kappa*gamma, data = Parameter_values_df_Aux)
summary(lm_interactions)

#### LASSO with interaction terms
X <- model.matrix(AIC ~ (a * b * kappa * gamma), data = Parameter_values_df_Aux)[, -1]  # Remove intercept column
y <- Parameter_values_df_Aux$AIC
lasso_model <- cv.glmnet(X, y, alpha = 1)  # alpha=1 for Lasso, 0 for Ridge
print(lasso_model$lambda.min)
lasso_coefficients <- coef(lasso_model, s = "lambda.min")

lm_results <- broom::tidy(lm_interactions) 
lasso_df <- as.data.frame(as.matrix(lasso_coefficients))
lasso_df$term <- rownames(lasso_df)
colnames(lasso_df) <- c("lasso_coefficient", "term")

# Step 3: Combine lm and Lasso results
combined_results <- merge(lm_results, lasso_df, by = "term", all.x = TRUE)
reg_lasso_coeff <- combined_results %>% 
  dplyr::select(term,estimate, p.value,lasso_coefficient) %>% 
  rename(parameter = term,
         lm_coeff=estimate,
         lm_p=p.value,
         lasso_coeff = lasso_coefficient) %>% 
  mutate(lm_p = round(lm_p, digits = 2))

reg_lasso_coeff
