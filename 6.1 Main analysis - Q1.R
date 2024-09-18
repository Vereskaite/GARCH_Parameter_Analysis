# Load necessary libraries
library(ggplot2)
library(factoextra) # for PCA visualization
library(dplyr)
library(sensitivity)

####### 1st QUESTION

### Regression  ########

# Parameter_values_df_Aux_norm <-  Parameter_values_df_Aux %>% 
#   mutate(a_norm = (a - min(Parameter_values_df_Aux$a))/(max(Parameter_values_df_Aux$a)-min(Parameter_values_df_Aux$a)),
#          b_norm = (b - min(Parameter_values_df_Aux$b))/(max(Parameter_values_df_Aux$b)-min(Parameter_values_df_Aux$b)),
#          kappa_norm = (kappa - min(Parameter_values_df_Aux$kappa))/(max(Parameter_values_df_Aux$kappa)-min(Parameter_values_df_Aux$kappa)),
#          gamma_norm = (gamma - min(Parameter_values_df_Aux$gamma))/(max(Parameter_values_df_Aux$gamma)-min(Parameter_values_df_Aux$gamma)))

# aic_model <- lm(AIC ~ a_norm + b_norm + kappa_norm + gamma_norm, data = Parameter_values_df_Aux_norm)
aic_model <- lm(AIC ~ a + b + kappa + gamma , data = Parameter_values_df_Aux)
bic_model <- lm(BIC ~ a + b + kappa + gamma + alpha + beta, data = Parameter_values_df_Aux)
rmse_model <- lm(RMSE ~ a + b + kappa + gamma + alpha + beta, data = Parameter_values_df_Aux)
mae_model <- lm(MAE ~ a + b + kappa + gamma + alpha + beta, data = Parameter_values_df_Aux)

# does it need scaling? because values are very different, a & b are more  -> normalised gives similar outcome
summary(aic_model)
summary(bic_model)
summary(rmse_model)
summary(mae_model)

plot(aic_model$residuals)
acf(aic_model$residuals)
plot(bic_model$residuals)
acf(bic_model$residuals)
plot(rmse_model$residuals)
acf(rmse_model$residuals)
plot(mae_model$residuals)
acf(mae_model$residuals)

#data does not fit into linear model, but for finding the most influential parameter - should work
#still needs fitting the right model (?)

### Principal Components ######

parameters <- Parameter_values_df_Aux %>%
  dplyr::select(a, b, kappa) # gamma

# Standardize the parameter data
parameters_scaled <- scale(parameters)

# Perform PCA
pca_result <- prcomp(parameters_scaled, center = TRUE, scale. = TRUE)

# View PCA summary to check how much variance is explained by each component
summary(pca_result)

# Visualize 
fviz_eig(pca_result)


pc_scores <- as.data.frame(pca_result$x)
pc_scores_with_metrics <- cbind(pc_scores, Parameter_values_df_Aux[, c("AIC", "BIC", "RMSE", "MAE")])
pc_scores_long <- pc_scores_with_metrics %>%
  tidyr::pivot_longer(cols = starts_with("PC"), 
               names_to = "Principal_Component", 
               values_to = "PC_Value")


# Fit linear regression models using principal components as predictors for performance metrics
aic_model_pca <- lm(AIC ~ PC1 + PC2 + PC3 , data = pc_scores_with_metrics) #PC4
summary(aic_model_pca)
plot(aic_model_pca$residuals)
plot(fitted(aic_model_pca), residuals(aic_model_pca))
acf(aic_model_pca$residuals)
Box.test(residuals(aic_model_pca), type = "Ljung-Box", lag = 20) #p<0.05 exists autocorr
qqnorm(residuals(aic_model_pca))

bic_model_pca <- lm(BIC ~ PC1 + PC2 + PC3 + PC4, data = pc_scores_with_metrics)
summary(bic_model_pca)

rmse_model_pca <- lm(RMSE ~ PC1 + PC2 + PC3, data = pc_scores_with_metrics) # + PC4
summary(rmse_model_pca)
plot(rmse_model_pca$residuals)
plot(fitted(rmse_model_pca), residuals(rmse_model_pca))
acf(rmse_model_pca$residuals)
Box.test(residuals(rmse_model_pca), type = "Ljung-Box", lag = 20) #p<0.05 exists autocorr
qqnorm(residuals(rmse_model_pca))

mae_model_pca <- lm(MAE ~ PC1 + PC2 + PC3 + PC4, data = pc_scores_with_metrics)
summary(bic_model_pca)

# Create the ggplot with facet wrap
ggplot(pc_scores_long, aes(x = PC_Value, y = AIC)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Principal_Component, scales = "free_x") +  # Facet wrap by PC1, PC2, PC3, PC4
  labs(title = "AIC vs Principal Components", 
       x = "Principal Component Value", 
       y = "AIC") +
  theme_minimal()

ggplot(pc_scores_long, aes(x = PC_Value, y = BIC)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Principal_Component, scales = "free_x") +  # Facet wrap by PC1, PC2, PC3, PC4
  labs(title = "BIC vs Principal Components", 
       x = "Principal Component Value", 
       y = "BIC") +
  theme_minimal()

ggplot(pc_scores_long, aes(x = PC_Value, y = RMSE)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Principal_Component, scales = "free_x") +  # Facet wrap by PC1, PC2, PC3, PC4
  labs(title = "RMSE vs Principal Components", 
       x = "Principal Component Value", 
       y = "RMSE") +
  theme_minimal()

ggplot(pc_scores_long, aes(x = PC_Value, y = MAE)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Principal_Component, scales = "free_x") +  # Facet wrap by PC1, PC2, PC3, PC4
  labs(title = "MAE vs Principal Components", 
       x = "Principal Component Value", 
       y = "MAE") +
  theme_minimal()

##### Sobol indices #########
# Assuming you have a data frame `data_frame` with columns: a, b, kappa, gamma, and AIC_values
# Fit a linear model to predict AIC based on the parameters a, b, kappa, gamma
linear_model <- lm(AIC ~ a + b + kappa, data = Parameter_values_df_Aux)
# aic_model_poly <- lm(AIC ~ poly(a, 2) + poly(b, 2) + poly(kappa, 2), data = Parameter_values_df_Aux)
summary(aic_model_poly)
plot(aic_model_poly$residuals)
acf(aic_model_poly$residuals)


# Define the model function that takes new input and returns predicted AIC
model_function <- function(X) {
  # X is a matrix where each row is a set of parameters (a, b, kappa, gamma)
  
  # Create a data frame from the matrix to match the format of the linear model
  param_df <- data.frame(
    a = X[,1],
    b = X[,2],
    kappa = X[,3]
  )
  
  # Use the linear model to predict AIC values for the given parameters
  predicted_AIC <- predict(linear_model, newdata = param_df)
  
  return(predicted_AIC)
}

# Assuming X1 and X2 are matrices of your input parameters (a, b, kappa, gamma)
# For simplicity, you can use the same matrix for X1 and X2 or different sets for better sampling
X1 <- as.matrix(Parameter_values_df_Aux[, c("a", "b", "kappa")])  # Input matrix for Sobol analysis
X2 <- as.matrix(Parameter_values_df_Aux[, c("a", "b", "kappa")])  # Duplicate or alternate matrix

# Perform Sobol sensitivity analysis
sobol_result <- sobol(model = model_function, X1 = X1, X2 = X2, nboot = 100)
sobol_result

# Display Sobol indices
print(sobol_result$S)  # First-order Sobol indices
print(sobol_result$V)  # Total Sobol indices

# Check some of the input data
plot(X$kappa)

# First-order Sobol indices plot
barplot(sobol_result$S[,1], main = "First-order Sobol Indices", 
        ylab = "Sensitivity Index", names.arg = c("a", "b", "kappa"))

# Total-order Sobol indices plot (? which?|)
barplot(sobol_result$V[1,1], main = "Total-order Sobol Indices", 
        ylab = "Sensitivity Index", names.arg = c("a", "b", "kappa", "gamma"))



######## Random forest ######

library(randomForest)

# Fit a random forest model
rf_model <- randomForest(AIC ~ a + b + kappa, data = Parameter_values_df_Aux)

# Get variable importance
importance_scores <- importance(rf_model)

# Plot variable importance
barplot(importance_scores[,1], main = "Variable Importance from Random Forest")

