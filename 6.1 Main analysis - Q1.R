# Load necessary libraries
library(ggplot2)
library(factoextra) # for PCA visualization
library(dplyr)

####### 1st QUESTION

### Regression  ########
Parameter_values_df <- Parameter_values %>%
  tidyr::pivot_wider(names_from = Parameter, values_from = Value) 

aic_model <- lm(AIC ~ a + b + kappa + gamma + alpha + beta, data = Parameter_values_df)
bic_model <- lm(BIC ~ a + b + kappa + gamma + alpha + beta, data = Parameter_values_df)
rmse_model <- lm(RMSE ~ a + b + kappa + gamma + alpha + beta, data = Parameter_values_df)
mae_model <- lm(MAE ~ a + b + kappa + gamma + alpha + beta, data = Parameter_values_df)

# does it need scaling? because values are very different, a & b are more 
summary(aic_model)
summary(bic_model)
summary(rmse_model)
summary(mae_model)

### Principal Components ######

parameters <- Parameter_values_df %>%
  dplyr::select(a, b, kappa, gamma)

# Standardize the parameter data
parameters_scaled <- scale(parameters)

# Perform PCA
pca_result <- prcomp(parameters_scaled, center = TRUE, scale. = TRUE)

# View PCA summary to check how much variance is explained by each component
summary(pca_result)

# Visualize 
fviz_eig(pca_result)


pc_scores <- as.data.frame(pca_result$x)
pc_scores_with_metrics <- cbind(pc_scores, Parameter_values_df[, c("AIC", "BIC", "RMSE", "MAE")])
pc_scores_long <- pc_scores_with_metrics %>%
  tidyr::pivot_longer(cols = starts_with("PC"), 
               names_to = "Principal_Component", 
               values_to = "PC_Value")


# Fit linear regression models using principal components as predictors for performance metrics
aic_model_pca <- lm(AIC ~ PC1 + PC2 + PC3 + PC4, data = pc_scores_with_metrics)
summary(aic_model_pca)

bic_model_pca <- lm(BIC ~ PC1 + PC2 + PC3 + PC4, data = pc_scores_with_metrics)
summary(bic_model_pca)

rmse_model_pca <- lm(RMSE ~ PC1 + PC2 + PC3 + PC4, data = pc_scores_with_metrics)
summary(aic_model_pca)

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

# Install and load the package
install.packages("sensitivity")
library(sensitivity)

# Define the model function (replace with your actual model)
my_model <- function(X) {
  a <- X[, 1]
  b <- X[, 2]
  kappa <- X[, 3]
  gamma <- X[, 4]
  
  # Example model
  AIC <- a * b + kappa * gamma
  return(AIC)
}

# Define parameter ranges
a_range <- c(0, 1)
b_range <- c(0, 2)
kappa_range <- c(0, 3)
gamma_range <- c(0, 3)

# Set up parameter ranges as a matrix
parameter_ranges <- data.frame(a = a_range, b = b_range, kappa = kappa_range, gamma = gamma_range)

# Number of samples
N <- 1000

# Generate Sobol sequences
X1 <- sobolSalt(N = N, X1 = parameter_ranges, nboot = 100)
X2 <- sobolSalt(N = N, X2 = parameter_ranges, nboot = 100)

# Perform Sobol sensitivity analysis
sobol_results <- sobolSalt(model = my_model, X1 = X1$X1, X2 = X2$X2, nboot = 100)

# Print and visualize results
print(sobol_results)
plot(sobol_results)

