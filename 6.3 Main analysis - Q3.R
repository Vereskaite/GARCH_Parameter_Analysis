### What is the relationship between parameters?
library(GGally)
library(corrplot)
library(car)

### Scatterplot matrix, histogram, correlation #####

parameters <- Parameter_values_df_Aux[, c("a", "b", "ab", "kappa", "gamma", "alpha", "beta")]

ggpairs(
  parameters,
  # lower = list(continuous = wrap("points", alpha = 0.3, color ="brown")),  # Scatterplot in the lower triangle
  lower =  list(continuous = function(data, mapping, ...) {
    ggplot(data = data, mapping = mapping) +
      geom_point(alpha = 0.3, color = "brown") +  # Brown scatterplot points
      geom_smooth(method = "lm", se = FALSE, color = "black", alpha = 0.7, size = 0.5, linetype = "dashed")  # Light blue smooth line
  }),
  diag = list(continuous = wrap("barDiag", fill = "darkblue")),  # Histogram on the diagonal
  upper = list(continuous = wrap("cor", size = 3)),
  axisLabels = "show",  # Show axis labels for clarity
  columnLabels = colnames(parameters)  # Use column names as labels
) +
theme(strip.text = element_text(size = 12))  # Adjust text size for be

### Spearman correlation ######
Parameter_values_df_Aux2 <- Parameter_values_df_Aux[ ,c("a", "b", "ab", "kappa", "gamma", "alpha", "beta",
                                                        "AIC","BIC","RMSE","MAE")]

cor_matrix <- cor.test(Parameter_values_df_Aux2, method = "spearman")
cor_matrix[upper.tri(cor_matrix, diag = FALSE)] <- NA
cor_matrix_df <- as.data.frame(cor_matrix)
cor_matrix_df


cor_matrix <- cor(Parameter_values_df_Aux2, method = "spearman")

# Create a correlogram
corrplot(cor_matrix, 
         method = "color",  # Use colors to represent correlation values
         type = "upper",    # Show only the upper triangle
         tl.col = "black",  # Text label color
         tl.srt = 45,       # Text label rotation
         addCoef.col = "black",  # Color of correlation coefficients
         number.cex = 0.7,  # Size of correlation coefficient text
         col = colorRampPalette(c("blue", "white", "red"))(200)) 

param_names <- colnames(Parameter_values_df_Aux2)

# Initialize matrices to store results
num_params <- length(param_names)
cor_matrix <- as.matrix(cor_matrix)
pval_matrix <- matrix(NA, nrow = num_params, ncol = num_params)

# Function to compute Spearman correlation and p-value
compute_cor_test <- function(x, y) {
  test <- cor.test(x, y, method = "spearman")
  return(c(correlation = test$estimate, p.value = test$p.value))
}

# Compute correlations and p-values for all pairs
for (i in 1:(num_params - 1)) {
  for (j in (i + 1):num_params) {
    param1 <- param_names[i]
    param2 <- param_names[j]
    result <- compute_cor_test(Parameter_values_df_Aux2[[param1]], Parameter_values_df_Aux2[[param2]])
    # cor_matrix[i, j] <- result["correlation"]
    pval_matrix[i, j] <- result["p.value"]
  }
}

cor_matrix[pval_matrix > 0.05 | is.na(pval_matrix)] <- NA

corrplot(cor_matrix, 
         method = "color",  # Use colors to represent correlation values
         type = "upper",    # Show only the upper triangle
         tl.col = "black",  # Text label color
         tl.srt = 45,       # Text label rotation
         addCoef.col = "black",  # Color of correlation coefficients
         number.cex = 0.7,  # Size of correlation coefficient text
         col = colorRampPalette(c("blue", "white", "red"))(200))  # Color scale


###### Multicolinearity check #######
metrics <- c("AIC", "BIC", "RMSE", "MAE")
parameters <- c("a", "b", "kappa", "gamma", "alpha", "beta")

vif_matrix <- matrix(NA, nrow = length(parameters), ncol = length(metrics), 
                     dimnames = list(parameters, metrics))

for (metric in metrics) {
  # Fit the linear model
  lm_model <- lm(as.formula(paste(metric, "~ a + b + kappa + gamma + alpha + beta")), data = Parameter_values_df_Aux)
  
  # Calculate VIF values
  vif_values <- vif(lm_model)
  
  # Store VIF values in the matrix
  vif_matrix[, metric] <- vif_values[parameters]
}

vif_df <- as.data.frame(vif_matrix)
print(vif_df)


#### solving multicolinearity issue
# Install and load necessary package

library(glmnet)

# Define your response variable and predictors
response_var <- "AIC"  # Replace with your actual response variable name
predictors <- c("a", "b", "kappa", "gamma", "alpha", "beta")

X <- as.matrix(Parameter_values_df_Aux[predictors])
Y <- Parameter_values_df_Aux[[response_var]]

# Fit the Lasso model using cross-validation
cv_model <- cv.glmnet(X, Y, alpha = 1)

# Get the best lambda value from cross-validation
best_lambda <- cv_model$lambda.min

# Fit the final Lasso model with the best lambda
lasso_model <- glmnet(X, Y, alpha = 1, lambda = best_lambda)
print(coef(lasso_model))

summary(lasso_model)

### checking if lasso model is a right fit
predicted_values <- predict(lasso_model, newx = X, s = best_lambda)

residuals <- Y - predicted_values

# Residual analysis for LASSO ########
plot(residuals, main = "Residuals from Lasso Regression", 
     xlab = "Index", ylab = "Residual", pch = 16, col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at 0

acf(residuals)
# lasso is also not the right one

plot(Y, predicted_values, 
     xlab = "Actual Values", 
     ylab = "Fitted Values",
     main = "Fitted Values vs. Actual Values",
     pch = 16, col = "blue")
abline(a = 0, b = 1, col = "red", lwd = 2)  # Add a 45-degree line (y = x) for reference

# Model vs Actual
predicted_AIC <- predict(lasso_model, newx = X, s = best_lambda)

plot(Y, type = "o", col = "blue", ylim = range(c(Y, predicted_AIC)), 
     xlab = "Index", ylab = "AIC Values", 
     main = "Actual AIC Values vs. Predicted AIC Values")
lines(predicted_AIC, col = "red", type = "o")

# # Add a legend to distinguish between the two lines
# legend("topright", legend = c("Actual AIC Values", "Predicted AIC Values"), 
#        col = c("blue", "red"))

