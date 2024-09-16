set.seed(123)
#### DEFINE DATA ##########
# P_t and N_t - zero-inflated lognormal
n <- 1000   

# Function to generate zero-inflated lognormal distribution with normalization
generate_zero_inflated_lognormal <- function(n, prob_zero, meanlog, sdlog, skew_direction = "right") {
  
  # Bernoulli distribution to generate zeros
  zero_component <- rbinom(n, 1, prob_zero)
  
  # Lognormal distribution for non-zero values
  non_zero_component <- rlnorm(n, meanlog = meanlog, sdlog = sdlog)
  
  # Adjust for skew direction and normalize between 0 and 1 (or -1 and 0)
  if (skew_direction == "right") {
    # Normalize to range between 0 and 1
    non_zero_component <- (non_zero_component - min(non_zero_component)) / (max(non_zero_component) - min(non_zero_component))
  } else if (skew_direction == "left") {
    # Normalize to range between -1 and 0
    non_zero_component <- -((non_zero_component - min(non_zero_component)) / (max(non_zero_component) - min(non_zero_component)))
  }
  
  # Combine zero and non-zero components
  sentiment <- ifelse(zero_component == 1, 0, non_zero_component)
  
  return(sentiment)
}

# Parameters          
prob_zero_positive <- 0.8        # Probability of zero sentiment (70% zeros)
prob_zero_negative <- 0.8        # Probability of zero sentiment (70% zeros)
meanlog_positive <- 0.5 # Mean log for positive sentiment (right-skewed)
sdlog_positive <- 0.7   # Standard deviation log for positive sentiment
meanlog_negative <- 0.5 # Mean log for negative sentiment (left-skewed)
sdlog_negative <- 0.7   # Standard deviation log for negative sentiment

# Generate positive and negative sentiment
positive_sentiment <- generate_zero_inflated_lognormal(n, prob_zero_positive, meanlog_positive, sdlog_positive, skew_direction = "right")
negative_sentiment <- generate_zero_inflated_lognormal(n, prob_zero_negative, meanlog_negative, sdlog_negative, skew_direction = "left")

# Combine positive and negative sentiment into a data frame
sentiment_data <- data.frame(
  Positive_Sentiment = positive_sentiment,
  Negative_Sentiment = negative_sentiment
)

# Plotting the results
p1 <- ggplot(sentiment_data, aes(x = Positive_Sentiment)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1, alpha = 0.7) +
  labs(title = "Zero-Inflated Positive Sentiment (0 to 1)", x = "Positive Sentiment", y = "Density") +
  theme_minimal()

p2 <- ggplot(sentiment_data, aes(x = Negative_Sentiment)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1, alpha = 0.7) +
  labs(title = "Zero-Inflated Negative Sentiment (-1 to 0)", x = "Negative Sentiment", y = "Density") +
  theme_minimal()

grid.arrange(p1, p2, ncol = 2)

P_t <- positive_sentiment
N_t <- negative_sentiment

# r
r <- numeric(n)
epsilon <- numeric(n)
sigma2 <- numeric(n)
f <- numeric(n)
z <- rnorm(n)
mu <- 0
omega <- 0.1
alpha <- 0.1
beta <- 0.1
a <- 0.5
b <- 1.5
kappa <- 4
gamma <- 4
sigma2[1] ^2

sigma2[1] <- omega / (1 - (alpha + beta)*(a+b))
epsilon[1] <- z[1] * sqrt(sigma2[1])
r[1] <- mu + epsilon[1]
f[1] <- a + 0.5 * b * ((exp(kappa * P_t[1]) - 1) / (exp(kappa * P_t[1]) + 1) - (exp(gamma * N_t[1]) - 1) / (exp(gamma * N_t[1]) + 1))


for (t in 2:n) {
  f[t] <- a + 0.5 * b * ((exp(kappa * P_t[t]) - 1) / (exp(kappa * P_t[t]) + 1) - (exp(gamma * N_t[t]) - 1) / (exp(gamma * N_t[t]) + 1))
  sigma2[t] <- f[t-1]*(omega + alpha * epsilon[t-1]^2 + beta * sigma2[t-1])
  epsilon[t] <- z[t] * sqrt(sigma2[t])
  r[t] <- mu + epsilon[t]
}


rm(mu,omega,alpha, beta, z, sigma2, epsilon,a,b,n,t, kappa,gamma, N_t_exp, P_t_exp, lambda)
# rm(f)
plot(r, type = "l")
plot(P_t, type = "l")
plot(N_t, type = "l")
plot(f, type = "l")



