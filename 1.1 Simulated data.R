set.seed(123)
n <- 10000  # Number of time steps

### SIMULATIONS WITH P_t N_t ZERO-INFLATED GAMMA DISTRIBUTION
####### Simulate r; P,N - zero inflated gamma with kappa = 4, gamma = 6 ########## 
# Parameters
mu <- 0
omega <- 0.1
alpha <- 0.1
beta <- 0.1
a <- 0.8
b <- 0.8
kappa <- 4
gamma <- 6

prob_zero_positive <- 0.8  # Probability of zero for positive sentiment
prob_zero_negative <- 0.8  # Probability of zero for negative sentiment
meanlog_positive <- 0.5  # Lognormal mean for positive sentiment
sdlog_positive <- 0.7    # Lognormal sd for positive sentiment
meanlog_negative <- 0.5  # Lognormal mean for negative sentiment
sdlog_negative <- 0.7    # Lognormal sd for negative sentiment

# Step 1: Simulate Zero-Inflated Lognormal Positive (P_t) and Negative (N_t) Sentiment Data
P_t_Aux <- ifelse(runif(n) < prob_zero_positive, 0, rlnorm(n, meanlog_positive, sdlog_positive))
N_t_Aux <- ifelse(runif(n) < prob_zero_negative, 0, -rlnorm(n, meanlog_negative, sdlog_negative))

library(caret)
P_t_Aux2 <- preProcess(as.data.frame(P_t_Aux), method=c("range"))
P_t <- c(predict(P_t_Aux2, as.data.frame(P_t_Aux)))


N_t_Aux2 <- preProcess(as.data.frame(N_t_Aux), method=c("range"),rangeBounds = c(-1, 0))
N_t <- c(predict(N_t_Aux2, as.data.frame(N_t_Aux)))


# P_t <- (P_t_Aux-min(P_t_Aux)/(max(P_t_Aux)-min(P_t_Aux)))
# N_t <-  -1 + (N_t_Aux - min(N_t_Aux)) / (-min(N_t_Aux) * (0 - (-1)))


# Step 2: Simulate NA-GARCH(1,1) Process
r_t <- numeric(n)
sigma2_t <- numeric(n)
f_t <- numeric(n)
epsilon_t <- numeric(n)

# Initial values
sigma2_t[1] <- omega / (1 - alpha - beta)  # Set initial variance
epsilon_t[1] <- rnorm(1, mean = 0, sd = sqrt(sigma2_t[1]))  # First shock
r_t[1] <- mu + epsilon_t[1]  # First return

# Loop through time to generate the process
for (t in 2:n) {
  # Calculate f_t based on P_t and N_t
  f_t[t-1] <- a + 0.5 * b * ((exp(kappa * P_t[[1]][t-1]) - 1) / (exp(kappa * P_t[[1]][t-1]) + 1) - 
                               (exp(gamma * N_t[[1]][t-1]) - 1) / (exp(gamma * N_t[[1]][t-1]) + 1))
  
  if (P_t[[1]][t-1] == 0 && N_t[[1]][t-1] == 0) {
    f_t[t-1] <- 1
  }
  # Update sigma^2 using GARCH(1,1) model
  sigma2_t[t] <- f_t[t-1] * (omega + alpha * epsilon_t[t-1]^2 + beta * sigma2_t[t-1])
  
  # Generate z_t from standard normal distribution
  z_t <- rnorm(1)
  
  # Calculate the new epsilon_t
  epsilon_t[t] <- z_t * sqrt(sigma2_t[t])
  
  # Update returns r_t
  r_t[t] <- mu + epsilon_t[t]
}

# Step 3: Combine Results into a Data Frame
simulated_data <- data.frame(
  Time = 1:n,
  Returns = r_t,
  Volatility = sqrt(sigma2_t),
  Positive_Sentiment = P_t,
  Negative_Sentiment = N_t,
  f_t = f_t
) %>% 
  rename(Positive_Sentiment = P_t_Aux,
         Negative_Sentiment = N_t_Aux)

# Plotting the results
p1 <- ggplot(simulated_data, aes(x = Positive_Sentiment)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  # geom_density(color = "red", size = 1, alpha = 0.7) +
  labs(title = "Zero-Inflated Positive Sentiment (0 to 1)", x = "Positive Sentiment", y = "Density") +
  theme_minimal()

p2 <- ggplot(simulated_data, aes(x = Negative_Sentiment)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  # geom_density(color = "red", size = 1, alpha = 0.7) +
  labs(title = "Zero-Inflated Negative Sentiment (-1 to 0)", x = "Negative Sentiment", y = "Density") +
  theme_minimal()

f_t_hist_k4_g6 <- grid.arrange(p1, p2, ncol = 2)

#### Simulated data checks
######## heatmap of P_t,N-t, f_t


f_t_heatmap_k4_g6 <- ggplot(simulated_data %>% 
         filter(f_t > 0), aes(x = as.integer(Positive_Sentiment*100), 
                                y = as.integer(Negative_Sentiment*100), fill = f_t)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = c("blue", "green", "red"),
    values = scales::rescale(c(0, 1, 2)),  # Blue for <1, white for =1, red for >1
    breaks = seq(0.8, 1.3, by = 0.1),
    limits = c(0.8, 1.3)
  ) + 
  labs(title = "Heatmap of Sentiment and f_t: kappa = 4, gamma = 6",
       x = "Positive Sentiment (P_t)",
       y = "Negative Sentiment (N_t)",
       fill = "f_t") +
  scale_y_reverse()+
  theme_minimal()


### Additional checks
simulated_data %>% 
  # filter(f_t > 1.5) %>% 
  ggplot(aes(x=Positive_Sentiment, y = f_t))+
  geom_line()

simulated_data %>% 
  # filter(f_t > 1.5) %>% 
  ggplot(aes(x=Negative_Sentiment, y = f_t))+
  geom_line()

hist(simulated_data$f_t, breaks = 100, xlim = c(0.8,2))


####### Simulate r; P,N - zero inflated gamma with kappa = 6, gamma = 4 ########## 
# Parameters
mu <- 0
omega <- 0.1
alpha <- 0.1
beta <- 0.1
a <- 0.8
b <- 0.8
kappa <- 6
gamma <- 4


prob_zero_positive <- 0.8  # Probability of zero for positive sentiment
prob_zero_negative <- 0.8  # Probability of zero for negative sentiment
meanlog_positive <- 0.5  # Lognormal mean for positive sentiment
sdlog_positive <- 0.7    # Lognormal sd for positive sentiment
meanlog_negative <- 0.5  # Lognormal mean for negative sentiment
sdlog_negative <- 0.7    # Lognormal sd for negative sentiment

# Step 1: Simulate Zero-Inflated Lognormal Positive (P_t) and Negative (N_t) Sentiment Data
P_t_Aux <- ifelse(runif(n) < prob_zero_positive, 0, rlnorm(n, meanlog_positive, sdlog_positive))
N_t_Aux <- ifelse(runif(n) < prob_zero_negative, 0, -rlnorm(n, meanlog_negative, sdlog_negative))

library(caret)
P_t_Aux2 <- preProcess(as.data.frame(P_t_Aux), method=c("range"))
P_t <- c(predict(P_t_Aux2, as.data.frame(P_t_Aux)))


N_t_Aux2 <- preProcess(as.data.frame(N_t_Aux), method=c("range"),rangeBounds = c(-1, 0))
N_t <- c(predict(N_t_Aux2, as.data.frame(N_t_Aux)))


# P_t <- (P_t_Aux-min(P_t_Aux)/(max(P_t_Aux)-min(P_t_Aux)))
# N_t <-  -1 + (N_t_Aux - min(N_t_Aux)) / (-min(N_t_Aux) * (0 - (-1)))


# Step 2: Simulate NA-GARCH(1,1) Process
r_t <- numeric(n)
sigma2_t <- numeric(n)
f_t <- numeric(n)
epsilon_t <- numeric(n)

# Initial values
sigma2_t[1] <- omega / (1 - alpha - beta)  # Set initial variance
epsilon_t[1] <- rnorm(1, mean = 0, sd = sqrt(sigma2_t[1]))  # First shock
r_t[1] <- mu + epsilon_t[1]  # First return

# Loop through time to generate the process
for (t in 2:n) {
  # Calculate f_t based on P_t and N_t
  f_t[t-1] <- a + 0.5 * b * ((exp(kappa * P_t[[1]][t-1]) - 1) / (exp(kappa * P_t[[1]][t-1]) + 1) - 
                               (exp(gamma * N_t[[1]][t-1]) - 1) / (exp(gamma * N_t[[1]][t-1]) + 1))
  
  if (P_t[[1]][t-1] == 0 && N_t[[1]][t-1] == 0) {
    f_t[t-1] <- 1
  }
  # Update sigma^2 using GARCH(1,1) model
  sigma2_t[t] <- f_t[t-1] * (omega + alpha * epsilon_t[t-1]^2 + beta * sigma2_t[t-1])
  
  # Generate z_t from standard normal distribution
  z_t <- rnorm(1)
  
  # Calculate the new epsilon_t
  epsilon_t[t] <- z_t * sqrt(sigma2_t[t])
  
  # Update returns r_t
  r_t[t] <- mu + epsilon_t[t]
}

# Step 3: Combine Results into a Data Frame
simulated_data <- data.frame(
  Time = 1:n,
  Returns = r_t,
  Volatility = sqrt(sigma2_t),
  Positive_Sentiment = P_t,
  Negative_Sentiment = N_t,
  f_t = f_t
) %>% 
  rename(Positive_Sentiment = P_t_Aux,
         Negative_Sentiment = N_t_Aux)

# Plotting the results
p1 <- ggplot(simulated_data, aes(x = Positive_Sentiment)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  # geom_density(color = "red", size = 1, alpha = 0.7) +
  labs(title = "Zero-Inflated Positive Sentiment (0 to 1)", x = "Positive Sentiment", y = "Density") +
  theme_minimal()

p2 <- ggplot(simulated_data, aes(x = Negative_Sentiment)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  # geom_density(color = "red", size = 1, alpha = 0.7) +
  labs(title = "Zero-Inflated Negative Sentiment (-1 to 0)", x = "Negative Sentiment", y = "Density") +
  theme_minimal()

f_t_hist_k6_g4 <- grid.arrange(p1, p2, ncol = 2)

######## heatmap of P_t,N-t, f_t


f_t_heatmap_k6_g4 <- ggplot(simulated_data %>% 
                              filter(f_t > 0), aes(x = as.integer(Positive_Sentiment*100), 
                                                   y = as.integer(Negative_Sentiment*100), fill = f_t)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = c("blue", "green", "red"),
    values = scales::rescale(c(0, 1, 2)),  # Blue for <1, white for =1, red for >1
    breaks = seq(0.8, 1.3, by = 0.1),
    limits = c(0.8, 1.3)
  ) + 
  labs(title = "Heatmap of Sentiment and f_t: kappa = 6, gamma = 4",
       x = "Positive Sentiment (P_t)",
       y = "Negative Sentiment (N_t)",
       fill = "f_t") +
  scale_y_reverse()+
  theme_minimal()

f_t_heatmap_k4_g4

### Additional checks
simulated_data %>% 
  # filter(f_t > 1.5) %>% 
  ggplot(aes(x=Positive_Sentiment, y = f_t))+
  geom_line()

simulated_data %>% 
  # filter(f_t > 1.5) %>% 
  ggplot(aes(x=Negative_Sentiment, y = f_t))+
  geom_line()

hist(simulated_data$f_t, breaks = 100, xlim = c(0.8,2))



####### Simulate r; P,N - zero inflated gamma with kappa = 4, gamma = 4 ########## 
# Parameters
mu <- 0
omega <- 0.1
alpha <- 0.1
beta <- 0.1
a <- 0.8
b <- 0.8
kappa <- 4
gamma <- 4

prob_zero_positive <- 0.8  # Probability of zero for positive sentiment
prob_zero_negative <- 0.8  # Probability of zero for negative sentiment
meanlog_positive <- 0.5  # Lognormal mean for positive sentiment
sdlog_positive <- 0.7    # Lognormal sd for positive sentiment
meanlog_negative <- 0.5  # Lognormal mean for negative sentiment
sdlog_negative <- 0.7    # Lognormal sd for negative sentiment

# Step 1: Simulate Zero-Inflated Lognormal Positive (P_t) and Negative (N_t) Sentiment Data
P_t_Aux <- ifelse(runif(n) < prob_zero_positive, 0, rlnorm(n, meanlog_positive, sdlog_positive))
N_t_Aux <- ifelse(runif(n) < prob_zero_negative, 0, -rlnorm(n, meanlog_negative, sdlog_negative))

library(caret)
P_t_Aux2 <- preProcess(as.data.frame(P_t_Aux), method=c("range"))
P_t <- c(predict(P_t_Aux2, as.data.frame(P_t_Aux)))


N_t_Aux2 <- preProcess(as.data.frame(N_t_Aux), method=c("range"),rangeBounds = c(-1, 0))
N_t <- c(predict(N_t_Aux2, as.data.frame(N_t_Aux)))


# P_t <- (P_t_Aux-min(P_t_Aux)/(max(P_t_Aux)-min(P_t_Aux)))
# N_t <-  -1 + (N_t_Aux - min(N_t_Aux)) / (-min(N_t_Aux) * (0 - (-1)))


# Step 2: Simulate NA-GARCH(1,1) Process
r_t <- numeric(n)
sigma2_t <- numeric(n)
f_t <- numeric(n)
epsilon_t <- numeric(n)

# Initial values
sigma2_t[1] <- omega / (1 - alpha - beta)  # Set initial variance
epsilon_t[1] <- rnorm(1, mean = 0, sd = sqrt(sigma2_t[1]))  # First shock
r_t[1] <- mu + epsilon_t[1]  # First return

# Loop through time to generate the process
for (t in 2:n) {
  # Calculate f_t based on P_t and N_t
  f_t[t-1] <- a + 0.5 * b * ((exp(kappa * P_t[[1]][t-1]) - 1) / (exp(kappa * P_t[[1]][t-1]) + 1) - 
                               (exp(gamma * N_t[[1]][t-1]) - 1) / (exp(gamma * N_t[[1]][t-1]) + 1))
  
  if (P_t[[1]][t-1] == 0 && N_t[[1]][t-1] == 0) {
    f_t[t-1] <- 1
  }
  # Update sigma^2 using GARCH(1,1) model
  sigma2_t[t] <- f_t[t-1] * (omega + alpha * epsilon_t[t-1]^2 + beta * sigma2_t[t-1])
  
  # Generate z_t from standard normal distribution
  z_t <- rnorm(1)
  
  # Calculate the new epsilon_t
  epsilon_t[t] <- z_t * sqrt(sigma2_t[t])
  
  # Update returns r_t
  r_t[t] <- mu + epsilon_t[t]
}

# Step 3: Combine Results into a Data Frame
simulated_data <- data.frame(
  Time = 1:n,
  Returns = r_t,
  Volatility = sqrt(sigma2_t),
  Positive_Sentiment = P_t,
  Negative_Sentiment = N_t,
  f_t = f_t
) %>% 
  rename(Positive_Sentiment = P_t_Aux,
         Negative_Sentiment = N_t_Aux)

# Plotting the results
p1 <- ggplot(simulated_data, aes(x = Positive_Sentiment)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  # geom_density(color = "red", size = 1, alpha = 0.7) +
  labs(title = "Zero-Inflated Positive Sentiment (0 to 1)", x = "Positive Sentiment", y = "Density") +
  theme_minimal()

p2 <- ggplot(simulated_data, aes(x = Negative_Sentiment)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
  # geom_density(color = "red", size = 1, alpha = 0.7) +
  labs(title = "Zero-Inflated Negative Sentiment (-1 to 0)", x = "Negative Sentiment", y = "Density") +
  theme_minimal()

f_t_hist_k4_g4 <- grid.arrange(p1, p2, ncol = 2)

######## heatmap of P_t,N-t, f_t


f_t_heatmap_k4_g4 <- ggplot(simulated_data %>% 
                              filter(f_t > 0), aes(x = as.integer(Positive_Sentiment*100), 
                                                   y = as.integer(Negative_Sentiment*100), fill = f_t)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = c("blue", "green", "red"),
    values = scales::rescale(c(0, 1, 2)),  # Blue for <1, white for =1, red for >1
    breaks = seq(0.8, 1.3, by = 0.1),
    limits = c(0.8, 1.3)
  ) + 
  labs(title = "Heatmap of Sentiment and f_t: kappa = gamma = 4",
       x = "Positive Sentiment (P_t)",
       y = "Negative Sentiment (N_t)",
       fill = "f_t") +
  scale_y_reverse()+
  theme_minimal()

f_t_heatmap_k4_g4

grid.arrange(f_t_heatmap_k4_g6, f_t_heatmap_k4_g4)
### Additional checks
simulated_data %>% 
  # filter(f_t > 1.5) %>% 
  ggplot(aes(x=Positive_Sentiment, y = f_t))+
  geom_line()

simulated_data %>% 
  # filter(f_t > 1.5) %>% 
  ggplot(aes(x=Negative_Sentiment, y = f_t))+
  geom_line()

hist(simulated_data$f_t, breaks = 100, xlim = c(0.8,2))




### SIMULATIONS WITH P_t N_t HALF NORMAL DISTRIBUTION

### SIMULATIONS WITH P_t N_t HALF NORMAL DISTRIBUTION
##### Simulate r; P,N - half-normal, kappa = 4, gamma = 4 #######

# Parameters
n <- 10000  # Number of time steps
mu <- 0
omega <- 0.1
alpha <- 0.1
beta <- 0.1
a <- 0.8
b <- 0.8
kappa <- 4
gamma <- 4

# Generate data for Half-Normal distribution
sigma <- 0.5

### Positive
generate_half_normal <- function(n, sigma) {
  data <- abs(rnorm(n, mean = 0, sd = sigma))
  
  min_val <- min(data)
  max_val <- max(data)
  standardized_data <- (data - min_val) / (max_val - min_val) * (1 - 0.01) + 0.01
  
  return(standardized_data)
}

standardized_data <- c()

while(length(standardized_data) < n) {
  additional_data <- generate_half_normal(n, sigma)  # Generate n samples each time
  standardized_data <- c(standardized_data, additional_data)
}

P_t <- standardized_data[1:n]

### Negative

generate_half_normal_negative <- function(n, sigma) {
  data <- -abs(rnorm(n, mean = 0, sd = sigma))  # Generate negative half-normal values
  
  min_val <- min(data)
  max_val <- max(data)
  standardized_data <- (data - min_val) / (max_val - min_val) * (-0.01 + 1) - 1
  
  return(standardized_data)
}

standardized_data <- c()

while(length(standardized_data) < n) {
  additional_data <- generate_half_normal_negative(n, sigma)  # Generate n samples each time
  standardized_data <- c(standardized_data, additional_data)
}

N_t <- standardized_data[1:n]

# Step 2: Simulate NA-GARCH(1,1) Process
r_t <- numeric(n)
sigma2_t <- numeric(n)
f_t <- numeric(n)
epsilon_t <- numeric(n)

# Initial values
sigma2_t[1] <- omega / (1 - alpha - beta)  # Set initial variance
epsilon_t[1] <- rnorm(1, mean = 0, sd = sqrt(sigma2_t[1]))  # First shock
r_t[1] <- mu + epsilon_t[1]  # First return

# Loop through time to generate the process
for (t in 2:n) {
 
  # Calculate f_t based on P_t and N_t  
  # if (P_t[[1]][t-1] == 0 && N_t[[1]][t-1] == 0) {
  #   f_t[t-1] <- 1
  # } else {
    f_t[t-1] <- a + 0.5 * b * ((exp(kappa * P_t[t-1]) - 1) / (exp(kappa * P_t[t-1]) + 1) - 
                                 (exp(gamma * N_t[t-1]) - 1) / (exp(gamma * N_t[t-1]) + 1))
  # }
  # Update sigma^2 using GARCH(1,1) model
  sigma2_t[t] <- f_t[t-1] * (omega + alpha * epsilon_t[t-1]^2 + beta * sigma2_t[t-1])
  
  # Generate z_t from standard normal distribution
  z_t <- rnorm(1)
  
  # Calculate the new epsilon_t
  epsilon_t[t] <- z_t * sqrt(sigma2_t[t])
  
  # Update returns r_t
  r_t[t] <- mu + epsilon_t[t]
}

r <- r_t

# Step 3: Combine Results into a Data Frame
simulated_data <- data.frame(
  Time = 1:n,
  Returns = r_t,
  Volatility = sqrt(sigma2_t),
  Positive_Sentiment = P_t,
  Negative_Sentiment = N_t,
  f_t = f_t
) 
# %>% 
#   rename(Positive_Sentiment = P_t_Aux,
#          Negative_Sentiment = N_t_Aux)


ggplot(simulated_data, aes(x = as.integer(Positive_Sentiment*100), 
                                                   y = as.integer(Negative_Sentiment*100), fill = f_t)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = c("blue", "green", "red"),
    values = scales::rescale(c(0, 1, 2)),  # Blue for <1, white for =1, red for >1
    breaks = seq(0.8, 1.6, by = 0.1),
    limits = c(0.8, 1.6)
  ) + 
  labs(title = "Heatmap of Sentiment and f_t: kappa = gamma = 4",
       x = "Positive Sentiment (P_t)",
       y = "Negative Sentiment (N_t)",
       fill = "f_t") +
  scale_y_reverse()+
  theme_minimal()



###### FINAL COMPARISON ########
f_heatmaps <- grid.arrange(f_t_heatmap_k4_g6, f_t_heatmap_k4_g4, f_t_heatmap_k6_g4)


###### FINAL DATA ######
r <- simulated_data$Returns
P_t <- simulated_data$Positive_Sentiment
N_t <- simulated_data$Negative_Sentiment
f_t <- simulated_data$f_t

Simulated_TS <- data.frame("r" = r,
           "P" = P_t,
           "N" = N_t,
           "f" = f_t)

Simulated_TS %>% 
  filter(P == 0 & N == 0)

#### Clean-up
all_objects <- ls()
objects_to_keep <- c("r", "P_t", "N_t","f_t","f_heatmaps","n", "simulated_data")
objects_to_remove <- setdiff(all_objects, objects_to_keep)
rm(list = objects_to_remove)
rm(all_objects,objects_to_remove, objects_to_keep)

#### GARCH check
plot(r, type = "l")
ARCH <- FinTS::ArchTest(r)
ARCH_Test <- print(ARCH$p.value)
# p-value < 0.05 therefore there exists ARCH effect and we move to GARCH analysis.
