set.seed(123)
#### DEFINE DATA ##########

# Generate sample data
# P_t and N_t - unif
n <- 1000
P_t <- runif(n, 0, 1)
N_t <- runif(n, -1, 0)

# exp
lambda <- 1
P_t_exp <- rexp(n, lambda) 
P_t <- (P_t_exp - min(P_t_exp)) / (max(P_t_exp) - min(P_t_exp))
max(P_t)

N_t_exp <- rexp(n, lambda) 
N_t <- -((N_t_exp - min(N_t_exp)) / (max(N_t_exp) - min(N_t_exp)))

# r
set.seed(123)
n <- 1000
r <- numeric(n)
epsilon <- numeric(n)
sigma2 <- numeric(n)
f <- numeric(n)
z <- rnorm(n)
mu <- 0.1
omega <- 0.1
alpha <- 0.2
beta <- 0.2
a <- 0.5
b <- 1.5
kappa <- 4
gamma <- 4
sigma2[1] ^2

sigma2[1] <- omega / (1 - (alpha + beta)*(a+b))
epsilon[1] <- z[1] * sqrt(sigma2[1])
r[1] <- mu + epsilon[1]
f[1] <- a + 0.5 * b * ((exp(kappa * P_t[1]) - 1) / (exp(kappa * P_t[1]) + 1) - (exp(gamma * N_t[1]) - 1) / (exp(gamma * N_t[1]) + 1))


# nesirunnina..
for (t in 2:n) {
  f[t] <- a + 0.5 * b * ((exp(kappa * P_t[t]) - 1) / (exp(kappa * P_t[t]) + 1) - (exp(gamma * N_t[t]) - 1) / (exp(gamma * N_t[t]) + 1))
  sigma2[t] <- f[t-1]*(omega + alpha * epsilon[t-1]^2 + beta * sigma2[t-1])
  epsilon[t] <- z[t] * sqrt(sigma2[t])
  r[t] <- mu + epsilon[t]
}


rm(mu,omega,alpha, beta, z, sigma2, epsilon,a,b,n,t, kappa,gamma, N_t_exp, P_t_exp, lambda)
rm(f)
plot(r, type = "l")
plot(P_t, type = "l")
plot(N_t, type = "l")
plot(f, type = "l")

