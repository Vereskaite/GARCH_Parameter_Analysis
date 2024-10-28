# Create trading time table
start_date <- as.POSIXct("2013-01-01 09:30:00")
end_date <- as.POSIXct("2013-12-31 16:00:00")

trading_times_Aux <- seq(from = start_date, to = end_date, by = "sec") %>%
  .[format(., "%H:%M:%S") >= "09:30:00" & format(., "%H:%M:%S") <= "16:00:00" & 
    !weekdays(.) %in% c("Saturday", "Sunday")]

trading_times <- data.frame(
  date = as.Date(trading_times_Aux),
  hour = as.numeric(format(trading_times_Aux, "%H")),
  minute = as.numeric(format(trading_times_Aux, "%M")),
  second = as.numeric(format(trading_times_Aux, "%S"))
)


# Simulate intraday data
Data_Simulation_function <- function(kappa, gamma) {
  # Parameters
  n <- nrow(trading_times)
  mu <- 0
  omega <- 0.1
  alpha <- 0.1
  beta <- 0.1
  a <- 0.8
  b <- 0.8
  kappa <- kappa
  gamma <- gamma
  
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
  
  # Simulate NA-GARCH(1,1) Process
  
  r_t <- numeric(n)
  sigma2_t <- numeric(n)
  f_t <- numeric(n)
  epsilon_t <- numeric(n)
  
  # Initial values
  sigma2_t[1] <- omega / (1 - (alpha + beta)*(a+b))  # Set initial variance
  epsilon_t[1] <- rnorm(1, mean = 0, sd = sqrt(sigma2_t[1]))  # First shock
  r_t[1] <- mu + epsilon_t[1]  # First return
  
  for (t in 2:n) {
    
    f_t[t-1] <- a + 0.5 * b * ((exp(kappa * P_t[t-1]) - 1) / (exp(kappa * P_t[t-1]) + 1) - 
                                 (exp(gamma * N_t[t-1]) - 1) / (exp(gamma * N_t[t-1]) + 1))
    
    sigma2_t[t] <- f_t[t-1] * (omega + alpha * epsilon_t[t-1]^2 + beta * sigma2_t[t-1])
    
    z_t <- rnorm(1)
    
    epsilon_t[t] <- z_t * sqrt(sigma2_t[t])
    
    r_t[t] <- mu + epsilon_t[t]
  }
  
  r <- r_t
  
  simulated_data <- data.frame(
    Time = 1:n,
    Returns = r_t,
    Volatility = sqrt(sigma2_t),
    Positive_Sentiment = P_t,
    Negative_Sentiment = N_t,
    f_t = f_t
  ) 
  
}

Data_k4_g4_full <- Data_Simulation_function(kappa = 4, gamma = 4)

# Join trading time and simulated intraday
Full_simulated_data <- cbind(Data_k4_g4_full, trading_times)

# trading data for modelling
Data_k4_g4 <- Full_simulated_data %>%
  filter(hour == 16, minute == 0, second == 0)

# Define RV function
hfanal <- function(da,int,basic=1){
  # Compute intraday LOG returns & realized volatility & number of trades in 
  # each trading day.
  #
  # int: time interval in minutes for which returns to be computed.
  # basic: in minutes, the base interval to process transaction data.
  # The idea of subsampling is to be done shortly. [September 14, 2010.]
  #
  # da: data in the format: date, hour, minute, second, price, volume
  #
  # if(!is.matrix(da))da=as.matrix(da)
  # First, remove trade outside of the normal trading hours
  istart=9*60*60+30*60
  iend=16*60*60
  timemid=da[,2]*3600+da[,3]*60+da[,4]
  da=cbind(da[,1],timemid,da[,5])
  colnames(da) <- c("Date","Time","Price")
  T0=nrow(da)
  idx=c(1:T0)[da[,2] >= istart]
  da=da[idx,]
  T1=nrow(da)
  jdx=c(1:T1)[da[,2] <= iend]
  da=da[jdx,]
  T=nrow(da)
  ##
  print(c(T0,T1,T))
  
  Ytot=NULL
  RV=NULL
  logrtn=NULL
  ntrad=NULL
  cnt = 0
  # Process through days
  while (cnt < T){
    date=da[(cnt+1),1]
    idx=c(1:T)[da[,1]==date]
    x=da[idx,]
    ntrad=c(ntrad,nrow(x))
    pp=log(x[,3])
    r1=diff(pp)
    y1=sum(r1^2)*252
    Ytot=c(Ytot,sqrt(y1))
    #
    m1=intraDay(x,basic)
    Pr=m1$Price
    m2=intraRtn(Pr,basic,int)
    rtn=m2$returns
    v1=sum(rtn^2)*252
    RV=c(RV,sqrt(v1))
    #
    logrtn=c(logrtn,rtn)
    cnt=cnt+nrow(x)
    print(cnt)
  }
  
  hfanal <- list(returns=logrtn,Ytot=Ytot,realized=RV,ntrad=ntrad)
}

hfanal_returns <- function(da, int, basic=1) {
  # Compute intraday LOG returns & realized volatility & number of trades in each trading day.
  #
  # int: time interval in minutes for which returns to be computed.
  # basic: in minutes, the base interval to process transaction data.
  #
  # da: data in the format: date, hour, minute, second, returns, volume
  
  istart = 9 * 60 * 60 + 30 * 60
  iend = 16 * 60 * 60
  timemid = da[, 2] * 3600 + da[, 3] * 60 + da[, 4]
  da = cbind(da[, 1], timemid, da[, 5])  # Now includes returns instead of price
  colnames(da) <- c("Date", "Time", "Returns")
  
  T0 = nrow(da)
  idx = which(da[, 2] >= istart)
  da = da[idx, ]
  T1 = nrow(da)
  jdx = which(da[, 2] <= iend)
  da = da[jdx, ]
  T = nrow(da)
  
  print(c(T0, T1, T))
  
  Ytot = NULL
  RV = NULL
  logrtn = NULL
  ntrad = NULL
  cnt = 0
  
  # Process through days
  while (cnt < T) {
    date = da[(cnt + 1), 1]
    idx = which(da[, 1] == date)
    x = da[idx, ]
    ntrad = c(ntrad, nrow(x))
    
    rtn = x[, 3]  # Directly using returns
    y1 = sum(rtn^2) * 252  # Annualized volatility
    Ytot = c(Ytot, sqrt(y1))  # Realized volatility
    
    # Using returns for realized volatility calculation
    v1 = sum(rtn^2) * 252  # Annualized realized volatility
    RV = c(RV, sqrt(v1)) 
    
    logrtn = c(logrtn, rtn)  # Collect returns
    cnt = cnt + nrow(x)
    print(cnt)
  }
  
  hfanal <- list(returns = logrtn, Ytot = Ytot, realized = RV, ntrad = ntrad)
}

# Prepare data for RV function

data_for_rv <- Full_simulated_data %>% 
  rename(price = Returns) %>% 
  mutate(volume = 1) %>% 
  select(date, hour, minute, second, price, volume)

m5 <- hfanal_returns(data_for_rv, 5)

plot(m5$realized, type = "l")



