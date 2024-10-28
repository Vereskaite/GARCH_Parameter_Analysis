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


hfanal_returns <- function(da, int, basic=1){
  # Compute intraday LOG returns & realized volatility & number of trades in 
  # each trading day.
  #
  # int: time interval in minutes for which returns to be computed.
  # basic: in minutes, the base interval to process transaction data.
  #
  # da: data in the format: date, hour, minute, second, return, volume
  
  istart <- 9 * 60 * 60 + 30 * 60
  iend <- 16 * 60 * 60
  timemid <- da[,2] * 3600 + da[,3] * 60 + da[,4]
  da <- cbind(da[,1], timemid, da[,5]) # Use return instead of price
  colnames(da) <- c("Date", "Time", "Return")
  T0 <- nrow(da)
  idx <- which(da[,2] >= istart)
  da <- da[idx, ]
  T1 <- nrow(da)
  jdx <- which(da[,2] <= iend)
  da <- da[jdx, ]
  T <- nrow(da)
  
  print(c(T0, T1, T))
  
  Ytot <- NULL
  RV <- NULL
  logrtn <- NULL
  ntrad <- NULL
  cnt <- 0
  
  # Process through days
  while (cnt < T){
    date <- da[(cnt+1), 1]
    idx <- which(da[,1] == date)
    x <- da[idx, ]
    ntrad <- c(ntrad, nrow(x))
    r1 <- x[, 3]  # Directly using returns
    y1 <- sum(r1^2) * 252
    Ytot <- c(Ytot, sqrt(y1))
    
    m1 <- intraDay(x, basic)
    Pr <- m1$Return  # Using "Return" now instead of "Price"
    m2 <- intraRtn(Pr, basic, int)
    rtn <- m2$returns
    v1 <- sum(rtn^2) * 252
    RV <- c(RV, sqrt(v1))
    
    logrtn <- c(logrtn, rtn)
    cnt <- cnt + nrow(x)
    print(cnt)
  }
  
  hfanal <- list(returns = logrtn, Ytot = Ytot, realized = RV, ntrad = ntrad)
}


"intraDay" <- function(da,basic){
  # da: matrix consisting of trade-by-trade
  # The format is Date, Time and Price (Time is in seconds from midnight)
  # basic: base time interval (measured in minutes)
  #
  # The program basically creates a series of intraday price series for the 
  # base interval "basic".
  # 
  #
  if(!is.matrix(da))da=as.matrix(da)
  ist = 9*3600+30*60
  int=basic*60
  Nob=6.5*3600/int
  # Nob is the number of intervals with length "int"
  y=NULL
  T=nrow(da)
  idx=c(1:T)[da[,2]==ist]
  if(length(idx)<1){
    y=da[1,3]
    jused = 1
  }
  else {
    y=da[length(idx),3]
    jused = length(idx)
  }
  for (i in 1:Nob){
    iend=ist+i*int
    idx=c(1:T)[(da[,2]-iend) <= 0]
    jj=idx[length(idx)]
    if(jj < jused){
      y=c(y,y[length(y)])
    }
    else {
      jused=jj
      y=c(y,da[jj,3])
    }
  }
  intraDay <- list(Price=y)
}

"intraRtn" <- function(Pr,basic,int){
  # computes the intradaily returns of interal "int".
  # The input "Pr" is the transaction price for each interval "basic".
  #
  multi = int/basic
  base = basic*60
  intval=int*60
  Nob = 6.5*3600/base
  T = Nob/multi
  idx=c(1,c(1:T)*multi+1)
  pp = log(Pr[idx])
  rtn=diff(pp)
  
  intraRtn <- list(returns=rtn)
}

# date, hour, minute, second, price, volume

Data_Simulation_function <- function(kappa, gamma) {
  # Parameters
  # days <- 252
  # intraday_steps <- 86400
  n <- 5896800
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

Data_k4_g4 <- Data_Simulation_function(kappa = 4, gamma = 4)

r_intraday <- Data_k4_g4$Returns/
P_t_intraday <- Data_k4_g4$Positive_Sentiment
N_t_intraday <- Data_k4_g4$Negative_Sentiment

data <- data.frame(date = paste0("d_",1:length(r_intraday)),
                   minute = paste0("m_",1:length(r_intraday)),
                   second = paste0("d_",1:length(r_intraday)),
                   price = r_intraday,
                   volume = 1)
head(data)



hfanal(data, 5)
