set.seed(123)
# Create trading time table
start_date <- as.POSIXct("2013-01-01 09:30:00")
end_date <- as.POSIXct("2013-06-30 16:00:00")

trading_times_Aux <- seq(from = start_date, to = end_date, by = "min") %>%
  .[format(., "%H:%M:%S") >= "09:30:00" & format(., "%H:%M:%S") <= "16:00:00" & 
      !weekdays(.) %in% c("Saturday", "Sunday")]

trading_times <- data.frame(
  full_date = trading_times_Aux,
  date = as.Date(trading_times_Aux),
  hour = as.numeric(format(trading_times_Aux, "%H")),
  minute = as.numeric(format(trading_times_Aux, "%M"))
  # second = as.numeric(format(trading_times_Aux, "%S"))
)

# create TS for returns
returns_min <- rnorm(nrow(trading_times), mean = 0, sd = 0.1)

returns_df <- trading_times %>% 
  cbind(returns_min)

returns_df_short <- returns_df %>%  select(full_date, returns_min)

#calculate it's RV for 1 min interval
daily_realized_volatility <- returns_df %>%
  group_by(date) %>%
  summarize(realized_vol = sqrt(sum(returns_min^2)))

plot(daily_realized_volatility$realized_vol, type = "l")

# model garch - take dailyvalues
garchspec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
           mean.model = list(armaOrder = c(1,1)))
?ugarchfit
garchfit <- ugarchfit(spec = garchspec, data = returns_df[,5])

  # forecast
?ugarchforecast
garchforecast <- ugarchforecast(garchfit, n.ahead= 100)
plot(garchforecast)

# Extract forecasted mean and volatility
mean_forecast <- garchforecast@forecast$seriesFor
volatility_forecast <- garchforecast@forecast$sigmaFor

# Plot mean and volatility forecasts
# par(mfrow = c(1, 1))  # Set up for two plots in one window
plot(mean_forecast, type = "l", main = "Mean Forecasts", ylab = "Forecasted Mean")
plot(volatility_forecast, type = "l", main = "Volatility Forecasts", ylab = "Forecasted Volatility")

# Summary statistics
mean_summary <- summary(mean_forecast)
volatility_summary <- summary(volatility_forecast)



# Load required library
library(rugarch)
library(ggplot2)

# Simulate returns data (1 year of daily returns)
set.seed(123)
n_days <- 252
simulated_returns <- rnorm(n_days, mean = 0, sd = 0.02)  # 2% daily vol

# Split data into in-sample (80%) and out-of-sample (20%) sets
in_sample_returns <- simulated_returns[1:(0.8 * n_days)]
out_sample_returns <- simulated_returns[(0.8 * n_days + 1):n_days]

# Specify and fit the GARCH(1,1) model with in-sample data
garchspec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                        mean.model = list(armaOrder = c(1,1)))
garchfit <- ugarchfit(spec = garchspec, data = in_sample_returns)

# Forecast volatility for out-of-sample period
n_ahead <- length(out_sample_returns)
garch_forecast <- ugarchforecast(garchfit, n.ahead = n_ahead)

# Extract forecasted volatility
forecasted_volatility <- garch_forecast@forecast$sigmaFor
actual_volatility <- abs(out_sample_returns)  # Using absolute returns as a proxy for realized vol

# Calculate RMSE
rmse <- sqrt(mean((forecasted_volatility - actual_volatility)^2))
print(paste("RMSE:", rmse))

# Plot forecasted vs actual volatility
df <- data.frame(
  Day = 1:n_ahead,
  ForecastedVolatility = forecasted_volatility,
  ActualVolatility = actual_volatility
)

ggplot(df, aes(x = Day)) +
  geom_line(aes(y = forecasted_volatility, color = "Forecasted Volatility")) +
  geom_line(aes(y = actual_volatility, color = "Actual Volatility")) +
  labs(title = "Forecasted vs Actual Volatility", y = "Volatility") +
  theme_minimal() +
  scale_color_manual("", values = c("Forecasted Volatility" = "blue", "Actual Volatility" = "red"))



###### Second try
