# Install and load the tidyquant package
library(tidyquant)

# Fetch Apple stock data
apple_stock <- tq_get("AAPL", from = "2020-01-01", to = Sys.Date())

# View the first few rows of the data
head(apple_stock)


r <- apple_stock %>% 
  pull(close)

r <- diff(r)

r <- r[1:1000]
