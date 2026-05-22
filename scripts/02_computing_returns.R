library(xts)

prices <- read.csv("data/adjusted_prices.csv")

prices_xts <- xts(prices[, -1],
                  order.by = as.Date(prices$Date))

returns <- prices_xts / lag(prices_xts) - 1

returns <- na.omit(returns)

head(returns)
summary(returns)


