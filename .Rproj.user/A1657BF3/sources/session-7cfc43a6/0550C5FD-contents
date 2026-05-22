library(xts)

prices <- read.csv("data/adjusted_prices.csv")

prices_xts <- xts(prices[, -1],
                  order.by = as.Date(prices$Date))

returns <- prices_xts / lag(prices_xts) - 1

returns <- na.omit(returns)

head(returns)
summary(returns)

write.csv(data.frame(Date = index(returns), coredata(returns)), "data/daily_returns.csv", row.names = FALSE)