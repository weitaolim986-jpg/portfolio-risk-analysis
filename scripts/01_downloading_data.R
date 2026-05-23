install.packages(c("quantmod","xts","ggplot2","PerformanceAnalytics"))
library(quantmod)
library(xts)
library(ggplot2)
library(PerformanceAnalytics)

start_date <- as.Date("2015-01-01")
end_date <- as.Date("2025-12-31")

SPY <- getSymbols("SPY",
                  src = "yahoo",
                  from = start_date,
                  to = end_date,
                  auto.assign = FALSE)

IEF <- getSymbols("IEF",
                  src = "yahoo",
                  from = start_date,
                  to = end_date,
                  auto.assign = FALSE)

GLD <- getSymbols("GLD",
                  src = "yahoo",
                  from = start_date,
                  to = end_date,
                  auto.assign = FALSE)

head(SPY)
head(IEF)
head(GLD)

spy_price <- Ad(SPY)
ief_price <- Ad(IEF)
gld_price <- Ad(GLD)

prices <- merge(spy_price, ief_price, gld_price)

colnames(prices) <- c("SPY", "IEF", "GLD")

head(prices)
summary(prices)
any(is.na(prices))
#there is no empty data

write.csv(data.frame(Date = index(prices), coredata(prices)),
          "data/adjusted_prices.csv",
          row.names = FALSE)

