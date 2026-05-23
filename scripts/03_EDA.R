library(ggplot2)
library(xts)

returns = read.csv("data/daily_returns.csv")
returns_xts = xts(returns[,-1],order.by = as.Date(returns$Date))

normalised_prices <- sweep(
  prices_xts,
  MARGIN = 2,
  STATS = as.numeric(first(prices_xts)),
  FUN = "/"
) * 100

matplot(index(normalised_prices),
        coredata(normalised_prices),
        type = "l",
        lty = 1,
        col = c("black", "red", "green"),
        xlab = "Date",
        ylab = "Value of $100 Investment",
        main = "Growth of $100 Invested in Each Asset")

legend("topleft",
       legend = colnames(normalised_prices),
       lty = 1,
       col = c("black", "red", "green"))

return_spy = returns$SPY
return_ief = returns$IEF
return_gld = returns$GLD
#how $100 will grow if invested in each asset 


png("figures/daily_returns_spy.png", width = 1000, height = 600)
plot(returns_xts$SPY, main = "Daily Returns of SPY", ylab = "Daily Return", xlab = "Date")
dev.off()

png("figures/daily_returns_ief.png", width = 1000, height = 600)
plot(returns_xts$IEF, main = "Daily Returns of IEF", ylab = "Daily Return", xlab = "Date")
dev.off()

png("figures/daily_returns_gld.png", width = 1000, height = 600)
plot(returns_xts$GLD, main = "Daily Returns of GLD", ylab = "Daily Return", xlab = "Date")
dev.off()

mean_daily = colMeans(returns_xts)
sd_daily = apply(returns_xts,2,sd)

annualized_return = mean_daily*252
annualized_volatility = sd_daily * sqrt(252)

asset_summary = data.frame(Asset = colnames(returns_xts),
                           Mean_daily_return = as.numeric(mean_daily),
                           Daily_volatility = as.numeric(sd_daily),
                           Annualized_return = as.numeric(annualized_return),
                           Annualized_volatility = as.numeric(annualized_volatility)
)

asset_summary[,-1] = round(asset_summary[,-1]*100,2)
asset_summary

correlation_matrix = cor(returns_xts)
round(correlation_matrix,3)
cor_df = as.data.frame(as.table(correlation_matrix))

ggplot(cor_df, aes(x = Var1, y = Var2, fill = Freq)) + 
  geom_tile() + geom_text(aes(label = round(Freq, 2))) + 
  labs(title = "Correlation Matrix of Asset Returns", 
       x = "Asset", y = "Asset", fill = "Correlation") + 
  theme_minimal()

ggsave("figures/correlation_heatmap.png", width = 7, height = 5)

