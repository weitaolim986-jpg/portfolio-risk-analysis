library(xts)

weights = rbind(
  Equity_heavy = c(0.8,0.1,0.1),
  Balanced = c(0.50,0.25,0.25),
  Defensive = c(0.30,0.40,0.30)
)

colnames(weights) = c("SPY","IEF","GLD")

portfolio_returns <- xts( coredata(returns_xts) %*% t(weights), order.by = index(returns_xts))

colnames(portfolio_returns) = rownames(weights)

head(portfolio_returns)

write.csv(data.frame(Date = index(portfolio_returns),coredata(portfolio_returns)),
          "data/portfolio_returns.csv",row.names = FALSE)

initial_investment = 10000
portfolio_values = initial_investment * cumprod(1 + portfolio_returns)

png("figures/05_growth_of_10000_by_portfolio.png", width = 1000, height = 600)
matplot(index(portfolio_values),coredata(portfolio_values),
        type = 'l',lty = 1, col=c('black','red','green'),
        xlab = "Date", ylab = "Portfolio Value in USD",
        main = "Growth of USD 10,000 Across Portfolios")

legend("topleft",legend = colnames(portfolio_values), lty = 1, col = c("black","red","green"))
dev.off()
