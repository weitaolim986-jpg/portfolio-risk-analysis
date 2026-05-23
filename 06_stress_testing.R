#two extreme conditions: Covid and inflation + rising interest rate 

library(xts)
portfolio_data = read.csv("data/portfolio_returns.csv")
portfolio_returns = xts(portfolio_data[,-1],order.by = as.Date(portfolio_data$Date))

covid = portfolio_returns['2020-02-19/2020-03-23']
rate_hike = portfolio_returns['2022-01-01/2022-12-31']

calculate_total_return = function(r){
  prod(1+r) - 1
}

calculate_max_drawdown = function(r){
  wealth = cumprod(r)
  running_peak = cummax(wealth)
  drawdown = wealth/running_peak-1
  min(drawdown)
}

covid_results = apply(covid,2,calculate_total_return)
rate_results = apply(rate_hike,2,calculate_total_return)

stress_return_table = data.frame(
  Portfolio = colnames(portfolio_returns),
  Covid_res = round(covid_results*100,2),
  Rate_res = round(rate_results*100,2)
)

covid_drawdown = apply(covid,2,calculate_max_drawdown)
rate_drawdown = apply(rate_hike,2,calculate_max_drawdown)

stress_drawdown_table = data.frame(
  Portfolio = colnames(portfolio_returns),
  Covid_res = round(covid_drawdown*100,2),
  Rate_res = round(rate_drawdown*100,2)
)

rownames(stress_return_table) = NULL
rownames(stress_drawdown_table) = NULL

risk_plot_data = data.frame(
  Portfolio = rep(risk_table$Portfolio,2),
  Risk_Measure = rep(c("VaR","Expected Shortfall"), each = 3),
  Dollar_Loss = c(risk_table$VaR_95_Dollars,
                  risk_table$Expected_Shortfall_95_Dollars)
)

library(ggplot2)

ggplot(risk_plot_data, aes(x = Portfolio, y = Dollar_Loss, fill = Risk_Measure)) + geom_bar(stat = "identity", position = "dodge") + labs(title = "One-Day 95% VaR and Expected Shortfall", x = "Portfolio", y = "Loss on a USD 10,000 Portfolio", fill = "Risk Measure") + theme_minimal()

ggsave('figures/var_expected_shortfall.png',width = 8, height = 5)

