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
  wealth = cumprod(c(1,1+r))
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

ggsave('figures/06_var_expected_shortfall.png',width = 8, height = 5)

covid_spy = spy_price['2020-02-19/2020-03-23']
covid_ief = ief_price['2020-02-19/2020-03-23']
covid_gld = gld_price['2020-02-19/2020-03-23']
rate_spy = spy_price['2022-01-01/2022-12-31']
rate_ief = ief_price['2022-01-01/2022-12-31']
rate_gld = gld_price['2022-01-01/2022-12-31']

dir.create(
  "figures",
  showWarnings = FALSE
)

# Function to combine and normalise prices
create_stress_price_data <- function(spy_price, ief_price, gld_price, period_name) {
  
  prices <- merge(
    spy_price,
    ief_price,
    gld_price
  )
  
  colnames(prices) <- c(
    "SPY",
    "IEF",
    "GLD"
  )
  
  prices <- na.omit(prices)
  
  first_prices <- as.numeric(
    prices[1, ]
  )
  
  normalised_core <- sweep(
    coredata(prices),
    MARGIN = 2,
    STATS = first_prices,
    FUN = "/"
  ) * 100
  
  normalised_prices <- xts(
    normalised_core,
    order.by = index(prices)
  )
  
  colnames(normalised_prices) <- colnames(prices)
  
  print(
    head(normalised_prices)
  )
  
  price_df <- data.frame(
    Date = as.Date(index(normalised_prices)),
    coredata(normalised_prices)
  )
  
  price_df_long <- reshape(
    price_df,
    varying = c(
      "SPY",
      "IEF",
      "GLD"
    ),
    v.names = "Normalised_Price",
    timevar = "Asset",
    times = c(
      "SPY",
      "IEF",
      "GLD"
    ),
    direction = "long"
  )
  
  rownames(price_df_long) <- NULL
  
  price_df_long$Period <- period_name
  
  return(price_df_long)
}

covid_asset_price_data <- create_stress_price_data(
  spy_price = covid_spy,
  ief_price = covid_ief,
  gld_price = covid_gld,
  period_name = "COVID-19 Sell-Off"
)

rate_asset_price_data <- create_stress_price_data(
  spy_price = rate_spy,
  ief_price = rate_ief,
  gld_price = rate_gld,
  period_name = "2022 Inflation / Rising-Rate Environment"
)

covid_asset_plot <- ggplot(
  covid_asset_price_data,
  aes(
    x = Date,
    y = Normalised_Price,
    colour = Asset
  )
) +
  geom_line(
    linewidth = 0.9
  ) +
  labs(
    title = "Asset Performance During COVID-19 Sell-Off",
    subtitle = "Normalised price index, starting value = 100",
    x = "Date",
    y = "Normalised Price Index",
    colour = "Asset"
  ) +
  theme_minimal()

print(covid_asset_plot)

ggsave(
  filename = "figures/21_asset_performance_covid_stress.png",
  plot = covid_asset_plot,
  width = 10,
  height = 6,
  dpi = 200
)

rate_asset_plot <- ggplot(
  rate_asset_price_data,
  aes(
    x = Date,
    y = Normalised_Price,
    colour = Asset
  )
) +
  geom_line(
    linewidth = 0.9
  ) +
  labs(
    title = "Asset Performance During 2022 Inflation and Rising-Rate Environment",
    subtitle = "Normalised price index, starting value = 100",
    x = "Date",
    y = "Normalised Price Index",
    colour = "Asset"
  ) +
  theme_minimal()

print(rate_asset_plot)

ggsave(
  filename = "figures/22_asset_performance_2022_stress.png",
  plot = rate_asset_plot,
  width = 10,
  height = 6,
  dpi = 200
)