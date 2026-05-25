calculate_historical_var_rate <- function(r, confidence) {
  r <- as.numeric(r)
  
  return_cutoff <- quantile(
    r,
    probs = 1 - confidence,
    names = FALSE
  )
  
  -return_cutoff
}

calculate_parametric_var_rate <- function(r, confidence) {
  r <- as.numeric(r)
  
  mean_return <- mean(r)
  volatility <- sd(r)
  
  return_cutoff <- mean_return +
    qnorm(1 - confidence) * volatility
  
  -return_cutoff
}

library(ggplot2)
library(xts)

rolling_window <- 500

confidence_levels <- c(
  0.95,
  0.99
)

portfolio_names <- colnames(
  portfolio_returns_xts
)

backtest_results <- data.frame()

for (portfolio in portfolio_names) {
  
  portfolio_returns <- as.numeric(
    portfolio_returns_xts[, portfolio]
  )
  
  portfolio_dates <- index(
    portfolio_returns_xts
  )
  
  for (confidence in confidence_levels) {
    
    confidence_label <- paste0(
      round(confidence * 100),
      "%"
    )
    
    for (i in (rolling_window + 1):length(portfolio_returns)) {
      
      historical_window <- portfolio_returns[
        (i - rolling_window):(i - 1)
      ]
      
      actual_return <- portfolio_returns[i]
      
      actual_loss <- -actual_return
      
      historical_var <- calculate_historical_var_rate(
        historical_window,
        confidence
      )
      
      parametric_var <- calculate_parametric_var_rate(
        historical_window,
        confidence
      )
      
      historical_exceedance <- actual_loss > historical_var
      
      parametric_exceedance <- actual_loss > parametric_var
      
      new_rows <- data.frame(
        Date = rep(portfolio_dates[i], 2),
        Portfolio = rep(portfolio, 2),
        Confidence_Level = rep(confidence_label, 2),
        Method = c(
          "Historical VaR",
          "Parametric VaR"
        ),
        Actual_Return = rep(actual_return, 2),
        Actual_Loss = rep(actual_loss, 2),
        Predicted_VaR = c(
          historical_var,
          parametric_var
        ),
        Exceedance = c(
          historical_exceedance,
          parametric_exceedance
        )
      )
      
      backtest_results <- rbind(
        backtest_results,
        new_rows
      )
    }
  }
}

write.csv(
  backtest_results,
  "data/var_backtesting_predictions.csv",
  row.names = FALSE
)


# Count how many exceedances occurred for each model and portfolio.
exceedance_counts <- aggregate(
  Exceedance ~ Portfolio + Confidence_Level + Method,
  data = backtest_results,
  FUN = sum
)

colnames(exceedance_counts)[4] <- "Number_of_Exceedances"

# Count how many days were tested for each model and portfolio.
test_days <- aggregate(
  Exceedance ~ Portfolio + Confidence_Level + Method,
  data = backtest_results,
  FUN = length
)

colnames(test_days)[4] <- "Number_of_Test_Days"

# Calculate the proportion of test days on which VaR was exceeded.
exceedance_rates <- aggregate(
  Exceedance ~ Portfolio + Confidence_Level + Method,
  data = backtest_results,
  FUN = mean
)

colnames(exceedance_rates)[4] <- "Exceedance_Rate"

# Combine the three summary tables.
backtesting_summary <- merge(
  exceedance_counts,
  test_days,
  by = c(
    "Portfolio",
    "Confidence_Level",
    "Method"
  )
)

backtesting_summary <- merge(
  backtesting_summary,
  exceedance_rates,
  by = c(
    "Portfolio",
    "Confidence_Level",
    "Method"
  )
)

# Add the theoretical exceedance rate.
backtesting_summary$Expected_Exceedance_Rate <- ifelse(
  backtesting_summary$Confidence_Level == "95%",
  0.05,
  0.01
)

# Convert rates into percentages for easier interpretation.
backtesting_summary$Exceedance_Rate_Percent <- round(
  backtesting_summary$Exceedance_Rate * 100,
  2
)

backtesting_summary$Expected_Exceedance_Rate_Percent <- round(
  backtesting_summary$Expected_Exceedance_Rate * 100,
  2
)

# Keep only the useful final columns.
backtesting_summary <- backtesting_summary[
  ,
  c(
    "Portfolio",
    "Confidence_Level",
    "Method",
    "Number_of_Exceedances",
    "Number_of_Test_Days",
    "Exceedance_Rate_Percent",
    "Expected_Exceedance_Rate_Percent"
  )
]

print(backtesting_summary)

write.csv(
  backtesting_summary,
  "data/var_backtesting_summary.csv",
  row.names = FALSE
)


plot_data <- subset(
  backtest_results,
  Portfolio == "Equity_heavy" &
    Confidence_Level == "99%"
)

backtest_plot <- ggplot(
  plot_data,
  aes(x = as.Date(Date))
) +
  geom_line(
    aes(
      y = Actual_Loss,
      colour = "Actual Daily Loss"
    )
  ) +
  geom_line(
    aes(
      y = Predicted_VaR,
      colour = Method
    )
  ) +
  facet_wrap(
    ~ Method,
    ncol = 1
  ) +
  labs(
    title = "Backtesting 99% VaR for the Equity-Heavy Portfolio",
    subtitle = "Exceedances occur when actual daily loss rises above predicted VaR",
    x = "Date",
    y = "Loss as a Proportion of Portfolio Value",
    colour = "Series"
  ) +
  theme_minimal()

ggsave(
  filename = "figures/11_var_backtesting_exceedances.png",
  plot = backtest_plot,
  width = 11,
  height = 8,
  dpi = 200
)