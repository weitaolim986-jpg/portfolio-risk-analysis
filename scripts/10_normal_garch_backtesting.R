library(rugarch)

balanced_returns_pct = portfolio_returns_xts[,"Balanced"]*100
defensive_returns_pct = portfolio_returns_xts[,"Defensive"]*100

rolling_window <- 500

refit_frequency <- 1

garch_backtest_results = data.frame()

create_garch_var_results <- function(
    garch_density,
    portfolio_name,
    confidence
) {
  confidence_label <- paste0(
    round(confidence * 100),
    "%"
  )
  
  lower_tail_probability <- 1 - confidence
  
  predicted_return_cutoff_pct <- garch_density$Mu +
    qnorm(lower_tail_probability) * garch_density$Sigma
  
  predicted_var_pct <- -predicted_return_cutoff_pct
  
  actual_return_pct <- garch_density$Realized
  
  actual_loss_pct <- -actual_return_pct
  
  forecast_dates <- as.Date(
    rownames(garch_density)
  )
  
  data.frame(
    Date = forecast_dates,
    Portfolio = portfolio_name,
    Confidence_Level = confidence_label,
    Method = "Normal GARCH VaR",
    Actual_Return = actual_return_pct / 100,
    Actual_Loss = actual_loss_pct / 100,
    Predicted_VaR = predicted_var_pct / 100,
    Exceedance = actual_loss_pct > predicted_var_pct
  )
}
  
for(returns_pct in list(equity_returns_pct,balanced_returns_pct,defensive_returns_pct)){
  garch_roll <- ugarchroll(
  spec = garch_spec_normal,
  data = returns_pct,
  n.ahead = 1,
  n.start = rolling_window,
  refit.every = refit_frequency,
  refit.window = "moving",
  window.size = rolling_window,
  solver = "hybrid",
  calculate.VaR = FALSE,
  keep.coef = TRUE
)

garch_density <- as.data.frame(
  garch_roll,
  which = "density"
)

garch_results_95 <- create_garch_var_results(
  garch_density = garch_density,
  portfolio_name = colnames(returns_pct),
  confidence = 0.95
)

garch_results_99 <- create_garch_var_results(
  garch_density = garch_density,
  portfolio_name = colnames(returns_pct),
  confidence = 0.99
)

garch_backtest_results <- rbind(
  garch_backtest_results,
  rbind(garch_results_95,garch_results_99)
)
}

garch_backtest_results$Method[
  garch_backtest_results$Method == "GARCH VaR"
  ] = "Normal GARCH VaR"

garch_backtest_results$Date <- as.Date(
  garch_backtest_results$Date
)

head(garch_backtest_results)

summarise_backtest_results <- function(results) {
  exceedance_counts <- aggregate(
    Exceedance ~ Portfolio + Confidence_Level + Method,
    data = results,
    FUN = sum
  )
  
  colnames(exceedance_counts)[4] <- "Number_of_Exceedances"
  
  test_days <- aggregate(
    Exceedance ~ Portfolio + Confidence_Level + Method,
    data = results,
    FUN = length
  )
  
  colnames(test_days)[4] <- "Number_of_Test_Days"
  
  exceedance_rates <- aggregate(
    Exceedance ~ Portfolio + Confidence_Level + Method,
    data = results,
    FUN = mean
  )
  
  colnames(exceedance_rates)[4] <- "Exceedance_Rate"
  
  summary_table <- merge(
    exceedance_counts,
    test_days,
    by = c(
      "Portfolio",
      "Confidence_Level",
      "Method"
    )
  )
  
  summary_table <- merge(
    summary_table,
    exceedance_rates,
    by = c(
      "Portfolio",
      "Confidence_Level",
      "Method"
    )
  )
  
  summary_table$Expected_Exceedance_Rate <- ifelse(
    summary_table$Confidence_Level == "95%",
    0.05,
    0.01
  )
  
  summary_table$Exceedance_Rate_Percent <- round(
    summary_table$Exceedance_Rate * 100,
    2
  )
  
  summary_table$Expected_Exceedance_Rate_Percent <- round(
    summary_table$Expected_Exceedance_Rate * 100,
    2
  )
  
  summary_table <- summary_table[
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
  
  return(summary_table)
}

garch_backtesting_summary <- summarise_backtest_results(
  garch_backtest_results
)

print(garch_backtesting_summary)

write.csv(
  garch_backtesting_summary,
  "data/garch_var_backtesting_summary.csv",
  row.names = FALSE
)

existing_backtest_file <- "data/var_backtesting_predictions.csv"

if (file.exists(existing_backtest_file)) {
  existing_backtest_results <- read.csv(
    existing_backtest_file
  )
  
  existing_backtest_results$Exceedance <- as.logical(
    existing_backtest_results$Exceedance
  )
  
  required_columns <- c(
    "Date",
    "Portfolio",
    "Confidence_Level",
    "Method",
    "Actual_Return",
    "Actual_Loss",
    "Predicted_VaR",
    "Exceedance"
  )
  
  combined_backtest_results <- rbind(
    existing_backtest_results[, required_columns],
    garch_backtest_results[, required_columns]
  )
  
  combined_backtesting_summary <- summarise_backtest_results(
    combined_backtest_results
  )
  
  print(combined_backtesting_summary)
  
  write.csv(
    combined_backtest_results,
    "data/combined_var_backtesting_predictions.csv",
    row.names = FALSE
  )
  
  write.csv(
    combined_backtesting_summary,
    "data/combined_var_backtesting_summary.csv",
    row.names = FALSE
  )
} else {
  message(
    "Earlier VaR backtesting predictions were not found. GARCH results were still saved separately."
  )
}

plot_data <- subset(
  garch_backtest_results,
  Confidence_Level == "99%"
)

if (nrow(plot_data) == 0) {
  stop(
    "No 99% GARCH VaR observations were found."
  )
}

library(ggplot2)

garch_var_plot <- ggplot(
  plot_data,
  aes(x = as.Date(Date))
) +
  geom_line(
    aes(
      y = Actual_Loss * 100,
      colour = "Actual Daily Loss"
    )
  ) +
  geom_line(
    aes(
      y = Predicted_VaR * 100,
      colour = "Predicted Normal GARCH VaR"
    )
  ) +
  geom_point(
    data = subset(
      plot_data,
      Exceedance == TRUE
    ),
    aes(
      y = Actual_Loss * 100
    ),
    size = 1.5
  ) +
  labs(
    title = "Backtesting 99% Normal GARCH VaR for the Equity-Heavy Portfolio",
    subtitle = "Points indicate days when actual loss exceeded predicted VaR",
    x = "Date",
    y = "Daily Loss / VaR (%)",
    colour = "Series"
  ) +
  theme_minimal()

print(garch_var_plot)

ggsave(
  filename = "figures/13_garch_99_var_backtesting.png",
  plot = garch_var_plot,
  width = 11,
  height = 6,
  dpi = 200
)

dir.create(
  "figures",
  showWarnings = FALSE
)

garch_99_plot_data <- subset(
  garch_backtest_results,
  Confidence_Level == "99%"
)

if (nrow(garch_99_plot_data) == 0) {
  stop(
    "No 99% Normal GARCH VaR results were found. Check your Confidence_Level labels."
  )
}

garch_99_all_portfolios_plot <- ggplot(
  garch_99_plot_data,
  aes(x = Date)
) +
  geom_line(
    aes(
      y = Actual_Loss * 100,
      colour = "Actual Daily Loss"
    ),
    linewidth = 0.35,
    alpha = 0.70
  ) +
  geom_line(
    aes(
      y = Predicted_VaR * 100,
      colour = "Predicted 99% Normal GARCH VaR"
    ),
    linewidth = 0.55
  ) +
  geom_point(
    data = subset(
      garch_99_plot_data,
      Exceedance == TRUE
    ),
    aes(
      y = Actual_Loss * 100
    ),
    colour = "red",
    size = 1.2
  ) +
  facet_wrap(
    ~ Portfolio,
    ncol = 1
  ) +
  labs(
    title = "Backtesting 99% Normal GARCH VaR Across Portfolios",
    subtitle = "Red points indicate days when actual loss exceeded predicted VaR",
    x = "Date",
    y = "Daily Loss / Predicted VaR (%)",
    colour = "Series"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

print(garch_99_all_portfolios_plot)

ggsave(
  filename = "figures/15_garch_99_var_backtesting_all_portfolios.png",
  plot = garch_99_all_portfolios_plot,
  width = 11,
  height = 10,
  dpi = 200
)

garch_all_combinations_plot_data <- garch_backtest_results

if (nrow(garch_all_combinations_plot_data) == 0) {
  stop(
    "The GARCH backtesting results table is empty."
  )
}

garch_all_combinations_plot <- ggplot(
  garch_all_combinations_plot_data,
  aes(x = Date)
) +
  geom_line(
    aes(
      y = Actual_Loss * 100,
      colour = "Actual Daily Loss"
    ),
    linewidth = 0.25,
    alpha = 0.65
  ) +
  geom_line(
    aes(
      y = Predicted_VaR * 100,
      colour = "Predicted Normal GARCH VaR"
    ),
    linewidth = 0.45
  ) +
  geom_point(
    data = subset(
      garch_all_combinations_plot_data,
      Exceedance == TRUE
    ),
    aes(
      y = Actual_Loss * 100
    ),
    colour = "red",
    size = 0.8
  ) +
  facet_grid(
    Confidence_Level ~ Portfolio
  ) +
  labs(
    title = "Detailed Normal GARCH VaR Backtesting Across All Portfolios",
    subtitle = "Red points indicate exceedances of the predicted VaR threshold",
    x = "Date",
    y = "Daily Loss / Predicted VaR (%)",
    colour = "Series"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 7
    ),
    strip.text = element_text(
      face = "bold"
    )
  )

print(garch_all_combinations_plot)

ggsave(
  filename = "figures/16_garch_var_backtesting_all_combinations.png",
  plot = garch_all_combinations_plot,
  width = 14,
  height = 8,
  dpi = 220
)

combined_backtesting_summary$Portfolio <- factor(
  combined_backtesting_summary$Portfolio,
  levels = c(
    "Equity_heavy",
    "Balanced",
    "Defensive"
  )
)

combined_backtesting_summary$Confidence_Level <- factor(
  combined_backtesting_summary$Confidence_Level,
  levels = c(
    "95%",
    "99%"
  )
)

combined_backtesting_summary$Method <- factor(
  combined_backtesting_summary$Method,
  levels = c(
    "Historical VaR",
    "Parametric VaR",
    "Normal GARCH VaR"
  )
)

reference_rates <- data.frame(
  Confidence_Level = factor(
    c("95%", "99%"),
    levels = c("95%", "99%")
  ),
  Expected_Rate_Percent = c(
    5,
    1
  )
)

model_comparison_plot <- ggplot(
  combined_backtesting_summary,
  aes(
    x = Method,
    y = Exceedance_Rate_Percent,
    fill = Method
  )
) +
  geom_col(
    width = 0.7
  ) +
  geom_hline(
    data = reference_rates,
    aes(
      yintercept = Expected_Rate_Percent
    ),
    linetype = "dashed",
    linewidth = 0.6,
    inherit.aes = FALSE
  ) +
  facet_grid(
    Confidence_Level ~ Portfolio
  ) +
  labs(
    title = "Backtesting Comparison of VaR Models Across Portfolios",
    subtitle = "Dashed lines indicate theoretically expected exceedance rates",
    x = "VaR Model",
    y = "Actual Exceedance Rate (%)",
    fill = "VaR Model"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    strip.text = element_text(
      face = "bold"
    )
  )

print(model_comparison_plot)

ggsave(
  filename = "figures/14_var_model_backtesting_comparison_all_portfolios.png",
  plot = model_comparison_plot,
  width = 13,
  height = 8,
  dpi = 220
)
