distribution.model = "std"
rolling_window_std = 500
refit_frequency_std = 1
garch_std_backtest_results = data.frame()

garch_spec_std <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "std"
)

create_std_garch_var_results <- function(
    student_t_density,
    portfolio_name,
    confidence
) {
  confidence_label <- paste0(
    round(confidence * 100),
    "%"
  )
  
  lower_tail_probability <- 1 - confidence
  
  standardized_probability = qdist(
    distribution = "std",
    p = lower_tail_probability,
    mu = 0,
    sigma = 1,
    shape = student_t_density$Shape
  )
  
  predicted_return_cutoff_pct <- student_t_density$Mu +
    standardized_probability * student_t_density$Sigma
  
  predicted_var_pct <- -predicted_return_cutoff_pct
  
  actual_return_pct <- student_t_density$Realized
  
  actual_loss_pct <- -actual_return_pct
  
  forecast_dates <- as.Date(
    rownames(student_t_density)
  )
  
  data.frame(
    Date = forecast_dates,
    Portfolio = portfolio_name,
    Confidence_Level = confidence_label,
    Method = "Student-t GARCH VaR",
    Actual_Return = actual_return_pct / 100,
    Actual_Loss = actual_loss_pct / 100,
    Predicted_VaR = predicted_var_pct / 100,
    Exceedance = actual_loss_pct > predicted_var_pct
  )
}

for(portfolio in colnames(portfolio_returns_xts)){
  returns_pct = portfolio_returns_xts[,portfolio]*100
  student_t_roll <- ugarchroll(
    spec = garch_spec_std,
    data = returns_pct,
    n.ahead = 1,
    n.start = rolling_window_std,
    refit.every = refit_frequency_std,
    refit.window = "moving",
    window.size = rolling_window_std,
    solver = "hybrid",
    calculate.VaR = FALSE,
    keep.coef = TRUE
  )
  
  student_t_density <- as.data.frame(
    student_t_roll,
    which = "density"
  )
  
  
  garch_std_results_95 <- create_std_garch_var_results(
    student_t_density,
    portfolio_name = portfolio,
    confidence = 0.95
  )
  
  garch_std_results_99 <- create_std_garch_var_results(
    student_t_density,
    portfolio_name = portfolio,
    confidence = 0.99
  )
  
  garch_std_backtest_results <- rbind(
    garch_std_backtest_results,
    rbind(garch_std_results_95,garch_std_results_99)
  )
}

write.csv(
  garch_std_backtest_results,
  "data/garch_std_var_backtesting_predictions.csv",
  row.names = FALSE
)


garch_std_backtesting_summary <- summarise_backtest_results(
  garch_std_backtest_results
)

print(garch_std_backtesting_summary)

write.csv(
  garch_std_backtesting_summary,
  "data/garch_std_var_backtesting_summary.csv",
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
  
  existing_backtest_results$Date = as.Date(existing_backtest_results$Date)
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
  
  combined_std_backtest_results <- rbind(
    existing_backtest_results[, required_columns],
    garch_backtest_results[, required_columns],
    garch_std_backtest_results[, required_columns]
  )
  
  combined_std_backtesting_summary <- summarise_backtest_results(
    combined_std_backtest_results
  )
  
  print(combined_std_backtesting_summary)
  
  write.csv(
    combined_std_backtest_results,
    "data/combined_var_std_backtesting_predictions.csv",
    row.names = FALSE
  )
  
  write.csv(
    combined_std_backtesting_summary,
    "data/combined_var_std_backtesting_summary.csv",
    row.names = FALSE
  )
} else {
  message(
    "Earlier VaR backtesting predictions were not found. GARCH results were still saved separately."
  )
}

plot_data <- subset(
  garch_std_backtest_results,
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
      colour = "Predicted GARCH VaR"
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
    title = "Backtesting 99% Student's T Distributed GARCH VaR for the Equity-Heavy Portfolio",
    subtitle = "Points indicate days when actual loss exceeded predicted VaR",
    x = "Date",
    y = "Daily Loss / VaR (%)",
    colour = "Series"
  ) +
  theme_minimal()

print(garch_var_plot)

ggsave(
  filename = "figures/17_garch_99_std_var_backtesting.png",
  plot = garch_var_plot,
  width = 11,
  height = 6,
  dpi = 200
)

dir.create(
  "figures",
  showWarnings = FALSE
)

garch_99_std_plot_data <- subset(
  garch_std_backtest_results,
  Confidence_Level == "99%"
)

if (nrow(garch_99_std_plot_data) == 0) {
  stop(
    "No 99% GARCH VaR results were found. Check your Confidence_Level labels."
  )
}

garch_99_std_all_portfolios_plot <- ggplot(
  garch_99_std_plot_data,
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
      colour = "Predicted 99% GARCH VaR"
    ),
    linewidth = 0.55
  ) +
  geom_point(
    data = subset(
      garch_99_std_plot_data,
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
    title = "Backtesting 99% Student's T Distributed GARCH VaR Across Portfolios",
    subtitle = "Red points indicate days when actual loss exceeded predicted VaR",
    x = "Date",
    y = "Daily Loss / Predicted VaR (%)",
    colour = "Series"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

print(garch_99_std_all_portfolios_plot)

ggsave(
  filename = "figures/18_garch_99_std_var_backtesting_all_portfolios.png",
  plot = garch_99_std_all_portfolios_plot,
  width = 11,
  height = 10,
  dpi = 200
)

garch_all_combinations_plot_data_std <- garch_std_backtest_results

if (nrow(garch_all_combinations_plot_data_std) == 0) {
  stop(
    "The GARCH backtesting results table is empty."
  )
}

garch_all_combinations_plot_std <- ggplot(
  garch_all_combinations_plot_data_std,
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
      colour = "Predicted GARCH VaR"
    ),
    linewidth = 0.45
  ) +
  geom_point(
    data = subset(
      garch_all_combinations_plot_data_std,
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
    title = "Detailed Student's T Distributed GARCH VaR Backtesting Across All Portfolios",
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

print(garch_all_combinations_plot_std)

ggsave(
  filename = "figures/19_garch_var_std_backtesting_all_combinations.png",
  plot = garch_all_combinations_plot_std,
  width = 14,
  height = 8,
  dpi = 220
)

combined_std_backtesting_summary$Portfolio <- factor(
  combined_std_backtesting_summary$Portfolio,
  levels = c(
    "Equity_heavy",
    "Balanced",
    "Defensive"
  )
)

combined_std_backtesting_summary$Confidence_Level <- factor(
  combined_std_backtesting_summary$Confidence_Level,
  levels = c(
    "95%",
    "99%"
  )
)

combined_std_backtesting_summary$Method <- factor(
  combined_std_backtesting_summary$Method,
  levels = c(
    "Historical VaR",
    "Parametric VaR",
    "GARCH VaR",
    "Student-t GARCH VaR"
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

model_comparison_plot_with_std <- ggplot(
  combined_std_backtesting_summary,
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

print(model_comparison_plot_with_std)

ggsave(
  filename = "figures/20_var_model_backtesting_comparison_all_portfolios_with_std.png",
  plot = model_comparison_plot_with_std,
  width = 13,
  height = 8,
  dpi = 220
)
