dir.create(
  "figures",
  showWarnings = FALSE
)

portfolio_return_file <- "data/portfolio_returns.csv"

if (!file.exists(portfolio_return_file)) {
  stop(
    "Cannot find data/portfolio_returns.csv. Run the portfolio construction script first."
  )
}

portfolio_returns_df <- read.csv(
  portfolio_return_file
)

portfolio_returns_xts <- xts(
  portfolio_returns_df[, -1],
  order.by = as.Date(portfolio_returns_df$Date)
)

head(portfolio_returns_xts)
summary(portfolio_returns_xts)

confidence_levels = c(0.95,0.99)

calculate_parametric_var <- function(r, confidence, value) {
  r <- as.numeric(r)
  
  mean_return <- mean(r)
  volatility <- sd(r)
  
  return_cutoff <- mean_return +
    qnorm(1 - confidence) * volatility
  
  var_dollars <- -return_cutoff * value
  
  return(var_dollars)
}

results_list <- list()

for (confidence in confidence_levels) {
  
  historical_results <- apply(
    portfolio_returns_xts,
    2,
    calculate_historical_var,
    confidence = confidence,
    value = portfolio_value
  )
  
  parametric_results <- apply(
    portfolio_returns_xts,
    2,
    calculate_parametric_var,
    confidence = confidence,
    value = portfolio_value
  )
  
  confidence_label <- paste0(
    round(confidence * 100),
    "%"
  )
  
  results_list[[confidence_label]] <- data.frame(
    Portfolio = colnames(portfolio_returns_xts),
    Confidence_Level = confidence_label,
    Historical_VaR_Dollars = round(
      unname(historical_results),
      2
    ),
    Parametric_VaR_Dollars = round(
      unname(parametric_results),
      2
    )
  )
}

var_comparison_table <- do.call(
  rbind,
  results_list
)

rownames(var_comparison_table) <- NULL

print(var_comparison_table)

var_comparison_table$Parametric_Minus_Historical <- round(
  var_comparison_table$Parametric_VaR_Dollars -
    var_comparison_table$Historical_VaR_Dollars,
  2
)

print(var_comparison_table)

write.csv(
  var_comparison_table,
  "data/historical_vs_parametric_var.csv",
  row.names = FALSE
)

library(ggplot2)
historical_plot_data <- data.frame(
  Portfolio = var_comparison_table$Portfolio,
  Confidence_Level = var_comparison_table$Confidence_Level,
  Method = "Historical VaR",
  VaR_Dollars = var_comparison_table$Historical_VaR_Dollars
)

parametric_plot_data <- data.frame(
  Portfolio = var_comparison_table$Portfolio,
  Confidence_Level = var_comparison_table$Confidence_Level,
  Method = "Parametric VaR",
  VaR_Dollars = var_comparison_table$Parametric_VaR_Dollars
)

var_plot_data <- rbind(
  historical_plot_data,
  parametric_plot_data
)

var_comparison_plot <- ggplot(
  var_plot_data,
  aes(
    x = Portfolio,
    y = VaR_Dollars,
    fill = Method
  )
) +
  geom_col(
    position = "dodge"
  ) +
  facet_wrap(
    ~ Confidence_Level
  ) +
  labs(
    title = "Historical VaR versus Parametric VaR",
    subtitle = "One-day estimated losses for a $10,000 portfolio",
    x = "Portfolio",
    y = "Estimated Dollar Loss",
    fill = "VaR Method"
  ) +
  theme_minimal()

ggsave(
  filename = "figures/08_historical_vs_parametric_var.png",
  plot = var_comparison_plot,
  width = 10,
  height = 6,
  dpi = 200
)

equity_heavy_returns <- as.numeric(
  portfolio_returns_xts[, "Equity_heavy"]
)

distribution_df <- data.frame(
  Return = equity_heavy_returns
)

distribution_plot <- ggplot(
  distribution_df,
  aes(x = Return)
) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 60,
    fill = "grey80",
    colour = "black"
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(equity_heavy_returns),
      sd = sd(equity_heavy_returns)
    ),
    linewidth = 1
  ) +
  labs(
    title = "Distribution of Equity-Heavy Portfolio Daily Returns",
    subtitle = "Histogram compared with a fitted normal distribution",
    x = "Daily Return",
    y = "Density"
  ) +
  theme_minimal()

ggsave(
  filename = "figures/09_equity_heavy_return_distribution.png",
  plot = distribution_plot,
  width = 9,
  height = 6,
  dpi = 200
)

png(
  filename = "figures/10_equity_heavy_normal_qq_plot.png",
  width = 1200,
  height = 850,
  res = 160
)

qqnorm(
  equity_heavy_returns,
  main = "Normal Q-Q Plot of Equity-Heavy Portfolio Returns"
)

qqline(
  equity_heavy_returns
)

dev.off()