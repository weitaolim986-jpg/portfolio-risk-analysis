install.packages("rugarch")
library(rugarch)

portfolio_name = "Equity_heavy"

equity_returns_pct <- portfolio_returns_xts[, portfolio_name] * 100

head(equity_returns_pct)
summary(equity_returns_pct)

garch_spec <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "norm"
)

garch_fit <- ugarchfit(
  spec = garch_spec,
  data = equity_returns_pct,
  solver = "hybrid"
)

show(garch_fit)

garch_parameters <- data.frame(
  Parameter = names(coef(garch_fit)),
  Estimate = as.numeric(coef(garch_fit))
)

print(garch_parameters)

write.csv(
  garch_parameters,
  "data/garch_equity_heavy_parameters.csv",
  row.names = FALSE
)

conditional_volatility_pct <- xts(
  as.numeric(sigma(garch_fit)),
  order.by = index(equity_returns_pct)
)

colnames(conditional_volatility_pct) <- "Conditional_Volatility_Percent"

volatility_df <- data.frame(
  Date = as.Date(index(conditional_volatility_pct)),
  Conditional_Volatility_Percent = as.numeric(conditional_volatility_pct)
)

head(volatility_df)

write.csv(
  volatility_df,
  "data/garch_equity_heavy_conditional_volatility.csv",
  row.names = FALSE
)

volatility_plot <- ggplot(
  volatility_df,
  aes(
    x = Date,
    y = Conditional_Volatility_Percent
  )
) +
  geom_line() +
  labs(
    title = "Normal GARCH Estimated Daily Volatility of the Equity-Heavy Portfolio",
    subtitle = "Conditional volatility from a Normal GARCH(1,1) model",
    x = "Date",
    y = "Estimated Daily Volatility (%)"
  ) +
  theme_minimal()

print(volatility_plot)

ggsave(
  filename = "figures/12_garch_equity_heavy_conditional_volatility.png",
  plot = volatility_plot,
  width = 10,
  height = 6,
  dpi = 200
)