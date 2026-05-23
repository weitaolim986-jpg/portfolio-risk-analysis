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