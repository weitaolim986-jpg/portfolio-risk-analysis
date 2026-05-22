calculate_annualized_return = function(r){
  total_growth= prod(1+r)
  num_of_years = length(r)/252
  total_growth^(1/num_of_years) - 1
}


calculate_annualized_volatility = function(r){
  sd(r)*sqrt(252)
}

calculate_max_drawdown = function(r){
  wealth = cumprod(c(1,1+r))
  running_peak = cummax(wealth)
  drawdown = wealth/running_peak- 1
  min(drawdown)
}

portfolio_summary = data.frame(
  Portfolio = colnames(portfolio_returns),
  Annualized_return = apply(portfolio_returns,2,calculate_annualized_return),
  Annualized_volatility = apply(portfolio_returns,2,calculate_annualized_volatility),
  Maximum_drawdown = apply(portfolio_returns,2,calculate_max_drawdown)
)

portfolio_summary[,-1] = round(portfolio_summary[,-1]*100,2)
portfolio_summary

rownames(portfolio_summary) = NULL

portfolio_value = 10000

calculate_historical_var = function(r,confidence = 0.95, value = 10000){
  loss_threshold = quantile(r,probs = 1 - confidence)
  -as.numeric(loss_threshold)*value
}

var_results = apply(portfolio_returns, 2,calculate_historical_var,confidence = 0.95, value= portfolio_value)

round(var_results,2)

calculate_expected_shortfall = function(r,confidence = 0.95,value = 10000){
  cutoff = quantile(r,probs = 1 - confidence)
  worst_returns = r[r <= cutoff]
  -mean(worst_returns) * value 
}

es_results = apply(portfolio_returns,2,calculate_expected_shortfall,confidence = 0.95, value = portfolio_value)

risk_table = data.frame(
  Portfolio = colnames(portfolio_returns),
  VaR_95_Dollars = round(var_results,2),
  Expected_Shortfall_95_Dollars = round(es_results,2)
)

risk_table 

rownames(risk_table) = NULL
