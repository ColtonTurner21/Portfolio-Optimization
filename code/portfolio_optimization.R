# Portfolio Optimization Main Script

# Load required libraries
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

# Example: Download data
symbols <- c("AAPL", "MSFT", "GOOG")
getSymbols(symbols, src = "yahoo", from = "2020-01-01", to = Sys.Date())
prices <- do.call(merge, lapply(symbols, function(sym) Ad(get(sym))))
returns <- na.omit(Return.calculate(prices))

# Portfolio specification
port_spec <- portfolio.spec(assets = colnames(returns))
port_spec <- add.constraint(port_spec, type = "full_investment")
port_spec <- add.constraint(port_spec, type = "long_only")
port_spec <- add.objective(port_spec, type = "return", name = "mean")
port_spec <- add.objective(port_spec, type = "risk", name = "StdDev")

# Optimize portfolio
opt_port <- optimize.portfolio(returns, port_spec, optimize_method = "ROI")

# Results
print(opt_port)