# Install and load packages
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("PortfolioAnalytics")
install.packages("ROI")
install.packages("ROI.plugin.quadprog")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("knitr")
install.packages("lubridate")
install.packages("tidyr")
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ggplot2)
library(reshape2)
library(knitr)
library(lubridate)
library(tidyr)


# Retrieve current stock and crypto data
tickers <- c("VOO", "VTI", "SPY", "VXUS", "AAPL", "MSFT", "NVDA", "KO", "BTC-USD", 
             "VNQ", "AGG", "JEPI")
getSymbols(tickers, from = Sys.Date()-365)
prices <- na.omit(merge(Cl(VOO), Cl(VTI), Cl(SPY),Cl(VXUS), Cl(AAPL), Cl(MSFT), Cl(NVDA), 
                        Cl(KO), Cl(`BTC-USD`),  Cl(VNQ), Cl(AGG), Cl(JEPI)))
colnames(prices) <- c("VOO", "VTI", "SPY", "VXUS", "AAPL", "MSFT", "NVDA", "KO",
                      "BTC", "VNQ", "AGG", "JEPI")
returns <- na.omit(Return.calculate(prices))



# Asset Allocation
wts <- c(
  VOO = 0.16,
  VTI = 0.16,
  SPY = 0.16,
  VXUS = 0.15,
  AAPL = 0.06,
  MSFT = 0.06,
  NVDA = 0.06,
  KO = 0.06,
  BTC = 0.015,
  VNQ = 0.04,
  AGG = 0.06,
  JEPI = 0.015  
)

# Portfolio Return
portfolio_returns <- as.numeric(as.matrix(returns[, names(wts)]) %*% wts)
cum_portfolio <- cumprod(1 + portfolio_returns) 

# Categories
returns$Index <- returns$VOO * wts["VOO"] + returns$VTI * wts["VTI"] + returns$SPY * wts["SPY"] +returns$VXUS * wts["VXUS"]
returns$Stocks <- returns$AAPL * wts["AAPL"] + returns$MSFT * wts["MSFT"] + returns$NVDA * wts["NVDA"] + returns$KO * wts["KO"]
returns$Crypto <- returns$BTC * wts["BTC"]
returns$REIT <- returns$VNQ * wts["VNQ"]
returns$bond <- returns$AGG * wts["AGG"]
returns$covered_call_ETF <- returns$JEPI * wts["JEPI"]
cum_index <- cumprod(1 + returns$Index)
cum_stocks <- cumprod(1 + returns$Stocks)
cum_crypto <- cumprod(1 + returns$Crypto)
cum_REIT <- cumprod(1 + returns$REIT)
cum_bond <- cumprod(1 + returns$bond)
cum_covered_call_ETF <- cumprod(1 + returns$covered_call_ETF)

# Plots
cum_portfolio_df <- data.frame(
  Date = index(returns),
  Portfolio = cum_portfolio
)
ggplot(cum_portfolio_df, aes(x = Date, y = Portfolio)) +
  geom_line(color = "black", size = 1.3) +
  labs(
    title = "Portfolio Return",
    x = "Date", y = "Growth of $1"
  ) +
  theme_classic()

cat_df <- data.frame(
  Date = index(returns),
  Index = cum_index,
  Stocks = cum_stocks,
  Crypto = cum_crypto,
  REIT = cum_REIT,
  Bond = cum_bond,
  Covered_Call_ETF = cum_covered_call_ETF
)
cat_long <- melt(cat_df, id.vars = "Date", variable.name = "Category", 
                 value.name = "CumulativeReturn")
ggplot(cat_long, aes(x = Date, y = CumulativeReturn, color = Category)) +
  geom_line(size = 1.5) +
  labs(title = "Returns by Asset Category",
       x = "Date",
       y = "Growth of $1") +
  theme_classic() +
  scale_color_manual(values = c("Index" = "blue", "Stocks" = "green","Crypto" = "orange", 
                                "REIT" = "red", "Bond" = "yellow",
                                "Covered_Call_ETF" = "purple"))


# Optimize Returns
assets = c("VOO", "VTI", "SPY", "VXUS", "AAPL", "MSFT", "NVDA", "KO", "BTC", 
           "VNQ", "AGG", "JEPI")
port <-portfolio.spec(assets = assets)
min_box <- c(0.1, 0.1, 0.1, 0.1, 0, 0, 0, 0, 0, 0, 0.05, 0)
max_box <- c(0.3, 0.3, 0.3, 0.3, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.05)
port <- add.constraint(port, type = "box", min = min_box, max = max_box)
port <- add.objective(port, type = "return", name = "mean")
port <- add.objective(port, type = "risk", name = "StdDev")
opt <- optimize.portfolio(R = returns[, assets],portfolio = port, optimize_method = "ROI", trace = TRUE)
weights_opt <- extractWeights(opt)
print(weights_opt)

# Plot optimization
opt_weights_df <- data.frame(
  Asset = names(weights_opt),
  Weight = as.numeric(weights_opt)
)

ggplot(opt_weights_df, aes(x = reorder(Asset, Weight), y = Weight, fill = Asset)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip () +
  labs(
    title = "Optimized Portfolio Allocation", 
    x = "Asset",
    y = "weight"
  ) +
  theme_classic()

# Performance Metrics
og_returns <- as.numeric(as.matrix(returns[, names(wts)]) %*% wts)
opt_returns <- as.numeric(as.matrix(returns[, names(weights_opt)]) %*% weights_opt)
dates <- index(returns)
og_returns_xts <- xts::xts(og_returns, order.by = dates)
opt_returns_xts <- xts::xts(opt_returns, order.by = dates)

og_ann_return <- Return.annualized(og_returns_xts, scale = 252) * 100
og_ann_vol <- sd(og_returns_xts, na.rm = TRUE) * sqrt(252) * 100
og_sharpe <- SharpeRatio.annualized(og_returns_xts, Rf = 0, scale = 252)
og_mdd <- maxDrawdown(og_returns_xts)

opt_ann_return <- Return.annualized(opt_returns_xts, scale = 252) * 100
opt_ann_vol <- sd(opt_returns_xts, na.rm = TRUE) * sqrt(252) * 100
opt_sharpe <-SharpeRatio.annualized(opt_returns_xts, Rf = 0, scale = 252)
opt_mdd <-maxDrawdown(opt_returns_xts)

metrics <-data.frame(
  Metric = c("Annualized Return (%)", "Annualized Volatility (%)", "Sharpe Ratio",
             "Max Drawdown"),
  Original = c(round(og_ann_return, 2), round(og_ann_vol, 2), round(og_sharpe, 2),
               round(og_mdd, 2)),
  Optimized = c(round(opt_ann_return, 2), round(opt_ann_vol, 2), round(opt_sharpe, 2),
                round(opt_mdd, 2))
)
print(metrics)
metrics_long <- metrics %>%
  pivot_longer(cols = c("Original", "Optimized"), names_to = "Portfolio", 
               values_to = "Value")
ggplot(metrics_long, aes(x = Metric, y = as.numeric(Value), fill = Portfolio)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(as.numeric(Value), 2),
            position = position_dodge(width = 0.9),
            vjust = -0.25, size = 3)) +
  labs(title = "Portfolio Metrics Comparison", y = "Value") +
  theme_classic()