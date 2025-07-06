rm(list=ls())
library(TradeTesterR)

# Load required libraries
library(quantmod)    # For technical indicators (BBands)
library(data.table)  # For data manipulation
library(lubridate)   # For date operations
library(tidyquant)   # For to.period function
library(plotly)      # For interactive plots
library(xts)         # For time series operations

# 1. BACKTEST PARAMETERS ----
instrument_type <- "Futures" # Futures, Stocks, Forex
is_trade_in_shares <- FALSE # TRUE to use trade_size_shares, FALSE to use trade_size_dollars (only for stocks)
trade_size_dollars <- 10000 # For forex/stocks
trade_size_shares <- 100 # For stocks, trade only a specific number of shares regardless of price
trade_size_contracts <- 4 # For futures
futures_multiplier <- 50 # For futures
spread <- 0.04 # Typical difference between bid/ask prices

# 2. LOAD AND PREPARE DATA ----
# Load example data (modify this section for your own data)
data_file <- system.file("extdata", "crude_wti_futures.csv", package = "TradeTesterR")
df <- fread(data_file)
setnames(df, c("date", "Open", "High", "Low", "Close", "Volume"))
price_small <- df[, .(date, Open, High, Low, Close)]
setkey(price_small, date)

# Convert to daily timeframe for strategy
price_xts <- xts(price_small[, .(Open, High, Low, Close)], order.by = price_small$date)
price_xts <- to.period(price_xts, period = "days", k = 1, indexAt = "startof", name = NULL, drop.time = FALSE)
price <- data.table(date = index(price_xts), coredata(price_xts))
setkey(price, date)

# 3. DEFINE STRATEGY FUNCTION ----
# Parameters in function signature can be optimized later
my_strat <- function(price, stop_loss_amount = 2000, boll_n = 20) {

  # Essential columns for backtesting
  price[, NextOpenTime := shift(date, n=1, type="lead")]
  price[, NextOpen := shift(Open, n=1, type="lead")]

  # Calculate indicators
  boll <- BBands(price[, .(High, Low, Close)], n = boll_n, sd = 2)
  price[, upperBB := boll[, "up"]]
  price[, lowerBB := boll[, "dn"]]
  price[, midBB := boll[, "mavg"]]
  price[, PrevClose := shift(Close, n=1, type="lag")]

  # Define entry signals (modify this for your strategy)
  price[, BuySignal := Close <= lowerBB & PrevClose > lowerBB]

  # Set entry parameters
  price[BuySignal == TRUE, EntryPrice := NextOpen]
  price[BuySignal == TRUE, EntryType := "Market"]
  price[BuySignal == TRUE, StopLoss := EntryPrice - (stop_loss_amount / (trade_size_contracts*futures_multiplier))]

  # Define exit signals (partial exits supported)
  price[, targetorder_long_1 := midBB]  # First target at middle band
  price[, targetorder_long_1_amount := .5]  # Close 50% of position
  price[, targetorder_long_2 := upperBB]  # Second target at upper band
  price[, targetorder_long_2_amount := 1]  # Close remaining position
  price[, CLOSE_CANDLES_SINCE_ENTRY := 10]  # Time-based exit after 10 candles

  # Calculate order size based on instrument type
  if (tolower(instrument_type) == "futures") {
    if ("BuySignal" %in% names(price)) {
      price[BuySignal == TRUE, OrderSize := futures_multiplier * trade_size_contracts]
    }
    if ("SellSignal" %in% names(price)) {
      price[SellSignal == TRUE, OrderSize := -1 * futures_multiplier * trade_size_contracts]
    }
  } else {
    if (isTRUE(is_trade_in_shares) & tolower(instrument_type) == "stocks") {
      if ("BuySignal" %in% names(price)) {
        price[BuySignal == TRUE, OrderSize := trade_size_shares]
      }
      if ("SellSignal" %in% names(price)) {
        price[SellSignal == TRUE, OrderSize := -trade_size_shares]
      }
    } else {
      if ("BuySignal" %in% names(price)) {
        price[BuySignal == TRUE, OrderSize := getOrderSize(EntryPrice, trade_size_dollars, instrument_type)]
      }
      if ("SellSignal" %in% names(price)) {
        price[SellSignal == TRUE, OrderSize := -getOrderSize(EntryPrice, trade_size_dollars, instrument_type)]
      }
    }
  }

  # Set entry time (next candle's open)
  setorder(price, date)
  price[, EntryTime := shift(date, 1, type = "lead")]

  # Prepare data for backtesting
  ask <- xts(price_small[, .(Open, High, Low, Close)], order.by = df$date)
  bid <- ask - spread
  lower_timeframe <- mergeAskBid(ask, bid)
  dat <- merge(lower_timeframe, price[], by = "date", all.x = TRUE)

  # Run backtest
  bt <- backtest(dat,
                 exit_fun=exit_TP_SL_signal,
                 AddToPosition = FALSE,
                 CloseTradeOnOppositeSignal = FALSE,
                 verbose=FALSE,
                 TradeTimeLimit = lubridate::weeks(100000))
  return(bt)
}

# 4. RUN BASE STRATEGY ----
print("========== BASE STRATEGY RESULTS ==========")
bt <- my_strat(price)
res <- bt$results

# Display results summary
print("Exit reasons:")
print(table(res$ExitReason))
print(paste("Base strategy return:", round(sum(res$Returns, na.rm = TRUE), 4)))

# Plot cumulative returns
pdat <- bt$results[!is.na(Returns)]
setorder(pdat, date)
plot_ly(pdat) %>%
  add_trace(x=~date, y = ~cumsum(Returns),
            text=~paste('Order ID:', Order_ID), type='scatter',
            mode='lines+markers', color=I('black')) %>%
  layout(title = "Cumulative Returns - Base Strategy",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Cumulative Returns"))

# Visualize a sample trade
if (nrow(pdat) > 0) {
  trade_num <- 1
  trade <- pdat[trade_num, ]
  candle_dat <- price[date >= (trade$EntryTime - days(5)) & date <= (trade$ExitTime + days(5))]

  plot_ly(candle_dat) |>
    add_trace(x=~date, type='candlestick',
              open=~Open, close=~Close, high=~High, low=~Low) |>
    add_trace(x=~date, y=~upperBB, type='scatter', mode='lines',
              line=list(color='red', dash='dot'), name='Upper BB') |>
    add_trace(x=~date, y=~lowerBB, type='scatter', mode='lines',
              line=list(color='red', dash='dot'), name='Lower BB') |>
    add_trace(x=~date, y=~midBB, type='scatter', mode='lines',
              line=list(color='orange', dash='dot'), name='Mid BB') |>
    add_trace(x=~EntryTime, y = ~EntryPrice, name = "Entry", data = trade,
              mode='markers', marker=list(size=10, color='green')) |>
    add_trace(x=~ExitTime, y = ~ExitPrice, name = "Exit", data = trade,
              mode='markers', marker=list(size=10, color='red')) |>
    layout(xaxis=list(title='Date', rangeslider=list(visible=FALSE)),
           yaxis=list(title='Price'),
           title=paste('Trade Example:', trade$ExitReason, '- Return:', round(trade$Returns, 4)),
           showlegend=TRUE)
}

# 5. SINGLE PARAMETER OPTIMIZATION ----
print("\n========== SINGLE PARAMETER OPTIMIZATION ==========")
initial_equity <- 100000

# Define parameter grid
opt_params_single <- data.table(
  stop_loss_amount = seq(500, 3000, by = 500)
)
print("Testing stop loss values:")
print(opt_params_single)

# Set optimization parameters
optimize_on <- "net_profit"
train_set_weeks <- 52  # Last 52 weeks for training
test_set_weeks <- 26   # Last 26 weeks for testing

# Run optimization
bt_test_single <- optimizeStrategy(price, opt_params_single, initial_equity,
                                   optimize_on, train_set_weeks, test_set_weeks)

print(paste("Single parameter optimization complete.",
            "Tested", nrow(opt_params_single), "parameter combinations."))
# Results are stored in bt_test_single$test_results for further analysis

# 6. MULTI-PARAMETER OPTIMIZATION ----
print("\n========== MULTI-PARAMETER OPTIMIZATION ==========")

# Define parameter grid
opt_params_multi <- expand.grid(
  boll_n = seq(20, 100, by = 20),
  stop_loss_amount = seq(1000, 3000, by = 1000)
)
print(paste("Testing", nrow(opt_params_multi), "parameter combinations"))
print(head(opt_params_multi, 10))

# Run optimization with detailed results
bt_test_multi <- optimizeStrategy(price, opt_params_multi, initial_equity,
                                  optimize_on, train_set_weeks, test_set_weeks,
                                  return_optimization_details = TRUE)

print(paste("Multi-parameter optimization complete.",
            "Tested", nrow(opt_params_multi), "parameter combinations."))
# Results in bt_test_multi$test_results, optimization history in bt_test_multi$optimization_history

# 7. VISUALIZE OPTIMIZATION RESULTS ----
print("\n========== OPTIMIZATION VISUALIZATION ==========")

# Get first training window results
first_window <- bt_test_multi$optimization_history[[1]]

# Create 3D surface and heatmap plots
plots <- plotMultiParameter(
  first_window,
  first_window$net_profit,
  "boll_n",
  "stop_loss_amount",
  "Net Profit"
)

# Display plots
print("Displaying 3D surface plot...")
plots$surface  # 3D surface plot
print("Displaying 2D heatmap...")
plots$heatmap  # 2D heatmap

# 8. IN-SAMPLE/OUT-OF-SAMPLE COMPARISON ----
print("\n========== IS/OS PERFORMANCE ANALYSIS ==========")

# Compare in-sample vs out-of-sample performance
isos_comparison <- compareISOS(bt_test_multi)

# Display results
print("Summary Statistics:")
print(isos_comparison$summary_stats)
print("\nPerformance Comparison:")
print(isos_comparison$performance_comparison)

# Display diagnostic plots
print("\nDisplaying IS/OS diagnostic plots...")
isos_comparison$plots$scatter
isos_comparison$plots$time_series
isos_comparison$plots$degradation
