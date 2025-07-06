rm(list=ls())
library(TradeTesterR)
library(quantmod)
#library(ermesPkg)
library(data.table)
library(lubridate)
library(tidyquant)
library(plotly)
library(xts)
library(caret)
library(stringr)

# 1. Parameters for backtest ----
instrument_type <- "Futures" # Futures, Stocks, Forex
is_trade_in_shares <- FALSE # TRUE to use trade_size_shares, FALSE to use trade_size_dollars (only for stocks)
trade_size_dollars <- 10000 # For forex/stocks
trade_size_shares <- 100 # For stocks, trade only a specific number of shares regardless of price
trade_size_contracts <- 4 # For futures
futures_multiplier <- 50 # For futures

spread <- 0.04 # Typical difference between bid/ask prices


# 1. Load Data -----
# CHANGE CODE HERE -------------------------------
data_file <- system.file("extdata", "crude_wti_futures.csv", package = "TradeTesterR")
df <- fread(data_file)
setnames(df, c("date", "Open", "High", "Low", "Close", "Volume"))
price_small <- df[, .(date, Open, High, Low, Close)]
setkey(price_small, date)
# Convert smaller timeframe to larger (4-hour in this case)
price_xts <- xts(price_small[, .(Open, High, Low, Close)], order.by = price_small$date)
price_xts <- to.period(price_xts, period = "days", k = 1, indexAt = "startof", name = NULL, drop.time = FALSE)
price <- data.table(date = index(price_xts), coredata(price_xts))
setkey(price, date)
#-------------------------------------------------

# If there are values you'd like to optimize, set them within the function(), e.g. take_profit = 100
my_strat <- function(price, stop_loss_amount = 2000, boll_n = 20) {

  # Always need these
  price[, NextOpenTime := shift(date, n=1, type="lead")]
  price[, NextOpen := shift(Open, n=1, type="lead")]

  # 2. Add Indicators -----
  # CHANGE CODE HERE -------------------------------
  boll <- BBands(price[, .(High, Low, Close)], n = boll_n, sd = 2)
  price[, upperBB := boll[, "up"]]
  price[, lowerBB := boll[, "dn"]]
  price[, midBB := boll[, "mavg"]]
  price[, PrevClose := shift(Close, n=1, type="lag")]
  #-------------------------------------------------

  # 3. Add Trading rules ----

  # Trigger buy/sell signals

  # CHANGE CODE HERE -------------------------------
  price[, BuySignal := Close <= lowerBB & PrevClose > lowerBB]

  # # Buy parameters
  price[BuySignal == TRUE, EntryPrice := NextOpen]
  price[BuySignal == TRUE, EntryType := "Market"]
  price[BuySignal == TRUE, StopLoss := EntryPrice - (stop_loss_amount / (trade_size_contracts*futures_multiplier))]



  # Exit Signals
  # These columns must be named exit_long and exit_short
  # They take a value from 0-1 that closes a portion of the trade
  # 1 = close full position, .5 = close half position, etc.

  price[, targetorder_long_1 := midBB]
  price[, targetorder_long_1_amount := .5]
  price[, targetorder_long_2 := upperBB]
  price[, targetorder_long_2_amount := 1]
  price[, CLOSE_CANDLES_SINCE_ENTRY := 10]
  # ------------------------------------------------

  # Order size
  # IF UNIQUE ORDER SIZING RULES NEEDED, comment this code out and write your own ---------
  # Example:

  #add to backtest function
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
  #----------------------------------------------------------------------------------------

  #  (Custom order sizing)
  # Change code here ----
  # price[BuySignal == TRUE & ATR > lastWeekATR, OrderSize := OrderSize / 3]
  # price[BuySignal == TRUE & ATR > lastWeekATR, OrderSize := OrderSize / 3]
  #
  # price[SellSignal == TRUE & ATR > 1.5, OrderSize := OrderSize * 2]
  #--------------------


  # Entry time

  # The time at candle close is the next candle's open
  setorder(price, date)
  price[, EntryTime := shift(date, 1, type = "lead")]


  # Load data to backtest on (usually a much smaller timeframe)
  ask <- xts(price_small[, .(Open, High, Low, Close)], order.by = df$date)
  bid <- ask - spread

  # Create a data.table from the ask/bid prices
  lower_timeframe <- mergeAskBid(ask, bid)

  dat <- merge(lower_timeframe, price[], by = "date", all.x = TRUE)

  # Since we are using only TP and SL for our exits, we can you a standard function
  # Found in the backtest_function.R file called: exit_TP_SL_signal
  # If you want to have additional exit conditions, you can modify this function

  # Backtest
  bt <- backtest(dat,
                 exit_fun=exit_TP_SL_signal,
                 AddToPosition = FALSE,
                 CloseTradeOnOppositeSignal = FALSE,
                 verbose=FALSE,
                 TradeTimeLimit = lubridate::weeks(100000))
  return(bt)
}


bt <- my_strat(price)
res <- bt$results
table(res$ExitReason)
#View(res)
pdat <- bt$results[!is.na(Returns)]
setorder(pdat, date)
plot_ly(pdat) %>%
  add_trace(x=~date, y = ~cumsum(Returns), #y=~cumsum(ProfitLoss),
            text=~paste('Order ID:', Order_ID), type='scatter',
            mode='lines+markers', color=I('black'))
# Look at an individual trade
trade_num <- 1
trade <- pdat[trade_num, ]
candle_dat <- price[date >= (trade$EntryTime - days(5)) & date <= (trade$ExitTime + days(55))]
plot_ly(candle_dat) |>
  # add candles
  add_trace(x=~date, type='candlestick', data = candle_dat,
            open=~Open, close=~Close, high=~High, low=~Low) |>
  # add indicators
  add_trace(x=~date, y=~upperBB, type='scatter', mode='lines',
            line=list(color='red'), name='Upper BB') |>
  add_trace(x=~date, y=~lowerBB, type='scatter', mode='lines',
            line=list(color='red'), name='Lower BB') |>
  add_trace(x=~date, y=~midBB, type='scatter', mode='lines',
            line=list(color='red'), name='Mid BB') |>
  # add entry/exit points
  add_trace(x=~EntryTime, y = ~EntryPrice, name = "Entry", data = trade,
            mode='lines+markers', color=I('black')) |>
  add_trace(x=~ExitTime, y = ~ExitPrice, name = "Exit", data = trade,
            mode='lines+markers', color=I('blue')) |>
  layout(xaxis=list(title='Date', rangeslider=list(visible=FALSE)),
         yaxis=list(title='Price'),
         title=paste('Trade #', trade_num, '-', trade$ExitReason, '(', round(trade$Returns, 1), ')'),
         showlegend=TRUE)
trade_num <- trade_num + 1

print(paste("Base strategy return:", sum(res$Returns, na.rm = TRUE)))


# 5. Single Parameter Optimization (Stop Loss Only)
initial_equity <- 100000



opt_params_single <- data.table(
  stop_loss_amount = seq(100, 600, by = 100)
)
print("Single parameter optimization grid:")
print(opt_params_single)

# Run optimization
optimize_on <- "net_profit"
train_set_weeks <- 52
test_set_weeks <- 26
# Add this to your boilerplate after creating test_set

bt_test <- optimizeStrategy(price, opt_params_single, initial_equity,
                            optimize_on, train_set_weeks, test_set_weeks)


# 6. Analyze single parameter results
results_single <- rbindlist(lapply(bt_test$test_results, function(x) x$results))
pdat <- results_single[!is.na(Returns)]

print("Single parameter optimization grid:")
print(opt_params_single)




# Run Multi-Parameter Optimization optimization
optimize_on <- "net_profit"
train_set_weeks <- 52
test_set_weeks <- 26

# 7. Multi-Parameter Optimization parameters
opt_params_multi <- expand.grid(
  boll_n = seq(20, 100, by = 10),
  stop_loss_amount = seq(1000, 4000, by = 1000)
)
print("Multi-parameter optimization grid:")
print(paste("Total parameter combinations:", nrow(opt_params_multi)))
print(head(opt_params_multi, 10))



bt_test <- optimizeStrategy(price, opt_params_multi, initial_equity,
                            optimize_on, train_set_weeks, test_set_weeks,
                            return_optimization_details = TRUE)



first_window <- bt_test$optimization_history[[1]]
# 2. Create 3D surface plot
plots <- plotMultiParameter(
  first_window,
  first_window$net_profit,
  "boll_n",
  "stop_loss_amount",
  "Net Profit"
)

# 3. Display the plots
plots$surface  # 3D surface plot
plots$heatmap  # 2D heatmap

# Run the IS/OS comparison
isos_comparison <- compareISOS(bt_test)

# View results
print(isos_comparison$summary_stats)
print(isos_comparison$performance_comparison)

# View plots
isos_comparison$plots$scatter
isos_comparison$plots$time_series
isos_comparison$plots$degradation
