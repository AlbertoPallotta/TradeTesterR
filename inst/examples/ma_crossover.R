# Dual moving average crossover
# ------------------------------
# This example demonstrates:
#   * the classic trend-following system: enter long when the fast average
#     crosses above the slow average, exit on the opposite cross;
#   * explicit crossover detection from current and previous values --
#     every temporal relationship visible in the data;
#   * a percentage-based stop loss.
#
# The dataset ships with the package. On the bundled data this produces
# 30 trades.

rm(list = ls())
library(TradeTesterR)
library(quantmod)
library(data.table)
library(lubridate)
library(xts)
library(stringr)

# Parameters --------------------------------------------------------------
trade_size_contracts <- 4
futures_multiplier   <- 50
spread               <- 0.04

# Data ---------------------------------------------------------------------
df <- fread(system.file("extdata", "crude_wti_futures.csv", package = "TradeTesterR"))
setnames(df, c("date", "Open", "High", "Low", "Close", "Volume"))
price_small <- df[, .(date, Open, High, Low, Close)]
setkey(price_small, date)
price_xts <- to.period(xts(price_small[, .(Open, High, Low, Close)],
                           order.by = price_small$date),
                       period = "days", k = 1, indexAt = "startof",
                       name = NULL, drop.time = FALSE)
price <- data.table(date = index(price_xts), coredata(price_xts))
setkey(price, date)

# Strategy -----------------------------------------------------------------
my_strat <- function(price, fast_period = 20, slow_period = 50, stop_loss_pct = 2) {

  # Make temporal relationships explicit
  price[, NextOpenTime := shift(date, n = 1, type = "lead")]
  price[, NextOpen     := shift(Open, n = 1, type = "lead")]

  # Indicators with full visibility
  price[, FastMA := SMA(Close, n = fast_period)]
  price[, SlowMA := SMA(Close, n = slow_period)]

  # Crossover detection with explicit logic
  price[, PrevFastMA := shift(FastMA, n = 1, type = "lag")]
  price[, PrevSlowMA := shift(SlowMA, n = 1, type = "lag")]
  price[, BuySignal := FastMA > SlowMA & PrevFastMA <= PrevSlowMA]

  # Entry mechanics completely transparent
  price[BuySignal == TRUE, EntryPrice := NextOpen]
  price[BuySignal == TRUE, EntryType := "Market"]
  price[BuySignal == TRUE, StopLoss := NextOpen * (1 - stop_loss_pct / 100)]

  # Exit: the opposite cross closes the position
  price[FastMA < SlowMA & PrevFastMA >= PrevSlowMA, exit_long := 1]

  # Position size
  price[BuySignal == TRUE, OrderSize := futures_multiplier * trade_size_contracts]

  # Entry time: orders placed at a bar's close become active at the next open
  setorder(price, date)
  price[, EntryTime := shift(date, 1, type = "lead")]

  # Execution feed: the full minute stream with a synthetic bid
  ask <- xts(price_small[, .(Open, High, Low, Close)], order.by = df$date)
  bid <- ask - spread
  dat <- merge(mergeAskBid(ask, bid), price[], by = "date", all.x = TRUE)

  backtest(dat,
           exit_fun = exit_TP_SL_signal,
           AddToPosition = FALSE,
           CloseTradeOnOppositeSignal = FALSE,
           verbose = FALSE,
           TradeTimeLimit = lubridate::weeks(100000))
}

# Run and inspect ----------------------------------------------------------
bt  <- my_strat(price)
res <- bt$results

table(res$ExitReason)
cat("trades:", uniqueN(res$Order_ID),
    "| net P&L:", round(sum(res$Returns), 2), "\n")
