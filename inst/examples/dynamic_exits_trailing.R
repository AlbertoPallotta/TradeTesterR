# Dynamic exits: a trailing stop and a moving profit target
# ---------------------------------------------------------
# This example demonstrates:
#   * standing exit orders that UPDATE every bar: a profit target that rides
#     the upper Bollinger Band and a protective stop that trails below the
#     market (previous low minus 0.8 ATR);
#   * how dynamic orders coexist with a fixed catastrophic stop loss;
#   * the exit labels these mechanisms produce in the results.
#
# The dataset ships with the package. On the bundled data this produces
# 14 trades.

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
my_strat <- function(price, boll_n = 20, atr_n = 14) {

  price[, NextOpenTime := shift(date, n = 1, type = "lead")]
  price[, NextOpen     := shift(Open, n = 1, type = "lead")]

  # Wider bands: entries only at pronounced extremes
  boll <- BBands(price[, .(High, Low, Close)], n = boll_n, sd = 2.5)
  price[, upperBB := boll[, "up"]]
  price[, lowerBB := boll[, "dn"]]
  price[, midBB   := boll[, "mavg"]]

  atr <- ATR(price[, .(High, Low, Close)], n = atr_n)
  price[, ATR := atr[, "atr"]]
  price[, PrevLow := shift(Low, n = 1, type = "lag")]

  # Entry: a continuous limit order resting at the lower band
  price[, BuySignal := TRUE]
  price[BuySignal == TRUE, EntryPrice := lowerBB]
  price[BuySignal == TRUE, EntryType := "Limit"]

  # Fixed catastrophic stop: $2,000 on the full position
  price[BuySignal == TRUE,
        StopLoss := EntryPrice - (2000 / (trade_size_contracts * futures_multiplier))]

  # DYNAMIC TARGET: the upper band, re-evaluated every bar
  price[, targetorder_long_1 := upperBB]
  price[, targetorder_long_1_amount := 1]     # close the full position

  # DYNAMIC STOP: previous low minus 0.8 ATR, trailing upward
  price[, stoporder_long_1 := PrevLow - 0.8 * ATR]
  price[, stoporder_long_1_amount := 1]       # close the full position

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

table(res$ExitReason)   # each exit mechanism signs its work here
cat("trades:", uniqueN(res$Order_ID),
    "| net P&L:", round(sum(res$Returns), 2), "\n")
