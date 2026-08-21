# Multiple exit conditions declared at once
# ------------------------------------------
# This example demonstrates:
#   * four exit mechanisms armed simultaneously on one position: a fixed
#     stop loss, a fixed take profit, a time limit, and an indicator signal;
#   * the engine's precedence when several could apply: stop/target first,
#     then signals, then time;
#   * a practical caution: fixed-dollar levels must be sanity-checked
#     against the instrument's price.
#
# The dataset ships with the package. On the bundled data this produces
# 11 trades.

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
my_strat <- function(price, boll_n = 20) {

  price[, NextOpenTime := shift(date, n = 1, type = "lead")]
  price[, NextOpen     := shift(Open, n = 1, type = "lead")]

  boll <- BBands(price[, .(High, Low, Close)], n = boll_n, sd = 2)
  price[, upperBB := boll[, "up"]]
  price[, lowerBB := boll[, "dn"]]
  price[, midBB   := boll[, "mavg"]]
  price[, PrevClose := shift(Close, n = 1, type = "lag")]

  # Entry: a continuous limit order resting at the lower band
  price[, BuySignal := TRUE]
  price[BuySignal == TRUE, EntryPrice := lowerBB]
  price[BuySignal == TRUE, EntryType := "Limit"]

  # Four exit conditions, all armed at once ------------------------------
  # 1. Fixed stop loss: $4,000 = 20 points here; on low-priced bars the
  #    level can go negative -- sanity-check dollar stops against the price
  price[BuySignal == TRUE,
        StopLoss := EntryPrice - (4000 / (trade_size_contracts * futures_multiplier))]
  # 2. Fixed take profit: $4,000 target
  price[BuySignal == TRUE,
        TakeProfit := EntryPrice + (4000 / (trade_size_contracts * futures_multiplier))]
  # 3. Time limit: 10 bars maximum
  price[BuySignal == TRUE, CLOSE_CANDLES_SINCE_ENTRY := 10]
  # 4. Indicator signal: close above the upper band
  price[Close > upperBB, exit_long := 1]

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
