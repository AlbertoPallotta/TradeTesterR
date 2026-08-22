# Mean reversion, long and short, with partial exits
# ---------------------------------------------------
# This example demonstrates:
#   * one strategy trading both directions: long after a close under the
#     lower Bollinger Band, short after a close over the upper band;
#   * PARTIAL exits on the long side: 50% taken at the middle band, the
#     remainder targeted at the upper band, under a 10-bar limit and a
#     $2,000 stop;
#   * an asymmetric design: the short side exits in full at the middle band.
#
# Requires TradeTesterR 0.1.1 or later (partial-exit accounting fix).
# The dataset ships with the package. On the bundled data this produces
# 146 trades.

rm(list = ls())
devtools::load_all()
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
my_strat <- function(price, boll_n = 20, partial_exit_amount = 0.5) {

  price[, NextOpenTime := shift(date, n = 1, type = "lead")]
  price[, NextOpen     := shift(Open, n = 1, type = "lead")]

  boll <- BBands(price[, .(High, Low, Close)], n = boll_n, sd = 2)
  price[, upperBB := boll[, "up"]]
  price[, lowerBB := boll[, "dn"]]
  price[, midBB   := boll[, "mavg"]]
  price[, PrevClose := shift(Close, n = 1, type = "lag")]

  # Entries: a close back through either band, executed at the next open
  price[, BuySignal := Close <= lowerBB & PrevClose > lowerBB]
  price[, SellSignal := Close >= upperBB & PrevClose < upperBB]
  price[BuySignal == TRUE,  `:=`(EntryPrice = NextOpen, EntryType = "Market")]
  price[SellSignal == TRUE, `:=`(EntryPrice = NextOpen, EntryType = "Market")]

  # Long exits: 50% at the middle band, remainder at the upper band
  price[, targetorder_long_1 := midBB]
  price[, targetorder_long_1_amount := partial_exit_amount]
  price[, targetorder_long_2 := upperBB]
  price[, targetorder_long_2_amount := 1]
  price[BuySignal == TRUE,
        StopLoss := EntryPrice - (2000 / (trade_size_contracts * futures_multiplier))]

  # Short exits: the full position at the middle band
  price[, targetorder_short_1 := midBB]
  price[, targetorder_short_1_amount := 1]
  price[SellSignal == TRUE,
        StopLoss := EntryPrice + (2000 / (trade_size_contracts * futures_multiplier))]

  # Time limit for both sides
  price[, CLOSE_CANDLES_SINCE_ENTRY := 10]

  # Position size
  price[BuySignal == TRUE,  OrderSize :=  futures_multiplier * trade_size_contracts]
  price[SellSignal == TRUE, OrderSize := -1 * futures_multiplier * trade_size_contracts]

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

table(res$ExitReason)   # partial legs appear as "...: 50%" rows
cat("trades:", uniqueN(res$Order_ID), "| rows:", nrow(res),
    "| net P&L:", round(sum(res$Returns), 2), "\n")
