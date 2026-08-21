# Stop-entry bracket at the Bollinger extremes (one side per run)
# ---------------------------------------------------------------
# This example demonstrates:
#   * STOP entries: a standing order that triggers when price breaks a level
#     (here, the previous bar's lower band), refreshed every bar -- most
#     placed orders never fill;
#   * a symmetric bracket around the entry: $2,000 stop loss and $2,000
#     take profit, plus a 5-bar time limit;
#   * the engine's data model: ONE pending order per row. A two-sided
#     bracket (stop-sell below, stop-buy above) is therefore run one side
#     at a time. This file runs the SHORT side; the long side is the mirror
#     (EntryPrice = PrevUpperBB, positive OrderSize, bracket flipped).
#
# The dataset ships with the package. On the bundled data the short side
# produces 15 trades.

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
  price[, PrevLowerBB := shift(lowerBB, n = 1, type = "lag")]

  # SHORT side: a stop-sell resting at the previous bar's lower band,
  # refreshed daily. It fills only if price breaks down through the level.
  price[, SellSignal := TRUE]
  price[SellSignal == TRUE, EntryPrice := PrevLowerBB]
  price[SellSignal == TRUE, EntryType := "Stop"]

  # Symmetric bracket: $2,000 either way, and a 5-bar time limit
  price[SellSignal == TRUE,
        StopLoss   := EntryPrice + (2000 / (trade_size_contracts * futures_multiplier))]
  price[SellSignal == TRUE,
        TakeProfit := EntryPrice - (2000 / (trade_size_contracts * futures_multiplier))]
  price[SellSignal == TRUE, CLOSE_CANDLES_SINCE_ENTRY := 5]

  # Position size (negative = short)
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

table(res$ExitReason)
cat("trades:", uniqueN(res$Order_ID),
    "| net P&L:", round(sum(res$Returns), 2), "\n")
