# Two-stage position sizing: base size, then volatility scaling
# --------------------------------------------------------------
# This example demonstrates:
#   * position sizing as two separate stages: a base allocation, then a
#     tactical adjustment from market conditions;
#   * halving size when current ATR exceeds a multiple of its weekly average;
#   * tagging each signal with its sizing decision for later analysis.
#
# The dataset ships with the package. On the bundled data this produces
# 85 trades.

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
# vol_threshold = 1.2 scales rarely on this data; try 1.05 to see frequent
# reductions in the executed trades.
my_strat <- function(price, RSI_threshold = 30, vol_threshold = 1.2) {

  price[, NextOpenTime := shift(date, n = 1, type = "lead")]
  price[, NextOpen     := shift(Open, n = 1, type = "lead")]

  boll <- BBands(price[, .(High, Low, Close)], n = 20, sd = 2)
  price[, upperBB := boll[, "up"]]
  price[, lowerBB := boll[, "dn"]]
  price[, midBB   := boll[, "mavg"]]
  price[, RSI := RSI(price[, Close], n = 14, maType = "SMA")]

  # ATR now, and its five-day average as the "normal" volatility reference
  atr <- ATR(price[, .(High, Low, Close)], n = 14)
  price[, ATR := atr[, "atr"]]
  price[, ATR_Weekly := frollmean(ATR, n = 5, align = "right")]
  price[, PrevClose := shift(Close, n = 1, type = "lag")]

  # Entries: Bollinger touch with RSI confirmation, both sides
  price[, BuySignal := Close <= lowerBB & PrevClose > lowerBB & RSI <= RSI_threshold]
  price[, SellSignal := Close >= upperBB & PrevClose < upperBB & RSI >= (100 - RSI_threshold)]
  price[BuySignal == TRUE,  `:=`(EntryPrice = NextOpen, EntryType = "Market")]
  price[SellSignal == TRUE, `:=`(EntryPrice = NextOpen, EntryType = "Market")]

  # STAGE 1: base position size
  price[BuySignal == TRUE,  OrderSize :=  futures_multiplier * trade_size_contracts]
  price[SellSignal == TRUE, OrderSize := -1 * futures_multiplier * trade_size_contracts]

  # STAGE 2: volatility-based scaling -- halve the position when current ATR
  # exceeds vol_threshold times its weekly average
  price[BuySignal == TRUE  & ATR > (vol_threshold * ATR_Weekly), OrderSize := OrderSize * 0.5]
  price[SellSignal == TRUE & ATR > (vol_threshold * ATR_Weekly), OrderSize := OrderSize * 0.5]

  # Tag each long signal with its sizing decision
  price[BuySignal == TRUE,
        VolAdjusted := ifelse(ATR > (vol_threshold * ATR_Weekly),
                              "Reduced Size", "Full Size")]

  # Exits: mid-band target, monetary stop, 10-bar limit
  price[Close >= midBB, exit_long  := 1]
  price[Close <= midBB, exit_short := 1]
  price[BuySignal == TRUE,
        StopLoss := EntryPrice - (2000 / (trade_size_contracts * futures_multiplier))]
  price[SellSignal == TRUE,
        StopLoss := EntryPrice + (2000 / (trade_size_contracts * futures_multiplier))]
  price[, CLOSE_CANDLES_SINCE_ENTRY := 10]

  # Entry time and execution feed
  setorder(price, date)
  price[, EntryTime := shift(date, 1, type = "lead")]
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

# The sizing decisions across signals, and the sizes actually executed
price[BuySignal == TRUE | SellSignal == TRUE, table(VolAdjusted, useNA = "ifany")]
res[, table(abs(OrderSize))]
