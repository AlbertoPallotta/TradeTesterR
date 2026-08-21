# Market-regime position scaling with the VIX
# --------------------------------------------
# This example demonstrates:
#   * classifying the market into volatility regimes from the VIX
#     (below 15 / 15-25 / above 25);
#   * scaling position size by regime: x1.5 in calm markets, x1.0 in
#     normal ones, x0.3 under stress;
#   * merging an external daily series onto the strategy timeframe;
#   * dollar-coherent stops: smaller size, wider stop, identical risk.
#
# The price dataset ships with the package. The VIX is downloaded once from
# Yahoo Finance, so this example needs an internet connection.
# On the bundled data this produces 85 trades.

rm(list = ls())
library(TradeTesterR)
library(quantmod)
library(tidyquant)         # used internally by load_data_from_yahoo
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

# VIX from Yahoo. The loader's default start date is 2020-01-01: pass the
# range explicitly so it covers the whole price history.
vix <- load_data_from_yahoo("^VIX", from = "2015-12-01")
vix_daily <- vix[, .(d = as.IDate(open_time), VIX = Close)]

# Merge the daily VIX close onto the strategy timeframe
price[, d := as.IDate(date)]
price[vix_daily, VIX := i.VIX, on = "d"]
price[, VIX := nafill(VIX, type = "locf")]   # carry over market holidays
price[, d := NULL]

# Strategy -----------------------------------------------------------------
my_strat <- function(price, RSI_threshold = 30) {

  price[, NextOpenTime := shift(date, n = 1, type = "lead")]
  price[, NextOpen     := shift(Open, n = 1, type = "lead")]

  boll <- BBands(price[, .(High, Low, Close)], n = 20, sd = 2)
  price[, upperBB := boll[, "up"]]
  price[, lowerBB := boll[, "dn"]]
  price[, midBB   := boll[, "mavg"]]
  price[, RSI := RSI(price[, Close], n = 14, maType = "SMA")]
  price[, PrevClose := shift(Close, n = 1, type = "lag")]

  # Entries: Bollinger touch with RSI confirmation, both sides.
  # Each signal uses its own bar's VIX close; entry occurs at the next open.
  price[, BuySignal := Close <= lowerBB & PrevClose > lowerBB & RSI <= RSI_threshold]
  price[, SellSignal := Close >= upperBB & PrevClose < upperBB & RSI >= (100 - RSI_threshold)]
  price[BuySignal == TRUE,  `:=`(EntryPrice = NextOpen, EntryType = "Market")]
  price[SellSignal == TRUE, `:=`(EntryPrice = NextOpen, EntryType = "Market")]

  # STAGE 1: base position size
  price[BuySignal == TRUE,  OrderSize :=  futures_multiplier * trade_size_contracts]
  price[SellSignal == TRUE, OrderSize := -1 * futures_multiplier * trade_size_contracts]

  # STAGE 2: regime-based scaling
  price[, MarketRegime := cut(VIX,
                              breaks = c(0, 15, 25, 100),
                              labels = c("Low Vol", "Normal", "High Vol"))]
  price[BuySignal == TRUE  & MarketRegime == "Low Vol",  OrderSize := OrderSize * 1.5]
  price[SellSignal == TRUE & MarketRegime == "Low Vol",  OrderSize := OrderSize * 1.5]
  price[BuySignal == TRUE  & MarketRegime == "High Vol", OrderSize := OrderSize * 0.3]
  price[SellSignal == TRUE & MarketRegime == "High Vol", OrderSize := OrderSize * 0.3]
  # "Normal" keeps the x1.0 base

  # Exits: mid-band target, a dollar-coherent stop (distance = $2,000/size),
  # 10-bar limit
  price[Close >= midBB, exit_long  := 1]
  price[Close <= midBB, exit_short := 1]
  price[BuySignal == TRUE,  StopLoss := EntryPrice - 2000 / abs(OrderSize)]
  price[SellSignal == TRUE, StopLoss := EntryPrice + 2000 / abs(OrderSize)]
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

# Average size by regime, the three size levels actually traded, and how
# the sample splits across regimes
price[BuySignal == TRUE | SellSignal == TRUE,
      .(AvgSize = mean(abs(OrderSize)), Signals = .N),
      by = MarketRegime][order(MarketRegime)]
res[, table(abs(OrderSize))]
price[!is.na(MarketRegime), round(100 * prop.table(table(MarketRegime)), 1)]
