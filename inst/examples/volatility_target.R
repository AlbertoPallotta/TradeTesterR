# Volatility-target position sizing
# ----------------------------------
# This example demonstrates:
#   * sizing each position so its expected daily dollar volatility matches a
#     budget: Position Size = Risk Budget / Asset Risk;
#   * a 15% annualized volatility target, converted to a daily figure;
#   * capping size at the maximum the dollar budget can deploy;
#   * stops scaled by the same volatility estimate, so risk units stay
#     consistent with the sizing model.
#
# The dataset ships with the package. On the bundled data this produces
# 115 trades.

rm(list = ls())
library(TradeTesterR)
library(quantmod)
library(data.table)
library(lubridate)
library(xts)
library(stringr)

# Parameters --------------------------------------------------------------
trade_size_dollars <- 10000            # risk budget base
target_vol         <- 0.15 / sqrt(252) # 15% annualized -> daily
spread             <- 0.04

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
my_strat <- function(price, RSI_threshold = 30, vol_window = 20) {

  price[, NextOpenTime := shift(date, n = 1, type = "lead")]
  price[, NextOpen     := shift(Open, n = 1, type = "lead")]

  boll <- BBands(price[, .(High, Low, Close)], n = 20, sd = 2)
  price[, upperBB := boll[, "up"]]
  price[, lowerBB := boll[, "dn"]]
  price[, midBB   := boll[, "mavg"]]
  price[, RSI := RSI(price[, Close], n = 14, maType = "SMA")]
  price[, PrevClose := shift(Close, n = 1, type = "lag")]

  # Historical volatility: rolling sd of daily returns
  price[, Ret1    := Close / shift(Close) - 1]
  price[, VolHist := frollapply(Ret1, n = vol_window, FUN = sd, align = "right")]

  # Entries: Bollinger touch with RSI confirmation, both sides
  price[, BuySignal := Close <= lowerBB & PrevClose > lowerBB & RSI <= RSI_threshold]
  price[, SellSignal := Close >= upperBB & PrevClose < upperBB & RSI >= (100 - RSI_threshold)]
  price[BuySignal == TRUE,  `:=`(EntryPrice = NextOpen, EntryType = "Market")]
  price[SellSignal == TRUE, `:=`(EntryPrice = NextOpen, EntryType = "Market")]

  # SIZING: Position Size = Risk Budget / Asset Risk
  price[BuySignal == TRUE,
        OrderSize :=  (trade_size_dollars * target_vol) / (EntryPrice * VolHist)]
  price[SellSignal == TRUE,
        OrderSize := -(trade_size_dollars * target_vol) / (EntryPrice * VolHist)]

  # Cap at the maximum the dollar budget can deploy, both sides
  price[BuySignal == TRUE  & OrderSize >  trade_size_dollars / EntryPrice,
        OrderSize :=  trade_size_dollars / EntryPrice]
  price[SellSignal == TRUE & OrderSize < -trade_size_dollars / EntryPrice,
        OrderSize := -trade_size_dollars / EntryPrice]

  # Exits: mid-band target, a two-sigma stop from the same volatility
  # estimate, 10-bar limit
  price[Close >= midBB, exit_long  := 1]
  price[Close <= midBB, exit_short := 1]
  price[BuySignal == TRUE,  StopLoss := EntryPrice * (1 - 2 * VolHist)]
  price[SellSignal == TRUE, StopLoss := EntryPrice * (1 + 2 * VolHist)]
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

# Size varies inversely with volatility
summary(abs(res$OrderSize))
sig <- price[(BuySignal == TRUE | SellSignal == TRUE) & !is.na(VolHist)]
cat("cor(size, VolHist) across signals:",
    round(sig[, cor(abs(OrderSize), VolHist)], 3), "\n")
cat("signals hitting the cap:",
    sig[abs(OrderSize) >= trade_size_dollars / EntryPrice - 1e-9, .N],
    "of", nrow(sig), "\n")
