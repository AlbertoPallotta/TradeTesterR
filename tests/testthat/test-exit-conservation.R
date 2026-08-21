# Regression tests for the v0.1.1 partial-exit ledger fix.
# Derived from the three-environment certification of 2026-08:
#   boilerplate: 104 rows | 70 trades | net -1899.919 | 0 violations
#   full-exit (stop-bracket): 15 trades | net -262.363
# These tests run the packaged strategies on the bundled WTI data.

library(data.table)
library(xts)
library(TTR)

make_prices <- function() {
  df <- fread(system.file("extdata", "crude_wti_futures.csv",
                          package = "TradeTesterR"))
  setnames(df, c("date", "Open", "High", "Low", "Close", "Volume"))
  price_small <- df[, .(date, Open, High, Low, Close)]
  setkey(price_small, date)
  px <- to.period(xts(price_small[, .(Open, High, Low, Close)],
                      order.by = price_small$date),
                  period = "days", k = 1, indexAt = "startof",
                  name = NULL, drop.time = FALSE)
  price <- data.table(date = index(px), coredata(px))
  setkey(price, date)
  list(df = df, price_small = price_small, price = price)
}

run_boilerplate <- function() {
  d <- make_prices(); df <- d$df; price_small <- d$price_small; price <- d$price
  spread <- 0.04; tsc <- 4; fm <- 50
  price[, NextOpenTime := shift(date, 1, type = "lead")]
  price[, NextOpen := shift(Open, 1, type = "lead")]
  b <- BBands(price[, .(High, Low, Close)], n = 20, sd = 2)
  price[, upperBB := b[, "up"]]; price[, lowerBB := b[, "dn"]]; price[, midBB := b[, "mavg"]]
  price[, PrevClose := shift(Close, 1, type = "lag")]
  price[, BuySignal := Close <= lowerBB & PrevClose > lowerBB]
  price[BuySignal == TRUE, `:=`(EntryPrice = NextOpen, EntryType = "Market",
        StopLoss = NextOpen - (2000 / (tsc * fm)))]
  price[, targetorder_long_1 := midBB];  price[, targetorder_long_1_amount := 0.5]
  price[, targetorder_long_2 := upperBB]; price[, targetorder_long_2_amount := 1]
  price[, CLOSE_CANDLES_SINCE_ENTRY := 10]
  price[BuySignal == TRUE, OrderSize := fm * tsc]
  setorder(price, date); price[, EntryTime := shift(date, 1, type = "lead")]
  ask <- xts(price_small[, .(Open, High, Low, Close)], order.by = df$date)
  dat <- merge(mergeAskBid(ask, ask - spread), price[], by = "date", all.x = TRUE)
  backtest(dat, exit_fun = exit_TP_SL_signal, AddToPosition = FALSE,
           CloseTradeOnOppositeSignal = FALSE, verbose = FALSE,
           TradeTimeLimit = lubridate::weeks(100000))$results
}

run_stop_bracket <- function() {
  d <- make_prices(); df <- d$df; price_small <- d$price_small; price <- d$price
  spread <- 0.04; tsc <- 4; fm <- 50
  price[, NextOpenTime := shift(date, 1, type = "lead")]
  price[, NextOpen := shift(Open, 1, type = "lead")]
  b <- BBands(price[, .(High, Low, Close)], n = 20, sd = 2)
  price[, upperBB := b[, "up"]]; price[, lowerBB := b[, "dn"]]
  price[, PrevLowerBB := shift(lowerBB, 1, type = "lag")]
  price[, SellSignal := TRUE]
  price[SellSignal == TRUE, `:=`(EntryPrice = PrevLowerBB, EntryType = "Stop",
        StopLoss = PrevLowerBB + (2000 / (tsc * fm)),
        TakeProfit = PrevLowerBB - (2000 / (tsc * fm)),
        CLOSE_CANDLES_SINCE_ENTRY = 5, OrderSize = -fm * tsc)]
  setorder(price, date); price[, EntryTime := shift(date, 1, type = "lead")]
  ask <- xts(price_small[, .(Open, High, Low, Close)], order.by = df$date)
  dat <- merge(mergeAskBid(ask, ask - spread), price[], by = "date", all.x = TRUE)
  backtest(dat, exit_fun = exit_TP_SL_signal, AddToPosition = FALSE,
           CloseTradeOnOppositeSignal = FALSE, verbose = FALSE,
           TradeTimeLimit = lubridate::weeks(100000))$results
}

test_that("partial-exit trades conserve position size (the v0.1.1 fix)", {
  res <- run_boilerplate()
  viol <- res[, .(exited = sum(abs(ExitAmount)),
                  entered = first(abs(OrderSize))), by = Order_ID][exited != entered]
  expect_equal(nrow(viol), 0)
  expect_true(all(res[, .(f = last(CurrentPosSize)), by = Order_ID]$f == 0))
})

test_that("boilerplate reproduces the certified post-fix values", {
  res <- run_boilerplate()
  expect_equal(nrow(res), 104)
  expect_equal(uniqueN(res$Order_ID), 70)
  expect_equal(sum(res$Returns), -1899.919, tolerance = 0.01)
  expect_equal(sum(res$ExitReason %like% "No exit"), 59)
})

test_that("full-exit strategies are unaffected by the ledger fix", {
  res <- run_stop_bracket()
  expect_equal(uniqueN(res$Order_ID), nrow(res))          # one leg per trade
  expect_equal(uniqueN(res$Order_ID), 15)
  expect_equal(sum(res$Returns), -262.363, tolerance = 0.01)
  viol <- res[, .(exited = sum(abs(ExitAmount)),
                  entered = first(abs(OrderSize))), by = Order_ID][exited != entered]
  expect_equal(nrow(viol), 0)
})
