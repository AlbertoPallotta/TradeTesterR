# TradeTesterR — User Guide

TradeTesterR is an educational, trade-level backtesting framework for R. Its
distinctive feature is a **dual-timeframe architecture**: strategy logic
(indicators, signals, order placement) is defined on one timeframe, while
order execution is simulated bar by bar against a finer timeframe with an
explicit bid/ask — so fills, stops, targets and gaps behave the way they do
at a broker rather than the way they look on a closing-price chart.

> **Version note.** This guide was written after the associated doctoral
> thesis was submitted, as part of the package's ongoing development. The
> thesis version of the code is permanently available under the tag
> `v0.1.0`.

---

## 1. Installation

```r
# install.packages("remotes")
remotes::install_github("AlbertoPallotta/TradeTesterR")          # latest
remotes::install_github("AlbertoPallotta/TradeTesterR@v0.1.0")   # thesis version
```

## 2. Quick start

Complete, runnable strategies ship inside the package, together with the
dataset they use (WTI crude futures, minute bars):

```r
library(TradeTesterR)

# copy a template into your project and open it
file.copy(system.file("examples", "boilerplate.R", package = "TradeTesterR"),
          "my_first_strategy.R")

# the bundled dataset
path <- system.file("extdata", "crude_wti_futures.csv", package = "TradeTesterR")
```

The `inst/examples/` folder holds two kinds of files. The two **boilerplates**
are complete workflows — strategy, backtest, performance, optimization. The
**strategy examples** are shorter single-purpose files, each demonstrating one
entry, exit, or position-sizing pattern (section 4).

## 3. Your first backtest, end to end

This section walks `boilerplate.R` from top to bottom. Open your copy
alongside it.

### 3.1 The shape of a strategy script

Every strategy script has the same skeleton:

1. **Parameters** — instrument settings, sizes, spread.
2. **Data** — load the fine timeframe, aggregate a strategy timeframe with
   `xts::to.period()`.
3. **`my_strat()`** — indicators, signals, order columns, the execution
   feed, and the `backtest()` call.
4. **Run and inspect** — results, analytics, plots.
5. **Optimize** — parameter grids and walk-forward evaluation.

### 3.2 What a strategy declares

Inside `my_strat()`, a strategy is a set of **columns** written onto the
strategy-timeframe table. Signals such as `BuySignal` are scaffolding for
building the order columns; the engine itself reads only the order columns:
entries are rows where `OrderSize` is non-zero, and `sign(OrderSize)` is the
trade direction. The full column contract is in section 5.

The boilerplate's strategy is a mean-reversion long: a market entry after a
close back under the lower Bollinger Band, a fixed monetary stop, a 50%
profit-take at the middle band with the remainder targeted at the upper
band, and a 10-bar time limit.

The script ends by assembling the execution feed and running the engine:

```r
ask <- xts(price_small[, .(Open, High, Low, Close)], order.by = df$date)
bid <- ask - spread
dat <- merge(mergeAskBid(ask, bid), price[], by = "date", all.x = TRUE)

bt  <- backtest(dat,
                exit_fun = exit_TP_SL_signal,
                AddToPosition = FALSE,
                CloseTradeOnOppositeSignal = FALSE,
                TradeTimeLimit = lubridate::weeks(100000),
                verbose = FALSE)
res <- bt$results
```

`merge()` snapshots the strategy table at that moment, so the assembly comes
after all order columns are written.

### 3.3 Reading the results

`bt$results` holds one row per **exit leg**. A trade that exits in one piece
occupies one row; a trade with a partial exit occupies several rows sharing
the same `Order_ID`. The essential columns: `Order_ID`, `Side`, `EntryTime`,
`EntryPrice`, `ExitTime`, `ExitPrice`, `ExitAmount`, `ExitReason`, `Returns`.

Two habits answer most questions:

```r
table(res$ExitReason)        # which mechanism closed each leg
uniqueN(res$Order_ID)        # number of trades (rows count legs)
```

`ExitReason` labels: `StopLoss` and `TakeProfit` for the fixed levels;
`Trailing stop ...` and `Trailing Target ...` for standing orders, with the
percentage of the position closed; `Exiting Long/Short: ...%` for signal
exits; and `No exit found/Time limit reached` for any of — the bar limit
(`CLOSE_CANDLES_SINCE_ENTRY`), the `TradeTimeLimit`, or a position still
open when the sample ends.

### 3.4 Performance at a glance

```r
analyze_performance(bt)
```

prints P&L, win rate, profit factor, expectancy, drawdown and
risk-adjusted figures for the whole run. Note that its `n_trades` field
counts exit legs; for the number of trades use `uniqueN(res$Order_ID)`.

### 3.5 Plots

All plotting functions take the backtest object:

```r
plotEquityCurve(bt)
plotDashboard(bt)                    # equity + drawdown overview
plotCumReturns(bt)
plotWinLossPie(bt)
plotTrades(bt, price_small)          # every trade on the price chart
plotSingleTrade(bt, price_small, trade_num = 1)
```

`plotSingleTrade()` is the inspection workhorse: one trade on the fine
timeframe with entry, exit, stop level and the exit reason annotated —
the fastest way to verify a strategy is doing what you meant.

### 3.6 Optimization and walk-forward evaluation

The boilerplate's final sections optimize the strategy on rolling windows:

```r
opt_params_single <- expand.grid(stop_loss_amount = c(100, 200, 300, 400, 500, 600))

bt_test <- optimizeStrategy(price, opt_params_single, initial_equity,
                            optimize_on = "net_profit",
                            train_set_weeks = 52, test_set_weeks = 26)
```

For each training window the grid is evaluated, the best parameter set by
`optimize_on` is selected, and it is then applied to the following test
window — a walk-forward design that separates in-sample selection from
out-of-sample evaluation. Multi-parameter grids work the same way through
`expand.grid()` with several columns.

Inspect the outcome with:

```r
plots <- plotMultiParameter(bt_test)   # parameter surfaces per window
isos  <- compareISOS(bt_test)          # in-sample vs out-of-sample
isos$plots$scatter
isos$plots$time_series
isos$plots$degradation
```

The IS/OS comparison is the honest summary of an optimization: how much of
the in-sample performance survives out of sample.

**A note on time.** The optimization sections re-run the backtest for every
parameter set in every window: expect several minutes on the full bundled
dataset. Run them once and keep the session.

## 4. Strategy examples

Each file in `inst/examples/` beyond the boilerplates demonstrates one
pattern and runs end to end on the bundled data.

| File | Demonstrates |
|---|---|
| `dynamic_exits_trailing.R` | A trailing stop and a moving profit target: standing exit orders re-evaluated every bar, alongside a fixed catastrophic stop. |
| `multi_exit_conditions.R` | Four exit mechanisms armed at once — stop, target, time limit, indicator signal — and the engine's precedence between them. |
| `stop_bracket_breakout.R` | Stop entries at the previous bar's band with a symmetric bracket; the one-pending-order-per-row model, run one side at a time. |
| `volatility_scaling.R` | Two-stage sizing: a base allocation halved when current ATR exceeds a multiple of its weekly average. |
| `volatility_target.R` | Sizing each position so expected daily dollar volatility matches a budget, with a deployment cap and volatility-scaled stops. |
| `vix_regime.R` | Regime classification from the VIX and per-regime size multipliers; merging an external daily series onto the strategy timeframe. Needs an internet connection once. |
| `mean_reversion_long_short.R` | Both directions in one strategy; partial exits on the long side — 50% at the mean, remainder at the band. Requires version 0.1.1 or later. |
| `ma_crossover.R` | The classic dual-MA trend system: enter on the golden cross, exit on the death cross, with a percentage stop. |

## 5. Reference: the order-column contract

The engine is column-driven. What it reads:

| Column | Meaning |
|---|---|
| `OrderSize` | Entry trigger and direction: non-zero rows are entries; `sign(OrderSize)` is the side (+ long, − short). For futures, typically contracts × multiplier. |
| `EntryPrice` | Order level. |
| `EntryType` | `"Market"`, `"Limit"`, or `"Stop"`. |
| `EntryTime` | When the order becomes active (conventionally the next bar's open). |
| `StopLoss`, `TakeProfit` | Fixed exit levels (absolute prices). Optional. |
| `CLOSE_CANDLES_SINCE_ENTRY` | Time exit after N strategy-timeframe bars. Optional. |
| `exit_long`, `exit_short` | Signal exits; a value 0–1 closes that fraction. Optional. |
| `targetorder_long_N` (+ `_amount`) | Standing, dynamically updating profit-target orders checked against the intrabar range; numbered for partial exits. Short-side equivalents exist. Optional. |
| `stoporder_long_N` (+ `_amount`) | Standing (trailing) stop orders, same mechanics. Optional. |
| `CancelOrder` | Cancels a pending limit/stop entry before it triggers. Optional. |

Two consequences worth internalising:

* **One pending order per row.** A strategy wanting simultaneous standing
  orders on both sides of the market runs each side separately.
* **Unconditional signals are a feature.** `BuySignal := TRUE` on every bar
  is the continuous order placement pattern: a persistent order refreshed
  each bar, each day's order superseding yesterday's unfilled one. Filled
  trades are typically a small fraction of placed orders.

## 6. Position-management flags

* **`CloseTradeOnOppositeSignal`** — `TRUE` gives stop-and-reverse behaviour;
  `FALSE` gives discrete trades that exit only through the explicit exit
  machinery. The recommended convention is `FALSE` — strategies should state
  their exits — and every packaged example passes it explicitly. The current
  signature default is `TRUE`; changing it is planned.
* **`AddToPosition`** — `FALSE` (recommended) ignores further entry signals
  while a position is held.
* **`TradeTimeLimit`** — a coarse safety net (default: 4 weeks). Set it very
  large when bar-based exits are doing the work.

## 7. Data requirements and caveats

* The execution feed should be materially **finer** than the strategy
  timeframe (intraday under daily). Daily-only sources such as Yahoo Finance
  (`load_data_from_yahoo()`) suit indicator prototyping, but a daily feed
  cannot faithfully simulate intrabar stop and limit fills.
* **Sanity-check fixed monetary levels against the price.** A $4,000 stop on
  four contracts is 20 points; on a $16 bar that implies a negative stop
  level. The engine places exactly the order the arithmetic implies.

## 8. Versions

* **v0.1.0** — the tagged thesis version.
* **v0.1.1** (in preparation) — fixes a partial-exit accounting defect in
  `exit_TP_SL_signal()` and adds exit-amount conservation regression tests.
* Planned — `CloseTradeOnOppositeSignal` default changed to `FALSE`;
  a `kelly_size()` helper for trade-ledger Kelly sizing; further examples.

---

*TradeTesterR is released under GPL-3. Author: Alberto Pallotta.*
