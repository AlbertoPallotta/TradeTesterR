# TradeTesterR 0.1.2

* analyze_performance(): trade statistics are now computed per trade
  (partial-exit legs aggregate to a single trade). The equity curve is
  padded onto a daily grid before annualization, correcting period length,
  CAGR, volatility and Sharpe figures. Walk-forward optimization metrics
  inherit these corrections (net_profit is unchanged).
* Removed duplicate definitions of xts_to_dt() and calculateTradeMetrics()
  (behavior unchanged).
* Example cleanups: a development loader line removed from
  mean_reversion_long_short.R; a dead commented dependency deleted from
  boilerplate.R.

# TradeTesterR 0.1.1

* Fixed: exit_TP_SL_signal() synthesized a phantom closing leg for any trade
  with two or more genuine exit legs (residual position was inferred from a
  per-row rather than cumulative figure). On the packaged boilerplate this
  removes 8 phantom rows (+$1,156 of spurious P&L) and recovers one
  previously suppressed trade; the example's net changes from -1427.39 to
  -1899.92 while all genuine trade legs are unchanged.
* New regression tests enforce exit-amount conservation per trade.
