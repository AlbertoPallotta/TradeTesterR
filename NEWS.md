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
