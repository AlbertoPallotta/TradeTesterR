# TradeTesterR 0.1.1

* Fixed: exit_TP_SL_signal() synthesized a phantom closing leg for any trade
  with two or more genuine exit legs (residual position was inferred from a
  per-row rather than cumulative figure). On the packaged boilerplate this
  removes 8 phantom rows (+$1,156 of spurious P&L) and recovers one
  previously suppressed trade; the example's net changes from -1427.39 to
  -1899.92 while all genuine trade legs are unchanged.
* New regression tests enforce exit-amount conservation per trade.