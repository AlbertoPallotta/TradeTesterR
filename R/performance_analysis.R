

#' Calculate trading performance metrics
#'
#' @description
#' Computes key performance metrics including profit factor, Sharpe ratio,
#' and Sortino ratio from backtest results.
#'
#' @param bt Backtest results object (output from backtest function)
#' @param initial_equity Starting capital
#' @param risk_free_rate Data.table with date and adjusted columns (e.g., from tq_get("^TNX"))
#' @param start Start date for metric calculation
#' @param end End date for metric calculation
#'
#' @return A data.table with columns:
#'   - profit_factor: Sum of winning trades / abs(sum of losing trades)
#'   - net_profit: Total profit/loss
#'   - sharpe: Sharpe ratio
#'   - sortino: Sortino ratio
#'
#' @examples
#' \dontrun{
#' # Get risk-free rate
#' rf_rate <- data.table(tq_get("^TNX"))
#'
#' # Calculate metrics
#' metrics <- calculateTradeMetrics(bt,
#'                                initial_equity = 10000,
#'                                risk_free_rate = rf_rate,
#'                                start = "2020-01-01",
#'                                end = "2021-12-31")
#' }
#' @import data.table
#' @importFrom PerformanceAnalytics SortinoRatio SharpeRatio
#' @importFrom xts xts
#' @export
calculateTradeMetrics <- function(bt, initial_equity, risk_free_rate, start, end) {
  returns <- bt$results$Returns
  profit_factor <- sum(returns[returns > 0], na.rm = TRUE) / abs(sum(returns[returns < 0], na.rm = TRUE))
  net_profit <- sum(returns, na.rm = TRUE)
  ret <- bt$results[, .(ExitTime, Returns)]
  ret[, Ret := initial_equity + cumsum(Returns)]
  ret[, PrevRet := shift(Ret, type = "lag")]
  ret[1, PrevRet := initial_equity]
  ret[, Ret := ((Returns + PrevRet) / PrevRet) - 1]
  ret[, Day := as.Date(ExitTime)]
  ret <- merge(ret,
               risk_free_rate[, .(date, adjusted)],
               by.x = "Day",
               by.y = "date",
               all = TRUE)
  ret[is.na(Ret), Ret := 0]
  ret[, adjusted := nafill(adjusted, type = "locf")]
  ret[, adjusted := adjusted / 100]
  ret <- ret[Day >= start & Day <= end]

  sortino <- SortinoRatio(ret$Ret,
                          MAR = 0,
                          FUN = "StdDev")[1]
  sharpe <- SharpeRatio(xts(ret$Ret, order.by = ret$Day),
                        Rf = 0,
                        FUN = "StdDev")[1]

  out <- data.table(profit_factor = profit_factor,
                    net_profit = net_profit,
                    sharpe = sharpe,
                    sortino = sortino)
  return(out)
}
