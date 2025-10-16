

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




#' Analyze backtest performance (trade-level + time-series)
#'
#' @description
#' Computes robust performance metrics using the equity curve when available
#' (preferred for Sharpe/Sortino, CAGR, drawdowns), and complements them with
#' trade-level metrics (profit factor, win rate, expectancy, etc.).
#'
#' @param bt A backtest object. Expected components:
#'   \itemize{
#'     \item \code{$equity_curve} (optional): data.frame/data.table with a date/time column and an \code{Equity} column.
#'     \item \code{$results}: data.frame/data.table of completed trades with \code{EntryTime}, \code{ExitTime}, \code{Returns} (P\&L in currency).
#'   }
#' @param initial_equity Numeric. Starting capital (default \code{10000}).
#' @param risk_free Either \emph{NULL} (assume 0), a single annualized numeric rate
#'   (e.g. \code{0.03} = 3\%), or a data.frame/data.table with columns
#'   \code{date} (or \code{Date}) and \code{rate} (or \code{adjusted}) as daily values
#'   in decimal or percent (the function auto-scales if \code{>1}).
#' @param scale Integer trading days per year for annualization (default \code{252}).
#' @param start,end Optional \code{Date} (or character coercible) to restrict analysis window.
#' @param min_days_for_cagr Integer or NULL. If set (e.g. 180L), hide CAGR when the sample has fewer than this many trading days.
#'
#' @return A \code{data.table} with both trade-level and time-series metrics:
#' \itemize{
#'   \item \strong{Trade-level:} total_pnl, n_trades, win_rate, profit_factor, avg_win, avg_loss, expectancy,
#'         max_trade_dd (P\&L path on trade closes), win_loss_ratio.
#'   \item \strong{Time-series:} cagr, ann_vol, sharpe, sortino, max_dd, max_dd_pct, ulcer_index.
#' }
#'
#' @details
#' Time-series metrics use \code{$equity_curve} if present; otherwise a step-wise
#' daily equity is built from summed P\&L at each \code{as.Date(ExitTime)}. This
#' approximation ignores mark-to-market between trade dates; use a true daily curve
#' for precise risk metrics.
#'
#' @examples
#' \dontrun{
#' # After you have a backtest object:
#' # bt <- my_test_strat(price)
#' metrics <- analyze_performance(bt, initial_equity = 10000, min_days_for_cagr = 180L)
#' print(metrics)
#' }
#'
#' @import data.table
#' @export
analyze_performance <- function(bt,
                                initial_equity = 10000,
                                risk_free = NULL,       # NULL, scalar annual rate (e.g. 0.03), or daily df with date+rate
                                scale = 252L,          # trading days per year
                                start = NULL,
                                end   = NULL,
                                min_days_for_cagr = NULL  # e.g. 180L to suppress CAGR on short samples; NULL = always compute
) {
  stopifnot(is.list(bt))
  if (!("results" %in% names(bt))) stop("bt$results not found.")
  res <- data.table::as.data.table(bt$results)
  if (!all(c("Returns", "ExitTime") %in% names(res))) {
    stop("bt$results must have at least 'Returns' and 'ExitTime'.")
  }

  # --- window trades (optional) ---
  to_date <- function(x) as.Date(x)
  if (!is.null(start)) { start <- to_date(start); res <- res[as.Date(ExitTime) >= start] }
  if (!is.null(end))   { end   <- to_date(end);   res <- res[as.Date(ExitTime) <= end] }
  if (nrow(res) == 0) stop("No trades in the selected window.")

  # ---------- Trade-level metrics ----------
  total_pnl   <- sum(res$Returns, na.rm = TRUE)
  n_trades    <- nrow(res)
  n_wins      <- sum(res$Returns > 0, na.rm = TRUE)
  n_losses    <- sum(res$Returns < 0, na.rm = TRUE)
  win_rate    <- if (n_trades > 0) 100 * n_wins / n_trades else NA_real_

  wins_sum      <- sum(res[Returns > 0, Returns], na.rm = TRUE)
  losses_sum    <- abs(sum(res[Returns < 0, Returns], na.rm = TRUE))
  profit_factor <- if (losses_sum > 0) wins_sum / losses_sum else Inf

  avg_win        <- if (n_wins   > 0) mean(res[Returns > 0, Returns]) else 0
  avg_loss       <- if (n_losses > 0) mean(res[Returns < 0, Returns]) else 0
  win_loss_ratio <- if (avg_loss != 0) abs(avg_win / avg_loss) else NA_real_
  expectancy     <- (n_wins/n_trades) * avg_win + (n_losses/n_trades) * avg_loss

  # Trade-close equity path (quick DD on trade closes)
  res <- res[order(ExitTime)]
  res[, Equity_tc := initial_equity + cumsum(Returns)]
  res[, Peak_tc   := cummax(Equity_tc)]
  res[, DD_tc     := Equity_tc / Peak_tc - 1]
  max_trade_dd    <- min(res$DD_tc, na.rm = TRUE)

  # ---------- Equity curve for time-series metrics ----------
  if ("equity_curve" %in% names(bt) && !is.null(bt$equity_curve)) {
    ec <- data.table::as.data.table(bt$equity_curve)
    date_col <- names(ec)[which.max(grepl("date|time|day", names(ec), ignore.case = TRUE))]
    eq_col   <- names(ec)[which.max(grepl("equity|nav|value", names(ec), ignore.case = TRUE))]
    if (!length(date_col) || !length(eq_col)) stop("equity_curve must have date/time and equity columns.")
    eq <- ec[, .(Day = as.Date(get(date_col)), Equity = as.numeric(get(eq_col)))]
  } else {
    daily_pnl <- res[, .(DailyPnL = sum(Returns, na.rm = TRUE)), by = .(Day = as.Date(ExitTime))]
    daily_pnl <- daily_pnl[!is.na(Day)]
    data.table::setorder(daily_pnl, Day)
    eq <- daily_pnl[, .(Day, Equity = initial_equity + cumsum(DailyPnL))]
  }

  # sanitize & window equity, then sort
  eq <- eq[!is.na(Day) & is.finite(Equity)]
  if (!is.null(start)) eq <- eq[Day >= start]
  if (!is.null(end))   eq <- eq[Day <= end]
  data.table::setorder(eq, Day)

  # pad to daily grid (ensures proper daily returns/annualization)
  if (nrow(eq) >= 1L) {
    all_days <- data.table::data.table(Day = seq(min(eq$Day), max(eq$Day), by = "day"))
    eq <- all_days[eq, on = "Day"]
    eq[, Equity := data.table::nafill(Equity, "locf")]
    eq <- eq[!is.na(Equity)]
  }

  if (nrow(eq) < 2L) {
    period_days  <- 0L
    period_years <- 0
    total_return <- NA_real_
    cagr <- ann_vol <- sharpe <- sortino <- max_dd <- max_dd_pct <- ulcer <- NA_real_
  } else {
    # Daily returns
    eq[, Lag := data.table::shift(Equity)]
    eq[, Ret := (Equity / Lag) - 1]
    eq <- eq[is.finite(Ret)]

    # Risk-free to daily Rf_d
    if (is.null(risk_free)) {
      eq[, Rf_d := 0]
    } else if (is.numeric(risk_free) && length(risk_free) == 1L) {
      eq[, Rf_d := risk_free / scale]
    } else {
      rf <- data.table::as.data.table(risk_free)
      dcol <- names(rf)[which.max(grepl("^date$|^Date$", names(rf)))]
      rcol <- names(rf)[which.max(grepl("rate|adjusted|rf", names(rf), ignore.case = TRUE))]
      if (!length(dcol) || !length(rcol)) stop("risk_free must have 'date' and 'rate/adjusted' columns.")
      rf[, Day := as.Date(get(dcol))]
      rf[, Rf_d := as.numeric(get(rcol))]
      rf[abs(Rf_d) > 1, Rf_d := Rf_d / 100]   # percent -> decimal
      rf[, Rf_d := Rf_d / scale]              # annual -> daily
      eq <- rf[, .(Day, Rf_d)][eq, on = "Day"]
      eq[is.na(Rf_d), Rf_d := 0]
    }

    # Excess returns
    eq[, Excess := Ret - Rf_d]

    # Period length & total return
    period_days  <- nrow(eq)                  # number of daily returns
    period_years <- period_days / scale
    gross        <- prod(1 + eq$Ret, na.rm = TRUE)
    total_return <- if (is.finite(gross)) gross - 1 else NA_real_

    # CAGR via trading days; optionally suppress for short samples
    cagr <- if (!is.null(min_days_for_cagr) && period_days < min_days_for_cagr) {
      NA_real_
    } else if (is.finite(gross) && gross > 0 && period_days > 0) {
      gross^(scale / period_days) - 1
    } else NA_real_

    # Vol / Sharpe / Sortino
    mu   <- mean(eq$Excess, na.rm = TRUE)
    sd_  <- stats::sd(eq$Excess, na.rm = TRUE)
    ddn  <- pmin(eq$Excess, 0)
    sdn  <- stats::sd(ddn[ddn < 0])
    ann_vol <- if (is.finite(sd_) && sd_ > 0) sd_ * sqrt(scale) else NA_real_
    sharpe  <- if (is.finite(sd_) && sd_ > 0) (mu * scale) / (sd_ * sqrt(scale)) else NA_real_
    sortino <- if (is.finite(sdn) && sdn > 0) (mu * scale) / (sdn * sqrt(scale)) else NA_real_

    # Drawdowns & Ulcer
    eq[, Peak := cummax(Equity)]
    eq[, DD   := Equity / Peak - 1]
    max_dd     <- suppressWarnings(min(eq$DD, na.rm = TRUE))
    max_dd_pct <- 100 * max_dd
    ulcer      <- sqrt(mean((100 * pmin(0, eq$DD))^2))
  }

  data.table::data.table(
    total_pnl      = total_pnl,
    n_trades       = n_trades,
    win_rate       = win_rate,
    profit_factor  = profit_factor,
    avg_win        = avg_win,
    avg_loss       = avg_loss,
    win_loss_ratio = win_loss_ratio,
    expectancy     = expectancy,
    max_trade_dd   = max_trade_dd,
    period_days    = period_days,
    period_years   = period_years,
    total_return   = total_return,
    cagr           = cagr,
    ann_vol        = ann_vol,
    sharpe         = sharpe,
    sortino        = sortino,
    max_dd         = max_dd,
    max_dd_pct     = max_dd_pct,
    ulcer_index    = ulcer
  )
}



calculateTradeMetrics <- function(bt, initial_equity, risk_free_rate = NULL, start = NULL, end = NULL) {
  .Deprecated("analyze_performance", msg = "calculateTradeMetrics() is deprecated; use analyze_performance().")
  m <- analyze_performance(bt, initial_equity = initial_equity,
                           risk_free = risk_free_rate, start = start, end = end)
  data.table::data.table(
    profit_factor = m$profit_factor,
    net_profit    = m$total_pnl,
    sharpe        = m$sharpe,
    sortino       = m$sortino
  )
}

getPerformanceMetrics <- function(bt, initial_equity = 10000, return_df = FALSE) {
  .Deprecated("analyze_performance", msg = "getPerformanceMetrics() is deprecated; use analyze_performance().")
  m <- analyze_performance(bt, initial_equity = initial_equity)
  if (return_df) return(as.data.frame(m))
  print(m)
  invisible(m)
}
