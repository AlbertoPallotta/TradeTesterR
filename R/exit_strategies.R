
#' Default exit function with multiple exit types
#'
#' @description
#' Implements various exit mechanisms including stop loss, take profit,
#' trailing stops/targets, and signal-based exits. Supports partial position exits.
#'
#' @param dis A data.table subset for one trade containing:
#'   - Price data (Open.a, High.a, Low.a, Close.a, Open.b, High.b, Low.b, Close.b)
#'   - Side: 1 for long, -1 for short
#'   - OrderSize: Position size
#'   - Optional: StopLoss, TakeProfit, exit_long, exit_short
#'   - Optional: targetorder_long_N, stoporder_long_N (for partial exits)
#'
#' @details
#' Exit types supported:
#' - Fixed stop loss and take profit
#' - Trailing stops: stoporder_long_1, stoporder_short_1
#' - Trailing targets: targetorder_long_1, targetorder_short_1
#' - Signal exits: exit_long, exit_short (values 0-1 for partial exits)
#' - Time-based exits (handled by backtest function)
#'
#' For partial exits, use numbered orders (e.g., targetorder_long_1, targetorder_long_2)
#' with corresponding _amount columns specifying exit fraction.
#'
#' @return A data.table with exit information including:
#'   - ExitTime, ExitPrice, ExitAmount, ExitReason
#'   - CurrentPosSize: Remaining position after exit
#'
#' @examples
#' \dontrun{
#' # In strategy definition:
#' price[, StopLoss := EntryPrice - 2 * ATR]
#' price[, TakeProfit := EntryPrice + 3 * ATR]
#'
#' # For partial exits:
#' price[, targetorder_long_1 := EntryPrice + 1 * ATR]
#' price[, targetorder_long_1_amount := 0.5]  # Exit 50%
#' }
#' @import data.table
#' @importFrom stringr str_detect
#' @export
exit_TP_SL_signal <- function(dis) {
  side <- data.table::first(dis)$Side

  # Trailing Targets ----
  if (side == 1 & sum(str_detect(names(dis), "targetorder_long")) > 0) {
    # Fill in the NAs for
    cols <- names(dis)[str_detect(names(dis), "targetorder_long[_0-9]+$")]
    for (col in cols) {
      amt_col <- paste0(col, "_amount")
      # shift the date
      shifted <- dis[, .(NextOpenTime, get(col), get(amt_col))]
      setnames(shifted, c("date", col, amt_col))

      dis <- merge(dis[, !c(col, amt_col), with = FALSE],
                   shifted, by = "date", all.x = TRUE)
      dis[, eval(col) := nafill(get(col), type = "locf")]
      dis[, eval(amt_col) := nafill(get(amt_col), type = "locf")]
      dis[which(High.b >= get(col))[1], ExitTime := date]
      dis[which(High.b >= get(col))[1], ExitPrice := get(col)]
      dis[which(High.b >= get(col))[1], ExitPerc := get(amt_col)]
      dis[which(High.b >= get(col))[1], ExitReason := paste0("Trailing Target Long: ", 100 * get(amt_col), "%")]
      dis[which(High.b >= get(col))[1] , OrigOrder:=Order_ID]
    }
  }
  if (side == -1 & sum(str_detect(names(dis), "targetorder_short")) > 0) {
    # Fill in the NAs for
    cols <- names(dis)[str_detect(names(dis), "targetorder_short[_0-9]+$")]
    for (col in cols) {
      amt_col <- paste0(col, "_amount")
      # shift the date
      shifted <- dis[, .(NextOpenTime, get(col), get(amt_col))]
      setnames(shifted, c("date", col, amt_col))

      dis <- merge(dis[, !c(col, amt_col), with = FALSE],
                   shifted, by = "date", all.x = TRUE)
      dis[, eval(col) := nafill(get(col), type = "locf")]
      dis[, eval(amt_col) := nafill(get(amt_col), type = "locf")]
      dis[which(Low.a <= get(col))[1], ExitTime := date]
      dis[which(Low.a <= get(col))[1], ExitPrice := get(col)]
      dis[which(Low.a <= get(col))[1], ExitPerc := get(amt_col)]
      dis[which(Low.a <= get(col))[1], ExitReason := paste0("Trailing Target Short: ", 100 * get(amt_col), "%")]
      dis[which(Low.a <= get(col))[1] , OrigOrder:=Order_ID]
    }
  }

  # Trailing Stop ----
  if (side == 1 & sum(str_detect(names(dis), "stoporder_long")) > 0) {
    # Fill in the NAs for
    cols <- names(dis)[str_detect(names(dis), "stoporder_long[_0-9]+$")]
    for (col in cols) {
      amt_col <- paste0(col, "_amount")
      # shift the date
      shifted <- dis[, .(NextOpenTime, get(col), get(amt_col))]
      setnames(shifted, c("date", col, amt_col))

      dis <- merge(dis[, !c(col, amt_col), with = FALSE],
                   shifted, by = "date", all.x = TRUE)
      dis[, eval(col) := nafill(get(col), type = "locf")]
      dis[, eval(amt_col) := nafill(get(amt_col), type = "locf")]
      dis[which(Low.a <= get(col))[1], ExitTime := date]
      dis[which(Low.a <= get(col))[1], ExitPrice := get(col)]
      dis[which(Low.a <= get(col))[1], ExitPerc := get(amt_col)]
      dis[which(Low.a <= get(col))[1], ExitReason := paste0("Trailing stop Long: ", 100 * get(amt_col), "%")]
      dis[which(Low.a <= get(col))[1] , OrigOrder:=Order_ID]
    }
  }
  if (side == -1 & sum(str_detect(names(dis), "stoporder_short")) > 0) {
    # Fill in the NAs for
    cols <- names(dis)[str_detect(names(dis), "stoporder_short[_0-9]+$")]
    for (col in cols) {
      amt_col <- paste0(col, "_amount")
      # shift the date
      shifted <- dis[, .(NextOpenTime, get(col), get(amt_col))]
      setnames(shifted, c("date", col, amt_col))

      dis <- merge(dis[, !c(col, amt_col), with = FALSE],
                   shifted, by = "date", all.x = TRUE)
      dis[, eval(col) := nafill(get(col), type = "locf")]
      dis[, eval(amt_col) := nafill(get(amt_col), type = "locf")]
      dis[which(High.b >= get(col))[1], ExitTime := date]
      dis[which(High.b >= get(col))[1], ExitPrice := get(col)]
      dis[which(High.b >= get(col))[1], ExitPerc := get(amt_col)]
      dis[which(High.b >= get(col))[1], ExitReason := paste0("Trailing stop Short: ", 100 * get(amt_col), "%")]
      dis[which(High.b >= get(col))[1] , OrigOrder:=Order_ID]
    }
  }
  # exit_long/short ----
  # Create close based on exit_long, exit_short (0-1)
  if(side == 1 & sum(str_detect(names(dis), "exit_long")) > 0) {
    cols <- names(dis)[str_detect(names(dis), "exit_long")]
    # Since we are using open time as the date, we need to shift 1 fwd to get the correct time
    for (col in cols) {
      dis[which(get(col) > 0)[1], ExitTime := NextOpenTime]
      dis[which(get(col) > 0)[1], ExitPrice := NextOpen]
      dis[which(get(col) > 0)[1], ExitPerc := get(col)]
      dis[which(get(col) > 0)[1], ExitReason := paste0("Exiting Long: ", 100 * get(col), "%")]
      dis[which(get(col) > 0)[1] , OrigOrder:=Order_ID]
    }
  }
  if(side == -1 & sum(str_detect(names(dis), "exit_short")) > 0) {
    cols <- names(dis)[str_detect(names(dis), "exit_short")]
    # Since we are using open time as the date, we need to shift 1 fwd to get the correct time
    for (col in cols) {
      dis[which(get(col) > 0)[1], ExitTime := NextOpenTime]
      dis[which(get(col) > 0)[1], ExitPrice := NextOpen]
      dis[which(get(col) > 0)[1], ExitPerc := get(col)]
      dis[which(get(col) > 0)[1], ExitReason := paste0("Exiting Short: ", 100 * get(col), "%")]
      dis[which(get(col) > 0)[1] , OrigOrder:=Order_ID]
    }
  }

  # If there's no exit signal, use the last row
  if(sum(!is.na(dis$ExitTime))== 0){
    mins <- as.numeric(median(diff(tail(dis$date, 100), units='mins')))
    dis[NROW(dis), ExitTime := as.POSIXct(date) + lubridate::minutes(mins)]
    dis[NROW(dis), ExitPrice := Close.b]
    dis[NROW(dis), ExitPerc := 1]
    dis[NROW(dis), ExitReason := "No exit found/Time limit reached"]
  }

  if (!"TakeProfit" %in% names(dis)) {
    dis[, TakeProfit := NA]
  }
  if (!"StopLoss" %in% names(dis)) {
    dis[, StopLoss := NA]
  }
  if(side==1) {
    tp_date <- data.table::first(dis[High.b >= TakeProfit])$date
    sl_date <- data.table::first(dis[Low.a <= StopLoss])$date
  } else if(side== -1) {
    tp_date <- data.table::first(dis[Low.a <= TakeProfit])$date
    sl_date <- data.table::first(dis[High.b >= StopLoss])$date
  }

  # Make sure there's a value for tp/sl date even if it's not in the dataset
  farfuture <- as.POSIXct('3025-01-01')

  tp_date <- min(tp_date, farfuture, na.rm = TRUE)
  sl_date <- min(sl_date, farfuture, na.rm = TRUE)

  if(tp_date < sl_date) {
    dis[date==tp_date, ExitTime:=tp_date]
    dis[date==tp_date, ExitPrice:=TakeProfit]
    dis[date==tp_date, ExitPerc:=1]
    dis[date==tp_date, ExitReason:='TakeProfit']
    dis[date==tp_date, OrigOrder:=Order_ID]
  } else if(sl_date < tp_date) {
    dis[date==sl_date, ExitTime:=sl_date]
    dis[date==sl_date, ExitPrice:=StopLoss]
    dis[date==sl_date, ExitPerc:=1]
    dis[date==sl_date, ExitReason:='StopLoss']
    dis[date==sl_date, OrigOrder:=Order_ID]
  } else if(tp_date == sl_date) {
    dis[date==tp_date, ExitTime:=tp_date]
    dis[date==tp_date, ExitPrice:=NA]
    dis[date==tp_date, ExitPerc:=1]
    dis[date==tp_date, ExitReason:='Exit Price Unknown. TP/SL hit on same candle']
    dis[date==tp_date, OrigOrder:=Order_ID]
  }
  curr_size <- dis[, OrderSize][1]
  exit_amount <- c()
  current_size <- c()
  for (row in 1:NROW(dis[!is.na(ExitPerc)])) {
    this <- dis[!is.na(ExitPerc)][row]
    exit_amount <- c(exit_amount,
                     curr_size * this$ExitPerc * -1)
    curr_size <- curr_size + exit_amount[row]
    current_size <- c(current_size, curr_size)
  }
  dis[!is.na(ExitPerc), ExitAmount := exit_amount]
  dis[!is.na(ExitPerc), CurrentPosSize := current_size]
  dis[, CurrentSize := OrderSize[1] + ExitAmount]

  # If some position remains in the end
  remaining <- min(abs(dis$CurrentSize), na.rm=TRUE)
  if (remaining > 0) {
    dis[NROW(dis), ExitAmount := remaining * -1 * side]
    dis[NROW(dis), ExitTime := data.table::last(date)]
    dis[NROW(dis), ExitPrice := data.table::last(Close.b)]
    dis[NROW(dis), ExitReason := "No exit found/Time limit reached"]
    dis[NROW(dis), CurrentPosSize := 0]
    dis[NROW(dis), CurrentSize := 0]
  }
  out <- dis[abs(ExitAmount) > 0]
  return(out)
}
