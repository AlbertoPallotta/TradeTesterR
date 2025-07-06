#' Run a backtest on a trading strategy
#'
#' @description
#' Executes a backtest by processing entry signals and applying exit logic
#' to simulate trading performance over historical data.
#'
#' @param dat A data.table containing merged price and signal data with columns:
#'   - date: POSIXct timestamps
#'   - Open.a, High.a, Low.a, Close.a: Ask prices
#'   - Open.b, High.b, Low.b, Close.b: Bid prices
#'   - OrderSize: Position size (positive for long, negative for short)
#'   - Additional columns as needed for exit logic
#' @param exit_fun Exit function to apply (default: exit_TP_SL_signal)
#' @param exit_args List of additional arguments to pass to exit function
#' @param CloseTradeOnOppositeSignal Logical. Close positions on opposite signals? (default: TRUE)
#' @param AddToPosition Logical. Allow adding to existing positions? (default: FALSE)
#' @param TradeTimeLimit Maximum time to hold a position (default: 4 weeks)
#' @param verbose Logical. Print trade results during execution? (default: TRUE)
#'
#' @return A list containing:
#'   - results: data.table of all trades with entry/exit details and returns
#'   - data: original data merged with trade information
#'
#' @examples
#' \dontrun{
#' # Basic backtest
#' bt <- backtest(strategy_data, verbose = TRUE)
#'
#' # Custom exit function and parameters
#' bt <- backtest(strategy_data,
#'                exit_fun = custom_exit,
#'                exit_args = list(trailing_pct = 0.02),
#'                TradeTimeLimit = lubridate::weeks(2))
#' }
#' @import data.table
#' @import lubridate
#' @import xts
#' @export
backtest <- function(dat,
                     #ask, bid,
                     #entry_fun, entry_args=list(),
                     exit_fun=NULL, exit_args=list(),
                     CloseTradeOnOppositeSignal=TRUE,
                     AddToPosition=FALSE,
                     TradeTimeLimit=lubridate::weeks(4),
                     verbose=TRUE) {

  # Load bid/ask historical data
  # dat <- mergeAskBid(ask, bid)
  #
  # # Create entry orders, takes in a list of arguments for the function (entry_args)
  # arg_names <- names(formals(entry_fun))
  # args <- c(list(copy(dat)), entry_args)
  # names(args)[1] <- arg_names[1]
  # user_dat <- rlang::exec('entry_fun', !!!args)
  #
  # # Columns that are in user_dat, but not dat (keep date column though)
  # user_cols <- c('date', names(user_dat)[!names(user_dat) %in% names(dat)])
  #
  # dat <- merge(dat, user_dat[, c(user_cols), with=FALSE], by='date', all=TRUE)
  entry <- dat[OrderSize!=0 & !is.na(OrderSize)]
  if (NROW(entry) == 0) {
    stop("No entry orders found in the data.")
  }
  # Return columns for prev/next order directions/sizes
  entry[, prev_order:=shift(OrderSize)]
  entry[, next_order:=shift(OrderSize, type='lead')]

  # Estimate the timeframe of our backtesting data in minutes
  mins <- getTimeFrameMins(dat)


  # Entry time will be the close of the current candle
  if(!'EntryTime' %in% names(entry)) {
    entry[, EntryTime:=date + lubridate::minutes(mins)]
  }
  entry[, Side:=sign(OrderSize)]

  # If EntryPrice isn't supplied by the entry_fun, create it
  if(!'EntryPrice' %in% names(entry)) {
    prices <- dat[, .(date, Open.a, Open.b)]
    setnames(prices, c('date', 'current.a', 'current.b'))
    entry <- merge(entry, prices, by.x='EntryTime', by.y='date')
    entry[, EntryPrice:=ifelse(Side>0, current.a, current.b)]
  }

  # If no entry type, all is 'Market'
  if(!'EntryType' %in% names(entry)) {
    entry[, EntryType:='Market']
  }

  entry[, Order_ID:=1:nrow(entry)]
  # Entry loop ------------------
  # Loop through all the market entry points and get result for each trade
  results <- data.table()
  prevEndDate <- entry[, data.table::first(date)]
  for(iter in 1:nrow(entry)) {
    this.entry <- entry[iter,]
    if(AddToPosition==FALSE) {
      if(isTRUE(this.entry$EntryTime < prevEndDate)) {

        #print('Skipping Order', Trade Open)
        next
      }
    }

    if(CloseTradeOnOppositeSignal) {
      nextOppSignal <- first(entry[EntryTime > this.entry$EntryTime &
                                     sign(OrderSize) == -1 * sign(this.entry$OrderSize),
                                   EntryTime])
      if(length(nextOppSignal) == 0) {
        this.dat <- dat[date>=this.entry$EntryTime &
                          date < (this.entry$EntryTime + TradeTimeLimit)]
      } else {
        this.dat <- dat[date>=this.entry$EntryTime & date < nextOppSignal]
      }
    } else {
      this.dat <- dat[date>=this.entry$EntryTime &
                        date < (this.entry$EntryTime + TradeTimeLimit)]
    }

    this.dat[, Side:=first(this.entry$Side)]

    this.dat[, OrderSize:=this.entry$OrderSize]
    this.dat[date==this.entry$EntryTime, EntryTime:=this.entry$EntryTime]
    this.dat[, EntryPrice:=this.entry$EntryPrice]
    this.dat[, Order_ID:=this.entry$Order_ID]

    # If TP / SL is present
    if('TakeProfit' %in% names(this.dat)) {
      this.dat[, TakeProfit:=this.entry$TakeProfit]
    } else {
      this.dat[, TakeProfit:=Inf * this.entry$Side]
    }
    if('StopLoss' %in% names(this.dat)) {
      this.dat[, StopLoss:=this.entry$StopLoss]
    } else {
      this.dat[, StopLoss:= -Inf * this.entry$Side]
    }

    # Limit/Stop Orders -----------------
    # If the entry type is limit or stop, cut out the data before that price is hit
    if(this.entry$EntryType=='Limit')
    {
      if(this.entry$Side == 1)
      {
        trade_entry_time <- first(this.dat[ Low.a <= this.entry$EntryPrice])$date
      }
      else if(this.entry$Side == -1)
      {
        trade_entry_time <- first(this.dat[ High.b >= this.entry$EntryPrice])$date
      }
      if(length(trade_entry_time) == 0)
      {
        next
      }
      # Special column name to cancel an order if true
      if('CancelOrder' %in% names(this.entry))
      {
        first_cancel_time <- first(this.dat[CancelOrder==TRUE & date>this.entry$EntryTime,
                                            date])
        if(length(first_cancel_time) != 0) {
          if(first_cancel_time <= trade_entry_time)
          {
            next
          }
        }
      }
      this.dat <- this.dat[date >= trade_entry_time]
      this.dat[, EntryTime:=trade_entry_time]
      this.entry[, EntryTime:=trade_entry_time]
    }

    if(this.entry$EntryType=='Stop') {
      if(this.entry$Side == 1) {
        trade_entry_time <- first(this.dat[ High.a >= this.entry$EntryPrice])$date
      } else if(this.entry$Side == -1) {
        trade_entry_time <- first(this.dat[ Low.b <= this.entry$EntryPrice])$date
      }
      if(length(trade_entry_time) == 0) {
        next
      }
      # Special column name to cancel an order if true
      if('CancelOrder' %in% names(this.entry))
      {
        first_cancel_time <- first(this.dat[CancelOrder==TRUE & date>this.entry$EntryTime,
                                            date])
        if(length(first_cancel_time) != 0) {
          if(first_cancel_time <= trade_entry_time)
          {
            next
          }
        }
      }
      this.dat <- this.dat[date >= trade_entry_time]
      this.dat[, EntryTime:=trade_entry_time]
      this.entry[, EntryTime:=trade_entry_time]
    }

    # Close after N candles -----
    if ("CLOSE_CANDLES_SINCE_ENTRY" %in% names(this.dat)) {

      candle_close <- this.entry[1, CLOSE_CANDLES_SINCE_ENTRY]
      if(!is.na(candle_close) && !is.null(candle_close) && candle_close > 0) {
        close_date <- this.dat[!is.na(NextOpen)][candle_close + 1, date]
        this.dat <- this.dat[date < close_date]
      }
    }


    if(nrow(this.dat) <= 2){
      warning(paste0('historical data is not granular enough: ', this.entry$date))
      next
    }

    # Trade Exit -------------------
    if(!is.null(exit_fun)) {
      arg_names <- names(formals(exit_fun))
      args <- c(list(copy(this.dat)), exit_args)
      names(args)[1] <- arg_names[1]
      exit <- rlang::exec('exit_fun', !!!args)
    } else {
      # If no exit function just use the TP/SL values
      # and if they're not there, just the last date
      exit <- exit_TP_SL_signal(this.dat)
    }

    prevEndDate <- last(exit)$ExitTime
    # Duplicate our entry info for every exit line (for partial closes)
    for(exit_rows in 1:nrow(exit)){
      if(exit_rows == 1) next
      this.entry <- rbind(this.entry, this.entry[1])
    }
    thistrade <- cbind(this.entry, exit[, !names(exit) %in% names(this.entry), with=FALSE])
    thistrade[, Returns := Side * (ExitPrice - EntryPrice) * abs(ExitAmount)]
    results <- rbind(results, thistrade, fill=TRUE)
    if(verbose == TRUE) {
      print(paste0('Trade ', thistrade$Order_ID, ': ', round(sum(thistrade$Returns, na.rm=TRUE), 1), ' | Total: ',
                   last(round(cumsum(results[!is.na(Returns)]$Returns), 1))))
    }
  }
  if (NROW(results) > 0) {
    dat <- merge(dat,
                 results[, c('date', names(results)[!names(results) %in% names(dat)]), with=FALSE],
                 all=TRUE)
  }


  return(list(results=results, data=dat))
}

