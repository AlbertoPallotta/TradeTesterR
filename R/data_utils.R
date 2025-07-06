
#' @import data.table
#' @importFrom xts xts
#' @export
xts_to_dt <- function(ts) {
  out <- data.table(date=as.POSIXct(index(ts)),
                    data.table(ts))
  return(out)
}


#' Convert data.table to xts time series
#'
#' @param dt A data.table with date and OHLC columns
#' @return An xts object with OHLC data
#'
#' @importFrom xts xts
#' @importFrom quantmod OHLC
#' @export
dt_to_xts <- function(dt) {
  out <- xts(OHLC(dt), order.by=as.POSIXct(dt$date))
  return(out)
}

#' Calculate timeframe in minutes
#'
#' @param dat A data.table with date column
#' @return Numeric value representing minutes between bars
#'
#' @export
getTimeFrameMins <- function(dat) {
  as.numeric(median(diff(tail(dat$date, 100))), units='mins')
}

#' Get the mode (most frequent value)
#'
#' @param v A vector
#' @return The most frequent value in the vector
#'
#' @export
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Check if the last candle is finished
#'
#' @param dat Time series data
#' @param tp Timeframe string (e.g., "m5", "h1", "d1", "w1")
#' @return Logical indicating if last candle is complete
#'
#' @importFrom xts to.weekly
#' @export
isLastCandleFinished <- function(dat, tp) {
  chr1 <- substring(tp, 1,1)
  chr2 <- substring(tp, 2,3)
  if(nchar(chr2)==0) chr2 <- 1
  chr2 <- as.numeric(chr2)
  if(chr1=='m') {
    if(diff(data.table::minute(tail(index(dat), 2))) == chr2) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  if(chr1=='h') {
    isLastMin <- data.table::minute(tail(index(dat), 1)) == 59
    isLastHour <- diff(data.table::hour(tail(index(dat), 2))) == chr2
    if(!isLastMin | !isLastHour){
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else if(chr1=='d') {

    isLastMin <- data.table::minute(tail(index(dat), 1)) == 59
    # Usually different for Fridays
    thisDay <- data.table::wday(tail(index(dat), 1))
    wdays <- data.table::wday(index(dat))
    thisDaysLastHour <- getMode(tail(data.table::hour(index(dat)[wdays==thisDay]), 20))
    isLastHour <- data.table::hour(tail(index(dat), 1)) >= thisDaysLastHour
    if(!isLastMin | !isLastHour){
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else if(chr1=='w'){
    dat <- to.weekly(ask, k=chr2, indexAt='endof', name=NULL, drop.time=FALSE)
    dayEnd <- getMode(data.table::wday(index(dat)))
    thisDay <- data.table::wday(tail(dat, 1))
    hourEnd <- getMode(data.table::hour(index(dat)))
    thisHour <- data.table::hour(tail(dat, 1))
    minuteEnd <- getMode(data.table::minute(index(dat)))
    thisMin <- data.table::minute(tail(dat, 1))
    if(thisDay != dayEnd | thisHour != hourEnd | thisMin != minuteEnd){
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
}

#' Merge data from different timeframes
#'
#' @param ask Price data
#' @param lgTP Larger timeframe
#' @param smTP Smaller timeframe
#' @param cols Columns to process
#' @param fun Function to apply
#' @param ... Additional arguments
#' @param onlyComplete Only use complete candles
#'
#' @importFrom xts to.minutes to.hourly to.daily to.weekly
##' @importFrom zoo na.locf
#' @export
mergeHighLowTimes <- function(ask, lgTP='h1', smTP='m1', cols='Close',
                              fun, ..., onlyComplete=TRUE) {

  lgTP <- tolower(lgTP)
  smTP <- tolower(smTP)
  chr1 <- substring(lgTP, 1,1)
  chr2 <- substring(lgTP, 2,3)
  if(nchar(chr2)==0) chr2 <- 1
  chr2 <- as.numeric(chr2)
  if(chr1=='m'){
    datLg <- to.minutes(ask, k=chr2, indexAt='endof', name=NULL)
  } else if(chr1=='h') {
    datLg <- to.hourly(ask, k=chr2, indexAt='endof', name=NULL)
  } else if(chr1=='d') {
    datLg <- to.daily(ask, k=chr2, indexAt='endof', name=NULL, drop.time=FALSE)
  } else if(chr1=='w'){
    datLg <- to.weekly(ask, k=chr2, indexAt='endof', name=NULL, drop.time=FALSE)
  }
  # If the large timeframe is partially complete remove candle
  if(onlyComplete==TRUE) {
    if(!isLastCandleFinished(datLg, lgTP)) {
      datLg <- datLg[-nrow(datLg)]
    }
  }
  chr1 <- substring(smTP, 1,1)
  chr2 <- substring(smTP, 2,3)
  if(nchar(chr2)==0) chr2 <- 1
  if(chr1=='m'){
    datSm <- to.minutes(ask, k=chr2, indexAt='startof', name=NULL)
  } else if(chr1=='h') {
    datSm <- to.hourly(ask, k=chr2, indexAt='startof', name=NULL)
  } else if(chr1=='d') {
    datSm <- to.daily(ask, k=chr2, indexAt='startof', name=NULL, drop.time=FALSE)
  } else if(chr1=='w'){
    datSm <- to.weekly(ask, k=chr2, indexAt='startof', name=NULL, drop.time=FALSE)
  }

  out <- fun(datLg[, cols], ...)
  #out <- fun(datLg[, cols], n=10)
  out2 <- merge(datSm, out, all=TRUE)
  out3 <- na.locf(out2, fromLast=FALSE, na.rm=FALSE)
  return(out3)
}



#' Merge ask and bid price data
#'
#' @description
#' Combines ask and bid price data into a single data.table with
#' suffixes .a (ask) and .b (bid).
#'
#' @param ask Ask prices (xts or data.table)
#' @param bid Bid prices (xts or data.table)
#'
#' @return A data.table with merged ask/bid data
#'
#' @import data.table
#' @importFrom xts is.xts
#' @export
mergeAskBid <- function(ask, bid) {
  if(is.xts(ask)) {
    ask <- data.table(date=index(ask), ask)
  }
  if(is.xts(bid)) {
    bid <- data.table(date=index(bid), bid)
  }

  dat <- merge(ask, bid, by='date', suffixes=c('.a', '.b'))
  return(dat)
}
#' Convert xts time series to data.table
#'
#' @param ts An xts time series object
#' @return A data.table with date column and original data
#'
#' @import data.table
#' @export
xts_to_dt <- function(ts) {
  out <- data.table(date=as.POSIXct(index(ts)),
                    data.table(ts))
  return(out)
}
