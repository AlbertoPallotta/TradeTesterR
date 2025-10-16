#' Create 3D surface and 2D heatmap for parameter optimization
#'
#' @description
#' Visualizes optimization results for two parameters, showing how
#' performance metrics vary across the parameter space.
#'
#' @param opt_params Data frame with parameter combinations (must have at least 2 parameters)
#' @param metric_values Numeric vector of metric values corresponding to each parameter combination
#' @param param1 Name of first parameter (x-axis)
#' @param param2 Name of second parameter (y-axis)
#' @param metric_name Name of the metric being plotted (default: "Net Profit")
#'
#' @return A list containing:
#'   - surface: 3D surface plot (plotly object)
#'   - heatmap: 2D heatmap (plotly object)
#'
#' @examples
#' \dontrun{
#' # After optimization
#' first_window <- optimization_output$optimization_history[[1]]
#'
#' # Plot optimization surface
#' plots <- plotMultiParameter(first_window,
#'                           first_window$net_profit,
#'                           "fast_ma", "slow_ma",
#'                           "Net Profit")
#'
#' # Display plots
#' plots$surface
#' plots$heatmap
#' }
#' @export
plotMultiParameter <- function(opt_params, metric_values, param1, param2, metric_name = "Net Profit") {

  # Create matrix for surface plot
  unique_param1 <- sort(unique(opt_params[[param1]]))
  unique_param2 <- sort(unique(opt_params[[param2]]))

  # Create empty matrix
  z_matrix <- matrix(NA,
                     nrow = length(unique_param2),
                     ncol = length(unique_param1))

  # Fill matrix with metric values
  for(i in 1:nrow(opt_params)) {
    row_idx <- which(unique_param2 == opt_params[[param2]][i])
    col_idx <- which(unique_param1 == opt_params[[param1]][i])
    z_matrix[row_idx, col_idx] <- metric_values[i]
  }

  # 3D Surface Plot
  p3d <- plot_ly(x = unique_param1,
                 y = unique_param2,
                 z = z_matrix,
                 type = 'surface',
                 colorscale = 'Viridis') %>%
    plotly::layout(title = "3D Optimization Surface",
           scene = list(
             xaxis = list(title = param1),
             yaxis = list(title = param2),
             zaxis = list(title = metric_name)
           ))

  # 2D Heatmap
  p2d <- plot_ly(x = unique_param1,
                 y = unique_param2,
                 z = z_matrix,
                 type = 'heatmap',
                 colorscale = 'Viridis') %>%
    layout(title = "2D Optimization Heatmap",
           xaxis = list(title = param1),
           yaxis = list(title = param2))

  return(list(surface = p3d, heatmap = p2d))
}







#' Plot equity curve from backtest results
#'
#' @description
#' Creates an interactive plot showing equity evolution and drawdown over time.
#' Displays both the cumulative equity line and the drawdown percentage in a
#' two-panel subplot layout.
#'
#' @param bt Backtest object returned from backtest() function containing a
#'   'results' component with trade data
#' @param initial_equity Starting capital for the strategy (default: 10000)
#'
#' @return A plotly object with equity curve and drawdown subplot
#'
#' @examples
#' \dontrun{
#' # Run backtest first
#' bt <- backtest(strategy_data)
#'
#' # Plot equity curve
#' plotEquityCurve(bt, initial_equity = 10000)
#' }
#'
#' @import plotly
#' @import data.table
#' @export
plotEquityCurve <- function(bt, initial_equity = 10000) {
  # Validate input
  if(!"results" %in% names(bt)) {
    stop("bt must be a backtest object with 'results' component")
  }

  # Get results and ensure chronological order
  res <- bt$results[!is.na(Returns)]

  if(nrow(res) == 0) {
    stop("No completed trades found in backtest results")
  }

  res <- res[order(ExitTime)]

  # Calculate equity and drawdown
  res[, CumReturns := cumsum(Returns)]
  res[, Equity := initial_equity + CumReturns]
  res[, Peak := cummax(Equity)]
  res[, Drawdown := (Equity - Peak) / Peak * 100]

  # Create subplot with equity and drawdown
  p1 <- plot_ly(res, x = ~ExitTime, y = ~Equity,
                type = 'scatter', mode = 'lines',
                name = 'Equity', line = list(color = 'blue')) %>%
    layout(yaxis = list(title = "Equity ($)"))

  p2 <- plot_ly(res, x = ~ExitTime, y = ~Drawdown,
                type = 'scatter', mode = 'lines',
                fill = 'tozeroy', fillcolor = 'rgba(255,0,0,0.2)',
                line = list(color = 'red'),
                name = 'Drawdown %') %>%
    layout(yaxis = list(title = "Drawdown (%)"))

  # Combine plots
  subplot(p1, p2, nrows = 2, shareX = TRUE, heights = c(0.7, 0.3)) %>%
    layout(title = "Strategy Performance",
           xaxis = list(title = "Date"),
           showlegend = FALSE)
}
#' Plot trade returns distribution
#'
#' @description
#' Creates a comprehensive visualization of trade returns including histogram
#' and returns over time bar chart.
#'
#' @param bt Backtest object returned from backtest() function
#'
#' @return A plotly object with two subplots showing returns analysis
#'
#' @examples
#' \dontrun{
#' bt <- backtest(strategy_data)
#' plotReturnsDistribution(bt)
#' }
#'
#' @import plotly
#' @import data.table
#' @export
plotReturnsDistribution <- function(bt) {
  # Validate input
  if(!"results" %in% names(bt)) {
    stop("bt must be a backtest object with 'results' component")
  }

  res <- bt$results[!is.na(Returns)]

  if(nrow(res) == 0) {
    stop("No completed trades found in backtest results")
  }

  # 1. Histogram
  p1 <- plot_ly(x = ~res$Returns, type = 'histogram',
                nbinsx = 30, name = 'Returns',
                marker = list(color = 'lightblue',
                              line = list(color = 'darkblue', width = 1))) %>%
    layout(xaxis = list(title = "Return ($)"),
           yaxis = list(title = "Frequency"))

  # 2. Returns over time
  p2 <- plot_ly(res, x = ~ExitTime, y = ~Returns,
                type = 'bar',
                marker = list(color = ~ifelse(Returns > 0, 'green', 'red'))) %>%
    layout(xaxis = list(title = "Date"),
           yaxis = list(title = "Return ($)"))

  # Combine the two plots
  subplot(p1, p2,
          nrows = 2,
          heights = c(0.5, 0.5),
          titleY = TRUE) %>%
    layout(title = "Returns Distribution Analysis",
           showlegend = FALSE)
}


#' Display performance metrics summary table
#'
#' @description
#' Calculates and displays key performance metrics including total P&L, win rate,
#' profit factor, drawdown, and Sharpe ratio in an interactive table format.
#'
#' @param bt Backtest object returned from backtest() function
#' @param initial_equity Starting capital for calculating percentages (default: 10000)
#'
#' @return A plotly table object with performance metrics
#'
#' @examples
#' \dontrun{
#' bt <- backtest(strategy_data)
#' plotPerformanceTable(bt, initial_equity = 10000)
#' }
#'
#' @import plotly
#' @import data.table
#' @export
plotPerformanceTable <- function(bt, initial_equity = 10000) {
  # Validate input
  if(!"results" %in% names(bt)) {
    stop("bt must be a backtest object with 'results' component")
  }

  res <- bt$results[!is.na(Returns)]

  if(nrow(res) == 0) {
    warning("No completed trades found in backtest results")
    # Return empty table
    metrics <- data.frame(
      Metric = c("No trades found"),
      Value = c("N/A")
    )
  } else {
    # Calculate metrics
    total_pnl <- sum(res$Returns, na.rm = TRUE)
    n_trades <- nrow(res)
    win_rate <- mean(res$Returns > 0) * 100

    # Profit factor
    wins <- sum(res[Returns > 0, Returns])
    losses <- abs(sum(res[Returns < 0, Returns]))
    profit_factor <- if(losses > 0) wins/losses else NA

    # Drawdown
    res <- res[order(ExitTime)]
    res[, Equity := initial_equity + cumsum(Returns)]
    res[, Peak := cummax(Equity)]
    res[, DD := Equity - Peak]
    max_dd <- min(res$DD)
    max_dd_pct <- (max_dd / max(res$Peak)) * 100

    # Average trade
    avg_win <- if(any(res$Returns > 0)) mean(res[Returns > 0, Returns]) else 0
    avg_loss <- if(any(res$Returns < 0)) mean(res[Returns < 0, Returns]) else 0

    # Sharpe (simplified - daily returns)
    if(n_trades > 1) {
      daily_returns <- res$Returns / initial_equity
      sharpe <- mean(daily_returns) / sd(daily_returns) * sqrt(252)
    } else {
      sharpe <- NA
    }

    # Create metrics table
    metrics <- data.frame(
      Metric = c("Total P&L", "Number of Trades", "Win Rate",
                 "Profit Factor", "Max Drawdown", "Max DD %",
                 "Avg Win", "Avg Loss", "Sharpe Ratio"),
      Value = c(
        sprintf("$%.2f", total_pnl),
        as.character(n_trades),
        sprintf("%.1f%%", win_rate),
        sprintf("%.2f", ifelse(is.na(profit_factor), 0, profit_factor)),
        sprintf("$%.2f", max_dd),
        sprintf("%.1f%%", max_dd_pct),
        sprintf("$%.2f", avg_win),
        sprintf("$%.2f", avg_loss),
        sprintf("%.2f", ifelse(is.na(sharpe), 0, sharpe))
      )
    )
  }

  # Create table
  plot_ly(
    type = 'table',
    header = list(
      values = c("Metric", "Value"),
      fill = list(color = '#1f77b4'),
      align = 'left',
      font = list(color = 'white', size = 14)
    ),
    cells = list(
      values = list(metrics$Metric, metrics$Value),
      fill = list(color = c('#f0f0f0', '#ffffff')),
      align = 'left',
      font = list(size = 12)
    )
  ) %>%
    layout(title = "Performance Metrics")
}




#' Plot returns histogram
#'
#' @description
#' Creates a histogram of trade returns
#'
#' @param bt Backtest object returned from backtest() function
#'
#' @return A plotly histogram
#'
#' @export
plotReturnsHistogram <- function(bt) {
  if(!"results" %in% names(bt)) {
    stop("bt must be a backtest object with 'results' component")
  }

  res <- bt$results[!is.na(Returns)]

  if(nrow(res) == 0) {
    stop("No completed trades found in backtest results")
  }

  plot_ly(x = ~res$Returns, type = 'histogram',
          nbinsx = 30, name = 'Returns',
          marker = list(color = 'lightblue',
                        line = list(color = 'darkblue', width = 1))) %>%
    layout(title = "Returns Distribution",
           xaxis = list(title = "Return ($)"),
           yaxis = list(title = "Frequency"))
}

#' Plot win/loss pie chart
#'
#' @description
#' Creates a pie chart showing the distribution of winning and losing trades
#'
#' @param bt Backtest object returned from backtest() function
#'
#' @return A plotly pie chart
#'
#' @export
plotWinLossPie <- function(bt) {
  if(!"results" %in% names(bt)) {
    stop("bt must be a backtest object with 'results' component")
  }

  res <- bt$results[!is.na(Returns)]

  if(nrow(res) == 0) {
    stop("No completed trades found in backtest results")
  }

  win_loss <- data.frame(
    Category = c("Wins", "Losses", "Breakeven"),
    Count = c(sum(res$Returns > 0),
              sum(res$Returns < 0),
              sum(res$Returns == 0)),
    Percentage = c(
      round(sum(res$Returns > 0)/nrow(res)*100, 1),
      round(sum(res$Returns < 0)/nrow(res)*100, 1),
      round(sum(res$Returns == 0)/nrow(res)*100, 1)
    )
  )

  plot_ly(win_loss,
          labels = ~paste0(Category, "\n", Percentage, "%"),
          values = ~Count,
          type = 'pie',
          marker = list(colors = c('green', 'red', 'gray')),
          textinfo = 'label+value') %>%
    layout(title = "Win/Loss Distribution")
}

#' Plot returns over time
#'
#' @description
#' Creates a bar chart of returns over time
#'
#' @param bt Backtest object returned from backtest() function
#'
#' @return A plotly bar chart
#'
#' @export
plotReturnsTimeline <- function(bt) {
  if(!"results" %in% names(bt)) {
    stop("bt must be a backtest object with 'results' component")
  }

  res <- bt$results[!is.na(Returns)]

  if(nrow(res) == 0) {
    stop("No completed trades found in backtest results")
  }

  plot_ly(res, x = ~ExitTime, y = ~Returns,
          type = 'bar',
          marker = list(color = ~ifelse(Returns > 0, 'green', 'red'))) %>%
    layout(title = "Returns Over Time",
           xaxis = list(title = "Date"),
           yaxis = list(title = "Return ($)"))
}

#' Plot individual trades on price chart
#'
#' @description
#' Visualizes specific trades overlaid on candlestick price chart with
#' technical indicators, entry/exit points, and connecting lines.
#'
#' @param bt Backtest object returned from backtest() function
#' @param price_data Price data.table with OHLC and calculated indicators
#' @param trades Vector of trade numbers to plot (default: last 5 trades)
#' @param indicators Character vector of indicator column names to display
#'
#' @return A plotly candlestick chart with trades and indicators
#'
#' @examples
#' \dontrun{
#' bt <- backtest(strategy_data)
#' # Plot last 3 trades with Bollinger Bands
#' plotTrades(bt, price, trades = c(1,2,3),
#'            indicators = c("upperBB", "lowerBB", "midBB"))
#' }
#'
#' @import plotly
#' @import data.table
#' @importFrom lubridate days
#' @export
plotTrades <- function(bt, price_data, trades = NULL, indicators = NULL) {
  # Validate input
  if(!"results" %in% names(bt)) {
    stop("bt must be a backtest object with 'results' component")
  }

  res <- bt$results[!is.na(Returns)]

  if(nrow(res) == 0) {
    stop("No completed trades found in backtest results")
  }

  # Default to last 5 trades or available trades
  if(is.null(trades)) {
    trades <- tail(1:nrow(res), min(5, nrow(res)))
  }

  # Validate trade numbers
  if(any(trades > nrow(res))) {
    stop("Trade numbers exceed available trades")
  }

  # Get trades data
  trade_data <- res[trades]

  # Get time window
  start_date <- min(trade_data$EntryTime) - lubridate::days(10)
  end_date   <- max(trade_data$ExitTime)  + lubridate::days(10)

  # Filter price data
  plot_data <- price_data[date >= start_date & date <= end_date]

  # Create candlestick chart
  p <- plot_ly(plot_data) %>%
    add_trace(x = ~date, type = 'candlestick',
              open = ~Open, close = ~Close,
              high = ~High, low = ~Low,
              name = "Price")

  # Add indicators if specified
  if(!is.null(indicators)) {
    for(ind in indicators) {
      if(ind %in% names(plot_data)) {
        p <- p %>%
          add_trace(x = ~date, y = ~get(ind),
                    type = 'scatter', mode = 'lines',
                    name = ind, line = list(width = 1))
      }
    }
  }

  # Add trade entries and exits
  for(i in 1:nrow(trade_data)) {
    trade <- trade_data[i]

    # Entry point
    p <- p %>%
      add_trace(x = trade$EntryTime, y = trade$EntryPrice,
                type = 'scatter', mode = 'markers',
                marker = list(size = 10, color = 'green', symbol = 'triangle-up'),
                name = paste("Entry", trade$Order_ID))

    # Exit point
    p <- p %>%
      add_trace(x = trade$ExitTime, y = trade$ExitPrice,
                type = 'scatter', mode = 'markers',
                marker = list(size = 10, color = 'red', symbol = 'triangle-down'),
                name = paste("Exit", trade$Order_ID))

    # Connect entry to exit
    p <- p %>%
      add_trace(x = c(trade$EntryTime, trade$ExitTime),
                y = c(trade$EntryPrice, trade$ExitPrice),
                type = 'scatter', mode = 'lines',
                line = list(color = ifelse(trade$Returns > 0, 'green', 'red'),
                            dash = 'dot', width = 1),
                showlegend = FALSE)
  }

  p %>% layout(
    title = "Trades on Chart",
    xaxis = list(title = "Date", rangeslider = list(visible = FALSE)),
    yaxis = list(title = "Price"),
    showlegend = TRUE
  )
}

#' Plot a single trade with indicators
#'
#' @description
#' Visualizes a specific trade on a candlestick chart with entry/exit points
#' and technical indicators used in the strategy.
#'
#' @param bt Backtest object returned from backtest() function
#' @param price_data Price data.table with OHLC and calculated indicators
#' @param trade_num Trade number to visualize (default: 1)
#' @param window_days Days before/after trade to show (default: 5)
#' @param indicators List of indicators to plot (auto-detects if NULL)
#'
#' @return A plotly candlestick chart showing the trade
#'
#' @examples
#' \dontrun{
#' bt <- backtest(strategy_data)
#'
#' # Plot first trade
#' plotSingleTrade(bt, price, 1)
#'
#' # Plot trade 5 with 10-day window
#' plotSingleTrade(bt, price, 5, window_days = 10)
#'
#' # Plot with specific indicators
#' plotSingleTrade(bt, price, 1, indicators = c("SMA20", "SMA50"))
#' }
#'
#' @import plotly
#' @import data.table
#' @importFrom lubridate days
#' @export
plotSingleTrade <- function(bt, price_data, trade_num = 1, window_days = 5, indicators = NULL) {

  # Validate inputs
  if(!"results" %in% names(bt)) {
    stop("bt must be a backtest object with 'results' component")
  }

  pdat <- bt$results[!is.na(Returns)]

  if(nrow(pdat) == 0) {
    stop("No completed trades found in backtest results")
  }

  if(trade_num > nrow(pdat) || trade_num < 1) {
    stop(paste("Invalid trade number. Please select between 1 and", nrow(pdat)))
  }

  # Get the specific trade
  trade <- pdat[trade_num]

  # Get data window around trade
  candle_dat <- price_data[date >= (trade$EntryTime - lubridate::days(window_days)) &
                             date <= (trade$ExitTime  + lubridate::days(window_days))]

  if(nrow(candle_dat) == 0) {
    stop("No price data available for the specified trade window")
  }

  # Auto-detect indicators if not specified
  if(is.null(indicators)) {
    # Common indicator patterns to look for
    possible_indicators <- c("upperBB", "lowerBB", "midBB", "SMA", "EMA", "RSI",
                             "MACD", "Signal", "ATR", "upperKC", "lowerKC")
    indicators <- names(candle_dat)[names(candle_dat) %in% possible_indicators]
  }

  # Create base candlestick chart
  p <- plot_ly(candle_dat) %>%
    add_trace(x = ~date, type = 'candlestick',
              open = ~Open, close = ~Close, high = ~High, low = ~Low,
              name = "Price", increasing = list(line = list(color = 'green')),
              decreasing = list(line = list(color = 'red')))

  # Add indicators if they exist
  for(ind in indicators) {
    if(ind %in% names(candle_dat)) {
      # Special styling for Bollinger Bands
      if(ind == "upperBB") {
        line_style <- list(color = 'red', dash = 'dot', width = 1)
      } else if(ind == "lowerBB") {
        line_style <- list(color = 'blue', dash = 'dot', width = 1)
      } else if(ind == "midBB") {
        line_style <- list(color = 'orange', dash = 'dot', width = 1)
      } else {
        line_style <- list(color = 'gray', width = 1)
      }

      p <- p %>%
        add_trace(x = candle_dat$date, y = candle_dat[[ind]],
                  type = 'scatter', mode = 'lines',
                  name = ind, line = line_style)
    }
  }

  # Add entry point
  p <- p %>%
    add_trace(x = trade$EntryTime, y = trade$EntryPrice,
              type = 'scatter', mode = 'markers+text',
              marker = list(size = 12, color = 'green',
                            symbol = 'triangle-up'),
              text = paste("Entry:", round(trade$EntryPrice, 2)),
              textposition = "top right",
              name = "Entry")

  # Add exit point
  exit_color <- ifelse(trade$Returns > 0, 'blue', 'red')
  p <- p %>%
    add_trace(x = trade$ExitTime, y = trade$ExitPrice,
              type = 'scatter', mode = 'markers+text',
              marker = list(size = 12, color = exit_color,
                            symbol = 'triangle-down'),
              text = paste("Exit:", round(trade$ExitPrice, 2)),
              textposition = "bottom right",
              name = "Exit")

  # Connect entry to exit with a dashed line
  p <- p %>%
    add_trace(x = c(trade$EntryTime, trade$ExitTime),
              y = c(trade$EntryPrice, trade$ExitPrice),
              type = 'scatter', mode = 'lines',
              line = list(color = ifelse(trade$Returns > 0, 'green', 'red'),
                          dash = 'dash', width = 2),
              name = ifelse(trade$Returns > 0, "Profit", "Loss"),
              showlegend = FALSE)

  # Add stop loss and take profit lines if they exist
  if("StopLoss" %in% names(trade) && !is.na(trade$StopLoss) && !is.infinite(trade$StopLoss)) {
    p <- p %>%
      add_trace(x = candle_dat$date,
                y = rep(trade$StopLoss, nrow(candle_dat)),
                type = 'scatter', mode = 'lines',
                line = list(color = 'red', dash = 'dash', width = 0.5),
                name = "Stop Loss")
  }

  if("TakeProfit" %in% names(trade) && !is.na(trade$TakeProfit) && !is.infinite(trade$TakeProfit)) {
    p <- p %>%
      add_trace(x = candle_dat$date,
                y = rep(trade$TakeProfit, nrow(candle_dat)),
                type = 'scatter', mode = 'lines',
                line = list(color = 'green', dash = 'dash', width = 0.5),
                name = "Take Profit")
  }

  # Create informative title
  profit_loss_text <- ifelse(trade$Returns > 0, "Profit", "Loss")
  title_text <- sprintf("Trade #%d | %s: $%.2f | Exit: %s | %d %s",
                        trade_num,
                        profit_loss_text,
                        abs(trade$Returns),
                        trade$ExitReason,
                        trade$Side,
                        ifelse(trade$Side == 1, "Long", "Short"))

  # Final layout
  p %>% layout(
    title = title_text,
    xaxis = list(title = "Date",
                 rangeslider = list(visible = FALSE)),
    yaxis = list(title = "Price"),
    showlegend = TRUE,
    hovermode = 'x unified'
  )
}

#' Simple cumulative returns plot
#'
#' @description
#' Creates a basic line plot of cumulative returns over time.
#' Useful for quick strategy performance assessment.
#'
#' @param bt Backtest object returned from backtest() function
#'
#' @return A plotly line chart showing cumulative returns
#'
#' @examples
#' \dontrun{
#' bt <- backtest(strategy_data)
#' plotCumReturns(bt)
#' }
#'
#' @import plotly
#' @import data.table
#' @export
plotCumReturns <- function(bt) {
  # Validate input
  if(!"results" %in% names(bt)) {
    stop("bt must be a backtest object with 'results' component")
  }

  res <- bt$results[!is.na(Returns)]

  if(nrow(res) == 0) {
    stop("No completed trades found in backtest results")
  }

  res <- res[order(ExitTime)]

  plot_ly(res, x = ~ExitTime, y = ~cumsum(Returns),
          type = 'scatter', mode = 'lines',
          line = list(color = 'blue'),
          name = 'Cumulative Returns') %>%
    layout(title = "Strategy Performance",
           xaxis = list(title = "Date"),
           yaxis = list(title = "Cumulative Returns ($)"))
}

#' Create a comprehensive performance dashboard
#'
#' @description
#' Combines equity curve, returns analysis, and performance metrics
#' into a single HTML dashboard that opens in the browser.
#'
#' @param bt Backtest object returned from backtest() function
#' @param initial_equity Starting capital (default: 10000)
#'
#' @return Opens an HTML page with interactive dashboard in the browser
#'
#' @export
plotDashboard <- function(bt, initial_equity = 10000) {
  # Validate input
  if(!"results" %in% names(bt)) {
    stop("bt must be a backtest object with 'results' component")
  }

  # Generate all plots
  p1 <- plotEquityCurve(bt, initial_equity)
  p2 <- plotReturnsHistogram(bt)
  p3 <- plotWinLossPie(bt)
  p4 <- plotReturnsTimeline(bt)
  p5 <- plotPerformanceTable(bt, initial_equity)

  # Combine into dashboard
  dashboard <- tagList(
    h1("TradeTesterR Backtest Results"),
    p1,
    br(),
    p2,
    br(),
    p3,
    br(),
    p4,
    br(),
    p5
  )

  # Display
  browsable(dashboard)
}
