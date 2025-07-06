


#' Optimize strategy parameters using walk-forward analysis
#'
#' @description
#' Performs walk-forward optimization by training on historical windows
#' and testing on subsequent out-of-sample periods.
#'
#' @param price A data.table with columns: date, Open, High, Low, Close, Volume
#' @param opt_params A data.frame where each row is a parameter combination to test
#' @param initial_equity Starting capital for performance calculations (default: 1000)
#' @param optimize_on Metric to optimize: "net_profit", "profit_factor", "sharpe", "sortino" (default: "net_profit")
#' @param train_set_weeks Number of weeks for training window (default: 52)
#' @param test_set_weeks Number of weeks for testing window (default: 26)
#' @param return_optimization_details Logical. Return detailed optimization history? (default: TRUE)
#'
#' @return If return_optimization_details = TRUE, returns a list with:
#'   - test_results: List of backtest results for each test window
#'   - optimization_history: List of all parameter performances in each training window
#'
#' If FALSE, returns only test_results (backward compatibility)
#'
#' @examples
#' \dontrun{
#' # Define parameter grid
#' opt_params <- expand.grid(
#'   fast_ma = c(10, 20, 30),
#'   slow_ma = c(50, 100, 200),
#'   stop_loss = c(0.02, 0.03)
#' )
#'
#' # Run optimization
#' results <- optimizeStrategy(price_data, opt_params,
#'                           initial_equity = 10000,
#'                           optimize_on = "sharpe")
#' }
#' @import data.table
#' @importFrom lubridate weeks
#' @importFrom tidyquant tq_get
#' @export
optimizeStrategy <- function(price, opt_params, initial_equity = 1000,
                             optimize_on = "net_profit",
                             train_set_weeks = 52, test_set_weeks = 26,
                             return_optimization_details = TRUE) {

  dates <- price$date
  train_start <- min(dates)
  risk_free_rate <- data.table(tq_get("^TNX"))

  bt_test <- list()
  optimization_history <- list()  # NEW: Store optimization details

  while (train_start < max(dates)) {
    train_end <- train_start + lubridate::weeks(train_set_weeks)
    test_start <- train_end
    test_end <- test_start + lubridate::weeks(test_set_weeks)

    train_set <- price[date >= train_start & date < train_end]
    test_set <- price[date >= test_start & date < test_end]

    if (NROW(test_set) < 100) break

    print(paste0("Starting optimization for ", train_start, " to ", train_end))

    results <- data.table()
    for (iter in 1:NROW(opt_params)) {
      print(paste("Parameter set ", iter, "of", nrow(opt_params)))

      args <- list()
      args[[1]] <- train_set
      args <- c(args, lapply(opt_params, function(xx) xx[iter]))
      bt <- do.call(my_strat, args)

      out <- calculateTradeMetrics(bt, initial_equity, risk_free_rate,
                                   train_start, train_end)
      results <- rbind(results, cbind(data.table(opt_params)[iter, ], out))
    }

    # NEW: Store the optimization results for this window
    optimization_history[[as.character(train_start)]] <- results

    print("Results on training set:")
    print(results)

    best <- results[which.max(get(optimize_on)), ]

    cat(paste("Test set with params:\n",
                paste0(best[1, 1:ncol(opt_params)], collapse = ", "), "\n"))

    args2 <- list()
    args2[[1]] <- test_set
    args2 <- c(args2, lapply(best[, 1:ncol(opt_params)], function(xx) xx))

    bt <- do.call(my_strat, args2)
    if (NROW(bt$results) == 0) {
      cat("No Trades for this test set\n")
      bt_test[[as.character(test_start)]] <- data.table()
    } else {
      bt_test[[as.character(test_start)]] <- bt

      out <- calculateTradeMetrics(bt, initial_equity, risk_free_rate,
                                   test_start, test_end)
      print(out)
    }

    train_start <- train_start + lubridate::weeks(test_set_weeks)
  }

  # At the end of the function:
  if(return_optimization_details) {
    # New behavior: return extended output
    return(list(
      test_results = bt_test,
      optimization_history = optimization_history
    ))
  } else {
    # Original behavior: return only bt_test (default)
    return(bt_test)
  }
}

#' Analyze parameter stability across walk-forward windows
#'
#' @description
#' Examines which parameters were selected in each optimization window
#' to assess consistency and robustness of parameter selection.
#'
#' @param optimization_output Output from optimizeStrategy() with return_optimization_details = TRUE
#' @param optimize_on The metric that was used for optimization (default: "net_profit")
#'
#' @return A list containing:
#'   - selected_parameters: data.table showing selected parameters for each window
#'   - stability_metrics: data.table with stability statistics for each parameter
#'   - plots: List of plotly visualizations showing parameter evolution
#'   - summary: Summary statistics including most/least stable parameters
#'
#' @examples
#' \dontrun{
#' # First run optimization
#' opt_results <- optimizeStrategy(price_data, opt_params)
#'
#' # Analyze parameter stability
#' stability <- analyzeParameterStability(opt_results, optimize_on = "sharpe")
#'
#' # View stability metrics
#' print(stability$stability_metrics)
#'
#' # Display parameter evolution plot
#' stability$plots$combined
#' }
#' @import data.table
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom stats sd median
#' @export
analyzeParameterStability <- function(optimization_output, optimize_on = "net_profit") {

  # Check if we have the required structure
  if (!is.list(optimization_output) ||
      !"test_results" %in% names(optimization_output) ||
      !"optimization_history" %in% names(optimization_output)) {
    stop("ERROR: optimization_output must contain both 'test_results' and 'optimization_history'.\n",
         "Please re-run optimizeStrategy with return_optimization_details = TRUE")
  }

  # Extract optimization history and test results
  opt_history <- optimization_output$optimization_history
  test_results <- optimization_output$test_results

  # Verify optimization history is not NULL or empty
  if (is.null(opt_history) || length(opt_history) == 0) {
    stop("ERROR: optimization_history is empty.\n",
         "Please re-run optimizeStrategy with return_optimization_details = TRUE")
  }

  # Identify parameter columns and performance metrics
  first_window <- opt_history[[1]]
  metric_cols <- c("profit_factor", "net_profit", "sharpe", "sortino")
  param_cols <- names(first_window)[!names(first_window) %in% metric_cols]

  # Extract selected parameters for each window
  selected_params <- data.table()

  for(i in seq_along(opt_history)) {
    window_name <- names(opt_history)[i]
    window_data <- opt_history[[i]]

    # Find the best parameter set based on the optimization metric
    if (optimize_on %in% names(window_data)) {
      best_idx <- which.max(window_data[[optimize_on]])
    } else {
      warning(paste("Optimization metric", optimize_on, "not found. Using net_profit."))
      best_idx <- which.max(window_data$net_profit)
    }

    if(length(best_idx) > 0) {
      best_params <- window_data[best_idx, param_cols, with = FALSE]
      best_params[, Window := as.Date(window_name)]
      best_params[, InSampleMetric := window_data[best_idx][[optimize_on]]]
      best_params[, WindowIndex := i]

      selected_params <- rbind(selected_params, best_params, fill = TRUE)
    }
  }

  # Calculate stability metrics for each parameter
  stability_metrics <- data.table()

  for(param in param_cols) {
    if(param %in% names(selected_params) && is.numeric(selected_params[[param]])) {
      values <- selected_params[[param]]

      # Calculate coefficient of variation only if mean is not zero
      mean_val <- mean(values, na.rm = TRUE)
      cv_val <- if(abs(mean_val) > 0.001) {
        sd(values, na.rm = TRUE) / abs(mean_val)
      } else {
        NA
      }

      param_stats <- data.table(
        Parameter = param,
        Mean = mean_val,
        Median = median(values, na.rm = TRUE),
        SD = sd(values, na.rm = TRUE),
        CV = cv_val,
        Min = min(values, na.rm = TRUE),
        Max = max(values, na.rm = TRUE),
        Range = max(values, na.rm = TRUE) - min(values, na.rm = TRUE),
        UniqueValues = length(unique(values)),
        Changes = sum(diff(values) != 0, na.rm = TRUE),
        ConsistencyScore = 1 - (length(unique(values)) / length(values))
      )

      stability_metrics <- rbind(stability_metrics, param_stats)
    }
  }

  # Create visualization of parameter evolution
  setorder(selected_params, Window)

  plots <- list()
  for(param in param_cols) {
    if(param %in% names(selected_params) && is.numeric(selected_params[[param]])) {
      p <- plot_ly(selected_params, x = ~Window, y = ~get(param),
                   type = 'scatter', mode = 'lines+markers',
                   marker = list(size = 10),
                   line = list(width = 2),
                   name = param) %>%
        layout(title = paste("Evolution of", param, "Across Walk-Forward Windows"),
               xaxis = list(title = "Optimization Window Start Date"),
               yaxis = list(title = param),
               hovermode = 'x unified')

      plots[[param]] <- p
    }
  }

  # Create a combined plot if multiple parameters
  if(length(param_cols) > 1) {
    # Normalize parameters to 0-1 scale for comparison
    selected_params_normalized <- copy(selected_params)
    for(param in param_cols) {
      if(param %in% names(selected_params_normalized) && is.numeric(selected_params_normalized[[param]])) {
        vals <- selected_params_normalized[[param]]
        min_val <- min(vals, na.rm = TRUE)
        max_val <- max(vals, na.rm = TRUE)
        if(max_val > min_val) {
          selected_params_normalized[, (param) := (vals - min_val) / (max_val - min_val)]
        }
      }
    }

    # Create combined plot
    p_combined <- plot_ly()
    for(param in param_cols) {
      if(param %in% names(selected_params_normalized) && is.numeric(selected_params_normalized[[param]])) {
        p_combined <- p_combined %>%
          add_trace(data = selected_params_normalized,
                    x = ~Window,
                    y = ~get(param),
                    type = 'scatter',
                    mode = 'lines+markers',
                    name = param)
      }
    }
    p_combined <- p_combined %>%
      layout(title = "Normalized Parameter Evolution (All Parameters)",
             xaxis = list(title = "Optimization Window Start Date"),
             yaxis = list(title = "Normalized Value (0-1)"),
             hovermode = 'x unified')

    plots[["combined"]] <- p_combined
  }

  return(list(
    selected_parameters = selected_params,
    stability_metrics = stability_metrics,
    plots = plots,
    summary = list(
      n_windows = nrow(selected_params),
      most_stable_param = stability_metrics[which.min(CV)]$Parameter,
      least_stable_param = stability_metrics[which.max(CV)]$Parameter
    )
  ))
}





#' Compare in-sample vs out-of-sample performance
#'
#' @description
#' Analyzes the degradation between optimization (in-sample) performance
#' and actual (out-of-sample) results to assess overfitting.
#'
#' @param optimization_output Output from optimizeStrategy() with return_optimization_details = TRUE
#' @param optimize_on The metric that was used for optimization (default: "net_profit")
#'
#' @return A list containing:
#'   - performance_comparison: data.table comparing IS and OOS metrics for each window
#'   - summary_stats: Summary statistics including average degradation and correlation
#'   - plots: List of visualizations (scatter plot, time series, degradation histogram)
#'   - recommendation: Text recommendation based on the analysis
#'
#' @examples
#' \dontrun{
#' # Analyze IS vs OOS performance
#' is_oos <- compareISOS(opt_results)
#'
#' # View summary
#' print(is_oos$summary_stats)
#'
#' # Plot IS vs OOS scatter
#' is_oos$plots$scatter
#' }
#' @import data.table
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom stats cor median
#' @export
compareISOS <- function(optimization_output, optimize_on = "net_profit") {

  # Check if we have the required structure
  if (!is.list(optimization_output) ||
      !"test_results" %in% names(optimization_output) ||
      !"optimization_history" %in% names(optimization_output)) {
    stop("ERROR: optimization_output must contain both 'test_results' and 'optimization_history'.\n",
         "Please re-run optimizeStrategy with return_optimization_details = TRUE")
  }

  opt_history <- optimization_output$optimization_history
  test_results <- optimization_output$test_results

  # Verify optimization history is not NULL or empty
  if (is.null(opt_history) || length(opt_history) == 0) {
    stop("ERROR: optimization_history is empty.\n",
         "Please re-run optimizeStrategy with return_optimization_details = TRUE")
  }

  # Extract IS and OOS performance for each window
  performance_comparison <- data.table()

  for(i in seq_along(opt_history)) {
    if (i > length(test_results)) {
      warning(paste("Window", i, "has optimization history but no test results"))
      next
    }

    window_name <- names(opt_history)[i]

    # Wrap in tryCatch to handle errors gracefully
    tryCatch({
      is_data <- opt_history[[i]]

      # Get the best in-sample performance
      if (optimize_on %in% names(is_data)) {
        best_is_idx <- which.max(is_data[[optimize_on]])
        best_is_performance <- is_data[best_is_idx][[optimize_on]]
      } else {
        warning(paste("Optimization metric", optimize_on, "not found. Using net_profit."))
        best_is_idx <- which.max(is_data$net_profit)
        best_is_performance <- is_data[best_is_idx]$net_profit
      }

      if(length(best_is_idx) > 0) {
        # Get corresponding out-of-sample performance
        test_window <- test_results[[i]]

        if(!is.null(test_window$results)) {
          oos_trades <- test_window$results[!is.na(Returns)]

          if(nrow(oos_trades) > 0) {
            # Calculate OOS metrics
            oos_net_profit <- sum(oos_trades$Returns, na.rm = TRUE)

            # Calculate other metrics
            winning_trades <- sum(oos_trades$Returns > 0)
            total_trades <- nrow(oos_trades)

            oos_profit_factor <- if(any(oos_trades$Returns < 0) && any(oos_trades$Returns > 0)) {
              sum(oos_trades$Returns[oos_trades$Returns > 0], na.rm = TRUE) /
                abs(sum(oos_trades$Returns[oos_trades$Returns < 0], na.rm = TRUE))
            } else {
              NA
            }

            # Simple Sharpe approximation (returns per trade / std dev)
            returns_sd <- sd(oos_trades$Returns, na.rm = TRUE)
            oos_sharpe <- if(!is.na(returns_sd) && returns_sd > 0 && nrow(oos_trades) > 1) {
              mean(oos_trades$Returns, na.rm = TRUE) / returns_sd
            } else {
              NA
            }

            # Get the selected parameters for this window
            param_cols <- names(is_data)[!names(is_data) %in% c("profit_factor", "net_profit", "sharpe", "sortino")]
            selected_params <- is_data[best_is_idx, param_cols, with = FALSE]

            # Create comparison row
            comparison_row <- data.table(
              Window = as.Date(window_name),
              WindowIndex = i,

              # In-sample metrics
              IS_NetProfit = best_is_performance,
              IS_ProfitFactor = if("profit_factor" %in% names(is_data)) is_data[best_is_idx]$profit_factor else NA,
              IS_Sharpe = if("sharpe" %in% names(is_data)) is_data[best_is_idx]$sharpe else NA,
              IS_Sortino = if("sortino" %in% names(is_data)) is_data[best_is_idx]$sortino else NA,

              # Out-of-sample metrics
              OOS_NetProfit = oos_net_profit,
              OOS_ProfitFactor = oos_profit_factor,
              OOS_Sharpe = oos_sharpe,
              OOS_WinRate = winning_trades / total_trades,
              OOS_NumTrades = total_trades,

              # Degradation metrics
              NetProfit_Degradation = if(abs(best_is_performance) > 0) {
                (best_is_performance - oos_net_profit) / abs(best_is_performance)
              } else {
                NA
              },
              Performance_Ratio = if(best_is_performance != 0) {
                oos_net_profit / best_is_performance
              } else {
                NA
              }
            )

            # Add selected parameters
            comparison_row <- cbind(comparison_row, selected_params)

            performance_comparison <- rbind(performance_comparison, comparison_row, fill = TRUE)
          }
        }
      }
    }, error = function(e) {
      warning(paste("Error processing window", i, "(", window_name, "):", e$message))
    })
  }

  # Calculate summary statistics
  if(nrow(performance_comparison) > 0) {
    summary_stats <- list(
      avg_is_performance = mean(performance_comparison$IS_NetProfit, na.rm = TRUE),
      avg_oos_performance = mean(performance_comparison$OOS_NetProfit, na.rm = TRUE),
      avg_degradation = mean(performance_comparison$NetProfit_Degradation, na.rm = TRUE),
      median_degradation = median(performance_comparison$NetProfit_Degradation, na.rm = TRUE),
      pct_profitable_oos = sum(performance_comparison$OOS_NetProfit > 0) / nrow(performance_comparison),
      pct_worse_oos = sum(performance_comparison$OOS_NetProfit < performance_comparison$IS_NetProfit) / nrow(performance_comparison),
      correlation = cor(performance_comparison$IS_NetProfit, performance_comparison$OOS_NetProfit, use = "complete.obs")
    )
  } else {
    summary_stats <- list()
  }

  # Create visualizations
  plots <- list()

  # 1. IS vs OOS Scatter Plot
  if(nrow(performance_comparison) > 0) {
    p_scatter <- plot_ly(performance_comparison,
                         x = ~IS_NetProfit,
                         y = ~OOS_NetProfit,
                         type = 'scatter',
                         mode = 'markers',
                         marker = list(size = 10),
                         text = ~paste("Window:", Window),
                         hovertemplate = paste(
                           "Window: %{text}<br>",
                           "IS Profit: %{x:.0f}<br>",
                           "OOS Profit: %{y:.0f}<br>",
                           "<extra></extra>"
                         ),
                         name = 'IS vs OOS') %>%
      add_trace(x = range(performance_comparison$IS_NetProfit),
                y = range(performance_comparison$IS_NetProfit),
                type = 'scatter',
                mode = 'lines',
                line = list(dash = 'dash', color = 'gray'),
                name = 'Perfect Correlation',
                showlegend = FALSE,
                text = NULL,
                hoverinfo = 'skip',
                inherit = FALSE) %>%
      layout(title = "In-Sample vs Out-of-Sample Performance",
             xaxis = list(title = "In-Sample Net Profit"),
             yaxis = list(title = "Out-of-Sample Net Profit"),
             hovermode = 'closest')

    plots[["scatter"]] <- p_scatter

    # 2. Performance Over Time
    p_time <- plot_ly(performance_comparison, x = ~Window) %>%
      add_trace(y = ~IS_NetProfit,
                type = 'scatter',
                mode = 'lines+markers',
                name = 'In-Sample',
                line = list(color = 'blue')) %>%
      add_trace(y = ~OOS_NetProfit,
                type = 'scatter',
                mode = 'lines+markers',
                name = 'Out-of-Sample',
                line = list(color = 'red')) %>%
      layout(title = "IS vs OOS Performance Over Time",
             xaxis = list(title = "Window Start Date"),
             yaxis = list(title = "Net Profit"),
             hovermode = 'x unified')

    plots[["time_series"]] <- p_time

    # 3. Degradation Distribution
    p_degradation <- plot_ly(x = ~performance_comparison$NetProfit_Degradation * 100,
                             type = 'histogram',
                             nbinsx = 20,
                             marker = list(color = 'lightblue',
                                           line = list(color = 'darkblue', width = 1))) %>%
      layout(title = "Distribution of Performance Degradation",
             xaxis = list(title = "Degradation (%)"),
             yaxis = list(title = "Frequency"))

    plots[["degradation"]] <- p_degradation
  }

  return(list(
    performance_comparison = performance_comparison,
    summary_stats = summary_stats,
    plots = plots,
    recommendation = if(length(summary_stats) > 0) {
      if(summary_stats$avg_degradation > 0.5) {
        "High degradation detected. Consider using more conservative parameters or different optimization metrics."
      } else if(summary_stats$pct_profitable_oos < 0.5) {
        "Less than 50% of windows profitable out-of-sample. Strategy may be overfit to historical data."
      } else {
        "Performance degradation within acceptable range. Strategy shows reasonable robustness."
      }
    } else {
      "Insufficient data for recommendation."
    }
  ))
}



