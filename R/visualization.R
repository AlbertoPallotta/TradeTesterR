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
