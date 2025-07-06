

#' Calculate position size based on capital and instrument type
#'
#' @description
#' Determines the appropriate position size given available capital,
#' instrument type, and leverage considerations.
#'
#' @param price Current price of the instrument
#' @param amount_in_dollars Dollar amount to allocate to the position
#' @param type Instrument type: "Stocks", "Futures", or "Forex" (case-insensitive)
#' @param leverage Leverage multiplier (default: 1)
#' @param contract_multiplier For futures, the dollar value per point (default: 1)
#'
#' @return Numeric position size:
#'   - Stocks: Number of shares
#'   - Futures: Number of contracts
#'   - Forex: Position size in units
#'
#' @examples
#' \dontrun{
#' # Stock position
#' shares <- getOrderSize(50.25, 10000, "Stocks")  # ~199 shares
#'
#' # Futures position (e.g., ES with $50 multiplier)
#' contracts <- getOrderSize(4200, 10000, "Futures",
#'                          leverage = 1,
#'                          contract_multiplier = 50)
#'
#' # Forex position with leverage
#' units <- getOrderSize(1.1850, 10000, "Forex", leverage = 10)
#' }
#' @export
getOrderSize <- function(price, amount_in_dollars, type = "Stocks", leverage = 1, contract_multiplier = 1) {
  type = tolower(type)

  if (type == "stocks") {
    return(amount_in_dollars / price)
  } else if (type == "futures") {
    return((amount_in_dollars * leverage) / (contract_multiplier * price))
  } else { # For something like forex
    return(amount_in_dollars * leverage)
  }
}
