#'
#' @title Custom function for getting specified number of decimal places in
#'   results for p-value
#' @name specify_decimal_p
#' @aliases specify_decimal_p
#' @description Function to format an R object for pretty printing with a
#'   specified (`k`) number of decimal places. The function also allows highly
#'   significant p-values to be denoted as "p < 0.001" rather than "p = 0.000".
#' @author Indrajeet Patil
#'
#' @param x A numeric variable.
#' @param k Number of digits after decimal point (should be an integer) (Default: `k = 3`).
#' @param p.value Decides whether the number is a p-value (Default: `FALSE`).
#'
#' @return Formatted numeric values.
#'
#' @export
#'

specify_decimal_p <- function(x,
                              k = 3,
                              p.value = FALSE) {

  # formatting the output properly
  output <-
    base::trimws(
      x = base::format(
        x = base::round(x = x, digits = k),
        nsmall = k
      ),
      which = "both"
    )
  # if it's a p-value, then format it properly
  if (isTRUE(p.value)) {
    # determing the class of output
    if (output < 0.001) {
      output <- "< 0.001"
    }
  }
  # this will return a character
  return(output)
}
