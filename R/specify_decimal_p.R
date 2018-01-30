#' @title custom function for getting specified number of decimal places in results for p-value
#' @name specify_decimal_p
#' @param x a numeric value
#' @param k the number of digits after decimal point (should be an integer)
#' @return formatted p-values from statistical tests
#'
#' @export

specify_decimal_p <- function(x, k = NULL) {
  # if the number of decimal places hasn't been specified, use the default of 3
  if (is.null(k))
    k <- 3
  # formatting the output properly
  output <-
    base::trimws(x = format(round(x, k), nsmall = k), which = "both")
  if (output < 0.001) {
    output <- "< 0.001" # this will return a character
  } else {
    output <- as.numeric(output) # this will return a numeric
  }
  return(output)
}
