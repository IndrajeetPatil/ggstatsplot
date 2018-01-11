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
  output <- trimws(format(round(x, k), nsmall = k))
  if (output < 0.001)
    output <- "< 0.001"
  return(output)

}
