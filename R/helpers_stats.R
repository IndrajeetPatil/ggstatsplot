#' @title Finding the outliers in the dataframe using Tukey's interquartile
#'   range rule
#' @name check_outlier
#' @author Indrajeet Patil
#' @description Returns a logical vector
#'
#' @param var A numeric vector.
#' @param coef Coefficient for outlier detection using Tukey's method.
#'   With Tukey's method, outliers are below (1st Quartile) or above (3rd
#'   Quartile) `coef` times the Inter-Quartile Range (IQR) (Default: `1.5`).
#'
#' @importFrom stats quantile
#'
#' @family helper_stats
#'
#' @keywords internal

# defining function to detect outliers
check_outlier <- function(var, coef = 1.5) {

  # compute the quantiles
  quantiles <- stats::quantile(
    x = var,
    probs = c(0.25, 0.75)
  )

  # compute the interquartile range
  IQR <- quantiles[2] - quantiles[1]

  # check for outlier and output a logical
  res <-
    ((var < (quantiles[1] - coef * IQR)) |
      (var > (quantiles[2] + coef * IQR)))

  # return the result
  return(res)
}

#
# @examples
# a <- NULL
# b <- "y"
#
# purrr::`%||%`(a,b)
#
