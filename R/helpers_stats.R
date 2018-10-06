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

#' @title Untable a dataset
#' @name untable
#' @description Given a tabulated dataset, this will untabulate it by repeating
#'   each row by the number of times it was repeated.
#'
#' @param data A data.frame to untable.
#' @param counts A column containing counts.
#'
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate_at
#' @importFrom dplyr everything
#' @importFrom tibble rowid_to_column
#' @importFrom rlang enquo
#'
#' @family helper_stats
#'
#' @examples
#' 
#' # have a look at the Titanic_full dataset first
#' Titanic_full <- untable(data = as.data.frame(Titanic), counts = Freq)
#' dplyr::glimpse(Titanic_full)
#' @export

# function body
untable <- function(data, counts) {
  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      counts = !!rlang::enquo(counts),
      dplyr::everything()
    )

  # a custom function to repeat dataframe `rep` number of times, which is going
  # to be count data for us
  rep_df <- function(df, rep) {
    df[base::rep(x = 1:nrow(df), times = rep), ]
  }

  # converting dataframe to full length based on count information
  data %<>%
    tibble::as_data_frame(.) %>%
    tibble::rowid_to_column(df = ., var = "id") %>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = dplyr::vars("id"),
      .funs = ~as.factor(.)
    ) %>%
    base::split(x = ., f = .$id) %>%
    purrr::map_dfr(
      .x = .,
      .f = ~rep_df(df = ., rep = .$counts)
    )

  # returned the expanded dataset
  return(data)
}


#
# @examples
# a <- NULL
# b <- "y"
#
# purrr::`%||%`(a,b)
#
