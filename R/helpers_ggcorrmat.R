#' @title Compute minimum, maximum, and median across all numeric columns in a
#'   dataframe.
#' @name numdf_summary
#' @author Indrajeet Patil
#'
#' @param df A dataframe.
#'
#' @return A tibble with minimum, median, and maximum values.
#'
#' @importFrom tibble as_tibble tribble
#' @importFrom dplyr select_if
#' @importFrom purrr is_bare_numeric
#' @importFrom stats median
#'
#' @examples
#' \donttest{
#' ggstatsplot:::numdf_summary(ggplot2::msleep)
#' }
#'
#' @keywords internal

# function body
numdf_summary <- function(df) {
  # minimum
  n_min <- tibble::as_tibble(x = df) %>%
    dplyr::select_if(.tbl = ., .predicate = purrr::is_bare_numeric) %>%
    min(., na.rm = TRUE)

  # median
  n_median <- tibble::as_tibble(x = df) %>%
    dplyr::select_if(.tbl = ., .predicate = purrr::is_bare_numeric) %>%
    purrr::flatten_dbl(.x = .) %>%
    stats::median(x = ., na.rm = TRUE)

  # maximum
  n_max <- tibble::as_tibble(x = df) %>%
    dplyr::select_if(.tbl = ., .predicate = purrr::is_bare_numeric) %>%
    max(., na.rm = TRUE)

  # form a tibble
  n_summary <-
    tibble::tribble(
      ~n_min, ~n_median, ~n_max,
      n_min, n_median, n_max
    )

  # return the tibble
  return(n_summary)
}

#' @title Convert a matrix to a tibble dataframe.
#' @name matrix_to_tibble
#' @author Indrajeet Patil
#'
#' @param df A matrix.
#' @inheritParams tibble::rownames_to_column
#'
#' @importFrom tibble as_tibble rownames_to_column
#'
#' @examples
#' set.seed(123)
#' cor_df <- cor(purrr::keep(iris, is.numeric))
#' ggstatsplot:::matrix_to_tibble(cor_df)
#' @keywords internal

# function body
matrix_to_tibble <- function(df, var = "variable") {
  # convert to tibble
  df %<>%
    base::as.data.frame(x = .) %>%
    tibble::rownames_to_column(., var = var) %>%
    tibble::as_tibble(x = .)

  # return the tibble
  return(df)
}
