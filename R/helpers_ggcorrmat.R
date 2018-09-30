#' @title Compute minimum, maximum, and median for a dataframe with numbers.
#'
#' @param df A dataframe.
#'
#' @return A tibble with minimum, median, and maximum values.
#'
#' @importFrom tibble as.tibble
#' @importFrom tibble tribble
#' @importFrom dplyr select_if
#' @importFrom purrr is_bare_numeric
#' @importFrom stats median
#'
#' @keywords internal

numdf_n_summary <- function(df) {
  # minimum
  n_min <- tibble::as.tibble(x = df) %>%
    dplyr::select_if(.tbl = ., .predicate = purrr::is_bare_numeric) %>%
    min(., na.rm = TRUE)

  # median
  n_median <- tibble::as.tibble(x = df) %>%
    dplyr::select_if(.tbl = ., .predicate = purrr::is_bare_numeric) %>%
    purrr::flatten_dbl(.x = .) %>%
    stats::median(x = ., na.rm = TRUE)

  # maximum
  n_max <- tibble::as.tibble(x = df) %>%
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
