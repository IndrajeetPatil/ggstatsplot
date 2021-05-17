#' @title Split dataframe into a list by grouping variable.
#' @description This function splits the dataframe into a list, with the length
#'   of the list equal to the factor levels of the grouping variable. Each
#'   element of the list will be a tibble.
#' @name grouped_list
#'
#' @inheritParams ggbetweenstats
#' @param grouping.var A single grouping variable (can be entered either as a
#'   bare name `x` or as a string `"x"`).
#'
#' @importFrom rlang enquo quo_text ensym quo_is_null
#'
#' @examples
#' \donttest{
#' ggstatsplot:::grouped_list(ggplot2::msleep, grouping.var = vore)
#' }
#' @keywords internal

# function body
grouped_list <- function(data, grouping.var = NULL) {
  # ensure the grouping variable works quoted or unquoted
  if (rlang::quo_is_null(rlang::enquo(grouping.var))) {
    return(as_tibble(data))
  }

  # creating a list; don't use `dplyr::group_list` because it removes names
  as_tibble(data) %>%
    split(f = .[[rlang::quo_text(rlang::ensym(grouping.var))]], drop = TRUE)
}
