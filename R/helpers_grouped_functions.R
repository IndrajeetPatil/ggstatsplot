#' @title Split dataframe into a list by grouping variable.
#' @description This function splits the dataframe into a list, with the length
#'   of the list equal to the factor levels of the grouping variable. Each
#'   element of the list will be a tibble.
#' @name grouped_list
#' @author \href{https://github.com/IndrajeetPatil}{Indrajeet Patil}
#'
#' @inheritParams ggbetweenstats
#' @param grouping.var A single grouping variable (can be entered either as a
#'   bare name `x` or as a string `"x"`).
#'
#' @importFrom dplyr filter mutate_if
#' @importFrom tibble as_tibble
#' @importFrom rlang !! enquo quo_text ensym := quo_is_null
#'
#' @examples
#' ggstatsplot:::grouped_list(data = ggplot2::msleep, grouping.var = vore)
#' @keywords internal

# function body
grouped_list <- function(data, grouping.var = NULL) {

  # ensure the grouping variable works quoted or unquoted
  if (!rlang::quo_is_null(rlang::enquo(grouping.var))) {
    grouping.var <- rlang::ensym(grouping.var)
  } else {
    return(tibble::as_tibble(data))
  }

  # creating a nested dataframe
  data %>%
    dplyr::mutate(.data = ., {{ grouping.var }} := as.factor({{ grouping.var }})) %>%
    dplyr::mutate_if(
      .tbl = .,
      .predicate = is.factor,
      .funs = ~ droplevels(.)
    ) %>%
    dplyr::filter(.data = ., !is.na({{ grouping.var }})) %>%
    tibble::as_tibble(x = .) %>%
    split(x = ., f = .[[rlang::quo_text(grouping.var)]], drop = TRUE)
}
