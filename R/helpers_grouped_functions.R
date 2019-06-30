#' @title Split dataframe into a list by grouping variable.
#' @description This function splits the dataframe into a list, with the length
#'   of the list equal to the factor levels of the grouping variable. Each
#'   element of the list will be a tibble.
#' @name grouped_list
#' @author Indrajeet Patil
#'
#' @inheritParams grouped_ggbetweenstats
#'
#' @importFrom dplyr filter mutate_if
#' @importFrom tibble as_tibble
#' @importFrom rlang !! enquo quo_name ensym :=
#'
#' @examples
#' ggstatsplot:::grouped_list(data = ggplot2::msleep, grouping.var = vore)
#' @keywords internal

# function body
grouped_list <- function(data, grouping.var) {

  # ensure the grouping variable works quoted or unquoted
  grouping.var <- rlang::ensym(grouping.var)

  # creating a nested dataframe
  data_list <- data %>%
    dplyr::mutate(
      .data = .,
      !!rlang::enquo(grouping.var) := as.factor(!!rlang::enquo(grouping.var))
    ) %>%
    dplyr::mutate_if(
      .tbl = .,
      .predicate = is.factor,
      .funs = ~ droplevels(.)
    ) %>%
    dplyr::filter(.data = ., !is.na(!!rlang::enquo(grouping.var))) %>%
    tibble::as_tibble(x = .) %>%
    split(x = ., f = .[[rlang::quo_text(grouping.var)]], drop = TRUE)

  # return the list
  return(data_list)
}
