#' @title Summary dataframe for categorical variables.
#' @name cat_label_df
#' @description Creating a dataframe with an added column corresponding to
#'   summary for categorical variables.
#'
#' @param data A dataframe containing summaries for categorical variables.
#'   Should contain columns named either `"perc"` or `"counts"` or both.
#' @param label.content Character decides what information needs to be displayed
#'   on the label in each pie or bar slice. Possible options are `"percentage"`
#'   (default), `"counts"`, `"both"`.
#' @param ... Ignored.
#' @inheritParams ggpiestats
#'
#' @importFrom dplyr mutate
#' @importFrom rlang !! :=
#'
#' @examples
#' \donttest{
#' # dataframe with label column
#' ggstatsplot:::cat_label_df(
#'   data = ggstatsplot:::cat_counter(mtcars, am, cyl),
#'   label.content = "both",
#'   perc.k = 1
#' )
#' }
#' @keywords internal

# function body
cat_label_df <- function(data,
                         label.content = "percentage",
                         perc.k = 1,
                         ...) {

  # checking what needs to be displayed in a label
  # only percentage
  if (label.content %in% c("percentage", "perc", "proportion", "prop", "%")) {
    data %<>% dplyr::mutate(label = paste0(round(perc, perc.k), "%"))
  }

  # only raw counts
  if (label.content %in% c("counts", "n", "count", "N")) {
    data %<>% dplyr::mutate(label = paste0(prettyNum(counts, big.mark = ",", scientific = FALSE)))
  }

  # both raw counts and percentages
  if (label.content %in% c("both", "mix", "all", "everything")) {
    data %<>% dplyr::mutate(label = paste0(
      prettyNum(counts, big.mark = ",", scientific = FALSE),
      "\n", "(", round(perc, perc.k), "%)"
    ))
  }

  return(data)
}


#' @title Counts and percentages across grouping variables.
#' @name cat_counter
#'
#' @param ... Additional grouping variables.
#' @inheritParams ggpiestats
#'
#' @importFrom rlang enquos !! quo_is_null ensym
#' @importFrom purrr discard
#' @importFrom dplyr select group_by summarize n arrange desc
#' @importFrom dplyr mutate mutate_at mutate_if group_by_at
#'
#' @examples
#' ggstatsplot:::cat_counter(data = ggplot2::mpg, "drv", cyl, "fl")
#' @keywords internal

# function body
cat_counter <- function(data, x, y = NULL, ...) {
  # massaging the inputs
  dots <- rlang::enquos(y, x, ..., .ignore_empty = "all")

  # discarding NULL arguments
  purrr::discard(.x = dots, .p = rlang::quo_is_null)

  # creating a dataframe with counts
  return(
    data %>%
      dplyr::group_by_at(.tbl = ., .vars = dots, .drop = TRUE) %>%
      dplyr::summarize(.data = ., counts = dplyr::n()) %>%
      dplyr::mutate(.data = ., perc = (counts / sum(counts)) * 100) %>%
      dplyr::ungroup(x = .) %>%
      dplyr::arrange(.data = ., dplyr::desc(!!rlang::ensym(x))) %>%
      dplyr::filter(.data = ., counts != 0L)
  )
}

#' @noRd
#'
#' @importFrom ipmisc p_value_formatter
#'
#' @keywords internal

# combine info about sample size plus
df_facet_label <- function(data, x, y, k = 3L) {
  data %>% {
    dplyr::full_join(
      x = cat_counter(data = ., x = {{ y }}) %>%
        dplyr::mutate(.data = ., N = paste0("(n = ", counts, ")", sep = "")),
      y = groupedstats::grouped_proptest(
        data = .,
        grouping.vars = {{ y }},
        measure = {{ x }}
      ) %>%
        dplyr::filter(.data = ., !is.na(significance)),
      by = rlang::as_name(rlang::ensym(y))
    ) %>%
      ipmisc::p_value_formatter(data = ., k = k) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        label = paste(
          "list(~chi['gof']^2~",
          "(",
          parameter,
          ")==",
          specify_decimal_p(x = statistic, k = k),
          ", ~italic(p)",
          p.value.formatted,
          ", ~italic(n)",
          "==",
          counts,
          ")",
          sep = " "
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data = ., -dplyr::matches("p.value.formatted"))
  }
}
