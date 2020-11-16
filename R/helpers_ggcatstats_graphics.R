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
#' @inheritParams ggpiestats
#'
#' @importFrom dplyr select group_by ungroup summarize n arrange desc mutate
#'
#' @examples
#' ggstatsplot:::cat_counter(data = ggplot2::mpg, drv, cyl)
#' @keywords internal

# function body
cat_counter <- function(data, x, y = NULL, ...) {
  # creating a dataframe with counts
  data %>%
    dplyr::group_by(., {{ y }}, {{ x }}, .drop = TRUE) %>%
    dplyr::summarize(.data = ., counts = dplyr::n(), .groups = "drop_last") %>%
    dplyr::mutate(.data = ., perc = (counts / sum(counts)) * 100) %>%
    dplyr::ungroup(x = .) %>%
    dplyr::arrange(.data = ., dplyr::desc({{ x }})) %>%
    dplyr::filter(.data = ., counts != 0L)
}

#' @importFrom dplyr group_modify rowwise ungroup
#' @importFrom rlang as_name ensym
#'
#' @noRd
#' @keywords internal

# combine info about sample size plus
df_facet_label <- function(data, x, y, k = 3L, ...) {
  dplyr::full_join(
    # descriptives
    x = cat_counter(data = data, x = {{ y }}) %>%
      dplyr::mutate(N = paste0("(n = ", prettyNum(counts, big.mark = ",", scientific = FALSE), ")")),
    # proportion tests
    y = dplyr::group_by(data, {{ y }}) %>%
      dplyr::group_modify(.f = ~ chisq_test_safe(., {{ x }})) %>%
      dplyr::ungroup(.) %>%
      signif_column(data = ., p = p.value) %>%
      dplyr::filter(.data = ., !is.na(significance)),
    by = rlang::as_name(rlang::ensym(y))
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      label = paste0(
        "list(~chi['gof']^2~",
        "(",
        df,
        ")==",
        specify_decimal_p(x = statistic, k = k),
        ", ~italic(p)==",
        specify_decimal_p(x = p.value, k = k, p.value = TRUE),
        ", ~italic(n)==",
        counts,
        ")"
      )
    ) %>%
    dplyr::ungroup()
}


# safer version of chi-squared test that returns NAs
# needed to work with `group_modify` since it will not work when NULL is returned
#
#' @importFrom stats chisq.test
#' @importFrom dplyr pull
#' @importFrom parameters model_parameters standardize_names
#'
#' @noRd

chisq_test_safe <- function(data, x, ...) {
  # create a table
  xtab <- table(data %>% dplyr::pull({{ x }}))

  # run chi-square test
  chi_result <-
    tryCatch(
      expr = parameters::model_parameters(stats::chisq.test(xtab)),
      error = function(e) NULL
    )

  # if not null, return tidy output, otherwise return NAs
  if (!is.null(chi_result)) {
    as_tibble(parameters::standardize_names(data = chi_result, style = "broom"))
  } else {
    tibble(
      statistic = NA_real_,
      p.value = NA_real_,
      df = NA_real_,
      method = "Chi-squared test for given probabilities"
    )
  }
}
