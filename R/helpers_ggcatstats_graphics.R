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
  data %>%
    dplyr::group_by_at(.tbl = ., .vars = dots, .drop = TRUE) %>%
    dplyr::summarize(.data = ., counts = dplyr::n()) %>%
    dplyr::mutate(.data = ., perc = (counts / sum(counts)) * 100) %>%
    dplyr::ungroup(x = .) %>%
    dplyr::arrange(.data = ., dplyr::desc(!!rlang::ensym(x))) %>%
    dplyr::filter(.data = ., counts != 0L)
}

#' @noRd
#' @keywords internal

# combine info about sample size plus
df_facet_label <- function(data, x, y, k = 3L, ...) {
  data %>% {
    dplyr::full_join(
      x = cat_counter(data = ., x = {{ y }}) %>%
        dplyr::mutate(N = paste0("(n = ", prettyNum(counts, big.mark = ",", scientific = FALSE), ")")),
      y = grouped_proptest(
        data = .,
        grouping.vars = {{ y }},
        measure = {{ x }}
      ) %>%
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
}


#' @title Function to run proportion test on grouped data.
#' @name grouped_proptest
#' @return Dataframe with percentages and statistical details from a proportion
#'  test.
#'
#' @importFrom tidyr nest unnest spread
#' @importFrom stats chisq.test
#' @importFrom rlang enquos
#' @importFrom dplyr group_by_at group_modify ungroup group_vars select
#' @importFrom dplyr count left_join
#'
#' @noRd

# function body
grouped_proptest <- function(data, grouping.vars, measure, ...) {
  # calculating percentages and running chi-squared test
  dplyr::group_by_at(data, rlang::enquos(grouping.vars)) %>%
    {
      dplyr::left_join(
        x = (.) %>%
          dplyr::count({{ measure }}) %>%
          dplyr::mutate(perc = paste0(specify_decimal_p((n / sum(n)) * 100, k = 2), "%")) %>%
          dplyr::select(-n) %>%
          tidyr::spread(data = ., key = {{ measure }}, value = perc),
        y = (.) %>%
          dplyr::group_modify(.f = ~ chisq_test_safe(., {{ measure }})),
        by = dplyr::group_vars(.)
      )
    } %>%
    dplyr::ungroup(.) %>%
    signif_column(data = ., p = p.value)
}

# safer version of chi-squared test that returns NAs
# needed to work with `group_modify` since it will not work when NULL is returned
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
