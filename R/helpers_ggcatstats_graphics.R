#' @title A dataframe with descriptive labels
#' @importFrom dplyr case_when
#' @noRd

# function body
descriptive_df <- function(data,
                           x,
                           y = NULL,
                           label.content = "percentage",
                           perc.k = 1,
                           ...) {
  # creating a dataframe with counts
  cat_counter(data, {{ x }}, {{ y }}) %>%
    dplyr::mutate(
      .label = dplyr::case_when(
        grepl("perc|prop", label.content) ~ paste0(round(perc, perc.k), "%"),
        grepl("count|n|N", label.content) ~ .prettyNum(counts),
        TRUE ~ paste0(.prettyNum(counts), "\n", "(", round(perc, perc.k), "%)")
      )
    ) %>%
    # reorder the category factor levels to order the legend
    dplyr::mutate({{ x }} := factor({{ x }}, unique({{ x }})))
}


#' @title Counts and percentages across grouping variables
#'
#' @importFrom dplyr select group_by ungroup tally n arrange desc mutate
#'
#' @noRd

# creating a dataframe with counts
cat_counter <- function(data, x, y = NULL, ...) {
  data %>%
    dplyr::group_by({{ y }}, {{ x }}, .drop = TRUE) %>%
    dplyr::tally(name = "counts") %>%
    dplyr::mutate(perc = (counts / sum(counts)) * 100) %>%
    dplyr::ungroup(.) %>%
    dplyr::arrange(dplyr::desc({{ x }})) %>%
    dplyr::filter(counts != 0L)
}

#' @title A dataframe with chi-squared test results
#'
#' @importFrom dplyr group_modify rowwise ungroup
#' @importFrom rlang as_name ensym
#' @importFrom statsExpressions format_num
#' @importFrom insight format_value
#'
#' @noRd

# combine info about sample size plus proportion test
onesample_df <- function(data, x, y, k = 2L, ...) {
  dplyr::full_join(
    # descriptives
    x = cat_counter(data, {{ y }}) %>%
      dplyr::mutate(N = paste0("(n = ", .prettyNum(counts), ")")),
    # proportion tests
    y = dplyr::group_by(data, {{ y }}) %>%
      dplyr::group_modify(.f = ~ chisq_test_safe(., {{ x }})) %>%
      dplyr::ungroup(.),
    by = rlang::as_name(rlang::ensym(y))
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      .label = paste0(
        "list(~chi['gof']^2~", "(", df, ")==", format_value(statistic, k),
        ", ~italic(p)=='", format_num(p.value, k, p.value = TRUE),
        "', ~italic(n)==", .prettyNum(counts), ")"
      ),
      .p.label = paste0("list(~italic(p)=='", format_num(p.value, k, TRUE), "')")
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
  xtab <- table(data %>% dplyr::pull({{ x }}))

  # run chi-square test
  result <- tryCatch(
    expr = parameters::model_parameters(suppressWarnings(stats::chisq.test(xtab))),
    error = function(e) NULL
  )

  # if not null, return tidy output, otherwise return NAs
  if (!is.null(result)) {
    as_tibble(parameters::standardize_names(result, style = "broom"))
  } else {
    tibble(
      statistic = NA_real_, p.value = NA_real_, df = NA_real_,
      method = "Chi-squared test for given probabilities"
    )
  }
}


#' @noRd

.prettyNum <- function(x) prettyNum(x, big.mark = ",", scientific = FALSE)
