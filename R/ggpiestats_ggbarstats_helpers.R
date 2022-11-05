#' @title A data frame with descriptive labels
#' @noRd
descriptive_data <- function(data,
                             x,
                             y = NULL,
                             label.content = "percentage",
                             perc.k = 1,
                             ...) {
  # creating a data frame with counts
  .cat_counter(data, {{ x }}, {{ y }}) %>%
    mutate(
      .label = case_when(
        grepl("perc|prop", label.content) ~ paste0(round(perc, perc.k), "%"),
        grepl("count|n|N", label.content) ~ .prettyNum(counts),
        TRUE ~ paste0(.prettyNum(counts), "\n", "(", round(perc, perc.k), "%)")
      )
    ) %>%
    # reorder the category factor levels to order the legend
    mutate({{ x }} := factor({{ x }}, unique({{ x }})))
}


#' @title Counts and percentages across grouping variables
#' @noRd
.cat_counter <- function(data, x, y = NULL, ...) {
  data %>%
    group_by({{ y }}, {{ x }}, .drop = TRUE) %>%
    tally(name = "counts") %>%
    mutate(perc = (counts / sum(counts)) * 100) %>%
    ungroup(.) %>%
    arrange(desc({{ x }})) %>%
    filter(counts != 0L)
}

#' @title A data frame with chi-squared test results
#' @noRd
onesample_data <- function(data, x, y, k = 2L, ...) {
  full_join(
    # descriptives
    x = .cat_counter(data, {{ y }}) %>%
      mutate(N = paste0("(n = ", .prettyNum(counts), ")")),
    # proportion tests
    y = group_by(data, {{ y }}) %>%
      group_modify(.f = ~ .chisq_test_safe(., {{ x }})) %>%
      ungroup(.),
    by = as_name(ensym(y))
  ) %>%
    rowwise() %>%
    mutate(
      .label = paste0(
        "list(~chi['gof']^2~", "(", df, ")==", format_value(statistic, k),
        ", ~italic(p)=='", format_value(p.value, k, ),
        "', ~italic(n)==", .prettyNum(counts), ")"
      ),
      .p.label = paste0("list(~italic(p)=='", format_value(p.value, k), "')")
    ) %>%
    ungroup()
}


# safer version of chi-squared test that returns NAs
# needed to work with `group_modify` since it will not work when NULL is returned
#'
#' @noRd
.chisq_test_safe <- function(data, x, ...) {
  xtab <- table(data %>% pull({{ x }}))

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
