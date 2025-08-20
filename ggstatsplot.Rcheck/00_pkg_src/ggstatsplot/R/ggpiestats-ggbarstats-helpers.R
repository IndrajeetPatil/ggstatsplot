#' @title A data frame with descriptive labels
#' @autoglobal
#' @noRd
descriptive_data <- function(
  data,
  x,
  y = NULL,
  label.content = "percentage",
  digits.perc = 1L,
  ...
) {
  .cat_counter(data, {{ x }}, {{ y }}) %>%
    mutate(
      .label = case_when(
        grepl("perc|prop", label.content) ~ paste0(round(perc, digits.perc), "%"),
        grepl("count|n|N", label.content) ~ .prettyNum(counts),
        .default = paste0(.prettyNum(counts), "\n", "(", round(perc, digits.perc), "%)")
      ), # reorder the category factor levels to order the legend
      {{ x }} := factor({{ x }}, unique({{ x }}))
    )
}


#' @title Counts and percentages across grouping variables
#' @autoglobal
#' @noRd
.cat_counter <- function(data, x, y = NULL, ...) {
  data %>%
    group_by({{ y }}, {{ x }}, .drop = TRUE) %>%
    tally(name = "counts") %>%
    mutate(perc = (counts / sum(counts)) * 100) %>%
    ungroup() %>%
    arrange(desc({{ x }})) %>%
    filter(counts != 0L)
}

#' @title A data frame with chi-squared test results
#' @autoglobal
#' @noRd
onesample_data <- function(data, x, y, digits = 2L, ratio = NULL, ...) {
  grouped_chi_squared_summary <- group_by(data, {{ y }}) %>%
    group_modify(.f = ~ .chisq_test_safe(., {{ x }}, ratio)) %>%
    ungroup()
  descriptive_summary <- .cat_counter(data, {{ y }}) %>% mutate(N = paste0("(n = ", .prettyNum(counts), ")"))

  full_join(descriptive_summary, grouped_chi_squared_summary, by = as_name(ensym(y))) %>%
    rowwise() %>%
    mutate(
      # nolint next: line_length_linter.
      .label = glue("list(~chi['gof']^2~({df})=={format_value(statistic, digits)}, ~italic(p)=='{format_value(p.value, digits)}', ~italic(n)=='{.prettyNum(counts)}')"),
      .p.label = glue("list(~italic(p)=='{format_value(p.value, digits)}')")
    ) %>%
    ungroup()
}


#' Safer version of chi-squared test that returns `NA`s
#' Needed to work with `dplyr::group_modify()` since it will not work when `NULL` is returned.
#' @autoglobal
#' @noRd
.chisq_test_safe <- function(data, x, ratio) {
  tryCatch(
    suppressWarnings(contingency_table(data, x, ratio = ratio)),
    error = function(e) {
      tibble(
        statistic = NA_real_, p.value = NA_real_, df = NA_real_,
        method = "Chi-squared test for given probabilities"
      )
    }
  )
}


#' @noRd
.prettyNum <- function(x) prettyNum(x, big.mark = ",", scientific = FALSE)
