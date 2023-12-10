#' @title A data frame with descriptive labels
#' @autoglobal
#' @noRd
descriptive_data <- function(
    data,
    x,
    y = NULL,
    label.content = "percentage",
    perc.k = 1L,
    ...) {
  .cat_counter(data, {{ x }}, {{ y }}) %>%
    mutate(
      .label = case_when(
        grepl("perc|prop", label.content) ~ paste0(round(perc, perc.k), "%"),
        grepl("count|n|N", label.content) ~ .prettyNum(counts),
        .default = paste0(.prettyNum(counts), "\n", "(", round(perc, perc.k), "%)")
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
onesample_data <- function(data, x, y, k = 2L, ...) {
  full_join(
    # descriptive summary
    x = .cat_counter(data, {{ y }}) %>%
      mutate(N = paste0("(n = ", .prettyNum(counts), ")")),
    # proportion test results
    y = group_by(data, {{ y }}) %>%
      group_modify(.f = ~ .chisq_test_safe(., {{ x }})) %>%
      ungroup(),
    by = as_name(ensym(y))
  ) %>%
    rowwise() %>%
    mutate(
      .label = glue("list(~chi['gof']^2~({df})=={format_value(statistic, k)}, ~italic(p)=='{format_value(p.value, k)}', ~italic(n)=='{.prettyNum(counts)}')"),
      .p.label = glue("list(~italic(p)=='{format_value(p.value, k)}')")
    ) %>%
    ungroup()
}


#' Safer version of chi-squared test that returns `NA`s
#' Needed to work with `group_modify()` since it will not work when `NULL` is returned
#' @autoglobal
#' @noRd
.chisq_test_safe <- function(data, x, ...) {
  xtab <- table(pull(data, {{ x }}))

  result <- tryCatch(
    expr = parameters::model_parameters(suppressWarnings(stats::chisq.test(xtab))),
    error = function(e) NULL
  )

  if (is.null(result)) {
    tibble(
      statistic = NA_real_, p.value = NA_real_, df = NA_real_,
      method = "Chi-squared test for given probabilities"
    )
  } else {
    insight::standardize_names(result, style = "broom") %>% as_tibble()
  }
}


#' @noRd
.prettyNum <- function(x) prettyNum(x, big.mark = ",", scientific = FALSE)
