#' @title Prepare data for pie/bar chart functions
#' @name .pie_bar_data_prep
#'
#' @description
#'
#' Shared data-preparation step for `ggpiestats()` and `ggbarstats()`:
#' selects relevant columns, drops missing values, untables counts, converts
#' to factors, and computes the number of levels for each variable.
#'
#' @inheritParams ggpiestats
#'
#' @return A named list with elements `data`, `test`, `x_levels`, `y_levels`.
#'
#' @autoglobal
#' @noRd
.pie_bar_data_prep <- function(data, x, y, counts) {
  data <- data |>
    select({{ x }}, {{ y }}, .counts = {{ counts }}) |>
    tidyr::drop_na()

  if (".counts" %in% names(data)) {
    data <- tidyr::uncount(data, weights = .counts)
  }

  data <- mutate(data, across(.cols = everything(), .fns = ~ as.factor(.x)))

  test <- ifelse(quo_is_null(enquo(y)), "one.way", "two.way")
  x_levels <- nlevels(pull(data, {{ x }}))
  y_levels <- ifelse(test == "one.way", 0L, nlevels(pull(data, {{ y }})))

  list(data = data, test = test, x_levels = x_levels, y_levels = y_levels)
}


#' @title Compute subtitle, caption, and pairwise comparisons for pie/bar charts
#' @name .pie_bar_subtitle_caption
#'
#' @description
#'
#' Shared helper for `ggpiestats()` and `ggbarstats()` that runs the
#' contingency table test, optionally computes a Bayes Factor caption, and
#' performs pairwise comparisons when applicable.
#'
#' @inheritParams ggpiestats
#'
#' @return A named list with elements `subtitle`, `caption`, `subtitle_df`,
#'   `caption_df`, and `mpc_df`.
#'
#' @autoglobal
#' @noRd
.pie_bar_subtitle_caption <- function(
  data,
  x,
  y,
  type,
  paired,
  bf.message,
  alternative,
  conf.level,
  digits,
  ratio,
  sampling.plan,
  fixed.margin,
  prior.concentration,
  x_levels,
  y_levels,
  p.adjust.method
) {
  .f.args <- list(
    data = data,
    x = {{ x }},
    y = {{ y }},
    alternative = alternative,
    conf.level = conf.level,
    digits = digits,
    paired = paired,
    ratio = ratio,
    sampling.plan = sampling.plan,
    fixed.margin = fixed.margin,
    prior.concentration = prior.concentration
  )

  stats <- .subtitle_caption(
    contingency_table,
    .f.args,
    type,
    bf.message,
    bf.condition = type != "bayes" && isFALSE(paired)
  )

  mpc_df <- .pairwise_contingency(
    data,
    {{ x }},
    {{ y }},
    x_levels,
    y_levels,
    paired,
    digits,
    conf.level,
    alternative,
    p.adjust.method
  )

  c(stats, list(mpc_df = mpc_df))
}


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
  all_lvls <- levels(pull(data, {{ x }}))

  .cat_counter(data, {{ x }}, {{ y }}) |>
    # Drop unused factor levels (including any absent y levels after filtering)
    # before complete() so it only expands to observed y groups, not all
    # factor levels defined on y that happen to have no data in this subset.
    droplevels() |>
    # Normalize x to a plain (unordered) factor so tidyr::complete()'s internal
    # full_join does not fail when the original x was an ordered factor.
    mutate({{ x }} := factor({{ x }}, all_lvls, ordered = FALSE)) |>
    # Fill in zero-count rows for missing (y, x) combinations so all panels
    # produce structurally identical data; patchwork can then deduplicate guides.
    tidyr::complete(
      {{ y }},
      {{ x }} := factor(all_lvls, all_lvls),
      fill = list(counts = 0L, perc = 0)
    ) |>
    mutate(
      .label = if_else(
        counts == 0L,
        NA_character_,
        if (grepl("perc|prop", label.content)) {
          paste0(round(perc, digits.perc), "%")
        } else if (grepl("count|n|N", label.content)) {
          .prettyNum(counts)
        } else {
          paste0(.prettyNum(counts), "\n", "(", round(perc, digits.perc), "%)")
        }
      ),
      {{ x }} := factor(
        {{ x }},
        if (length(all_lvls)) all_lvls else unique({{ x }})
      )
    )
}


#' @title Counts and percentages across grouping variables
#' @autoglobal
#' @noRd
.cat_counter <- function(data, x, y = NULL, ...) {
  data |>
    group_by({{ y }}, {{ x }}, .drop = TRUE) |>
    tally(name = "counts") |>
    mutate(perc = (counts / sum(counts)) * 100) |>
    ungroup() |>
    arrange(desc({{ x }})) |>
    filter(counts != 0L)
}

#' @title A data frame with chi-squared test results
#' @autoglobal
#' @noRd
onesample_data <- function(data, x, y, digits = 2L, ratio = NULL, ...) {
  grouped_chi_squared_summary <- group_by(data, {{ y }}) |>
    group_modify(.f = ~ .chisq_test_safe(., {{ x }}, ratio)) |>
    ungroup()
  descriptive_summary <- .cat_counter(data, {{ y }}) |>
    mutate(N = paste0("(n = ", .prettyNum(counts), ")"))

  full_join(
    descriptive_summary,
    grouped_chi_squared_summary,
    by = as_name(ensym(y))
  ) |>
    rowwise() |>
    mutate(
      .label = glue(
        "list(~chi['gof']^2~({df})=={format_value(statistic, digits)}, ",
        "~italic(p)=='{format_value(p.value, digits)}', ",
        "~italic(n)=='{.prettyNum(counts)}')"
      ),
      .p.label = glue("list(~italic(p)=='{format_value(p.value, digits)}')")
    ) |>
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
      # nocov start
      tibble(
        statistic = NA_real_,
        p.value = NA_real_,
        df = NA_real_,
        method = "Chi-squared test for given probabilities"
      )
    } # nocov end
  )
}


#' @noRd
.prettyNum <- function(x) prettyNum(x, big.mark = ",", scientific = FALSE)


#' @autoglobal
#' @noRd
.pairwise_contingency <- function(
  data,
  x,
  y,
  x_levels,
  y_levels,
  paired,
  digits,
  conf.level,
  alternative,
  p.adjust.method
) {
  if (x_levels < 3L || y_levels < 2L || isTRUE(paired)) {
    return(NULL)
  }

  tryCatch(
    suppressWarnings(pairwise_contingency_table(
      data = data,
      x = {{ x }},
      y = {{ y }},
      digits = digits,
      conf.level = conf.level,
      alternative = alternative,
      p.adjust.method = p.adjust.method
    )),
    error = function(e) NULL
  )
}
