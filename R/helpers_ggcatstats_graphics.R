#'  @title A dataframe with descriptive labels
#'
#' @importFrom dplyr mutate
#' @importFrom rlang !! :=
#'
#' @noRd

# function body
cat_label_df <- function(data,
                         x,
                         y = NULL,
                         label.content = "percentage",
                         perc.k = 1,
                         ...) {
  # creating a dataframe with counts
  data %<>% cat_counter(., {{ x }}, {{ y }})

  # checking what needs to be displayed in a label
  # only percentage
  if (label.content %in% c("percentage", "perc", "proportion", "prop", "%")) {
    data %<>% dplyr::mutate(label = paste0(round(perc, perc.k), "%"))
  }

  # only raw counts
  if (label.content %in% c("counts", "n", "count", "N")) {
    data %<>% dplyr::mutate(label = paste0(.prettyNum(counts)))
  }

  # both raw counts and percentages
  if (label.content %in% c("both", "mix", "all", "everything")) {
    data %<>% dplyr::mutate(label = paste0(.prettyNum(counts), "\n", "(", round(perc, perc.k), "%)"))
  }

  # reorder the category factor levels to order the legend
  return(data %<>% dplyr::mutate(.data = ., {{ x }} := factor({{ x }}, unique({{ x }}))))
}


#' @title Counts and percentages across grouping variables
#'
#' @importFrom dplyr select group_by ungroup tally n arrange desc mutate
#'
#' @noRd

# creating a dataframe with counts
cat_counter <- function(data, x, y = NULL, ...) {
  data %>%
    dplyr::group_by(.data = ., {{ y }}, {{ x }}, .drop = TRUE) %>%
    dplyr::tally(x = ., name = "counts") %>%
    dplyr::mutate(.data = ., perc = (counts / sum(counts)) * 100) %>%
    dplyr::ungroup(.) %>%
    dplyr::arrange(.data = ., dplyr::desc({{ x }})) %>%
    dplyr::filter(.data = ., counts != 0L)
}

#' @title A dataframe with chi-squared test results
#'
#' @importFrom dplyr group_modify rowwise ungroup
#' @importFrom rlang as_name ensym
#'
#' @noRd

# combine info about sample size plus
df_facet_label <- function(data, x, y, k = 3L, ...) {
  dplyr::full_join(
    # descriptives
    x = cat_counter(data = data, x = {{ y }}) %>%
      dplyr::mutate(N = paste0("(n = ", .prettyNum(counts), ")")),
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
  xtab <- table(data %>% dplyr::pull({{ x }}))

  # run chi-square test
  result <-
    tryCatch(
      expr = parameters::model_parameters(suppressWarnings(stats::chisq.test(xtab))),
      error = function(e) NULL
    )

  # if not null, return tidy output, otherwise return NAs
  if (!is.null(result)) {
    as_tibble(parameters::standardize_names(data = result, style = "broom"))
  } else {
    tibble(
      statistic = NA_real_,
      p.value = NA_real_,
      df = NA_real_,
      method = "Chi-squared test for given probabilities"
    )
  }
}


#' @noRd

.prettyNum <- function(x) prettyNum(x, big.mark = ",", scientific = FALSE)
