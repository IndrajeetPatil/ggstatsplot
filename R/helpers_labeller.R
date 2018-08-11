
#' @title Create text labels
#' @name tz_labeller
#'
#' @param tidy_df Tidy dataframe from `broom::tidy`.
#' @param glance_df Glance model summary dataframe from `broom::glance`.
#' @param k Number of decimal places expected for results displayed in labels.
#' @param statistic Which statistic is to be displayed (either `"t"` or `"z"`).
#'
#' @keywords internal
#'

tz_labeller <- function(tidy_df, glance_df, statistic, k) {

  # getting tidy and glance dataframes ready
  tidy_df <- tidy_df
  glance_df <- glance_df

  # if the statistic is t-value
  if (statistic == "t") {
    if ("df.residual" %in% names(glance_df)) {
      tidy_df %<>%
        purrrlyr::by_row(
          .d = .,
          ..f = ~paste(
            "list(~italic(beta)==",
            ggstatsplot::specify_decimal_p(x = .$estimate, k = k),
            ", ~italic(t)",
            "(",
            glance_df$df.residual,
            ")==",
            .$statistic,
            ", ~italic(p)",
            .$p.value.formatted2,
            ")",
            sep = ""
          ),
          .collate = "rows",
          .to = "label",
          .labels = TRUE
        )
    } else {
      # for objects like rlm there will be no parameter
      tidy_df %<>%
        purrrlyr::by_row(
          .d = .,
          ..f = ~paste(
            "list(~italic(beta)==",
            ggstatsplot::specify_decimal_p(x = .$estimate, k = k),
            ", ~italic(t)",
            "==",
            .$statistic,
            ", ~italic(p)",
            .$p.value.formatted2,
            ")",
            sep = ""
          ),
          .collate = "rows",
          .to = "label",
          .labels = TRUE
        )
    }
  } else if (statistic == "z") {
    # if the statistic is z-value
    tidy_df %<>%
      purrrlyr::by_row(
        .d = .,
        ..f = ~paste(
          "list(~italic(beta)==",
          ggstatsplot::specify_decimal_p(x = .$estimate, k = k),
          ", ~italic(z)==",
          .$statistic,
          ", ~italic(p)",
          .$p.value.formatted2,
          ")",
          sep = ""
        ),
        .collate = "rows",
        .to = "label",
        .labels = TRUE
      )
  }

  # return the final dataframe
  return(tidy_df)
}
