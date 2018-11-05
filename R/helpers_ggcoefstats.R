#' @title Create text labels
#' @name tfz_labeller
#'
#' @param tidy_df Tidy dataframe from `broom::tidy`.
#' @param glance_df Glance model summary dataframe from `broom::glance`.
#' @param statistic Which statistic is to be displayed (either `"t"` or `"f"`or
#'   `"z"`).
#' @inheritParams ggcoefstats
#'
#' @keywords internal

# function body
tfz_labeller <- function(tidy_df,
                         glance_df = NULL,
                         statistic,
                         effsize = "eta",
                         partial = TRUE,
                         k) {

  # getting tidy and glance dataframes ready
  tidy_df <- tidy_df
  glance_df <- glance_df

  #--------------------------- t-statistic ------------------------------------

  # if the statistic is t-value
  if (statistic == "t") {
    if ("df.residual" %in% names(glance_df)) {
      tidy_df %<>%
        purrrlyr::by_row(
          .d = .,
          ..f = ~ paste(
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
          ..f = ~ paste(
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
    #--------------------------- z-statistic ---------------------------------
  } else if (statistic == "z") {
    # if the statistic is z-value
    tidy_df %<>%
      purrrlyr::by_row(
        .d = .,
        ..f = ~ paste(
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

    #--------------------------- f-statistic ---------------------------------
  } else if (statistic == "f") {

    # which effect size is needed?
    if (effsize == "eta") {
      if (isTRUE(partial)) {
        tidy_df %<>%
          purrrlyr::by_row(
            .d = .,
            ..f = ~ paste(
              "list(~italic(F)",
              "(",
              .$df1,
              ",",
              .$df2,
              ")==",
              .$statistic,
              ", ~italic(p)",
              .$p.value.formatted2,
              ", ~italic(eta)[p]^2==",
              ggstatsplot::specify_decimal_p(x = .$estimate, k = k),
              ")",
              sep = ""
            ),
            .collate = "rows",
            .to = "label",
            .labels = TRUE
          )
      } else {
        tidy_df %<>%
          purrrlyr::by_row(
            .d = .,
            ..f = ~ paste(
              "list(~italic(F)",
              "(",
              .$df1,
              ",",
              .$df2,
              ")==",
              .$statistic,
              ", ~italic(p)",
              .$p.value.formatted2,
              ", ~italic(eta)^2==",
              ggstatsplot::specify_decimal_p(x = .$estimate, k = k),
              ")",
              sep = ""
            ),
            .collate = "rows",
            .to = "label",
            .labels = TRUE
          )
      }
    } else if (effsize == "omega") {
      if (isTRUE(partial)) {
        tidy_df %<>%
          purrrlyr::by_row(
            .d = .,
            ..f = ~ paste(
              "list(~italic(F)",
              "(",
              .$df1,
              ",",
              .$df2,
              ")==",
              .$statistic,
              ", ~italic(p)",
              .$p.value.formatted2,
              ", ~italic(omega)[p]^2==",
              ggstatsplot::specify_decimal_p(x = .$estimate, k = k),
              ")",
              sep = ""
            ),
            .collate = "rows",
            .to = "label",
            .labels = TRUE
          )
      } else {
        tidy_df %<>%
          purrrlyr::by_row(
            .d = .,
            ..f = ~ paste(
              "list(~italic(F)",
              "(",
              .$df1,
              ",",
              .$df2,
              ")==",
              .$statistic,
              ", ~italic(p)",
              .$p.value.formatted2,
              ", ~italic(omega)^2==",
              ggstatsplot::specify_decimal_p(x = .$estimate, k = k),
              ")",
              sep = ""
            ),
            .collate = "rows",
            .to = "label",
            .labels = TRUE
          )
      }
    }
  }

  # return the final dataframe
  return(tidy_df)
}
