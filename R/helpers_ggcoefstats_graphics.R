#' @title Create labels with statistical details for `ggcoefstats`
#' @name ggcoefstats_label_maker
#'
#' @param ... Currently ignored.
#' @param tidy_df A tidy dataframe.
#' @inheritParams ggcoefstats
#'
#' @importFrom dplyr mutate rowwise
#'
#' @keywords internal

# function body
ggcoefstats_label_maker <- function(tidy_df,
                                    statistic = NULL,
                                    k = 2L,
                                    effsize = "eta",
                                    ...) {

  #----------------------- p-value cleanup ------------------------------------

  # formatting the p-values
  tidy_df %<>%
    signif_column(data = ., p = p.value) %>%
    dplyr::rowwise()

  #--------------------------- t-statistic ------------------------------------

  # if the statistic is t-value
  if (statistic == "t") {
    # check if df info is available somewhere
    if ("df.error" %in% names(tidy_df)) {
      # adding a new column with residual `df`
      tidy_df %<>%
        dplyr::mutate(
          label = paste0(
            "list(~widehat(italic(beta))==",
            specify_decimal_p(x = estimate, k = k),
            ", ~italic(t)",
            "(",
            specify_decimal_p(x = df.error, k = 0L),
            ")==",
            specify_decimal_p(x = statistic, k = k),
            ", ~italic(p)==",
            specify_decimal_p(x = p.value, k = k, p.value = TRUE),
            ")"
          )
        )
    } else {
      # for objects like `rlm` there will be no parameter
      tidy_df %<>%
        dplyr::mutate(
          label = paste0(
            "list(~widehat(italic(beta))==",
            specify_decimal_p(x = estimate, k = k),
            ", ~italic(t)==",
            specify_decimal_p(x = statistic, k = k),
            ", ~italic(p)==",
            specify_decimal_p(x = p.value, k = k, p.value = TRUE),
            ")"
          )
        )
    }
  }

  #--------------------------- z-statistic ---------------------------------

  # if the statistic is z-value
  if (statistic == "z") {
    tidy_df %<>%
      dplyr::mutate(
        label = paste0(
          "list(~widehat(italic(beta))==",
          specify_decimal_p(x = estimate, k = k),
          ", ~italic(z)==",
          specify_decimal_p(x = statistic, k = k),
          ", ~italic(p)==",
          specify_decimal_p(x = p.value, k = k, p.value = TRUE),
          ")"
        )
      )
  }

  #--------------------------- chi^2-statistic ---------------------------------

  # if the statistic is chi^2-value
  if (statistic == "c") {
    tidy_df %<>%
      dplyr::mutate(
        label = paste0(
          "list(~widehat(italic(beta))==",
          specify_decimal_p(x = estimate, k = k),
          ", ~italic(chi)^2==",
          specify_decimal_p(x = statistic, k = k),
          ", ~italic(p)==",
          specify_decimal_p(x = p.value, k = k, p.value = TRUE),
          ")"
        )
      )
  }

  #--------------------------- f-statistic ---------------------------------

  if (statistic == "f") {
    # which effect size is needed?
    if (effsize == "eta") effsize.text <- list(quote(widehat(italic(eta)[p]^2)))
    if (effsize == "omega") effsize.text <- list(quote(widehat(italic(omega)[p]^2)))

    # which effect size is needed?
    tidy_df %<>%
      dplyr::mutate(
        label = paste0(
          "list(~italic(F)",
          "(",
          df1,
          "*\",\"*",
          df2,
          ")==",
          specify_decimal_p(x = statistic, k = k),
          ", ~italic(p)==",
          specify_decimal_p(x = p.value, k = k, p.value = TRUE),
          ", ~",
          effsize.text,
          "==",
          specify_decimal_p(x = estimate, k = k),
          ")"
        )
      )
  }

  # return the final dataframe
  return(dplyr::ungroup(tidy_df))
}
