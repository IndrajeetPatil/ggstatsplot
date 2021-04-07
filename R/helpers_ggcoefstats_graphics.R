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

  # all operations will have to be done rowwise
  tidy_df %<>% dplyr::rowwise()

  #--------------------------- t-statistic ------------------------------------

  # if the statistic is t-value
  if (statistic == "t") {
    # if not present, add NA column for dfs
    if (!"df.error" %in% names(tidy_df)) tidy_df %<>% dplyr::mutate(df.error = NA)

    tidy_df %<>%
      dplyr::mutate(
        label = dplyr::case_when(
          is.na(df.error) || is.infinite(df.error) ~ paste0(
            "list(~widehat(italic(beta))=='", format_value(estimate, k),
            "', ~italic(t)=='", format_value(statistic, k),
            "', ~italic(p)=='", format_num(p.value, k, TRUE), "')"
          ),
          TRUE ~ paste0(
            "list(~widehat(italic(beta))=='", format_value(estimate, k),
            "', ~italic(t)", "('", format_value(df.error, 0L), "')=='", format_value(statistic, k),
            "', ~italic(p)=='", format_num(p.value, k, TRUE), "')"
          )
        )
      )
  }

  #--------------------------- z-statistic ---------------------------------

  # if the statistic is z-value
  if (statistic == "z") {
    tidy_df %<>%
      dplyr::mutate(
        label = paste0(
          "list(~widehat(italic(beta))=='", format_value(estimate, k),
          "', ~italic(z)=='", format_value(statistic, k),
          "', ~italic(p)=='", format_num(p.value, k, TRUE), "')"
        )
      )
  }

  #--------------------------- chi^2-statistic ---------------------------------

  # if the statistic is chi^2-value
  if (statistic == "c") {
    # if not present, add NA column for dfs
    if (!"df.error" %in% names(tidy_df)) tidy_df %<>% dplyr::mutate(df.error = NA)

    tidy_df %<>%
      dplyr::mutate(
        label = paste0(
          "list(~widehat(italic(beta))=='", format_value(estimate, k),
          "', ~italic(chi)^2~", "('", format_value(df.error, 0L), "')==", format_value(statistic, k),
          "', ~italic(p)=='", format_num(p.value, k, TRUE), "')"
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
          "list(~italic(F)", "('", format_value(df, 0L), "'*\",\"*'", format_value(df.error, 0L),
          "')=='", format_value(statistic, k),
          "', ~italic(p)=='", format_num(p.value, k, TRUE),
          "', ~", effsize.text, "=='", format_value(estimate, k), "')"
        )
      )
  }

  # return the ungrouped dataframe
  dplyr::ungroup(tidy_df)
}
