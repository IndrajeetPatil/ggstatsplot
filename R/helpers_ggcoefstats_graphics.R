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


#' @title Tidying anova-like objects
#' @name lm_effsize_standardizer
#'
#' @param object The object of the following class: `aov`, `aovlist`, `anova`,
#'   `Gam`, `manova`, `maov`.
#' @param effsize Character describing the effect size to be displayed: `"eta"`
#'   (default) or `"omega"`.
#' @param conf.level Numeric specifying Level of confidence for the confidence
#'   interval (Default: `0.95`).
#' @param ... Ignored.
#'
#' @importFrom effectsize eta_squared omega_squared
#' @importFrom broomExtra tidy_parameters
#' @importFrom insight standardize_names
#' @importFrom rlang exec
#' @importFrom dplyr filter right_join rename
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#'
#' # model
#' mod <-
#'   stats::aov(
#'     formula = mpg ~ wt + qsec + Error(disp / am),
#'     data = mtcars
#'   )
#'
#' # dataframe with effect size and confidence intervals
#' ggstatsplot:::lm_effsize_standardizer(mod)
#' @keywords internal

# defining the function body
lm_effsize_standardizer <- function(object,
                                    effsize = "eta",
                                    conf.level = 0.95,
                                    ...) {
  # stats details
  stats_df <- broomExtra::tidy_parameters(object, ...)

  # creating numerator and denominator degrees of freedom
  if (dim(dplyr::filter(stats_df, term == "Residuals"))[[1]] > 0L) {
    stats_df$df2 <- stats_df$df[nrow(stats_df)]
  }

  # function to compute effect sizes
  if (effsize == "eta") {
    .f <- effectsize::eta_squared
  } else {
    .f <- effectsize::omega_squared
  }

  # computing effect size
  effsize_df <-
    rlang::exec(
      .fn = .f,
      model = object,
      partial = TRUE,
      ci = conf.level
    ) %>%
    insight::standardize_names(data = ., style = "broom")

  # combine them in the same place
  dplyr::right_join(
    x = dplyr::filter(.data = stats_df, !is.na(statistic)), # for `aovlist` objects
    y = effsize_df,
    by = "term"
  ) %>% # renaming to standard term 'estimate'
    dplyr::rename(.data = ., "df1" = "df")
}
