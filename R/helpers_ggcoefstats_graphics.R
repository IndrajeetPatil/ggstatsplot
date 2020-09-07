#' @title Create labels with statistical details for `ggcoefstats`
#' @name ggcoefstats_label_maker
#'
#' @param ... Currently ignored.
#' @param tidy_df A tidy dataframe.
#' @param ... Currently ignored.
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
                                    partial = TRUE,
                                    ...) {

  #----------------------- p-value cleanup ------------------------------------

  # formatting the p-values
  tidy_df %<>%
    signif_column(data = ., p = p.value) %>%
    ipmisc::p_value_formatter(data = ., k = k)

  #--------------------------- t-statistic ------------------------------------

  # if the statistic is t-value
  if (statistic == "t") {
    # check if df info is available somewhere
    if ("df.error" %in% names(tidy_df)) {
      # adding a new column with residual `df`
      tidy_df %<>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          label = paste(
            "list(~widehat(italic(beta))==",
            specify_decimal_p(x = estimate, k = k),
            ", ~italic(t)",
            "(",
            specify_decimal_p(x = df.error, k = 0L),
            ")==",
            specify_decimal_p(x = statistic, k = k),
            ", ~italic(p)",
            p.value.formatted,
            ")",
            sep = ""
          )
        )
    } else {
      # for objects like `rlm` there will be no parameter
      tidy_df %<>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          label = paste(
            "list(~widehat(italic(beta))==",
            specify_decimal_p(x = estimate, k = k),
            ", ~italic(t)",
            "==",
            specify_decimal_p(x = statistic, k = k),
            ", ~italic(p)",
            p.value.formatted,
            ")",
            sep = ""
          )
        )
    }
  }

  #--------------------------- z-statistic ---------------------------------

  # if the statistic is z-value
  if (statistic == "z") {
    tidy_df %<>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        label = paste(
          "list(~widehat(italic(beta))==",
          specify_decimal_p(x = estimate, k = k),
          ", ~italic(z)==",
          specify_decimal_p(x = statistic, k = k),
          ", ~italic(p)",
          p.value.formatted,
          ")",
          sep = ""
        )
      )
  }

  #--------------------------- chi^2-statistic ---------------------------------

  # if the statistic is chi^2-value
  if (statistic %in% c("c", "chi")) {
    tidy_df %<>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        label = paste(
          "list(~widehat(italic(beta))==",
          specify_decimal_p(x = estimate, k = k),
          ", ~italic(chi)^2==",
          specify_decimal_p(x = statistic, k = k),
          ", ~italic(p)",
          p.value.formatted,
          ")",
          sep = ""
        )
      )
  }

  #--------------------------- f-statistic ---------------------------------

  if (statistic == "f") {
    # which effect size is needed?
    if (effsize == "eta") {
      if (isTRUE(partial)) {
        effsize.text <- list(quote(widehat(italic(eta)[p]^2)))
      } else {
        effsize.text <- list(quote(widehat(italic(eta)^2)))
      }
    }

    if (effsize == "omega") {
      if (isTRUE(partial)) {
        effsize.text <- list(quote(widehat(italic(omega)[p]^2)))
      } else {
        effsize.text <- list(quote(widehat(italic(omega)^2)))
      }
    }

    # which effect size is needed?
    tidy_df %<>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        label = paste(
          "list(~italic(F)",
          "(",
          df1,
          "*\",\"*",
          df2,
          ")==",
          specify_decimal_p(x = statistic, k = k),
          ", ~italic(p)",
          p.value.formatted,
          ", ~",
          effsize.text,
          "==",
          specify_decimal_p(x = estimate, k = k),
          ")",
          sep = ""
        )
      )
  }

  # return the final dataframe
  return(dplyr::ungroup(tidy_df))
}


#' @name extract_statistic
#'
#' @importFrom insight find_statistic
#' @importFrom purrr pmap keep
#'
#' @noRd

extract_statistic <- function(x, ...) {
  # if not a dataframe, figure out what's the relevant statistic
  statistic <- insight::find_statistic(x)

  # standardize statistic type symbol for regression models
  # checking entered strings to extract the statistic
  grep_stat <- function(x, pattern) {
    if (isTRUE(grepl(pattern, x, ignore.case = TRUE))) {
      return(tolower(substring(x, 1, 1)))
    } else {
      return(NA_character_)
    }
  }

  # extracting statistic value
  purrr::pmap(
    .l =
      list(
        pattern = list("^t", "^f", "^z", "^chi"),
        x = list(statistic)
      ),
    .f = grep_stat
  ) %>%
    purrr::keep(.x = ., .p = ~ !is.na(.)) %>%
    .[[1]]
}


#' @title Confidence intervals for (partial) eta-squared and omega-squared for
#'   linear models.
#' @name lm_effsize_standardizer
#' @description This function will convert a linear model object to a dataframe
#'   containing statistical details for all effects along with effect size
#'   measure and its confidence interval. For more details, see
#'   `effectsize::eta_squared` and `effectsize::omega_squared`.
#' @return A dataframe with results from `stats::lm()` with partial eta-squared,
#'   omega-squared, and bootstrapped confidence interval for the same.
#'
#' @param object The linear model object (can be of class `lm`, `aov`, `anova`, or
#'   `aovlist`).
#' @param effsize Character describing the effect size to be displayed: `"eta"`
#'   (default) or `"omega"`.
#' @param partial Logical that decides if partial eta-squared or omega-squared
#'   are returned (Default: `TRUE`). If `FALSE`, eta-squared or omega-squared
#'   will be returned. Valid only for objects of class `lm`, `aov`, `anova`, or
#'   `aovlist`.
#' @param conf.level Numeric specifying Level of confidence for the confidence
#'   interval (Default: `0.95`).
#' @param ... Ignored.
#'
#' @importFrom effectsize eta_squared omega_squared
#' @importFrom broomExtra tidy_parameters
#' @importFrom stats anova na.omit lm
#' @importFrom rlang exec
#' @importFrom dplyr matches everything contains
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
                                    partial = TRUE,
                                    conf.level = 0.95,
                                    ...) {
  # for `lm` objects, `anova` object should be created
  if (class(object)[[1]] == "lm") object <- stats::anova(object)

  # stats details
  stats_df <- broomExtra::tidy_parameters(object, ...)

  # creating numerator and denominator degrees of freedom
  if (dim(dplyr::filter(stats_df, term == "Residuals"))[[1]] > 0L) {
    # create a new column for residual degrees of freedom
    # always going to be the last column
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
      partial = partial,
      ci = conf.level
    ) %>%
    broomExtra::easystats_to_tidy_names(.) %>%
    dplyr::filter(.data = ., !grepl(pattern = "Residuals", x = term, ignore.case = TRUE)) %>%
    dplyr::select(.data = ., -dplyr::matches("group"))

  # combine them in the same place
  dplyr::right_join(
    x = dplyr::filter(.data = stats_df, !is.na(statistic)), # for `aovlist` objects
    y = effsize_df,
    by = "term"
  ) %>% # renaming to standard term 'estimate'
    dplyr::rename(
      .data = .,
      "estimate" = dplyr::matches("eta|omega"),
      "df1" = "df",
      "conf.level" = "ci",
      "F.value" = "statistic"
    ) %>%
    dplyr::select(
      .data = .,
      term,
      F.value,
      dplyr::contains("df"),
      p.value,
      dplyr::everything(),
      -dplyr::contains("square")
    )
}
