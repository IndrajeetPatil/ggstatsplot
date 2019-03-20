#' @title Computing confidence intervals for the Kendall's coefficient of
#'   concordance (aka Kendall's *W*).
#' @name kendall_w_ci
#' @author Chuck Powell, Indrajeet Patil
#'
#' @importFrom dplyr tibble
#'
#' @inheritParams subtitle_anova_parametric
#' @param id.variable Variable identifying a unique observation.
#'
#' @examples
#'
#' set.seed(123)
#'
#' ggstatsplot:::kendall_w_ci(
#'   data = iris_long,
#'   x = condition,
#'   y = value,
#'   id.variable = id
#' )
#' @keywords internal

# this function will return Kendall and CI's for the specified conf.level
kendall_w_ci <- function(data,
                         x,
                         y,
                         id.variable,
                         conf.level = 0.95,
                         ...) {
  # creating a dataframe from entered data
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y),
      id.variable = !!rlang::enquo(id.variable)
    ) %>%
    tibble::as_tibble(x = .)

  # ensure these are factors
  data$x <- factor(data$x)
  data$id.variable <- factor(data$id.variable)

  # get sample size and number of measurements
  sample_size <- length(unique(data$id.variable))
  no_measurements <- length(levels(data$x))

  # run the friedman test
  friedman_stat <-
    stats::friedman.test(
      formula = y ~ x | id.variable,
      data = data,
      na.action = na.omit
    )

  # calculating Kendall's W
  # ref: http://www.tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
  kendall_w <- (friedman_stat$statistic[[1]]) /
    (sample_size * (no_measurements - 1))

  # calculate confidence around our chi square
  chi_square_ci <- chi_ncp_ci(
    chi.square = friedman_stat$statistic[[1]],
    conf.level = conf.level,
    df = friedman_stat$parameter[[1]],
    tol = 1e-09,
    jumping.prop = 0.1
  )

  # compute confidence intervals
  effsize.LL <-
    chi_square_ci$Lower.Limit / (sample_size * (no_measurements - 1))
  effsize.UL <-
    chi_square_ci$Upper.Limit / (sample_size * (no_measurements - 1))

  # since we're using ncp, ensure that the results are sane
  if (effsize.LL < 0) {
    effsize.LL <- 0
  }

  if (effsize.UL > 1) {
    effsize.UL <- 1
  }

  # preparing a dataframe out of the results
  results_df <-
    dplyr::tibble(
      "estimate" = kendall_w,
      "conf.low" = effsize.LL,
      "conf.high" = effsize.UL
    )

  # return the tibble
  return(results_df)
}

#' @title Computing confidence intervals for chi-squared statistic.
#' @name chi_ncp_ci
#' @author Chuck Powell
#'
#' @inheritParams kendall_w_ci
#' @inheritParams stats::pchisq
#' @param chi.square A value of chi-squared statistic.
#' @param tol Tolerance.
#' @param jumping.prop Jumping proportion.
#'
#' @importFrom stats pchisq
#' @keywords internal

# function body
chi_ncp_ci <- function(chi.square,
                       conf.level = 0.95,
                       df,
                       tol = 1e-09,
                       jumping.prop = 0.1,
                       ...) {

  # input checks
  if (jumping.prop <= 0 | jumping.prop >= 1) {
    stop("The Jumping Proportion ('jumping.prop') must be between zero and one.")
  }

  # checking chi-squared value
  if (chi.square < 0) {
    stop("Your 'chi.square' is not correctly specified.")
  }

  # checking confidence intervals
  if (conf.level >= 1 | conf.level <= 0) {
    stop("Your confidence level ('conf.level') must be between 0 and 1.")
  }

  # needed confidence interval
  alpha.lower <- alpha.upper <- (1 - conf.level) / 2

  # initializing FAILED variable
  FAILED <- NULL

  # lower bound of CI
  if (alpha.lower > 0) {
    LL.0 <- 0.01
    Diff <- stats::pchisq(q = chi.square, df = df, ncp = LL.0) -
      (1 - alpha.lower)
    if (stats::pchisq(q = chi.square, df = df, ncp = LL.0) < (1 -
      alpha.lower)) {
      FAILED <- if (stats::pchisq(q = chi.square, df = df, ncp = 0) <
        1 - alpha.lower) {
        LL.0 <- 1e-08
      }
      if (stats::pchisq(q = chi.square, df = df, ncp = LL.0) <
        1 - alpha.lower) {
        FAILED <- TRUE
      }
      if (FAILED == TRUE) {
        warning(
          "The size of the effect combined with the degrees of freedom is too small
          to determine a lower confidence limit for the 'alpha.lower'
          (or the (1/2)(1-'conf.level') symmetric) value specified (set to zero).",
          call. = FALSE
        )
      }
    }
    if (is.null(FAILED)) {
      LL.1 <- LL.2 <- LL.0
      while (Diff > tol) {
        LL.2 <- LL.1 * (1 + jumping.prop)
        Diff <- stats::pchisq(
          q = chi.square,
          df = df,
          ncp = LL.2
        ) -
          (1 - alpha.lower)
        LL.1 <- LL.2
      }
      LL.1 <- LL.2 / (1 + jumping.prop)
      LL.Bounds <- c(LL.1, (LL.1 + LL.2) / 2, LL.2)
      Diff <-
        stats::pchisq(q = chi.square, df = df, ncp = LL.Bounds[2]) -
        (1 - alpha.lower)
      while (abs(Diff) > tol) {
        Diff.1 <- stats::pchisq(
          q = chi.square,
          df = df,
          ncp = LL.Bounds[1]
        ) -
          (1 - alpha.lower) > tol
        Diff.2 <-
          stats::pchisq(
            q = chi.square,
            df = df,
            ncp = LL.Bounds[2]
          ) -
            (1 - alpha.lower) > tol
        Diff.3 <-
          stats::pchisq(
            q = chi.square,
            df = df,
            ncp = LL.Bounds[3]
          ) -
            (1 - alpha.lower) > tol
        if (Diff.1 == TRUE & Diff.2 == TRUE & Diff.3 ==
          FALSE) {
          LL.Bounds <- c(LL.Bounds[2], (LL.Bounds[2] +
            LL.Bounds[3]) / 2, LL.Bounds[3])
        }
        if (Diff.1 == TRUE & Diff.2 == FALSE & Diff.3 ==
          FALSE) {
          LL.Bounds <- c(LL.Bounds[1], (LL.Bounds[1] +
            LL.Bounds[2]) / 2, LL.Bounds[2])
        }
        Diff <-
          stats::pchisq(
            q = chi.square,
            df = df,
            ncp = LL.Bounds[2]
          ) -
          (1 - alpha.lower)
      }
      LL <- LL.Bounds[2]
    }
  }

  # if everything fails
  if (!is.null(FAILED)) {
    LL <- 0
  }

  # upper bound of CI
  if (alpha.upper > 0) {
    FAILED.Up <- NULL
    UL.0 <- LL + 0.01
    Diff <- stats::pchisq(q = chi.square, df = df, ncp = UL.0) -
      alpha.upper
    if (Diff < 0) {
      UL.0 <- 1e-08
    }
    Diff <- stats::pchisq(q = chi.square, df = df, ncp = UL.0) -
      alpha.upper
    if (Diff < 0) {
      FAILED.Up <- TRUE
      warning(
        "The size of the effect combined with the degrees of freedom is too small
        to determine an upper confidence limit for the 'alpha.upper'
        (or (1/2)(1-'conf.level') symmetric) value specified.",
        call. = FALSE
      )
    }
    if (is.null(FAILED.Up)) {
      UL.1 <- UL.2 <- UL.0
      while (Diff > tol) {
        UL.2 <- UL.1 * (1 + jumping.prop)
        Diff <- stats::pchisq(
          q = chi.square,
          df = df,
          ncp = UL.2
        ) -
          alpha.upper
        UL.1 <- UL.2
      }
      UL.1 <- UL.2 / (1 + jumping.prop)
      UL.Bounds <- c(UL.1, (UL.1 + UL.2) / 2, UL.2)
      Diff <-
        stats::pchisq(q = chi.square, df = df, ncp = UL.Bounds[2]) -
        alpha.upper
      while (abs(Diff) > tol) {
        Diff.1 <- stats::pchisq(
          q = chi.square,
          df = df,
          ncp = UL.Bounds[1]
        ) -
          alpha.upper > tol
        Diff.2 <-
          stats::pchisq(
            q = chi.square,
            df = df,
            ncp = UL.Bounds[2]
          ) -
            alpha.upper > tol
        Diff.3 <-
          stats::pchisq(
            q = chi.square,
            df = df,
            ncp = UL.Bounds[3]
          ) -
            alpha.upper > tol
        if (Diff.1 == TRUE & Diff.2 == TRUE & Diff.3 ==
          FALSE) {
          UL.Bounds <- c(UL.Bounds[2], (UL.Bounds[2] +
            UL.Bounds[3]) / 2, UL.Bounds[3])
        }
        if (Diff.1 == TRUE & Diff.2 == FALSE & Diff.3 ==
          FALSE) {
          UL.Bounds <- c(UL.Bounds[1], (UL.Bounds[1] +
            UL.Bounds[2]) / 2, UL.Bounds[2])
        }
        Diff <-
          stats::pchisq(
            q = chi.square,
            df = df,
            ncp = UL.Bounds[2]
          ) -
          alpha.upper
      }
      UL <- UL.Bounds[2]
    }

    # if everything fails
    if (!is.null(FAILED.Up)) {
      UL <- NA
    }
  }

  # both bounds okay
  if (alpha.lower > 0 & alpha.upper > 0) {
    return(list(
      Lower.Limit = LL,
      Upper.Limit = UL
    ))
  }

  # only upper bound okay
  if (alpha.lower == 0 & alpha.upper > 0) {
    return(list(
      Lower.Limit = 0,
      Upper.Limit = UL
    ))
  }

  # only lower bound okay
  if (alpha.lower > 0 & alpha.upper == 0) {
    return(list(
      Lower.Limit = LL,
      Upper.Limit = Inf
    ))
  }
}
