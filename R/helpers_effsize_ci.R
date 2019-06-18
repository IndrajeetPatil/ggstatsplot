#' #' @title A heteroscedastic one-way ANOVA for trimmed means with confidence
#'   interval for effect size.
#' @name t1way_ci
#' @description Custom function to get confidence intervals for effect size
#'   measure for robust ANOVA.
#'
#' @param data A dataframe (or a tibble) from which variables specified are to
#'   be taken. A matrix or tables will **not** be accepted.
#' @param x The grouping variable from the dataframe `data`.
#' @param y The response (a.k.a. outcome or dependent) variable from the
#'   dataframe `data`.
#' @param nboot Number of bootstrap samples for computing confidence interval
#'   for the effect size (Default: `100`).
#' @param tr Trim level for the mean when carrying out `robust` tests. If you
#'   get error stating "Standard error cannot be computed because of Winsorized
#'   variance of 0 (e.g., due to ties). Try to decrease the trimming level.",
#'   try to play around with the value of `tr`, which is by default set to
#'   `0.1`. Lowering the value might help.
#' @param conf.type A vector of character strings representing the type of
#'   intervals required. The value should be any subset of the values `"norm"`,
#'   `"basic"`, `"perc"`, `"bca"`. For more, see `?boot::boot.ci`.
#' @param conf.level Scalar between 0 and 1. If unspecified, the defaults return
#'   `95%` lower and upper confidence intervals (`0.95`).
#' @inheritDotParams boot::boot
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom WRS2 t1way
#' @importFrom boot boot boot.ci
#' @importFrom stats na.omit
#'
#' @examples
#'
#' \donttest{
#' set.seed(123)
#' ggstatsplot:::t1way_ci(
#'   data = dplyr::filter(ggplot2::msleep, vore != "insecti"),
#'   x = vore,
#'   y = brainwt,
#'   tr = 0.05,
#'   nboot = 50,
#'   conf.level = 0.99,
#'   conf.type = "perc"
#' )
#' }
#'
#' @keywords internal

t1way_ci <- function(data,
                     x,
                     y,
                     tr = 0.1,
                     nboot = 100,
                     conf.level = 0.95,
                     conf.type = "norm",
                     ...) {
  # creating a dataframe from entered data
  data <- dplyr::select(
    .data = data,
    x = !!rlang::enquo(x),
    y = !!rlang::enquo(y)
  ) %>%
    dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
    tibble::as_tibble(x = .)

  # running robust one-way anova
  fit <-
    WRS2::t1way(
      formula = stats::as.formula(y ~ x),
      data = data,
      tr = tr
    )

  # function to obtain 95% CI for xi
  xici <- function(formula, data, tr, indices) {
    # allows boot to select sample
    d <- data[indices, ]
    # running the function
    fit <-
      WRS2::t1way(
        formula = stats::as.formula(formula),
        data = d,
        tr = tr
      )

    # return the value of interest: effect size
    return(fit$effsize)
  }

  # save the bootstrapped results to an object
  bootobj <- boot::boot(
    data = data,
    statistic = xici,
    R = nboot,
    formula = y ~ x,
    tr = tr,
    parallel = "multicore",
    ...
  )

  # get 95% CI from the bootstrapped object
  bootci <- boot::boot.ci(
    boot.out = bootobj,
    conf = conf.level,
    type = conf.type
  )

  # extracting ci part
  if (conf.type == "norm") {
    ci <- bootci$normal
  } else if (conf.type == "basic") {
    ci <- bootci$basic
  } else if (conf.type == "perc") {
    ci <- bootci$perc
  } else if (conf.type == "bca") {
    ci <- bootci$bca
  }

  # preparing a dataframe out of the results
  results_df <-
    tibble::as_tibble(
      x = cbind.data.frame(
        "xi" = bootci$t0,
        ci,
        "F.value" = fit$test,
        "df1" = fit$df1,
        "df2" = fit$df2,
        "p.value" = fit$p.value,
        "nboot" = bootci$R,
        tr
      )
    )

  # selecting the columns corresponding to the confidence intervals
  if (conf.type == "norm") {
    results_df %<>%
      dplyr::select(
        .data = .,
        xi,
        conf.low = V2,
        conf.high = V3,
        F.value,
        df1,
        df2,
        dplyr::everything()
      )
  } else {
    results_df %<>%
      dplyr::select(
        .data = .,
        xi,
        conf.low = V4,
        conf.high = V5,
        F.value,
        df1,
        df2,
        dplyr::everything()
      )
  }

  # returning the results
  return(results_df)
}

#' @title Paired samples robust *t*-tests with confidence
#'   interval for effect size.
#' @name yuend_ci
#' @description Custom function to get confidence intervals for effect size
#'   measure for paired samples robust t-tests.
#'
#' @inheritParams t1way_ci
#' @inheritDotParams boot::boot
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom WRS2 yuend
#' @importFrom boot boot boot.ci
#' @importFrom stats na.omit
#'
#' @examples
#'
#' \donttest{
#' ggstatsplot:::yuend_ci(
#'   data = dplyr::filter(
#'     .data = ggstatsplot::iris_long,
#'     condition %in% c("Sepal.Length", "Petal.Length")
#'   ),
#'   x = condition,
#'   y = value,
#'   nboot = 50,
#'   tr = 0.2
#' )
#' }
#'
#' @keywords internal

# function body
yuend_ci <- function(data,
                     x,
                     y,
                     tr = 0.1,
                     nboot = 100,
                     conf.level = 0.95,
                     conf.type = "norm",
                     ...) {
  # creating a dataframe from entered data
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    ) %>%
    dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
    tibble::as_tibble(x = .)

  # jamovi needs data to be wide format and not long format
  data_wide <-
    long_to_wide_converter(
      data = data,
      x = x,
      y = y
    )

  # sample size
  sample_size <- nrow(data_wide)

  # running robust one-way anova
  fit <-
    WRS2::yuend(
      x = data_wide[2],
      y = data_wide[3],
      tr = tr
    )

  # function to obtain 95% CI for xi
  xici <- function(data, tr, indices) {
    # allows boot to select sample
    d <- data[indices, ]

    # running the function
    fit <-
      WRS2::yuend(
        x = d[2],
        y = d[3],
        tr = tr
      )

    # return the value of interest: effect size
    return(fit$effsize)
  }

  # save the bootstrapped results to an object
  bootobj <- boot::boot(
    statistic = xici,
    R = nboot,
    data = data_wide,
    tr = tr,
    parallel = "multicore",
    ...
  )

  # get 95% CI from the bootstrapped object
  bootci <- boot::boot.ci(
    boot.out = bootobj,
    conf = conf.level,
    type = conf.type
  )

  # extracting ci part
  if (conf.type == "norm") {
    ci <- bootci$normal
  } else if (conf.type == "basic") {
    ci <- bootci$basic
  } else if (conf.type == "perc") {
    ci <- bootci$perc
  } else if (conf.type == "bca") {
    ci <- bootci$bca
  }

  # preparing a dataframe out of the results
  results_df <-
    tibble::as_tibble(
      x = cbind.data.frame(
        "xi" = bootci$t0,
        ci,
        "t.value" = fit$test,
        "df" = fit$df,
        "p.value" = fit$p.value,
        "nboot" = bootci$R,
        tr,
        n = sample_size
      )
    )

  # selecting the columns corresponding to the confidence intervals
  if (conf.type == "norm") {
    results_df %<>%
      dplyr::select(
        .data = .,
        xi,
        conf.low = V2,
        conf.high = V3,
        t.value,
        df,
        dplyr::everything()
      )
  } else {
    results_df %<>%
      dplyr::select(
        .data = .,
        xi,
        conf.low = V4,
        conf.high = V5,
        t.value,
        df,
        dplyr::everything()
      )
  }

  # returning the results
  return(results_df)
}

#' @title Robust correlation coefficient and its confidence interval
#' @name robcor_ci
#' @description Custom function to get confidence intervals for percentage bend
#'   correlation coefficient.
#' @return A tibble with percentage bend correlation coefficient, along with its
#'   confidence intervals, and the number of bootstrap samples used to generate
#'   confidence intervals. Additionally, it also includes information about
#'   sample size, bending constant, no. of bootstrap samples, etc.
#'
#' @param x A vector containing the explanatory variable.
#' @param y The response - a vector of length the number of rows of `x`.
#' @param beta bending constant (Default: `0.1`). For more, see `?WRS2::pbcor`.
#' @inheritParams t1way_ci
#' @inheritDotParams boot::boot
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom WRS2 pbcor
#' @importFrom boot boot boot.ci
#' @importFrom stats na.omit
#'
#' @examples
#'
#' \donttest{
#' ggstatsplot:::robcor_ci(
#'   data = mtcars,
#'   x = "hp",
#'   y = "mpg",
#'   beta = .01,
#'   nboot = 125,
#'   conf.level = .99,
#'   conf.type = c("basic")
#' )
#' }
#'
#' @keywords internal

# function body
robcor_ci <- function(data,
                      x,
                      y,
                      beta = 0.1,
                      nboot = 100,
                      conf.level = 0.95,
                      conf.type = "norm",
                      ...) {
  # creating a dataframe from entered data
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    ) %>%
    dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
    tibble::as_tibble(x = .)

  # getting the p.value for the correlation coefficient
  fit <-
    WRS2::pbcor(
      x = data$x,
      y = data$y,
      beta = beta
    )

  # function to obtain 95% CI for xi
  robcor_ci <- function(data, x, y, beta = beta, indices) {
    # allows boot to select sample
    d <- data[indices, ]
    # allows boot to select sample
    fit <-
      WRS2::pbcor(
        x = d$x,
        y = d$y,
        beta = beta
      )

    # return the value of interest: correlation coefficient
    return(fit$cor)
  }

  # save the bootstrapped results to an object
  bootobj <- boot::boot(
    data = data,
    statistic = robcor_ci,
    R = nboot,
    x = x,
    y = y,
    beta = beta,
    parallel = "multicore",
    ...
  )

  # get 95% CI from the bootstrapped object
  bootci <- boot::boot.ci(
    boot.out = bootobj,
    conf = conf.level,
    type = conf.type
  )

  # extracting ci part
  if (conf.type == "norm") {
    ci <- bootci$normal
  } else if (conf.type == "basic") {
    ci <- bootci$basic
  } else if (conf.type == "perc") {
    ci <- bootci$perc
  } else if (conf.type == "bca") {
    ci <- bootci$bca
  }

  # preparing a dataframe out of the results
  results_df <-
    tibble::as_tibble(x = cbind.data.frame(
      "estimate" = bootci$t0,
      ci,
      "p.value" = fit$p.value,
      "statistic" = fit$test[[1]],
      "n" = fit$n,
      "nboot" = bootci$R,
      beta
    ))

  # selecting the columns corresponding to the confidence intervals
  if (conf.type == "norm") {
    results_df %<>%
      dplyr::select(
        .data = .,
        estimate,
        conf.low = V2,
        conf.high = V3,
        dplyr::everything()
      )
  } else {
    results_df %<>%
      dplyr::select(
        .data = .,
        estimate,
        conf.low = V4,
        conf.high = V5,
        dplyr::everything()
      )
  }

  # returning the results
  return(results_df)
}
