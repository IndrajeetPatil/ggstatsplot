#' #' @title A heteroscedastic one-way ANOVA for trimmed means with confidence
#'   interval for effect size.
#' @name t1way_ci
#' @description Custom function to get confidence intervals for effect size
#'   measure for robust ANOVA.
#'
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param x The grouping variable.
#' @param y The response - a vector of length the number of rows of `x`.
#' @param nboot Number of bootstrap samples for computing effect size (Default:
#'   `100`).
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
#' \dontrun{
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
#' \dontrun{
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


#' @title A correlation test with confidence interval for effect size.
#' @name cor_test_ci
#' @description Custom function to get confidence intervals for effect size
#'   measure for parametric or non-parametric correlation coefficient.
#'
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param x A vector containing the explanatory variable.
#' @param y The response - a vector of length the number of rows of `x`.
#' @param nboot Number of bootstrap samples for computing effect size (Default:
#'   `100`).
#' @inheritDotParams boot::boot
#' @inheritParams stats::cor.test
#' @inheritParams t1way_ci
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom WRS2 t1way
#' @importFrom boot boot boot.ci
#' @importFrom stats na.omit
#'
#' @examples
#' \dontrun{
#' ggstatsplot:::cor_test_ci(
#'   data = ggplot2::msleep,
#'   x = brainwt,
#'   y = sleep_total,
#'   nboot = 25,
#'   conf.level = 0.99,
#'   conf.type = "perc",
#'   method = "spearman",
#'   continuity = TRUE,
#'   alternative = "greater"
#' )
#' }
#' 
#' @keywords internal

# function body
cor_test_ci <- function(data,
                        x,
                        y,
                        method = "spearman",
                        exact = FALSE,
                        continuity = TRUE,
                        alternative = "two.sided",
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
    tibble::as_tibble(x = .)

  # running correlation and creating a tidy dataframe
  tidy_df <- broom::tidy(
    x = stats::cor.test(
      formula = stats::as.formula(~ x + y),
      data = data,
      method = method,
      exact = exact,
      continuity = continuity,
      alternative = alternative
    )
  )

  # function to obtain 95% CI for xi
  corci <- function(formula,
                      x,
                      y,
                      method = method,
                      exact = exact,
                      continuity = continuity,
                      alternative = alternative,
                      indices) {
    # allows boot to select sample
    d <- data[indices, ]
    # allows boot to select sample
    boot_df <-
      broom::tidy(
        stats::cor.test(
          formula = stats::as.formula(~ x + y),
          data = d,
          method = method,
          exact = exact,
          continuity = continuity,
          alternative = alternative,
          na.action = na.omit
        )
      )

    # return the value of interest: effect size
    return(boot_df$estimate)
  }

  # save the bootstrapped results to an object
  bootobj <- boot::boot(
    data = data,
    statistic = corci,
    R = nboot,
    x = x,
    y = y,
    method = method,
    exact = exact,
    continuity = continuity,
    alternative = alternative,
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
        "r" = tidy_df$estimate,
        ci,
        "statistic" = tidy_df$statistic,
        "p.value" = tidy_df$p.value,
        "nboot" = bootci$R,
        "method" = tidy_df$method,
        "alternative" = as.character(alternative)
      )
    )

  # selecting the columns corresponding to the confidence intervals
  if (conf.type == "norm") {
    results_df %<>%
      dplyr::select(
        .data = .,
        r,
        conf.low = V2,
        conf.high = V3,
        p.value,
        conf,
        nboot,
        dplyr::everything()
      )
  } else {
    results_df %<>%
      dplyr::select(
        .data = .,
        r,
        conf.low = V4,
        conf.high = V5,
        p.value,
        conf,
        nboot,
        dplyr::everything()
      )
  }

  # return the final dataframe
  return(results_df)
}


#' @title Chi-squared test of association with confidence interval for effect
#'   size (Cramer's *V*).
#' @name chisq_v_ci
#' @description Custom function to get confidence intervals for effect size
#'   measure for chi-squared test of association (Contingency Tables analyses,
#'   i.e.).
#'
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param rows The variable to use as the rows in the contingency table.
#' @param cols the variable to use as the columns in the contingency table.
#' @param nboot Number of bootstrap samples for computing effect size (Default:
#'   `25`).
#' @inheritParams t1way_ci
#' @inheritDotParams boot::boot
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom jmv contTables
#' @importFrom boot boot
#' @importFrom boot boot.ci
#' @importFrom stats na.omit
#'
#' @examples
#' \dontrun{
#' ggstatsplot:::chisq_v_ci(
#'   data = ggstatsplot::Titanic_full,
#'   rows = Sex,
#'   cols = Survived,
#'   nboot = 12,
#'   conf.level = 0.90,
#'   conf.type = "norm"
#' )
#' }
#' 
#' @keywords internal

# function body
chisq_v_ci <- function(data,
                       rows,
                       cols,
                       nboot = 25,
                       conf.level = 0.95,
                       conf.type = "norm",
                       ...) {
  # creating a dataframe from entered data
  data <-
    dplyr::select(
      .data = data,
      rows = !!rlang::enquo(rows),
      cols = !!rlang::enquo(cols)
    ) %>%
    dplyr::filter(.data = ., !is.na(rows), !is.na(cols)) %>%
    tibble::as_tibble(x = .)

  # results from jamovi
  jmv_df <- jmv::contTables(
    data = data,
    rows = "rows",
    cols = "cols",
    phiCra = TRUE
  )

  # the basic function to get the confidence interval
  cramer_ci <- function(data,
                          rows,
                          cols,
                          phiCra,
                          indices) {
    # allows boot to select sample
    d <- data[indices, ]
    # allows boot to select sample
    boot_df <- jmv::contTables(
      data = d,
      rows = "rows",
      cols = "cols",
      phiCra = phiCra
    )

    # return the value of interest: effect size
    return(as.data.frame(boot_df$nom)$`v[cra]`[[1]])
  }

  # save the bootstrapped results to an object
  bootobj <- boot::boot(
    data = data,
    statistic = cramer_ci,
    R = nboot,
    rows = rows,
    cols = cols,
    phiCra = TRUE,
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

  # preparing the dataframe
  results_df <-
    tibble::as_tibble(
      x = cbind.data.frame(
        "Cramer.V" = as.data.frame(jmv_df$nom)$`v[cra]`[[1]],
        ci,
        tibble::as_tibble(as.data.frame(jmv_df$chiSq))
      )
    )

  # selecting the columns corresponding to the confidence intervals
  if (conf.type == "norm") {
    results_df %<>%
      dplyr::select(
        .data = .,
        chi.sq = `value[chiSq]`,
        df = `df[chiSq]`,
        Cramer.V,
        conf.low = V2,
        conf.high = V3,
        p.value = `p[chiSq]`,
        dplyr::everything()
      )
  } else {
    results_df %<>%
      dplyr::select(
        .data = .,
        chi.sq = `value[chiSq]`,
        df = `df[chiSq]`,
        Cramer.V,
        conf.low = V4,
        conf.high = V5,
        p.value = `p[chiSq]`,
        dplyr::everything()
      )
  }

  # return the final dataframe
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
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param x A vector containing the explanatory variable.
#' @param y The response - a vector of length the number of rows of `x`.
#' @param nboot Number of bootstrap samples for computing effect size (Default:
#'   `100`).
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
#' \dontrun{
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

#' @title Confidence interval for effect size for Kruskal-Wallis test.
#' @name kw_eta_h_ci
#' @description Custom function to get confidence intervals for effect size
#'   measure for Kruskal-Wallis Rank Sum Test.
#'
#' @inheritParams t1way_ci
#' @inheritDotParams boot::boot
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom stats kruskal.test
#' @importFrom boot boot boot.ci
#' @importFrom stats na.omit
#'
#' @examples
#' \dontrun{
#' ggstatsplot:::kw_eta_h_ci(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   nboot = 100,
#'   conf.level = 0.90,
#'   conf.type = "basic"
#' )
#' }
#' 
#' @keywords internal

# function to get confidence intervals
kw_eta_h_ci <- function(data,
                        x,
                        y,
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

  # custom function to get eta-squared value
  kw_eta_h <- function(data,
                         x,
                         y) {
    # creating a dataframe from entered data
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(x),
        y = !!rlang::enquo(y)
      ) %>%
      dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
      tibble::as_tibble(x = .)

    # no. of levels (parameter; k)
    parameter <- length(levels(as.factor(data$x)))

    # no. of observations (n)
    sample_size <- nrow(data)

    # running the function
    fit <-
      stats::kruskal.test(
        formula = y ~ x,
        data = data
      )

    # calculating the eta-squared estimate using the H-statistic
    # ref. http://www.tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
    effsize <-
      (fit$statistic[[1]] - parameter + 1) /
        (sample_size - parameter)

    # return the value of interest: effect size
    return(effsize[[1]])
  }

  # eta-squared value
  eta_sq_H <- kw_eta_h(
    data = data,
    x = x,
    y = y
  )

  # function to obtain 95% CI for for eta-squared
  eta_h_ci <- function(data, x, y, indices) {
    # allows boot to select sample
    d <- data[indices, ]

    # running the function
    fit <-
      kw_eta_h(
        data = d,
        x = x,
        y = y
      )

    # return the value of interest: effect size
    return(fit)
  }

  # save the bootstrapped results to an object
  bootobj <- boot::boot(
    data = data,
    x = x,
    y = y,
    statistic = eta_h_ci,
    R = nboot,
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
      "eta_sq_H" = eta_sq_H,
      ci,
      "nboot" = bootci$R
    ))

  # selecting the columns corresponding to the confidence intervals
  if (conf.type == "norm") {
    results_df %<>%
      dplyr::select(
        .data = .,
        eta_sq_H,
        conf.low = V2,
        conf.high = V3,
        conf,
        nboot
      )
  } else {
    results_df %<>%
      dplyr::select(
        .data = .,
        eta_sq_H,
        conf.low = V4,
        conf.high = V5,
        conf,
        nboot
      )
  }

  # returning the results
  return(results_df)
}
