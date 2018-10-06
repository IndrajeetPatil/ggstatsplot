#' @title A heteroscedastic one-way ANOVA for trimmed means with confidence
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
#' @param conf.level Scalar between 0 and 1. If `NULL`, the defaults return 95%
#'   lower and upper confidence intervals (`0.95`).
#' @inheritDotParams boot::boot
#'
#' @importFrom tibble as_data_frame
#' @importFrom dplyr select
#' @importFrom rlang enquo
#' @importFrom WRS2 t1way
#' @importFrom boot boot
#' @importFrom boot boot.ci
#' @importFrom stats na.omit
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
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
    stats::na.omit(.) %>%
    tibble::as.tibble(x = .)

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
    tibble::as_data_frame(
      x = cbind.data.frame(
        "xi" = bootci$t0,
        ci,
        "F-value" = fit$test,
        "df1" = fit$df1,
        "df2" = fit$df2,
        "p-value" = fit$p.value,
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
        `F-value`,
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
        `F-value`,
        df1,
        df2,
        dplyr::everything()
      )
  }

  # returning the results
  return(results_df)
}

#' @title A correlation test with confidence interval for effect size.
#' @name cor_tets_ci
#' @description Custom function to get confidence intervals for effect size
#'   measure for parametric or non-parametric correlation coefficient.
#'
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param x A vector containing the explanatory variable.
#' @param y The response - a vector of length the number of rows of `x`.
#' @param nboot Number of bootstrap samples for computing effect size (Default:
#'   `100`).
#' @param conf.type A vector of character strings representing the type of
#'   intervals required. The value should be any subset of the values `"norm"`,
#'   `"basic"`, `"perc"`, `"bca"`. For more, see `?boot::boot.ci`.
#' @param conf.level Scalar between 0 and 1. If `NULL`, the defaults return 95%
#'   lower and upper confidence intervals (`0.95`).
#' @inheritParams stats::cor.test
#' @inheritDotParams boot::boot
#'
#' @importFrom tibble as_data_frame
#' @importFrom dplyr select
#' @importFrom rlang enquo
#' @importFrom WRS2 t1way
#' @importFrom boot boot
#' @importFrom boot boot.ci
#' @importFrom stats na.omit
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#'
#' @keywords internal

cor_tets_ci <- function(data,
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
    stats::na.omit(.) %>%
    tibble::as.tibble(x = .)

  # running correlation and creating a tidy dataframe
  tidy_df <- broom::tidy(
    x = stats::cor.test(
      formula = stats::as.formula(~x + y),
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
          formula = stats::as.formula(~x + y),
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
    tibble::as_data_frame(
      x = cbind.data.frame(
        "r" = tidy_df$estimate,
        ci,
        "statistic" = tidy_df$statistic,
        "p-value" = tidy_df$p.value,
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
        `p-value`,
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
        `p-value`,
        conf,
        nboot,
        dplyr::everything()
      )
  }

  # return the final dataframe
  return(results_df)
}



#' @title Chi-squared test of association with confidence interval for effect size
#'   (Cramer's V).
#' @name chisq_v_ci
#' @description Custom function to get confidence intervals for effect size
#'   measure for chi-squared test of association (Contingency Tables analyses, i.e.).
#'
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param rows The variable to use as the rows in the contingency table.
#' @param cols the variable to use as the columns in the contingency table.
#' @param nboot Number of bootstrap samples for computing effect size (Default:
#'   `25`).
#' @param conf.type A vector of character strings representing the type of
#'   intervals required. The value should be any subset of the values `"norm"`,
#'   `"basic"`, `"perc"`, `"bca"`. For more, see `?boot::boot.ci`.
#' @param conf.level Scalar between 0 and 1. If `NULL`, the defaults return 95%
#'   lower and upper confidence intervals (`0.95`).
#' @inheritDotParams boot::boot
#'
#' @importFrom tibble as_data_frame
#' @importFrom dplyr select
#' @importFrom rlang enquo
#' @importFrom jmv contTables
#' @importFrom boot boot
#' @importFrom boot boot.ci
#' @importFrom stats na.omit
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#'
#' @keywords internal

chisq_v_ci <- function(data,
                       rows,
                       cols,
                       nboot = 25,
                       conf.level = 0.95,
                       conf.type = "norm",
                       ...) {
  # creating a dataframe from entered data
  data <- dplyr::select(
    .data = data,
    rows = !!rlang::enquo(rows),
    cols = !!rlang::enquo(cols)
  ) %>%
    stats::na.omit(.) %>%
    tibble::as.tibble(x = .)

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
    tibble::as_data_frame(
      x = cbind.data.frame(
        # cramer' V and confidence intervals
        "Cramer's V" = as.data.frame(jmv_df$nom)$`v[cra]`[[1]],
        ci,
        # getting rest of the details from chi-square test
        as.data.frame(jmv_df$chiSq) %>%
          tibble::as_data_frame()
      )
    )

  # selecting the columns corresponding to the confidence intervals
  if (conf.type == "norm") {
    results_df %<>%
      dplyr::select(
        .data = .,
        chi.sq = `value[chiSq]`,
        df = `df[chiSq]`,
        `Cramer's V`,
        conf.low = V2,
        conf.high = V3,
        `p-value` = `p[chiSq]`,
        dplyr::everything()
      )
  } else {
    results_df %<>%
      dplyr::select(
        .data = .,
        chi.sq = `value[chiSq]`,
        df = `df[chiSq]`,
        `Cramer's V`,
        conf.low = V4,
        conf.high = V5,
        `p-value` = `p[chiSq]`,
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
#' @param conf.type A vector of character strings representing the type of
#'   intervals required. The value should be any subset of the values `"norm"`,
#'   `"basic"`, `"perc"`, `"bca"`. For more, see `?boot::boot.ci`.
#' @param conf.level Scalar between 0 and 1. If `NULL`, the defaults return 95%
#'   lower and upper confidence intervals (`0.95`).
#' @inheritDotParams boot::boot
#'
#' @importFrom tibble as_data_frame
#' @importFrom dplyr select
#' @importFrom rlang enquo
#' @importFrom WRS2 pbcor
#' @importFrom boot boot
#' @importFrom boot boot.ci
#' @importFrom stats na.omit
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#'
#' @keywords internal

robcor_ci <- function(data,
                      x,
                      y,
                      beta = 0.1,
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
    stats::na.omit(.) %>%
    tibble::as.tibble(x = .)

  # getting the p-value for the correlation coefficient
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
    tibble::as_data_frame(x = cbind.data.frame(
      "r" = bootci$t0,
      ci,
      "p-value" = fit$p.value,
      "n" = fit$n,
      "nboot" = bootci$R,
      beta
    ))

  # selecting the columns corresponding to the confidence intervals
  if (conf.type == "norm") {
    results_df %<>%
      dplyr::select(
        .data = .,
        r,
        conf.low = V2,
        conf.high = V3,
        `p-value`,
        conf,
        n,
        beta,
        nboot
      )
  } else {
    results_df %<>%
      dplyr::select(
        .data = .,
        r,
        conf.low = V4,
        conf.high = V5,
        `p-value`,
        conf,
        n,
        beta,
        nboot
      )
  }

  # returning the results
  return(results_df)
}
