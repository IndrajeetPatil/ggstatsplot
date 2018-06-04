#'
#' @title A heteroscedastic one-way ANOVA for trimmed means with confidence
#'   interval for effect size.
#' @name t1way_ci
#' @aliases
#' @description Custom function to get confidence intervals for effect size
#'   measure for robust ANOVA
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
#' @examples
#'
#' # basic call
#'
#' # important for reproducibility
#' # set.seed(123)
#'
#' # ggstatsplot:::t1way_ci(
#' # data = iris,
#' # x = Species,
#' # y = Sepal.Length,
#' # conf.type = "norm"
#' # )
#'
#' @keywords internal
#'

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
  )

  # getting the p-value for the correlation coefficient
  fit <-
    WRS2::t1way(formula = stats::as.formula(y ~ x),
                data = data,
                tr = tr)

  # function to obtain 95% CI for xi
  xici <- function(formula, data, tr = tr, indices) {
    # allows boot to select sample
    d <- data[indices, ]
    # allows boot to select sample
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
    parallel =  "multicore",
    ...
  )

  # get 95% CI from the bootstrapped object
  bootci <- boot::boot.ci(boot.out = bootobj,
                          conf = conf.level,
                          type = conf.type)

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
        "xi" =  bootci$t0,
        ci,
        "F-value" = fit$test,
        "df1" = fit$df1,
        "df2" = fit$df2,
        "p-value" = fit$p.value,
        "nboot" =  bootci$R,
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
        `p-value`,
        conf,
        tr,
        nboot
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
        `p-value`,
        conf,
        tr,
        nboot
      )
  }

  # returning the results
  return(results_df)

}
