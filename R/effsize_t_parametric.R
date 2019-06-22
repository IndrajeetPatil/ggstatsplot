#' @title Calculating Cohen's *d* or Hedge's *g* (for between-/within- or one
#'   sample designs).
#' @name effsize_t_parametric
#' @author Chuck Powell
#'
#' @param formula This function only accepts the variables in `formula` format
#'   e.g. `sleep_rem ~ vore` (two sample) or `~ vore` (one sample).
#' @param mu If conducting a single sample test against a mean (Default: `0`).
#' @param hedges.correction Logical indicating whether to apply Hedges
#'   correction, Hedge's *g* (Default: `TRUE`).
#' @param noncentral Logical indicating whether to use non-central
#'   *t*-distributions for computing the confidence intervals (Default: `TRUE`).
#' @param tobject Object with the *t*-test specification.
#' @inheritParams ggbetweenstats
#' @inheritParams subtitle_t_parametric
#'
#' @importFrom stats t.test na.omit cor qt pt uniroot
#' @importFrom tibble tibble
#' @importFrom methods is
#'
#' @details
#' This function is a rewrite of functionality provided in `lsr::cohensD` and
#' `effsize::cohen.d`.
#'
#' References-
#' \itemize{
#' \item Cooper, Harris, Hedges, Larry V., Valentine, Jeffrey C., The Handbook
#' of Research Synthesis and Meta-Analysis, 2009. \item Cumming, G., Finch, S.,
#' A Primer On The Understanding, Use, And Calculation Of Confidence Intervals
#' That Are Based On Central And Noncentral Distributions, Educational and
#' Psychological Measurement, Vol. 61 No. 4, August 2001 532-574. \item Cohen,
#' J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.)
#' Hillsdale, NJ: Lawrence Erlbaum Associates. \item David C. Howell (2010).
#' Confidence Intervals on Effect Size
#' }
#'
#' @examples
#' \donttest{
#' #---------------- two-sample test ------------------------------------
#'
#' # creating a smaller dataset
#' msleep_short <- dplyr::filter(
#'   .data = ggplot2::msleep,
#'   vore %in% c("carni", "herbi")
#' )
#'
#' # with defaults
#' tobj1 <- t.test(
#'   formula = sleep_rem ~ vore,
#'   data = msleep_short
#' )
#' ggstatsplot:::effsize_t_parametric(
#'   formula = sleep_rem ~ vore,
#'   data = msleep_short,
#'   tobject = tobj1
#' )
#'
#' # changing defaults
#' tobj2 <- t.test(
#'   formula = sleep_rem ~ vore,
#'   data = msleep_short,
#'   mu = 1,
#'   paired = FALSE,
#'   conf.level = .99
#' )
#' ggstatsplot:::effsize_t_parametric(
#'   formula = sleep_rem ~ vore,
#'   data = msleep_short,
#'   mu = 1, # ignored in this case
#'   paired = FALSE,
#'   hedges.correction = TRUE,
#'   conf.level = .99,
#'   noncentral = FALSE,
#'   tobject = tobj2
#' )
#'
#' #---------------- one-sample test ------------------------------------
#'
#' tobj3 <- t.test(
#'   x = msleep_short$sleep_rem,
#'   mu = 2,
#'   conf.level = .90
#' )
#' ggstatsplot:::effsize_t_parametric(
#'   formula = ~sleep_rem,
#'   data = msleep_short,
#'   mu = 2,
#'   hedges.correction = TRUE,
#'   conf.level = .90,
#'   noncentral = TRUE,
#'   tobject = tobj3
#' )
#' }
#' @keywords internal

# function body
effsize_t_parametric <- function(formula = NULL,
                                 data = NULL,
                                 mu = 0,
                                 paired = FALSE,
                                 hedges.correction = TRUE,
                                 conf.level = .95,
                                 var.equal = FALSE,
                                 noncentral = TRUE,
                                 tobject = NULL,
                                 ...) {

  # -------------- input checking -------------------

  if (!is(formula, "formula") | !is(data, "data.frame")) {
    stop("arguments must include a formula and a data frame")
  }
  if (length(formula) == 2 & length(all.vars(formula)) > 1) {
    stop("Your formula has too many items on the rhs")
  }
  if (length(formula) == 3 & length(all.vars(formula)) > 2) {
    stop("Your formula has too many variables")
  }
  if (is.null(tobject)) {
    stop("This is an internal function and requires a tobject as
         part of its call")
  }

  # -------------- single sample compare to mu -------------------

  if (length(formula) == 2 & length(all.vars(formula)) == 1) {
    method <- "Cohen's d"
    x <- eval(formula[[2]], data)
    x <- x[!is.na(x)]
    n <- length(x)
    sd.est <- sd(x)
    df <- length(x) - 1
    mean.diff <- mean(x) - mu
    d <- mean.diff / sd.est
    Sigmad <- sqrt((n / (n / 2)^2) + (d^2 / (2 * n)))
    Z <- -stats::qt((1 - conf.level) / 2, df)
    tvalue <- tobject$statistic
    dfvalue <- tobject$parameter
    civalue <- conf.level
    twosamples <- FALSE
    paired <- NA_character_
  }

  # ---------------two independent samples by factor -------------------

  # two samples by factor
  if (length(formula) == 3 & !isTRUE(paired)) {
    # getting `x` and `y` in required format
    outcome <- eval(formula[[2]], data)
    group <- eval(formula[[3]], data)
    group <- factor(group)

    # test relevant variables
    x <- split(outcome, group)
    y <- x[[2]]
    x <- x[[1]]
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
    sq.devs <- (c(x - mean(x), y - mean(y)))^2
    n <- length(sq.devs)
    n1 <- length(x)
    n2 <- length(y)
    if (isTRUE(var.equal)) {
      sd.est <- sqrt(sum(sq.devs) / (n - 2))
    } else {
      sd.est <- sqrt((var(x) + var(y)) / 2)
    }
    mean.diff <- mean(x) - mean(y)
    df <- tobject$parameter
    d <- mean.diff / sd.est
    Sigmad <- sqrt((n1 + n2) / (n1 * n2) + 0.5 * d^2 / (n1 + n2))
    Z <- -stats::qt((1 - conf.level) / 2, df)
    method <- "Cohen's d"
    tvalue <- tobject$statistic
    dfvalue <- tobject$parameter
    civalue <- conf.level
    twosamples <- TRUE
  }

  # -------------- two paired samples in matching columns -------------------

  # if the data is in tidy format
  if (length(formula) == 3 & isTRUE(paired)) {
    if (is.factor(eval(formula[[3]], data)) ||
      is.character(eval(formula[[3]], data))) {
      # getting `x` and `y` in required format
      outcome <- eval(formula[[2]], data)
      group <- eval(formula[[3]], data)
      group <- droplevels(as.factor(group))
      x <- split(outcome, group)
      y <- x[[2]]
      x <- x[[1]]
    } else {
      x <- eval(formula[[2]], data)
      y <- eval(formula[[3]], data)
      ind <- !is.na(x) & !is.na(y)
      x <- x[ind]
      y <- y[ind]
    }

    # test relevant variables
    n <- length(x)
    df <- n - 1
    r <- cor(x, y)
    sd.est <- sd(x - y)
    mean.diff <- mean(x) - mean(y)
    d <- mean.diff / sd.est
    Sigmad <- sqrt(((1 / n) + (d^2 / n)) * 2 * (1 - r)) # paired
    Z <- -qt((1 - conf.level) / 2, df)
    method <- "Cohen's d"
    tvalue <- tobject$statistic
    dfvalue <- tobject$parameter
    civalue <- conf.level
    twosamples <- FALSE
  }

  # -------------- apply hedges correction -------------------

  if (hedges.correction == TRUE) {
    method <- "Hedges's g"
    d <- d * (n - 3) / (n - 2.25)
  }

  lower.ci <- c(d - Z * Sigmad)
  upper.ci <- c(d + Z * Sigmad)

  # -------------- calculate NCP intervals -------------------

  if (isTRUE(noncentral)) {
    st <- max(0.1, tvalue)
    end1 <- tvalue
    while (stats::pt(q = tvalue, df = dfvalue, ncp = end1) > (1 - civalue) / 2) {
      end1 <- end1 + st
    }
    ncp1 <- uniroot(
      function(x)
        (1 - civalue) / 2 - stats::pt(
          q = tvalue,
          df = dfvalue,
          ncp = x
        ),
      c(2 * tvalue - end1, end1)
    )$root
    end2 <- tvalue
    while (stats::pt(q = tvalue, df = dfvalue, ncp = end2) < (1 + civalue) / 2) {
      end2 <- end2 - st
    }
    ncp2 <- uniroot(
      function(x)
        (1 + civalue) / 2 - stats::pt(
          q = tvalue,
          df = dfvalue,
          ncp = x
        ),
      c(end2, 2 * tvalue - end2)
    )$root

    if (isTRUE(twosamples)) {
      ncp.upper.ci <- ncp1 * sqrt(1 / n1 + 1 / n2)
      ncp.lower.ci <- ncp2 * sqrt(1 / n1 + 1 / n2)
    } else {
      ncp.upper.ci <- ncp1 / sqrt(dfvalue)
      ncp.lower.ci <- ncp2 / sqrt(dfvalue)
    }
  }

  # -------------- return results desired ------------------

  if (isTRUE(noncentral)) {
    lower.ci <- min(ncp.lower.ci, ncp.upper.ci)
    upper.ci <- max(ncp.lower.ci, ncp.upper.ci)
  }

  # return the final dataframe with results
  return(tibble::tibble(
    method = method,
    estimate = d,
    conf.low = lower.ci,
    conf.high = upper.ci,
    conf.level = conf.level,
    alternative = "two.sided",
    paired = paired,
    noncentral = noncentral,
    var.equal = var.equal
  ))
}
