
#'
#' @title Making text subtitle for one sample t-test and its nonparametric and
#'   robust equivalents.
#' @name subtitle_onesample
#' @author Indrajeet Patil
#'
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken. This argument is optional.
#' @param x A numeric variable.
#' @param type Type of statistic expected (`"parametric"` or `"nonparametric"`
#'   or `"robust"` or `"bayes"`). Abbreviations accepted are `"p"` or `"np"` or
#'   `"r"` or `"bf"`, respectively.
#' @param test.value A number specifying the value of the null hypothesis
#'   (Default: `0`).
#' @param bf.prior A number between 0.5 and 2 (default `0.707`), the prior width
#'   to use in calculating Bayes factors.
#' @param robust.estimator If `test = "robust"` robust estimator to be used
#'   (`"onestep"` (Default), `"mom"`, or `"median"`). For more, see
#'   `?WRS2::onesampb`.
#' @param nboot Number of bootstrap samples for robust one-sample location test
#'   (Default: `100`).
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @inheritParams specify_decimal_p
#'
#' @examples
#'
#' subtitle_onesample(x = iris$Sepal.Length, type = "r")
#'
#' @export
#'

subtitle_onesample <- function(data = NULL,
                               x,
                               type = "parametric",
                               test.value = 0,
                               bf.prior = 0.707,
                               robust.estimator = "onestep",
                               nboot = 100,
                               k = 3,
                               messages = TRUE) {

  # ========================================== dataframe ==============================================================

  # preparing a dataframe out of provided inputs
  if (!is.null(data)) {
    # if dataframe is provided
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(x)
      )
  } else {
    # if vectors are provided
    data <-
      base::cbind.data.frame(x = x)
  }

  # convert to a tibble
  data %<>%
    tibble::as_data_frame(x = .)

  # ========================================== stats ==================================================================

    # common test
  jmv_results <- jmv::ttestOneS(
    data = data,
    vars = "x",
    students = TRUE,
    bf = TRUE,
    bfPrior = bf.prior,
    wilcoxon = TRUE,
    # Mann-Whitney U test
    testValue = test.value,
    hypothesis = "dt",
    # two-sided hypothesis-testing
    effectSize = TRUE,
    miss = "listwise"
    # excludes a row from all analyses if one of its entries is missing
  )

  # =================================== parametric =====================================================
  if (type == "parametric" || type == "p") {
    # preparing the subtitle
    subtitle <- base::substitute(
      expr =
        paste(
          italic("t"),
          "(",
          df,
          ") = ",
          estimate,
          ", ",
          italic("p"),
          " = ",
          pvalue,
          ", ",
          italic("d"),
          " = ",
          effsize,
          ", ",
          italic("n"),
          " = ",
          n
        ),
      env = base::list(
        estimate = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_results$ttest)$`stat[stud]`, k),
        # df is integer value for Student's t-test
        df = as.data.frame(jmv_results$ttest)$`df[stud]`,
        pvalue = ggstatsplot::specify_decimal_p(
          x = as.data.frame(jmv_results$ttest)$`p[stud]`,
          k,
          p.value = TRUE
        ),
        effsize = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_results$ttest)$`es[stud]`, k),
        n = nrow(x = data)
      )
    )

    # ========================================== non-parametric =====================================
  } else if (type == "nonparametric" || type == "np") {
    # preparing the subtitle
    subtitle <- base::substitute(
      expr =
        paste(
          italic("U"),
          " = ",
          estimate,
          ", ",
          italic("p"),
          " = ",
          pvalue,
          ", ",
          italic("d"),
          " = ",
          effsize,
          ", ",
          italic("n"),
          " = ",
          n
        ),
      env = base::list(
        estimate = as.data.frame(jmv_results$ttest)$`stat[wilc]`,
        pvalue = ggstatsplot::specify_decimal_p(
          x = as.data.frame(jmv_results$ttest)$`p[wilc]`,
          k,
          p.value = TRUE
        ),
        effsize = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_results$ttest)$`es[wilc]`, k),
        n = nrow(x = data)
      )
    )

    # ========================================== robust ===============================================

    } else if (type == "robust" || type == "r") {

    # running one-sample percentile bootstrap
    rob_os <- WRS2::onesampb(
      x = data$x,
      est = robust.estimator,
      nboot = nboot,
      nv = test.value
    )

    # displaying message about bootstrap
    if (isTRUE(messages)) {
      base::message(cat(
        crayon::green("Note:"),
        crayon::blue(
          "95% CI for robsut location measure",
          crayon::yellow("median, Huber Psi"),
          "computed with ",
          crayon::yellow(nboot),
          "bootstrap samples."
        )
      ))
    }

    # preparing the subtitle
    subtitle <- base::substitute(
      expr =
        paste(
          "M"[robust],
          " = ",
          estimate,
          ", 95% CI [",
          LL,
          ", ",
          UL,
          "], ",
          italic("p"),
          " = ",
          pvalue,
          ", ",
          italic("n"),
          " = ",
          n
        ),
      env = base::list(
        estimate = ggstatsplot::specify_decimal_p(x = rob_os$estimate[[1]], k),
        LL = ggstatsplot::specify_decimal_p(x = rob_os$ci[[1]], k),
        UL = ggstatsplot::specify_decimal_p(x = rob_os$ci[[2]], k),
        pvalue = ggstatsplot::specify_decimal_p(
          x = rob_os$p.value[[1]],
          k,
          p.value = TRUE
        ),
        n = rob_os$n[[1]]
      )
    )
    # ========================================== bayes ==================================================================
  } else if (type == "bayes" || type == "bf") {
    # preparing the subtitle
    subtitle <- base::substitute(
      expr =
        paste(
          italic("t"),
          "(",
          df,
          ") = ",
          estimate,
          ", log"["e"],
          "(BF"["10"],
          ") = ",
          bf,
          ", log"["e"],
          "(error) = ",
          bf_error,
          "% , ",
          italic("d"),
          " = ",
          effsize,
          ", ",
          italic("n"),
          " = ",
          n
        ),
      env = base::list(
        # df is integer value for Student's t-test
        df = as.data.frame(jmv_results$ttest)$`df[stud]`,
        estimate = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_results$ttest)$`stat[stud]`, k),
        bf = ggstatsplot::specify_decimal_p(
          x = log(
            x = as.data.frame(jmv_results$ttest)$`stat[bf]`,
            base = exp(1)
          ),
          k = 1
        ),
        bf_error = ggstatsplot::specify_decimal_p(
          x = log(
            x = as.data.frame(jmv_results$ttest)$`err[bf]`,
            base = exp(1)
          ),
          k = 1
        ),
        effsize = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_results$ttest)$`es[stud]`, k),
        n = nrow(x = data)
      )
    )
  }

  # return the subtitle
  return(subtitle)

}
