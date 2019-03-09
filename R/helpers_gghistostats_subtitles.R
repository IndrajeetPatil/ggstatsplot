#' @title Making text subtitle for one sample t-test and its nonparametric and
#'   robust equivalents.
#' @name subtitle_t_onesample
#' @author Indrajeet Patil
#'
#' @param x A numeric variable.
#' @param test.value A number specifying the value of the null hypothesis
#'   (Default: `0`).
#' @param robust.estimator If `test = "robust"` robust estimator to be used
#'   (`"onestep"` (Default), `"mom"`, or `"median"`). For more, see
#'   `?WRS2::onesampb`.
#' @param effsize.type Type of effect size needed for *parametric* tests. The
#'   argument can be `"biased"` (`"d"` for Cohen's *d*) or `"unbiased"`
#'   (`"g"` Hedge's *g* for **t-test**). The default is
#' @param ... Additional arguments.
#' @inheritParams ggbetweenstats
#' @inheritParams t1way_ci
#'
#' @importFrom dplyr select bind_rows summarize mutate mutate_at mutate_if
#' @importFrom dplyr group_by n arrange
#' @importFrom jmv ttestOneS
#' @importFrom WRS2 onesampb
#' @importFrom scales percent
#' @importFrom crayon green blue yellow red
#' @importFrom psych cohen.d.ci
#' @importFrom groupedstats specify_decimal_p
#' @importFrom ellipsis check_dots_used
#' @importFrom rcompanion wilcoxonOneSampleR
#'
#' @seealso \code{\link{gghistostats}}
#'
#' @examples
#'
#' # for reproducibility
#' set.seed(123)
#'
#' ggstatsplot::subtitle_t_onesample(
#'   data = iris,
#'   x = Sepal.Length,
#'   test.value = 5,
#'   type = "r"
#' )
#' @export

subtitle_t_onesample <- function(data,
                                 x,
                                 type = "parametric",
                                 test.value = 0,
                                 bf.prior = 0.707,
                                 robust.estimator = "onestep",
                                 effsize.type = "g",
                                 effsize.noncentral = TRUE,
                                 conf.level = 0.95,
                                 conf.type = "norm",
                                 nboot = 100,
                                 k = 2,
                                 messages = TRUE,
                                 ...) {

  # check the dots
  ellipsis::check_dots_used()

  # ====================== dataframe ========================================

  # preparing a dataframe out of provided inputs
  data <- dplyr::select(.data = data, x = !!rlang::enquo(x)) %>%
    tidyr::drop_na(data = .) %>%
    tibble::as_tibble(x = .)

  # sample size
  sample_size <- nrow(data)

  # ========================== stats ==========================================

  # standardize the type of statistics
  stats.type <- stats_type_switch(stats.type = type)

  # decide whether to run bayesian tests
  if (stats.type == "bayes") {
    bf <- TRUE
  } else {
    bf <- FALSE
  }

  # common test
  stats_df <-
    jmv::ttestOneS(
      data = data,
      vars = "x",
      students = TRUE,
      bf = bf,
      bfPrior = bf.prior,
      wilcoxon = TRUE,
      testValue = test.value,
      hypothesis = "dt",
      effectSize = TRUE,
      miss = "listwise"
    )

  # extracting the relevant information
  stats_df <- as.data.frame(stats_df$ttest)

  # ========================= parametric ======================================
  if (stats.type == "parametric") {

    # deciding which effect size to use (Hedge's g or Cohen's d)
    if (effsize.type %in% c("unbiased", "g")) {
      hedges.correction <- TRUE
      effsize.text <- quote(italic("g"))
    } else if (effsize.type %in% c("biased", "d")) {
      hedges.correction <- FALSE
      effsize.text <- quote(italic("d"))
    }

    # creating tobject
    tobj <- stats::t.test(
      x = data$x,
      mu = test.value,
      conf.level = conf.level,
      alternative = "two.sided",
      na.action = na.omit
    )

    # creating effect size info
    effsize_df <- effsize_t_parametric(
      formula = ~x,
      data = data,
      tobject = tobj,
      mu = test.value,
      hedges.correction = hedges.correction,
      conf.level = conf.level
    )

    # preparing subtitle
    subtitle <- subtitle_template(
      no.parameters = 1L,
      stat.title = NULL,
      statistic.text = quote(italic("t")),
      statistic = stats_df$`stat[stud]`,
      parameter = stats_df$`df[stud]`,
      p.value = stats_df$`p[stud]`,
      effsize.text = effsize.text,
      effsize.estimate = effsize_df$estimate,
      effsize.LL = effsize_df$conf.low,
      effsize.UL = effsize_df$conf.high,
      n = sample_size,
      conf.level = conf.level,
      k = k,
      k.parameter = 0L
    )

    # ========================== non-parametric ==============================
  } else if (stats.type == "nonparametric") {
    # setting up the Mann-Whitney U-test and getting its summary
    stats_df <-
      broom::tidy(stats::wilcox.test(
        x = data$x,
        alternative = "two.sided",
        na.action = na.omit,
        mu = test.value,
        exact = FALSE,
        correct = TRUE,
        conf.int = TRUE,
        conf.level = conf.level
      ))

    # effect size dataframe
    effsize_df <- rcompanion::wilcoxonOneSampleR(
      x = data$x,
      mu = test.value,
      ci = TRUE,
      conf = conf.level,
      type = conf.type,
      R = nboot,
      histogram = FALSE,
      digits = k
    )

    # message about effect size measure
    if (isTRUE(messages)) {
      effsize_ci_message(nboot = nboot, conf.level = conf.level)
    }

    # preparing subtitle
    subtitle <- subtitle_template(
      no.parameters = 0L,
      parameter = NULL,
      parameter2 = NULL,
      stat.title = NULL,
      statistic.text = quote("log"["e"](italic("V"))),
      statistic = log(stats_df$statistic[[1]]),
      p.value = stats_df$p.value[[1]],
      effsize.text = quote(italic(r)),
      effsize.estimate = effsize_df$r[[1]],
      effsize.LL = effsize_df$lower.ci[[1]],
      effsize.UL = effsize_df$upper.ci[[1]],
      n = sample_size,
      conf.level = conf.level,
      k = k
    )
    # ======================= robust =========================================
  } else if (stats.type == "robust") {

    # running one-sample percentile bootstrap
    stats_df <- WRS2::onesampb(
      x = data$x,
      est = robust.estimator,
      nboot = nboot,
      nv = test.value
    )

    # displaying message about bootstrap
    if (isTRUE(messages)) {
      base::message(cat(
        crayon::green("Note: "),
        crayon::blue("95% CI for robust location measure computed with "),
        crayon::yellow(nboot),
        crayon::blue(" bootstrap samples.\n"),
        sep = ""
      ))
    }

    # preparing the subtitle
    subtitle <- base::substitute(
      expr =
        paste(
          italic("M")[robust],
          " = ",
          estimate,
          ", CI"["95%"],
          " [",
          LL,
          ", ",
          UL,
          "], ",
          italic("p"),
          " = ",
          p.value,
          ", ",
          italic("n"),
          " = ",
          n
        ),
      env = base::list(
        estimate = specify_decimal_p(x = stats_df$estimate[[1]], k = k),
        LL = specify_decimal_p(x = stats_df$ci[[1]], k = k),
        UL = specify_decimal_p(x = stats_df$ci[[2]], k = k),
        p.value = specify_decimal_p(
          x = stats_df$p.value[[1]],
          k = k,
          p.value = TRUE
        ),
        n = sample_size
      )
    )
    # ===================== bayes ============================================
  } else if (stats.type == "bayes") {
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
          ", ",
          italic("r")["Cauchy"],
          " = ",
          bf_prior,
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
        df = stats_df$`df[stud]`,
        estimate = specify_decimal_p(x = stats_df$`stat[stud]`, k = k),
        bf = specify_decimal_p(x = log(stats_df$`stat[bf]`), k = k),
        bf_prior = specify_decimal_p(x = bf.prior, k = k),
        effsize = specify_decimal_p(x = stats_df$`es[stud]`, k = k),
        n = sample_size
      )
    )
  }

  # return the subtitle
  return(subtitle)
}
