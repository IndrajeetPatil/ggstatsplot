#' @title Making text subtitle for one sample t-test and its nonparametric and
#'   robust equivalents.
#' @name subtitle_t_onesample
#' @author Indrajeet Patil
#'
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken. This argument is optional.
#' @param x A numeric variable.
#' @param test.value A number specifying the value of the null hypothesis
#'   (Default: `0`).
#' @param robust.estimator If `test = "robust"` robust estimator to be used
#'   (`"onestep"` (Default), `"mom"`, or `"median"`). For more, see
#'   `?WRS2::onesampb`.
#' @param nboot Number of bootstrap samples for robust one-sample location test
#'   (Default: `100`).
#' @inheritParams ggbetweenstats
#'
#' @importFrom dplyr select bind_rows summarize mutate mutate_at mutate_if
#' @importFrom dplyr group_by n arrange
#' @importFrom jmv ttestOneS
#' @importFrom WRS2 onesampb
#' @importFrom scales percent
#' @importFrom crayon green blue yellow red
#' @importFrom psych cohen.d.ci
#' @importFrom groupedstats specify_decimal_p
#'
#' @seealso \code{\link{gghistostats}}
#'
#' @examples
#' 
#' # for reproducibility
#' set.seed(123)
#' 
#' subtitle_t_onesample(
#'   x = iris$Sepal.Length,
#'   type = "r"
#' )
#' @export

subtitle_t_onesample <- function(data = NULL,
                                 x,
                                 type = "parametric",
                                 test.value = 0,
                                 bf.prior = 0.707,
                                 robust.estimator = "onestep",
                                 conf.level = 0.95,
                                 nboot = 100,
                                 k = 2,
                                 messages = TRUE) {

  # ====================== dataframe ==========================================

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
    dplyr::filter(.data = ., !is.na(x)) %>%
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

    # confidence intervals for Cohen's d
    effsize_df <-
      psych::cohen.d.ci(
        d = stats_df$`es[stud]`,
        n1 = sample_size,
        alpha = 1 - conf.level
      ) %>%
      tibble::as_tibble(x = .)

    # preparing subtitle
    subtitle <- subtitle_template(
      no.parameters = 1L,
      stat.title = NULL,
      statistic.text = quote(italic("t")),
      statistic = stats_df$`stat[stud]`,
      parameter = stats_df$`df[stud]`,
      p.value = stats_df$`p[stud]`,
      effsize.text = quote(italic("d")),
      effsize.estimate = stats_df$`es[stud]`,
      effsize.LL = effsize_df$lower[[1]],
      effsize.UL = effsize_df$upper[[1]],
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

    # preparing subtitle
    subtitle <- subtitle_template(
      no.parameters = 0L,
      parameter = NULL,
      parameter2 = NULL,
      stat.title = NULL,
      statistic.text = quote("log"["e"](italic("W"))),
      statistic = log(stats_df$statistic[[1]]),
      p.value = stats_df$p.value[[1]],
      effsize.text = quote(Delta["HLS"]),
      effsize.estimate = stats_df$estimate[[1]],
      effsize.LL = stats_df$conf.low[[1]],
      effsize.UL = stats_df$conf.high[[1]],
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
        p.value = specify_decimal_p(x = stats_df$p.value[[1]], k = k, p.value = TRUE),
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
          ", Prior width = ",
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
