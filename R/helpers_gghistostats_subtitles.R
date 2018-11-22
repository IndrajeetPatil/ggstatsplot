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
    stats::na.omit(.) %>%
    tibble::as_data_frame(x = .)

  # ========================== stats ==========================================

  # decide whether to run bayesian tests
  if (type %in% c("bayes", "bf")) {
    bf <- TRUE
  } else {
    bf <- FALSE
  }

  # common test
  jmv_results <-
    jmv::ttestOneS(
      data = data,
      vars = "x",
      students = TRUE,
      bf = bf,
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

  # ========================= parametric ======================================
  if (type %in% c("parametric", "p")) {

    # confidence intervals for Cohen's d
    ci_df <-
      psych::cohen.d.ci(
        d = as.data.frame(jmv_results$ttest)$`es[stud]`,
        n1 = length(data$x),
        alpha = .05
      ) %>%
      tibble::as_data_frame(x = .)

    # preparing the subtitle
    subtitle <-
      base::substitute(
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
            ", 95% CI [",
            LL,
            ", ",
            UL,
            "]",
            ", ",
            italic("n"),
            " = ",
            n
          ),
        env = base::list(
          estimate = ggstatsplot::specify_decimal_p(
            x = as.data.frame(jmv_results$ttest)$`stat[stud]`,
            k = k,
            p.value = FALSE
          ),
          df = as.data.frame(jmv_results$ttest)$`df[stud]`,
          pvalue = ggstatsplot::specify_decimal_p(
            x = as.data.frame(jmv_results$ttest)$`p[stud]`,
            k,
            p.value = TRUE
          ),
          effsize = ggstatsplot::specify_decimal_p(
            x = as.data.frame(jmv_results$ttest)$`es[stud]`,
            k = k,
            p.value = FALSE
          ),
          LL = ggstatsplot::specify_decimal_p(x = ci_df$lower[[1]], k = k),
          UL = ggstatsplot::specify_decimal_p(x = ci_df$upper[[1]], k = k),
          n = nrow(x = data)
        )
      )

    # ========================== non-parametric ==============================
  } else if (type %in% c("nonparametric", "np")) {
    # preparing the subtitle
    subtitle <-
      base::substitute(
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
          effsize = ggstatsplot::specify_decimal_p(
            x = as.data.frame(jmv_results$ttest)$`es[wilc]`,
            k = k,
            p.value = FALSE
          ),
          n = nrow(x = data)
        )
      )

    # ======================= robust =========================================
  } else if (type %in% c("robust", "r")) {

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
        crayon::green("Note: "),
        crayon::blue(
          "95% CI for robust location measure",
          crayon::yellow("median, Huber Psi"),
          "computed with ",
          crayon::yellow(nboot),
          "bootstrap samples.\n"
        ),
        sep = ""
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
    # ===================== bayes ============================================
  } else if (type %in% c("bayes", "bf")) {
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
        df = as.data.frame(jmv_results$ttest)$`df[stud]`,
        estimate = ggstatsplot::specify_decimal_p(
          x = as.data.frame(jmv_results$ttest)$`stat[stud]`,
          k = k,
          p.value = FALSE
        ),
        bf = ggstatsplot::specify_decimal_p(
          x = log(
            x = as.data.frame(jmv_results$ttest)$`stat[bf]`,
            base = exp(1)
          ),
          k = k,
          p.value = FALSE
        ),
        bf_prior = ggstatsplot::specify_decimal_p(
          x = bf.prior,
          k = k,
          p.value = FALSE
        ),
        effsize = ggstatsplot::specify_decimal_p(
          x = as.data.frame(jmv_results$ttest)$`es[stud]`,
          k = k,
          p.value = FALSE
        ),
        n = nrow(x = data)
      )
    )
  }

  # return the subtitle
  return(subtitle)
}
