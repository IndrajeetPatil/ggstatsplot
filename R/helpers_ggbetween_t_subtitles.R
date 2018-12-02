#' @title Making text subtitle for the t-test (between-/within-subjects
#'   designs).
#' @name subtitle_t_parametric
#' @author Indrajeet Patil
#'
#' @param effsize.noncentral Logical indicating whether to use non-central
#'   *t*-distributions for computing the confidence interval for Cohen's *d*
#'   or Hedge's *g* (Default: `FALSE`).
#' @param conf.level A scalar value between 0 and 1. If unspecified, the
#'    default is to return `95%` lower and upper confidence intervals (`0.95`).
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: `k = 2`).
#' @param ... Additional arguments (ignored).
#' @inheritParams subtitle_anova_parametric
#' @inheritParams stats::t.test
#'
#' @importFrom dplyr select mutate_at
#' @importFrom rlang !! enquo
#' @importFrom stats t.test na.omit
#' @importFrom effsize cohen.d
#'
#' @examples
#' 
#' # creating a smaller dataset
#' msleep_short <- dplyr::filter(
#'   .data = ggplot2::msleep,
#'   vore %in% c("carni", "herbi")
#' )
#' 
#' # with defaults
#' subtitle_t_parametric(
#'   data = msleep_short,
#'   x = vore,
#'   y = sleep_rem
#' )
#' 
#' # changing defaults
#' subtitle_t_parametric(
#'   data = msleep_short,
#'   x = vore,
#'   y = sleep_rem,
#'   var.equal = TRUE,
#'   k = 2,
#'   effsize.type = "d"
#' )
#' @export

# function body
subtitle_t_parametric <- function(data,
                                  x,
                                  y,
                                  paired = FALSE,
                                  effsize.type = "g",
                                  effsize.noncentral = FALSE,
                                  conf.level = 0.95,
                                  var.equal = FALSE,
                                  k = 2,
                                  ...) {

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    ) %>%
    dplyr::filter(.data = ., !is.na(x), !is.na(y))

  # convert the grouping variable to factor and drop unused levels
  data %<>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "x",
      .funs = ~ base::droplevels(x = base::as.factor(x = .))
    ) %>%
    tibble::as_tibble(x = .)

  # sample size
  sample_size <- nrow(data)

  # setting up the t-test model and getting its summary
  t_stat <-
    stats::t.test(
      formula = y ~ x,
      data = data,
      paired = paired,
      alternative = "two.sided",
      var.equal = var.equal,
      na.action = na.omit
    )

  # deciding which effect size to use
  if (effsize.type %in% c("unbiased", "g")) {
    # Hedge's g is an unbiased estimate of the effect size
    hedges.correction <- TRUE
  } else if (effsize.type %in% c("biased", "d")) {
    hedges.correction <- FALSE
  }

  # effect size object
  t_effsize <-
    effsize::cohen.d(
      formula = y ~ x,
      data = data,
      paired = paired,
      hedges.correction = hedges.correction,
      na.rm = TRUE,
      conf.level = conf.level,
      noncentral = effsize.noncentral
    )

  # when paired samples t-test is run df is going to be integer
  if (isTRUE(paired)) {
    k.df <- 0
  } else {
    k.df <- k
  }

  # preparing the subtitle
  if (effsize.type %in% c("unbiased", "g")) {

    # preparing subtitle with Hedge's g
    subtitle <-
      # extracting the elements of the statistical object
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
            italic("g"),
            " = ",
            effsize,
            ", CI"[conf.level],
            " [",
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
            x = t_stat[[1]],
            k = k,
            p.value = FALSE
          ),
          df = ggstatsplot::specify_decimal_p(
            x = t_stat[[2]],
            k = k.df,
            p.value = FALSE
          ),
          pvalue = ggstatsplot::specify_decimal_p(
            x = t_stat[[3]],
            k = k,
            p.value = TRUE
          ),
          effsize = ggstatsplot::specify_decimal_p(
            x = t_effsize[[3]],
            k = k,
            p.value = FALSE
          ),
          conf.level = paste(conf.level * 100, "%", sep = ""),
          LL = ggstatsplot::specify_decimal_p(
            x = t_effsize$conf.int[[1]],
            k = k,
            p.value = FALSE
          ),
          UL = ggstatsplot::specify_decimal_p(
            x = t_effsize$conf.int[[2]],
            k = k,
            p.value = FALSE
          ),
          n = sample_size
        )
      )
  } else if (effsize.type %in% c("biased", "d")) {

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
            ", CI"[conf.level],
            " [",
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
          estimate = ggstatsplot::specify_decimal_p(x = t_stat[[1]], k = k),
          df = ggstatsplot::specify_decimal_p(x = t_stat[[2]], k.df),
          pvalue = ggstatsplot::specify_decimal_p(
            x = t_stat[[3]],
            k = k,
            p.value = TRUE
          ),
          effsize = ggstatsplot::specify_decimal_p(x = t_effsize[[3]], k = k),
          conf.level = paste(conf.level * 100, "%", sep = ""),
          LL = ggstatsplot::specify_decimal_p(x = t_effsize$conf.int[[1]], k = k),
          UL = ggstatsplot::specify_decimal_p(x = t_effsize$conf.int[[2]], k = k),
          n = sample_size
        )
      )
  }

  # return the subtitle
  return(subtitle)
}


#' @title Making text subtitle for the Mann-Whitney U-test
#'   (between-/within-subjects designs).
#' @author Indrajeet Patil
#'
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @param ... Additional arguments (ignored).
#' @inheritParams subtitle_t_parametric
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom stats wilcox.test
#' @importFrom coin wilcox_test
#'
#' @examples
#' subtitle_mann_nonparametric(
#'   data = sleep,
#'   x = group,
#'   y = extra
#' )
#' @export

# function body
subtitle_mann_nonparametric <-
  function(data,
             x,
             y,
             paired = FALSE,
             k = 2,
             messages = TRUE,
             ...) {

    # creating a dataframe
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(x),
        y = !!rlang::enquo(y)
      ) %>%
      dplyr::filter(.data = ., !is.na(x), !is.na(y))

    # convert the grouping variable to factor and drop unused levels
    data %<>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "x",
        .funs = ~ base::droplevels(x = base::as.factor(x = .))
      ) %>%
      tibble::as_tibble(x = .)

    # sample size
    sample_size <- nrow(data)

    # setting up the Mann-Whitney U-test and getting its summary
    mann_stat <-
      stats::wilcox.test(
        formula = y ~ x,
        data = data,
        paired = paired,
        alternative = "two.sided",
        na.action = na.omit,
        exact = FALSE,
        correct = TRUE,
        conf.int = TRUE,
        conf.level = 0.95
      )

    # computing Z score
    z_stat <-
      coin::wilcox_test(
        formula = y ~ x,
        data = data,
        distribution = "asymptotic",
        alternative = "two.sided",
        conf.int = TRUE,
        conf.level = 0.95
      )

    # displaying message about which test was run
    if (isTRUE(messages)) {
      base::message(cat(
        crayon::green("Note: "),
        crayon::blue(
          "Two-sample Wilcoxon test, also known as Mann-Whitney test, was run.\n"
        ),
        sep = ""
      ))
    }

    # mann_stat input represents the U-test summary derived from `stats`
    # library, while Z is from Exact `Wilcoxon-Pratt Signed-Rank Test` from
    # `coin` library
    # effect size is computed as `r = z/sqrt(n)`
    subtitle <-
      base::substitute(
        expr =
          paste(
            italic(U),
            " = ",
            estimate,
            ", ",
            italic(Z),
            " = ",
            z_value,
            ", ",
            italic(" p"),
            " = ",
            pvalue,
            ", ",
            italic("r"),
            " = ",
            r,
            ", ",
            italic("n"),
            " = ",
            n
          ),
        env = base::list(
          estimate = ggstatsplot::specify_decimal_p(
            x = mann_stat$statistic[[1]],
            k = k,
            p.value = FALSE
          ),
          z_value = ggstatsplot::specify_decimal_p(
            x = coin::statistic(z_stat)[[1]],
            k = k,
            p.value = FALSE
          ),
          pvalue = ggstatsplot::specify_decimal_p(
            x = mann_stat$p.value[[1]],
            k = k,
            p.value = TRUE
          ),
          r = ggstatsplot::specify_decimal_p(
            x = (coin::statistic(z_stat)[[1]] / sqrt(length(data$y))),
            k = k,
            p.value = FALSE
          ),
          n = sample_size
        )
      )

    # return the subtitle
    return(subtitle)
  }

#' @rdname subtitle_mann_nonparametric
#' @aliases subtitle_mann_nonparametric
#' @export

subtitle_t_nonparametric <- subtitle_mann_nonparametric

#' @title Making text subtitle for the robust t-test
#'   (between- and within-subjects designs).
#' @name subtitle_t_robust
#' @author Indrajeet Patil
#'
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @param ... Additional arguments (ignored).
#' @inheritParams subtitle_t_parametric
#' @inheritParams yuend_ci
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom WRS2 yuen yuen.effect.ci
#'
#' @examples
#' 
#' # with defaults
#' subtitle_t_robust(
#'   data = sleep,
#'   x = group,
#'   y = extra
#' )
#' 
#' # changing defaults
#' subtitle_t_robust(
#'   data = ToothGrowth,
#'   x = supp,
#'   y = len,
#'   nboot = 10,
#'   k = 1,
#'   tr = 0.2
#' )
#' 
#' # within-subjects design
#' ggstatsplot::subtitle_t_robust(
#'   data = dplyr::filter(
#'     ggstatsplot::intent_morality,
#'     condition %in% c("accidental", "attempted"),
#'     harm == "Poisoning"
#'   ),
#'   x = condition,
#'   y = rating,
#'   paired = TRUE,
#'   nboot = 25
#' )
#' @export

# function body
subtitle_t_robust <-
  function(data,
             x,
             y,
             tr = 0.1,
             paired = FALSE,
             nboot = 100,
             conf.level = 0.95,
             conf.type = "norm",
             k = 2,
             messages = TRUE,
             ...) {

    # creating a dataframe
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(x),
        y = !!rlang::enquo(y)
      )

    # convert the grouping variable to factor and drop unused levels
    data %<>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "x",
        .funs = ~ base::droplevels(x = base::as.factor(x = .))
      )

    # ---------------------------- between-subjects design --------------------

    # running bayesian analysis
    if (!isTRUE(paired)) {

      # removing NAs
      data %<>%
        stats::na.omit(.)

      # sample size
      sample_size <- nrow(data)

      # Yuen's test for trimmed means
      t_robust_stat <-
        WRS2::yuen(
          formula = y ~ x,
          data = data,
          tr = tr
        )

      # computing effect sizes
      t_robust_effsize <-
        WRS2::yuen.effect.ci(
          formula = y ~ x,
          data = data,
          tr = tr,
          nboot = nboot,
          alpha = 1 - conf.level
        )

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
              italic(xi),
              " = ",
              effsize,
              ", CI"[conf.level],
              " [",
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
              x = t_robust_stat$test[[1]],
              k = k,
              p.value = FALSE
            ),
            df = ggstatsplot::specify_decimal_p(
              x = t_robust_stat$df[[1]],
              k = k,
              p.value = FALSE
            ),
            pvalue = ggstatsplot::specify_decimal_p(
              x = t_robust_stat$p.value[[1]],
              k = k,
              p.value = TRUE
            ),
            effsize = ggstatsplot::specify_decimal_p(
              x = t_robust_effsize$effsize[[1]],
              k = k,
              p.value = FALSE
            ),
            conf.level = paste(conf.level * 100, "%", sep = ""),
            LL = ggstatsplot::specify_decimal_p(
              x = t_robust_effsize$CI[[1]][[1]],
              k = k,
              p.value = FALSE
            ),
            UL = ggstatsplot::specify_decimal_p(
              x = t_robust_effsize$CI[[2]][[1]],
              k = k,
              p.value = FALSE
            ),
            n = sample_size
          )
        )

      # ---------------------------- within-subjects design -------------------
    } else {

      # getting dataframe of results from the custom function
      yuend_results <-
        yuend_ci(
          data = data,
          x = x,
          y = y,
          tr = tr,
          nboot = nboot,
          conf.level = conf.level,
          conf.type = conf.type
        )

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
              italic(xi),
              " = ",
              effsize,
              ", CI"[conf.level],
              " [",
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
              x = yuend_results$`t-value`[[1]],
              k = k,
              p.value = FALSE
            ),
            df = ggstatsplot::specify_decimal_p(
              x = yuend_results$df[[1]],
              k = k,
              p.value = FALSE
            ),
            pvalue = ggstatsplot::specify_decimal_p(
              x = yuend_results$`p-value`[[1]],
              k,
              p.value = TRUE
            ),
            effsize = ggstatsplot::specify_decimal_p(
              x = yuend_results$xi[[1]],
              k = k,
              p.value = FALSE
            ),
            conf.level = paste(conf.level * 100, "%", sep = ""),
            LL = ggstatsplot::specify_decimal_p(
              x = yuend_results$conf.low[[1]],
              k = k,
              p.value = FALSE
            ),
            UL = ggstatsplot::specify_decimal_p(
              x = yuend_results$conf.high[[1]],
              k = k,
              p.value = FALSE
            ),
            n = yuend_results$n[[1]]
          )
        )
    }

    # message about effect size measure
    if (isTRUE(messages)) {
      effsize_ci_message(nboot = nboot, conf.level = conf.level)
    }

    # return the subtitle
    return(subtitle)
  }

#' @title Making text subtitle for the bayesian t-test.
#' @name subtitle_t_bayes
#' @author Indrajeet Patil
#'
#' @param bf.prior A number between 0.5 and 2 (default `0.707`), the prior width
#'   to use in calculating Bayes factors.
#' @param ... Additional arguments (ignored).
#' @inheritParams subtitle_t_parametric
#'
#' @importFrom jmv ttestIS ttestPS
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#' 
#' # between-subjects design
#' 
#' subtitle_t_bayes(
#'   data = mtcars,
#'   x = am,
#'   y = wt,
#'   paired = FALSE
#' )
#' 
#' # within-subjects design
#' 
#' subtitle_t_bayes(
#'   data = dplyr::filter(
#'     ggstatsplot::intent_morality,
#'     condition %in% c("accidental", "attempted"),
#'     harm == "Poisoning"
#'   ),
#'   x = condition,
#'   y = rating,
#'   paired = TRUE
#' )
#' @export

# function body
subtitle_t_bayes <- function(data,
                             x,
                             y,
                             bf.prior = 0.707,
                             paired = FALSE,
                             k = 2,
                             ...) {

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    )

  # convert the grouping variable to factor and drop unused levels
  data %<>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "x",
      .funs = ~ base::droplevels(x = base::as.factor(x = .))
    )

  # -------------------------- between-subjects design ------------------------

  # running bayesian analysis
  if (!isTRUE(paired)) {

    # removing NAs
    data %<>%
      stats::na.omit(.)

    # sample size
    sample_size <- nrow(data)

    # independent samples design
    jmv_results <-
      jmv::ttestIS(
        data = data,
        vars = "y",
        group = "x",
        students = TRUE,
        effectSize = TRUE,
        bf = TRUE,
        bfPrior = bf.prior,
        hypothesis = "different",
        miss = "listwise"
      )

    # --------------------- within-subjects design ---------------------------
  } else if (isTRUE(paired)) {

    # jamovi needs data to be wide format and not long format
    data_wide <- long_to_wide_converter(
      data = data,
      x = x,
      y = y
    )

    # dependent samples design
    jmv_results <-
      jmv::ttestPS(
        data = na.omit(data_wide),
        pairs = list(list(
          i1 = colnames(data_wide)[[2]], i2 = colnames(data_wide)[[3]]
        )),
        students = TRUE,
        effectSize = TRUE,
        bf = TRUE,
        bfPrior = bf.prior,
        hypothesis = "different",
        miss = "listwise"
      )

    # sample size
    sample_size <- nrow(data_wide)
  }

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
          k = k
        ),
        bf = ggstatsplot::specify_decimal_p(
          x = log(
            x = as.data.frame(jmv_results$ttest)$`stat[bf]`,
            base = exp(1)
          ),
          k = 1,
          p.value = FALSE
        ),
        bf_prior = ggstatsplot::specify_decimal_p(
          x = bf.prior,
          k = 3,
          p.value = FALSE
        ),
        effsize = ggstatsplot::specify_decimal_p(
          x = as.data.frame(jmv_results$ttest)$`es[stud]`,
          k = k,
          p.value = FALSE
        ),
        n = sample_size
      )
    )

  # return the message
  return(subtitle)
}
