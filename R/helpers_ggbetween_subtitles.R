# adding bayes factor message for null hypothesis for t-test (one-sample,
# independent, or paired)

bf_message_ttest <- function(jmv_results,
                             bf.prior,
                             caption) {

  # prepare the bayes factor message
  bf_message <- base::substitute(
    atop(top.text,
      expr =
        paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          bf,
          # ", log"["e"],
          # "(error) = ",
          # bf_error,
          # "%",
          ", Prior width = ",
          bf_prior
        )
    ),
    env = base::list(
      top.text = caption,
      bf = ggstatsplot::specify_decimal_p(x = log(
        x = (1 / as.data.frame(jmv_results$ttest)$`stat[bf]`),
        base = exp(1)
      ), k = 1),
      # bf_error = ggstatsplot::specify_decimal_p(x = log(
      #   x = (1 / as.data.frame(jmv_results$ttest)$`err[bf]`),
      #   base = exp(1)
      # ), k = 1),
      bf_prior = ggstatsplot::specify_decimal_p(x = bf.prior, k = 3)
    )
  )

  # return the message
  return(bf_message)
}

#' @title Making text subtitle for the between-subject anova designs.
#' @name subtitle_ggbetween_anova_parametric
#' @author Indrajeet Patil
#'
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param x The grouping variable.
#' @param y The response - a vector of length the number of rows of `x`.
#' @param nboot Number of bootstrap samples for computing effect size (Default:
#'   `100`).
#' @param effsize.type Type of effect size needed for *parametric* tests. The
#'   argument can be `"biased"` (`"d"` for Cohen's *d* for **t-test**;
#'   `"partial_eta"` for partial eta-squared for **anova**) or `"unbiased"`
#'   (`"g"` Hedge's *g* for **t-test**; `"partial_omega"` for partial
#'   omega-squared for **anova**)).
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @param ... Additional arguments (ignored).
#' @inheritParams stats::oneway.test
#' @inheritParams groupedstats::specify_decimal_p
#'
#' @importFrom dplyr select
#' @importFrom rlang enquo
#' @importFrom stats lm
#'
#' @examples
#' # with defaults
#' subtitle_ggbetween_anova_parametric(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem
#' )
#' 
#' # modifying the defaults
#' subtitle_ggbetween_anova_parametric(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   effsize.type = "partial_eta",
#'   k = 2,
#'   var.equal = TRUE,
#'   nboot = 10
#' )
#' @export

# function body
subtitle_ggbetween_anova_parametric <-
  function(data,
             x,
             y,
             effsize.type = "partial_omega",
             nboot = 100,
             var.equal = FALSE,
             k = 3,
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
      stats::na.omit(.) %>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "x",
        .funs = ~ base::droplevels(x = base::as.factor(x = .))
      )

    # Welch's ANOVA run by default
    aov_stat <-
      stats::oneway.test(
        formula = y ~ x,
        data = data,
        subset = NULL,
        na.action = na.omit,
        var.equal = var.equal
      )

    # preparing the subtitles with appropriate effect sizes
    if (effsize.type == "unbiased" || effsize.type == "partial_omega") {
      # partial omega-squared is the biased estimate of effect size for parametric ANOVA
      aov_effsize_ci <- sjstats::omega_sq(
        model = stats::lm(
          formula = y ~ x,
          data = data,
          na.action = na.omit
        ),
        partial = TRUE,
        ci.lvl = 0.95,
        n = nboot
      )

      # displaying message about bootstrap
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::green("Note: "),
          crayon::blue(
            "95% CI for partial omega-squared was computed with",
            crayon::yellow(nboot),
            "bootstrap samples.\n"
          ),
          sep = ""
        ))
      }

      # aov_stat input represents the anova object summary derived from car library
      subtitle <-
        # extracting the elements of the statistical object
        base::substitute(
          expr =
            paste(
              italic("F"),
              "(",
              df1,
              ",",
              df2,
              ") = ",
              estimate,
              ", ",
              italic("p"),
              " = ",
              pvalue,
              ", p",
              omega^2,
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
            estimate = ggstatsplot::specify_decimal_p(x = aov_stat$statistic[[1]], k),
            df1 = aov_stat$parameter[[1]],
            # numerator degrees of freedom are always integer
            df2 = ggstatsplot::specify_decimal_p(x = aov_stat$parameter[[2]], k),
            pvalue = ggstatsplot::specify_decimal_p(x = aov_stat$p.value[[1]], k, p.value = TRUE),
            effsize = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$partial.omegasq[[1]], k),
            LL = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$conf.low[[1]], k),
            UL = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$conf.high[[1]], k),
            n = nrow(x = data)
          )
        )
    } else if (effsize.type == "biased" || effsize.type == "partial_eta") {
      # getting confidence interval for partial eta-squared
      aov_effsize_ci <- sjstats::eta_sq(
        model = stats::lm(
          formula = y ~ x,
          data = data,
          na.action = na.omit
        ),
        partial = TRUE,
        ci.lvl = 0.95,
        n = nboot
      )

      # displaying message about bootstrap
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::green("Note: "),
          crayon::blue(
            "95% CI for partial eta-squared was computed with",
            crayon::yellow(nboot),
            "bootstrap samples.\n"
          ),
          sep = ""
        ))
      }

      # aov_stat input represents the anova object summary derived from car library
      subtitle <-
        # extracting the elements of the statistical object
        base::substitute(
          expr =
            paste(
              italic("F"),
              "(",
              df1,
              ",",
              df2,
              ") = ",
              estimate,
              ", ",
              italic("p"),
              " = ",
              pvalue,
              ", p",
              eta^2,
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
            estimate = ggstatsplot::specify_decimal_p(x = aov_stat$statistic[[1]], k),
            df1 = aov_stat$parameter[[1]],
            # numerator degrees of freedom are always integer
            df2 = ggstatsplot::specify_decimal_p(x = aov_stat$parameter[[2]], k),
            pvalue = ggstatsplot::specify_decimal_p(
              x = aov_stat$p.value[[1]],
              k,
              p.value = TRUE
            ),
            effsize = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$partial.etasq[[1]], k),
            LL = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$conf.low[[1]], k),
            UL = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$conf.high[[1]], k),
            n = nrow(x = data)
          )
        )
    }

    # return the subtitle
    return(subtitle)
  }


#' @title Making text subtitle for the t-test (between-/within-subjects designs).
#' @name subtitle_ggbetween_t_parametric
#' @author Indrajeet Patil
#'
#' @param effsize.noncentral Logical indicating whether to use non-central
#'   *t*-distributions for computing the confidence interval for Cohen's *d*
#'   or Hedge's *g* (Default: `FALSE`).
#'  @param conf.level Scalar between 0 and 1. If unspecified, the defaults return 95%
#'   lower and upper confidence intervals (`0.95`).
#' @param ... Additional arguments (ignored).
#' @inheritParams subtitle_ggbetween_anova_parametric
#' @inheritParams stats::t.test
#' @inheritParams groupedstats::specify_decimal_p
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom stats t.test
#' @importFrom effsize cohen.d
#'
#' @examples
#' 
#' # creating a smaller dataset
#' msleep_short <- dplyr::filter(ggplot2::msleep, vore %in% c("carni", "herbi"))
#' 
#' # with defaults
#' subtitle_ggbetween_t_parametric(
#'   data = msleep_short,
#'   x = vore,
#'   y = sleep_rem
#' )
#' 
#' # changing defaults
#' subtitle_ggbetween_t_parametric(
#'   data = msleep_short,
#'   x = vore,
#'   y = sleep_rem,
#'   var.equal = TRUE,
#'   k = 2,
#'   effsize.type = "d"
#' )
#' @export

# function body
subtitle_ggbetween_t_parametric <-
  function(data,
             x,
             y,
             paired = FALSE,
             effsize.type = "g",
             effsize.noncentral = FALSE,
             conf.level = 0.95,
             var.equal = FALSE,
             k = 3,
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
      stats::na.omit(.) %>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "x",
        .funs = ~ base::droplevels(x = base::as.factor(x = .))
      )

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
    if (effsize.type == "unbiased" || effsize.type == "g") {
      # Hedge's g is an unbiased estimate of the effect size
      hedges.correction <- TRUE
    } else if (effsize.type == "biased" || effsize.type == "d") {
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
    if (effsize.type == "unbiased" || effsize.type == "g") {

      # preparing subtitle with Hedge's
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
            estimate = ggstatsplot::specify_decimal_p(x = t_stat[[1]], k),
            df = ggstatsplot::specify_decimal_p(x = t_stat[[2]], k.df),
            pvalue = ggstatsplot::specify_decimal_p(
              x = t_stat[[3]],
              k,
              p.value = TRUE
            ),
            effsize = ggstatsplot::specify_decimal_p(x = t_effsize[[3]], k),
            LL = ggstatsplot::specify_decimal_p(x = t_effsize$conf.int[[1]], k),
            UL = ggstatsplot::specify_decimal_p(x = t_effsize$conf.int[[2]], k),
            n = sample_size
          )
        )
    } else if (effsize.type == "biased" || effsize.type == "d") {

      # preparing subtitle with Cohen's d
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
            estimate = ggstatsplot::specify_decimal_p(x = t_stat[[1]], k),
            df = ggstatsplot::specify_decimal_p(x = t_stat[[2]], k.df),
            pvalue = ggstatsplot::specify_decimal_p(
              x = t_stat[[3]],
              k,
              p.value = TRUE
            ),
            effsize = ggstatsplot::specify_decimal_p(x = t_effsize[[3]], k),
            LL = ggstatsplot::specify_decimal_p(x = t_effsize$conf.int[[1]], k),
            UL = ggstatsplot::specify_decimal_p(x = t_effsize$conf.int[[2]], k),
            n = sample_size
          )
        )
    }

    # return the subtitle
    return(subtitle)
  }


#' @title Making text subtitle for the Mann-Whitney U-test
#'   (between-/within-subjects designs).
#' @name subtitle_ggbetween_mann_nonparametric
#' @author Indrajeet Patil
#'
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @param ... Additional arguments (ignored).
#' @inheritParams subtitle_ggbetween_t_parametric
#' @inheritParams groupedstats::specify_decimal_p
#'
#' @importFrom dplyr select
#' @importFrom rlang enquo
#' @importFrom stats wilcox.test
#' @importFrom coin wilcox_test
#'
#' @examples
#' subtitle_ggbetween_mann_nonparametric(
#'   data = sleep,
#'   x = group,
#'   y = extra
#' )
#' @export

# function body
subtitle_ggbetween_mann_nonparametric <-
  function(data,
             x,
             y,
             paired = FALSE,
             k = 3,
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
      stats::na.omit(.) %>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "x",
        .funs = ~ base::droplevels(x = base::as.factor(x = .))
      )

    # sample size
    sample_size <- nrow(data)

    # setting up the Mann-Whitney U-test and getting its summary
    mann_stat <- stats::wilcox.test(
      formula = y ~ x,
      data = data,
      paired = paired,
      alternative = "two.sided",
      na.action = na.omit,
      exact = FALSE,
      # asymptotic
      correct = TRUE,
      conf.int = TRUE,
      conf.level = 0.95
    )

    # computing Z score
    z_stat <- coin::wilcox_test(
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
    subtitle <-
      # extracting the elements of the statistical object
      base::substitute(
        expr =
          paste(
            # "Mann-Whitney: ",
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
          estimate = ggstatsplot::specify_decimal_p(x = mann_stat$statistic[[1]], k),
          z_value = ggstatsplot::specify_decimal_p(x = coin::statistic(z_stat)[[1]], k),
          pvalue = ggstatsplot::specify_decimal_p(x = mann_stat$p.value[[1]], k, p.value = TRUE),
          # effect size is r = z/sqrt(n)
          r = ggstatsplot::specify_decimal_p(x = (
            coin::statistic(z_stat)[[1]] / sqrt(length(data$y))
          ), k),
          n = sample_size
        )
      )

    # return the subtitle
    return(subtitle)
  }

#' @title Making text subtitle for the robust t-test
#'   (between- and within-subjects designs).
#' @name subtitle_ggbetween_t_rob
#' @author Indrajeet Patil
#'
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @param ... Additional arguments (ignored).
#' @inheritParams subtitle_ggbetween_t_parametric
#' @inheritParams groupedstats::specify_decimal_p
#' @inheritParams yuend_ci
#'
#' @importFrom dplyr select
#' @importFrom rlang enquo
#' @importFrom WRS2 yuen
#' @importFrom WRS2 yuen.effect.ci
#'
#' @examples
#' 
#' # with defaults
#' subtitle_ggbetween_t_rob(
#'   data = sleep,
#'   x = group,
#'   y = extra
#' )
#' 
#' # changing defaults
#' subtitle_ggbetween_t_rob(
#'   data = ToothGrowth,
#'   x = supp,
#'   y = len,
#'   nboot = 10,
#'   k = 1,
#'   tr = 0.2
#' )
#' 
#' # within-subjects design
#' ggstatsplot::subtitle_ggbetween_t_rob(
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
subtitle_ggbetween_t_rob <-
  function(data,
             x,
             y,
             tr = 0.1,
             paired = FALSE,
             nboot = 100,
             conf.level = 0.95,
             conf.type = "norm",
             k = 3,
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

    ## ---------------------------- between-subjects design --------------------

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

      # t_robust_stat input represents the t-test object summary derived from WRS2
      # library
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
              italic(xi),
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
            estimate = ggstatsplot::specify_decimal_p(x = t_robust_stat$test[[1]], k),
            df = ggstatsplot::specify_decimal_p(x = t_robust_stat$df[[1]], k),
            pvalue = ggstatsplot::specify_decimal_p(
              x = t_robust_stat$p.value[[1]],
              k,
              p.value = TRUE
            ),
            effsize = ggstatsplot::specify_decimal_p(x = t_robust_effsize$effsize[[1]], k),
            LL = ggstatsplot::specify_decimal_p(x = t_robust_effsize$CI[[1]][[1]], k),
            UL = ggstatsplot::specify_decimal_p(x = t_robust_effsize$CI[[2]][[1]], k),
            n = sample_size
          )
        )

      ## ---------------------------- within-subjects design ---------------------
    } else {

      # getting dataframe of results from the custom function
      yuend_results <- yuend_ci(
        data = data,
        x = x,
        y = y,
        tr = tr,
        nboot = nboot,
        conf.level = conf.level,
        conf.type = conf.type
      )

      # t_robust_stat input represents the t-test object summary derived from WRS2 library
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
              italic(xi),
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
            estimate = ggstatsplot::specify_decimal_p(x = yuend_results$`t-value`[[1]], k),
            df = ggstatsplot::specify_decimal_p(x = yuend_results$df[[1]], k),
            pvalue = ggstatsplot::specify_decimal_p(
              x = yuend_results$`p-value`[[1]],
              k,
              p.value = TRUE
            ),
            effsize = ggstatsplot::specify_decimal_p(x = yuend_results$xi[[1]], k),
            LL = ggstatsplot::specify_decimal_p(x = yuend_results$conf.low[[1]], k),
            UL = ggstatsplot::specify_decimal_p(x = yuend_results$conf.high[[1]], k),
            n = yuend_results$n[[1]]
          )
        )
    }

    # displaying message about bootstrap
    if (isTRUE(messages)) {
      base::message(cat(
        crayon::green("Note: "),
        crayon::blue(
          "95% CI for explanatory measure of effect size was computed with",
          crayon::yellow(nboot),
          "bootstrap samples.\n"
        ),
        sep = ""
      ))
    }

    # return the subtitle
    return(subtitle)
  }

#' @title Making text subtitle for the bayesian t-test.
#' @name subtitle_ggbetween_t_bayes
#' @author Indrajeet Patil
#'
#' @param bf.prior A number between 0.5 and 2 (default `0.707`), the prior width
#'   to use in calculating Bayes factors.
#' @param ... Additional arguments (ignored).
#' @inheritParams subtitle_ggbetween_t_parametric
#'
#' @importFrom jmv ttestIS ttestPS
#'
#' @examples
#' 
#' # for reproducibility
#' set.seed(123)
#' 
#' # between-subjects design
#' 
#' subtitle_ggbetween_t_bayes(
#'   data = mtcars,
#'   x = am,
#'   y = wt,
#'   paired = FALSE
#' )
#' 
#' # within-subjects design
#' 
#' subtitle_ggbetween_t_bayes(
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
subtitle_ggbetween_t_bayes <- function(data,
                                       x,
                                       y,
                                       bf.prior = 0.707,
                                       paired = FALSE,
                                       k = 3,
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

  ## ---------------------------- between-subjects design ---------------------------

  # running bayesian analysis
  if (!isTRUE(paired)) {

    # removing NAs
    data %<>%
      stats::na.omit(.)

    # sample size
    sample_size <- nrow(data)

    # independent samples design
    jmv_results <- jmv::ttestIS(
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

    ## ---------------------------- within-subjects design ---------------------------
  } else if (isTRUE(paired)) {

    # jamovi needs data to be wide format and not long format
    data_wide <- long_to_wide_converter(data = data, x = x, y = y)

    # dependent samples design
    jmv_results <- jmv::ttestPS(
      data = na.omit(data_wide),
      pairs = list(list(
        i1 = colnames(data_wide)[[3]], i2 = colnames(data_wide)[[2]]
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
        # ", log"["e"],
        # "(error) = ",
        # bf_error,
        # "% , ",
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
      estimate = ggstatsplot::specify_decimal_p(
        x = as.data.frame(jmv_results$ttest)$`stat[stud]`,
        k = k
      ),
      bf = ggstatsplot::specify_decimal_p(
        x = log(
          x = as.data.frame(jmv_results$ttest)$`stat[bf]`,
          base = exp(1)
        ),
        k = 1
      ),
      bf_prior = ggstatsplot::specify_decimal_p(
        x = bf.prior,
        k = 3
      ),
      # bf_error = ggstatsplot::specify_decimal_p(
      #   x = log(
      #     x = as.data.frame(jmv_results$ttest)$`err[bf]`,
      #     base = exp(1)
      #   ),
      #   k = 1
      # ),
      effsize = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_results$ttest)$`es[stud]`, k),
      n = sample_size
    )
  )

  # return the message
  return(subtitle)
}


#' @title Making text subtitle for the Kruskal-Wallis test (nonparametric ANOVA)
#'   (between-subjects designs).
#' @name subtitle_ggbetween_kw_nonparametric
#' @author Indrajeet Patil
#'
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @param ... Additional arguments (ignored).
#' @inheritParams subtitle_ggbetween_t_parametric
#' @inheritParams groupedstats::specify_decimal_p
#'
#' @importFrom dplyr select
#' @importFrom rlang enquo
#' @importFrom stats kruskal.test
#'
#' @examples
#' subtitle_ggbetween_kw_nonparametric(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem
#' )
#' @export

# function body
subtitle_ggbetween_kw_nonparametric <-
  function(data,
             x,
             y,
             messages = TRUE,
             k = 3,
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
      stats::na.omit(.) %>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "x",
        .funs = ~ base::droplevels(x = base::as.factor(x = .))
      )

    # sample size
    sample_size <- nrow(data)

    # setting up the anova model and getting its summary
    kw_stat <- stats::kruskal.test(
      formula = y ~ x,
      data = data,
      na.action = na.omit
    )

    # preparing the subtitle
    subtitle <-
      # extracting the elements of the statistical object
      base::substitute(
        expr =
          paste(
            "Kruskal-Wallis: ",
            italic(chi)^2,
            "(",
            df,
            ") = ",
            estimate,
            ", ",
            italic("p"),
            " = ",
            pvalue,
            ", ",
            italic("n"),
            " = ",
            n
          ),
        env = base::list(
          estimate = ggstatsplot::specify_decimal_p(x = kw_stat$statistic[[1]], k),
          df = kw_stat$parameter[[1]],
          # degrees of freedom are always integer
          pvalue = ggstatsplot::specify_decimal_p(
            x = kw_stat$p.value[[1]],
            k,
            p.value = TRUE
          ),
          n = sample_size
        )
      )

    # letting the user know that this test doesn't have agreed upon effect size
    if (isTRUE(messages)) {
      base::message(cat(
        crayon::red("Note: "),
        crayon::blue(
          "No effect size available for Kruskal-Wallis Rank Sum Test."
        ),
        sep = ""
      ))
    }

    # return the subtitle
    return(subtitle)
  }

#' @title Making text subtitle for the robust ANOVA
#'   (between-subjects designs).
#' @name subtitle_ggbetween_rob_anova
#' @author Indrajeet Patil
#'
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @param ... Additional arguments (ignored).
#' @inheritParams t1way_ci
#' @inheritParams subtitle_ggbetween_t_parametric
#' @inheritParams groupedstats::specify_decimal_p
#'
#' @importFrom dplyr select
#' @importFrom rlang enquo
#'
#' @examples
#' 
#' # examples not executed due to time constraints
#' \dontrun{
#' # for reproducibility
#' set.seed(123)
#' 
#' # going with the defaults
#' subtitle_ggbetween_rob_anova(
#'   data = ggplot2::midwest,
#'   x = state,
#'   y = percbelowpoverty,
#'   nboot = 10
#' )
#' 
#' # changing defaults
#' subtitle_ggbetween_rob_anova(
#'   data = ggplot2::midwest,
#'   x = state,
#'   y = percollege,
#'   tr = 0.2,
#'   nboot = 10
#' )
#' }
#' @export

# function body
subtitle_ggbetween_rob_anova <-
  function(data,
             x,
             y,
             tr = 0.1,
             nboot = 100,
             messages = TRUE,
             k = 3,
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
      stats::na.omit(.) %>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "x",
        .funs = ~ base::droplevels(x = base::as.factor(x = .))
      )

    # sample size
    sample_size <- nrow(data)

    # setting up the Bootstrap version of the heteroscedastic one-way ANOVA for
    # trimmed means
    robust_aov_stat <- t1way_ci(
      data = data,
      x = x,
      y = y,
      tr = tr,
      nboot = nboot,
      conf.level = 0.95,
      conf.type = "norm"
    )

    # displaying message about bootstrap
    if (isTRUE(messages)) {
      base::message(cat(
        crayon::green("Note: "),
        crayon::blue(
          "95% CI for explanatory measure of effect size was computed with",
          crayon::yellow(nboot),
          "bootstrap samples.\n"
        ),
        sep = ""
      ))
    }

    # robust_aov_stat input represents the robust anova object summary derived
    # from WRS2 library
    subtitle <-
      # extracting the elements of the statistical object
      base::substitute(
        expr =
          paste(
            italic("F"),
            "(",
            df1,
            ",",
            df2,
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
          estimate = ggstatsplot::specify_decimal_p(x = robust_aov_stat$`F-value`[[1]], k),
          df1 = robust_aov_stat$df1[[1]],
          # degrees of freedom are always integer
          df2 = ggstatsplot::specify_decimal_p(x = robust_aov_stat$df2[[1]], k),
          pvalue = ggstatsplot::specify_decimal_p(
            x = robust_aov_stat$`p-value`[[1]],
            k,
            p.value = TRUE
          ),
          effsize = ggstatsplot::specify_decimal_p(x = robust_aov_stat$xi[[1]], k),
          LL = ggstatsplot::specify_decimal_p(x = robust_aov_stat$conf.low[[1]], k),
          UL = ggstatsplot::specify_decimal_p(x = robust_aov_stat$conf.high[[1]], k),
          n = sample_size
        )
      )

    # displaying the details of the test that was run
    if (isTRUE(messages)) {
      base::message(cat(
        crayon::green("Note: "),
        crayon::blue(
          "In case of error, try reducing the trimming level",
          crayon::yellow(tr),
          "and/or increasing the number of bootstrap samples",
          crayon::yellow(nboot)
        ),
        sep = ""
      ))
    }

    # return the subtitle
    return(subtitle)
  }
