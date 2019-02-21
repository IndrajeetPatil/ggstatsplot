#' @title Making text subtitle for the t-test (between-/within-subjects
#'   designs).
#' @name subtitle_t_parametric
#' @author Indrajeet Patil, Chuck Powell
#'
#' @param effsize.noncentral Logical indicating whether to use non-central
#'   *t*-distributions for computing the confidence interval for Cohen's *d*
#'   or Hedge's *g* (Default: `TRUE`).
#' @inheritParams subtitle_anova_parametric
#' @inheritParams stats::t.test
#'
#' @importFrom dplyr select mutate_at
#' @importFrom rlang !! enquo
#' @importFrom stats t.test na.omit qt pt uniroot
#'
#' @seealso subtitle_t_parametric
#'
#' @details Cohen's *d* is calculated in the traditional fashion as the
#'   difference between means or mean minus *mu* divided by the estimated
#'   standardized deviation.  By default Hedge's correction is applied
#'   (*N*-3)/(*N*-2.25) to produce *g*. For independent samples *t*-test, there
#'   are two possibilities implemented. If the *t*-test did not make a
#'   homogeneity of variance assumption, (the Welch test), the variance term
#'   will mirror the Welch test, otherwise a pooled and weighted estimate is
#'   used. If a paired samples *t*-test was requested, then effect size desired is
#'   based on the standard deviation of the differences.
#'
#'   The computation of the confidence intervals defaults to a use of
#'   non-central Student-*t* distributions (`effsize.noncentral = TRUE`);
#'   otherwise a central distribution is used.
#'
#'   When computing confidence intervals the variance of the effect size *d* or *g* is
#'   computed using the conversion formula reported in Cooper et al. (2009)
#'
#'   - `((n1+n2)/(n1*n2) + .5*d^2/df) * ((n1+n2)/df)` (independent samples)
#'
#'   - `sqrt(((1 / n) + (d^2 / n)) * 2 * (1 - r))`  (paired case)
#'
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
                                  effsize.noncentral = TRUE,
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
    dplyr::mutate_if(.tbl = ., .predicate = is.character, .funs = as.factor) %>%
    dplyr::mutate_if(.tbl = ., .predicate = is.factor, .funs = droplevels) %>%
    tibble::as_tibble(x = .)

  # properly removing NAs if it's a paired design
  if (isTRUE(paired) && is.factor(data$x)) {
    data %<>%
      long_to_wide_converter(
        data = .,
        x = x,
        y = y
      ) %>%
      tidyr::gather(data = ., key, value, -rowid) %>%
      dplyr::rename(.data = ., x = key, y = value) %>%
      dplyr::mutate(x = factor(x))

    # sample size
    sample_size <- length(unique(data$rowid))

    # removing the unnecessary `rowid` column
    data %<>%
      dplyr::select(.data = ., -rowid)
  } else {
    # remove NAs listwise for between-subjects design
    data %<>%
      dplyr::filter(.data = ., !is.na(x), !is.na(y))

    # sample size
    sample_size <- nrow(data)
  }

  # deciding which effect size to use (Hedge's g or Cohen's d)
  if (effsize.type %in% c("unbiased", "g")) {
    hedges.correction <- TRUE
    effsize.text <- quote(italic("g"))
  } else if (effsize.type %in% c("biased", "d")) {
    hedges.correction <- FALSE
    effsize.text <- quote(italic("d"))
  }

  # setting up the t-test model and getting its summary

  tobject <- stats::t.test(
    formula = y ~ x,
    data = data,
    paired = paired,
    alternative = "two.sided",
    var.equal = var.equal,
    na.action = na.omit
  )

  stats_df <-
    broom::tidy(tobject)

  # effect size object
  effsize_df <-
    effsize_t_parametric(
      formula = y ~ x,
      data = data,
      paired = paired,
      hedges.correction = hedges.correction,
      conf.level = conf.level,
      noncentral = effsize.noncentral,
      var.equal = var.equal,
      tobject = tobject
    )

  # when paired samples t-test is run df is going to be integer
  # ditto for when variance is assumed to be equal
  if (isTRUE(paired) || isTRUE(var.equal)) {
    k.df <- 0L
  } else {
    k.df <- k
  }

  # preparing subtitle
  subtitle <- subtitle_template(
    no.parameters = 1L,
    stat.title = NULL,
    statistic.text = quote(italic("t")),
    statistic = stats_df$statistic[[1]],
    parameter = stats_df$parameter[[1]],
    p.value = stats_df$p.value[[1]],
    effsize.text = effsize.text,
    effsize.estimate = effsize_df$estimate,
    effsize.LL = effsize_df$conf.low,
    effsize.UL = effsize_df$conf.high,
    n = sample_size,
    conf.level = conf.level,
    k = k,
    k.parameter = k.df
  )

  # return the subtitle
  return(subtitle)
}


#' @title Making text subtitle for the Mann-Whitney *U*-test
#'   (between-subjects designs).
#' @author Indrajeet Patil, Chuck Powell
#' @details Two-sample Wilcoxon test, also known as Mann-Whitney test, is
#'   carried out. The effect size estimate for this test is Spearman's *rho*
#'   as the ranks of the `y` variable related to the factor `x`.
#'
#' @inheritParams subtitle_anova_parametric
#' @inheritParams subtitle_t_parametric
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom stats wilcox.test
#' @importFrom psych corr.test
#'
#' @details For the two independent samples case, the Mann-Whitney *U*-test is
#'   calculated and *W* is reported from *stats::wilcox.test*. For the paired
#'   samples case the Wilcoxon signed rank test is run and *V* is reported.
#'
#'   Since there is no single commonly accepted method for reporting effect size
#'   for these tests we are computing and reporting Spearman's *rho* a.k.a. *r*
#'   along with the confidence intervals associated with the estimate.
#'
#'   We have selected *Spearman's rho* which should be nearly identical to rank
#'   bi-serial and Somer's *d* for the case of `x` as two factors (including) as
#'   a pre/post measure and with *y* treated as ranks rather than raw scores.
#'
#'   *Note* The `wilcox.test` function in the stats package does not follow the
#'   same convention as `t.test` and reverse the sign of the test statistic
#'   based on the sign of the grouped differences.  It calculates a different
#'   test statistic estimate with the same significance level. Consider manually
#'   reordering your factor levels if appropriate as shown in the second example
#'   below.
#'
#' @examples
#' subtitle_mann_nonparametric(
#'   data = sleep,
#'   x = group,
#'   y = extra
#' )
#'
#' # creating a smaller dataset
#' msleep_short <- dplyr::filter(
#'   .data = ggplot2::msleep,
#'   vore %in% c("carni", "herbi")
#' )
#'
#' # with defaults
#' subtitle_t_nonparametric(
#'   data = msleep_short,
#'   x = vore,
#'   y = sleep_rem
#' )
#'
#' # The order of the grouping factor matters when computing *W*
#' # Changing default alphabeical order manually
#' msleep_short$vore <- factor(msleep_short$vore,
#'   levels = c("herbi", "carni")
#' )
#'
#' # note the change in the reported W value but the identical
#' # value for *p* and the reversed effect size
#' subtitle_t_nonparametric(
#'   data = msleep_short,
#'   x = vore,
#'   y = sleep_rem
#' )
#' @export

# function body
subtitle_mann_nonparametric <- function(data,
                                        x,
                                        y,
                                        paired = FALSE,
                                        k = 2,
                                        conf.level = 0.95,
                                        messages = TRUE,
                                        ...) {

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    ) %>%
    tidyr::drop_na(data = .)

  if (!is.numeric(data$y)) {
    stop("y variable must be numeric")
  }

  if (is.numeric(data$x)) {
    # setting up the test and getting its summary
    stats_df <-
      broom::tidy(stats::wilcox.test(
        x = data$x,
        y = data$y,
        paired = paired,
        alternative = "two.sided",
        na.action = na.omit,
        exact = FALSE,
        correct = TRUE,
        conf.int = TRUE,
        conf.level = conf.level
      ))
    # sample size
    sample_size <- nrow(data)
  } else {
    data <-
      data %>%
      dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
      tibble::as_tibble(x = .)

    # setting up the test and getting its summary
    stats_df <-
      broom::tidy(stats::wilcox.test(
        formula = y ~ x,
        data = data,
        paired = paired,
        alternative = "two.sided",
        na.action = na.omit,
        exact = FALSE,
        correct = TRUE,
        conf.int = TRUE,
        conf.level = conf.level
      ))
    # sample size
    if (!isTRUE(paired)) {
      sample_size <- nrow(data)
    } else {
      sample_size <- .5 * nrow(data)
    }
  }

  if (is.factor(data$x)) {
    data$x <- as.integer(data$x)
  } else if (is.numeric(data$x)) {
    d1 <- data.frame(y = data$x, x = 0)
    d2 <- data.frame(y = data$y, x = 1)
    data <- rbind(d1, d2)
  }

  # preparing effect size and ci's
  effsize_list <- psych::corr.test(
    x = data$x,
    y = data$y,
    ci = TRUE,
    method = "spearman",
    alpha = 1 - conf.level
  )

  if (isTRUE(paired)) {
    statistic.text <- quote("log"["e"](italic("V")))
  } else {
    statistic.text <- quote("log"["e"](italic("W")))
  }

  # preparing subtitle
  subtitle <- subtitle_template(
    no.parameters = 0L,
    parameter = NULL,
    parameter2 = NULL,
    stat.title = NULL,
    statistic.text = statistic.text,
    statistic = log(stats_df$statistic[[1]]),
    p.value = stats_df$p.value[[1]],
    effsize.text = quote(italic(r)["Spearman"]),
    effsize.estimate = effsize_list$r,
    effsize.LL = effsize_list$ci$lower,
    effsize.UL = effsize_list$ci$upper,
    n = sample_size,
    conf.level = conf.level,
    k = k
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
#' @inheritParams subtitle_t_parametric
#' @inheritParams yuend_ci
#' @inheritParams subtitle_anova_parametric
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
subtitle_t_robust <- function(data,
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
    ) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x)))

  # when paired robust t-test is run, `df` is going to be an integer
  if (isTRUE(paired)) {
    k.df <- 0
  } else {
    k.df <- k
  }

  # ---------------------------- between-subjects design --------------------

  # running bayesian analysis
  if (!isTRUE(paired)) {

    # removing NAs
    data %<>%
      stats::na.omit(.)

    # sample size
    sample_size <- nrow(data)

    # Yuen's test for trimmed means
    stats_df <-
      WRS2::yuen(
        formula = y ~ x,
        data = data,
        tr = tr
      )

    # computing effect size and its confidence interval
    effsize_df <-
      WRS2::yuen.effect.ci(
        formula = y ~ x,
        data = data,
        tr = tr,
        nboot = nboot,
        alpha = 1 - conf.level
      )

    # preparing subtitle
    subtitle <- subtitle_template(
      no.parameters = 1L,
      stat.title = NULL,
      statistic.text = quote(italic("t")),
      statistic = stats_df$test[[1]],
      parameter = stats_df$df[[1]],
      p.value = stats_df$p.value[[1]],
      effsize.text = quote(italic(xi)),
      effsize.estimate = effsize_df$effsize[[1]],
      effsize.LL = effsize_df$CI[[1]][[1]],
      effsize.UL = effsize_df$CI[[2]][[1]],
      n = sample_size,
      conf.level = conf.level,
      k = k,
      k.parameter = k.df
    )

    # ---------------------------- within-subjects design -------------------
  } else {

    # getting dataframe of results from the custom function
    stats_df <-
      yuend_ci(
        data = data,
        x = x,
        y = y,
        tr = tr,
        nboot = nboot,
        conf.level = conf.level,
        conf.type = conf.type
      )

    # preparing subtitle
    subtitle <- subtitle_template(
      no.parameters = 1L,
      stat.title = NULL,
      statistic.text = quote(italic("t")),
      statistic = stats_df$t.value[[1]],
      parameter = stats_df$df[[1]],
      p.value = stats_df$p.value[[1]],
      effsize.text = quote(italic(xi)),
      effsize.estimate = stats_df$xi[[1]],
      effsize.LL = stats_df$conf.low[[1]],
      effsize.UL = stats_df$conf.high[[1]],
      n = stats_df$n[[1]],
      conf.level = conf.level,
      k = k,
      k.parameter = k.df
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
#' @inheritParams subtitle_t_parametric
#' @inheritParams subtitle_anova_parametric
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
    ) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x)))

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
        df = as.data.frame(jmv_results$ttest)$`df[stud]`,
        estimate = specify_decimal_p(
          x = as.data.frame(jmv_results$ttest)$`stat[stud]`,
          k = k
        ),
        bf = specify_decimal_p(
          x = log(
            x = as.data.frame(jmv_results$ttest)$`stat[bf]`,
            base = exp(1)
          ),
          k = 1,
          p.value = FALSE
        ),
        bf_prior = specify_decimal_p(
          x = bf.prior,
          k = 3,
          p.value = FALSE
        ),
        effsize = specify_decimal_p(
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


#' @title Calculating Cohen's *d* or Hedge's *g* (for between-/within- or one
#'   sample designs).
#' @name effsize_t_parametric
#' @author Chuck Powell
#'
#' @param formula This function only accepts the variables in `formula` format
#'   e.g. `sleep_rem ~ vore` or `~ vore`.
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
#' Confidence Intervals on Effect Size, retrieved from
#' (\url{https://www.uvm.edu/~dhowell/methods7/Supplements/Confidence\%20Intervals\%20on\%20Effect\%20Size.pdf}).
#' }
#'
#' @examples
#' \dontrun{
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
    mean.diff <- mean(y) - mean(x)
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
    if (tvalue > 0 && isTRUE(paired)) {
      tvalue <- tvalue * -1
    }
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
    return(tibble::tibble(
      method = method,
      estimate = d,
      conf.low = ncp.lower.ci,
      conf.high = ncp.upper.ci,
      conf.level = conf.level,
      alternative = "two.sided",
      paired = paired,
      noncentral = noncentral,
      var.equal = var.equal
    ))
  } else {
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
}
