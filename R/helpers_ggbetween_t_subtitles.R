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
#' @inheritParams subtitle_template
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
                                  stat.title = NULL,
                                  ...) {

  # creating a dataframe
  data <-
    dplyr::select(.data = data, x = {{ x }}, y = {{ y }}) %>%
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

  # tidy dataframe from model object
  stats_df <-
    broomExtra::tidy(tobject)

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
    stat.title = stat.title,
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
#'   carried out.
#'
#' @inheritParams subtitle_anova_parametric
#' @inheritParams subtitle_t_parametric
#' @inheritParams t1way_ci
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo exec
#' @importFrom stats wilcox.test
#' @importFrom psych corr.test
#' @importFrom rcompanion wilcoxonR wilcoxonPairedR
#'
#' @details For the two independent samples case, the Mann-Whitney *U*-test is
#'   calculated and *W* is reported from *stats::wilcox.test*. For the paired
#'   samples case the Wilcoxon signed rank test is run and *V* is reported.
#'
#'   Since there is no single commonly accepted method for reporting effect size
#'   for these tests we are computing and reporting *r* (computed as
#'   \eqn{Z/\sqrt{N}}) along with the confidence intervals associated with the
#'   estimate. Note that *N* here corresponds to total *sample size* for
#'   independent/between-subjects designs, and to total number of *pairs* (and
#'   **not** *observations*) for repeated measures/within-subjects designs.
#'
#'   *Note:* The *stats::wilcox.test* function does not follow the
#'   same convention as *stats::t.test*. The sign of the *V* test statistic
#'   will always be positive since it is **the sum of the positive signed ranks**.
#'   Therefore *V* will vary in magnitude but not significance based solely
#'   on the order of the grouping variable. Consider manually
#'   reordering your factor levels if appropriate as shown in the second example
#'   below.
#'
#' @examples
#' \donttest{
#' set.seed(123)
#'
#' # -------------- between-subjects design ------------------------
#' # simple function call
#' ggstatsplot::subtitle_mann_nonparametric(
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
#' # modifying few things
#' ggstatsplot::subtitle_mann_nonparametric(
#'   data = msleep_short,
#'   x = vore,
#'   y = sleep_rem,
#'   nboot = 200,
#'   conf.level = 0.99,
#'   conf.type = "bca"
#' )
#'
#' # The order of the grouping factor matters when computing *V*
#' # Changing default alphabeical order manually
#' msleep_short$vore <- factor(msleep_short$vore,
#'   levels = c("herbi", "carni")
#' )
#'
#' # note the change in the reported *V* value but the identical
#' # value for *p* and the reversed effect size
#' ggstatsplot::subtitle_mann_nonparametric(
#'   data = msleep_short,
#'   x = vore,
#'   y = sleep_rem
#' )
#'
#' # -------------- within-subjects design ------------------------
#' # using dataset included in the package
#' ggstatsplot::subtitle_mann_nonparametric(
#'   data = VR_dilemma,
#'   x = modality,
#'   y = score,
#'   paired = TRUE,
#'   conf.level = 0.90,
#'   conf.type = "perc",
#'   nboot = 200,
#'   k = 5
#' )
#' }
#' @export

# function body
subtitle_mann_nonparametric <- function(data,
                                        x,
                                        y,
                                        paired = FALSE,
                                        k = 2,
                                        conf.level = 0.95,
                                        conf.type = "norm",
                                        nboot = 100,
                                        stat.title = NULL,
                                        messages = TRUE,
                                        ...) {

  # creating a dataframe
  data <-
    dplyr::select(.data = data, x = {{ x }}, y = {{ y }}) %>%
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
      tidyr::drop_na(data = .)

    # sample size
    sample_size <- nrow(data)
  }

  # setting up the test and getting its summary
  stats_df <-
    broomExtra::tidy(stats::wilcox.test(
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

  # function to compute effect sizes
  if (isTRUE(paired)) {
    .f <- rcompanion::wilcoxonPairedR
  } else {
    .f <- rcompanion::wilcoxonR
  }

  # computing effect size
  effsize_df <- rlang::exec(
    .fn = .f,
    x = data$y,
    g = data$x,
    ci = TRUE,
    conf = conf.level,
    type = conf.type,
    R = nboot,
    histogram = FALSE,
    digits = k
  ) %>%
    tibble::as_tibble(x = .)

  # message about effect size measure
  if (isTRUE(messages)) {
    effsize_ci_message(nboot = nboot, conf.level = conf.level)
  }

  # statistic text
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
    stat.title = stat.title,
    statistic.text = statistic.text,
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
                              stat.title = NULL,
                              messages = TRUE,
                              ...) {

  # creating a dataframe
  data <-
    dplyr::select(.data = data, x = {{ x }}, y = {{ y }}) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
    tibble::as_tibble(x = .)

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
      stat.title = stat.title,
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

    # converting to long format and then getting it back in wide so that the
    # rowid variable can be used as the block variable
    data_within <-
      long_to_wide_converter(
        data = data,
        x = x,
        y = y
      ) %>%
      tidyr::gather(data = ., key, value, -rowid) %>%
      dplyr::arrange(.data = ., rowid)

    # sample size
    sample_size <- length(unique(data_within$rowid))

    # getting dataframe of results from the custom function
    stats_df <-
      yuend_ci(
        data = data_within,
        x = key,
        y = value,
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
      n = sample_size,
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
#' @examples
#' # for reproducibility
#' set.seed(123)
#'
#' # between-subjects design
#'
#' ggstatsplot::subtitle_t_bayes(
#'   data = mtcars,
#'   x = am,
#'   y = wt,
#'   paired = FALSE
#' )
#'
#' # within-subjects design
#'
#' ggstatsplot::subtitle_t_bayes(
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
    dplyr::select(.data = data, x = {{ x }}, y = {{ y }}) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
    tibble::as_tibble(.)

  # preparing the subtitle
  subtitle <-
    bf_ttest(
      data = data,
      x = x,
      y = y,
      paired = paired,
      bf.prior = bf.prior,
      caption = NULL,
      output = "h1",
      k = k
    )

  # return the message
  return(subtitle)
}
