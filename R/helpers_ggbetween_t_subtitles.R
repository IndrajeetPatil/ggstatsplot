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
#' @importFrom stats t.test na.omit qt pt uniroot
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
    dplyr::mutate_if(is.character, as.factor) %>%
    dplyr::mutate_if(is.factor, droplevels) %>%
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
  stats_df <-
    broom::tidy(stats::t.test(
      formula = y ~ x,
      data = data,
      paired = paired,
      alternative = "two.sided",
      var.equal = var.equal,
      na.action = na.omit
    ))

  # effect size object
  effsize_df <-
    effect_t_parametric(
      formula = y ~ x,
      data = data,
      paired = paired,
      hedges.correction = hedges.correction,
      conf.level = conf.level,
      noncentral = effsize.noncentral
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
#'   (between-/within-subjects designs).
#' @author Indrajeet Patil
#' @details Two-sample Wilcoxon test, also known as Mann-Whitney test, is
#'   carried out. The effect size estimate for this test is Hodges–Lehmann–Sen
#'   estimator.
#'
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @param ... Additional arguments (ignored).
#' @inheritParams subtitle_t_parametric
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom stats wilcox.test
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
      tidyr::drop_na(data = .) %>%
      dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
      tibble::as_tibble(x = .)

    # sample size
    sample_size <- nrow(data)

    # setting up the Mann-Whitney U-test and getting its summary
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


#' @title Calculating Cohen or Hedges (between-/within- or one
#'   sample designs).
#' @name effect_t_parametric
#' @author Chuck Powell
#'
#' @param formula this function only accepts the variables in
#'   `formula` format e.g. `sleep_rem ~ vore` or `~ vore`.
#' @param data a data frame or tibble containing the data
#' @param mu if conducting a single sample test against a mean (Default: `0`).
#' @param paired Logical indicating whether to treat the data as a oaired sample
#'   e.g. `post ~ pre`.
#' @param hedges.correction Logical indicating whether to apply Hedges correction,
#'   Hedge's *g* (Default: `TRUE`).
#' @param conf.level A scalar value between 0 and 1. If unspecified, the
#'    default is to return `95%` lower and upper confidence intervals (`0.95`).
#' @param noncentral Logical indicating whether to use non-central
#'   *t*-distributions for computing the confidence intervals (Default: `TRUE`).
#'
#' @importFrom stats t.test na.omit cor qt pt uniroot
#' @importFrom tibble tibble
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
#' effect_t_parametric(
#'   formula = sleep_rem ~ vore,
#'   data = msleep_short,
#' )
#'
#' # changing defaults
#' effect_t_parametric(
#'   formula = sleep_rem ~ vore,
#'   data = msleep_short,
#'   mu = 1, # ignored in this case
#'   paired = FALSE,
#'   hedges.correction = TRUE,
#'   conf.level = .99,
#'   noncentral = FALSE
#' )
#' @export

# function body
effect_t_parametric <- function(formula = NULL,
                                data = NULL,
                                mu = 0,
                                paired = FALSE,
                                hedges.correction = TRUE,
                                conf.level = .95,
                                noncentral = TRUE) {
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
    Z <- -qt((1 - conf.level) / 2, df)
    lower.ci <- c(d - Z * sqrt(sd(x)))
    upper.ci <- c(d + Z * sqrt(sd(x)))
    tobject <- t.test(x, mu = mu, var.equal = TRUE, conf.level = conf.level)
    tvalue <- tobject$statistic
    dfvalue <- tobject$parameter
    civalue <- attr(tobject$conf.int, "conf.level")
    twosamples <- FALSE
  }


  # ---------------two independent samples by factor -------------------

  if (length(formula) == 3 & !isTRUE(paired)) { # two samples by factor
    outcome <- eval(formula[[2]], data)
    group <- eval(formula[[3]], data)
    group <- factor(group)
    if (nlevels(group) != 2L) {
      stop("grouping factor must have exactly 2 levels")
    }
    x <- split(outcome, group)
    y <- x[[2]]
    x <- x[[1]]
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
    sq.devs <- (c(x - mean(x), y - mean(y)))^2
    n <- length(sq.devs)
    n1 <- length(x)
    n2 <- length(y)
    psd <- sqrt(sum(sq.devs) / (n - 2))
    sd.est <- psd
    mean.diff <- mean(x) - mean(y)
    df <- length(x) + length(y) - 2
    d <- mean.diff / sd.est
    Sigmad <- sqrt((n1 + n2) / (n1 * n2) + 0.5 * d^2 / (n1 + n2))
    Z <- -qt((1 - conf.level) / 2, df)
    lower.ci <- c(d - Z * Sigmad)
    upper.ci <- c(d + Z * Sigmad)
    method <- "Cohen's d"
    tobject <- t.test(x, y, var.equal = TRUE, conf.level = conf.level)
    tvalue <- tobject$statistic
    dfvalue <- tobject$parameter
    civalue <- attr(tobject$conf.int, "conf.level")
    twosamples <- TRUE
  }


  # -------------- two paired samples in matching columns -------------------

  if (length(formula) == 3 & isTRUE(paired)) {
    if (is.factor(eval(formula[[3]], data))) {
      outcome <- eval(formula[[2]], data)
      group <- eval(formula[[3]], data)
      group <- droplevels(group)
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
    if (length(x) != length(y)) {
      stop("paired samples requires samples of the same size")
    }
    n <- length(x)
    df <- n - 1
    r <- cor(x, y)
    sd.est <- sd(x - y)
    mean.diff <- mean(y) - mean(x)
    d <- mean.diff / sd.est
    Sigmad <- sqrt(((1 / n) + (d^2 / n)) * 2 * (1 - r)) # paired
    Z <- -qt((1 - conf.level) / 2, df)
    lower.ci <- c(d - Z * Sigmad)
    upper.ci <- c(d + Z * Sigmad)
    method <- "Cohen's d"
    diffscores <- as.vector(y - x)
    tobject <- t.test(diffscores, mu = 0, var.equal = TRUE, conf.level = conf.level)
    tvalue <- tobject$statistic
    dfvalue <- tobject$parameter
    civalue <- attr(tobject$conf.int, "conf.level")
    twosamples <- FALSE
  }

  # -------------- apply hedges correction -------------------

  if (hedges.correction == TRUE) {
    method <- "Hedges's g"
    d <- d * (n - 3) / (n - 2.25)
  }

  # -------------- calculate NCP intervals -------------------

  if (isTRUE(noncentral)) {
    st <- max(0.1, tvalue)
    end1 <- tvalue
    while (pt(q = tvalue, df = dfvalue, ncp = end1) > (1 - civalue) / 2) {
      end1 <- end1 + st
    }
    ncp1 <- uniroot(
      function(x)
        (1 - civalue) / 2 - pt(
          q = tvalue,
          df = dfvalue,
          ncp = x
        ),
      c(2 * tvalue - end1, end1)
    )$root
    end2 <- tvalue
    while (pt(q = tvalue, df = dfvalue, ncp = end2) < (1 + civalue) / 2) {
      end2 <- end2 - st
    }
    ncp2 <- uniroot(
      function(x)
        (1 + civalue) / 2 - pt(
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
    return(tibble(
      method = method,
      estimate = d,
      conf.low = ncp.lower.ci,
      conf.high = ncp.upper.ci
    ))
  } else {
    return(tibble(
      method = method,
      estimate = d,
      conf.low = lower.ci,
      conf.high = upper.ci
    ))
  }
}
