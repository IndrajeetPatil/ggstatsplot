#' @title Making text subtitle for the between-subject anova designs.
#' @name subtitle_anova_parametric
#' @author Indrajeet Patil
#'
#' @inheritParams t1way_ci
#' @param effsize.type Type of effect size needed for *parametric* tests. The
#'   argument can be `"biased"` (`"d"` for Cohen's *d* for **t-test**;
#'   `"partial_eta"` for partial eta-squared for **anova**) or `"unbiased"`
#'   (`"g"` Hedge's *g* for **t-test**; `"partial_omega"` for partial
#'   omega-squared for **anova**)).
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: `k = 2`).
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @param ... Additional arguments.
#' @inheritParams stats::oneway.test
#' @inheritParams subtitle_t_parametric
#' @inheritParams lm_effsize_standardizer
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom stats lm oneway.test na.omit
#' @importFrom sjstats eta_sq omega_sq
#'
#' @examples
#' # with defaults
#' subtitle_anova_parametric(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   k = 3
#' )
#'
#' # modifying the defaults
#' subtitle_anova_parametric(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   effsize.type = "biased",
#'   partial = FALSE,
#'   var.equal = TRUE,
#'   nboot = 10
#' )
#' @export

# function body
subtitle_anova_parametric <- function(data,
                                      x,
                                      y,
                                      effsize.type = "unbiased",
                                      partial = TRUE,
                                      conf.level = 0.95,
                                      nboot = 100,
                                      var.equal = FALSE,
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
    tidyr::drop_na(data = .) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
    tibble::as_tibble(x = .)

  # sample size
  sample_size <- nrow(data)

  # number of decimal places for degree of freedom
  if (isTRUE(var.equal)) {
    k.df <- 0
  } else {
    k.df <- k
  }

  # figuring out which effect size to use
  effsize.type <- effsize_type_switch(effsize.type)

  # preparing the subtitles with appropriate effect sizes
  if (effsize.type == "unbiased") {
    effsize <- "omega"
    if (isTRUE(partial)) {
      effsize.text <- quote(omega["p"]^2)
    } else {
      effsize.text <- quote(omega^2)
    }
  } else if (effsize.type == "biased") {
    effsize <- "eta"
    if (isTRUE(partial)) {
      effsize.text <- quote(eta["p"]^2)
    } else {
      effsize.text <- quote(eta^2)
    }
  }

  # Welch's ANOVA run by default
  stats_df <-
    stats::oneway.test(
      formula = y ~ x,
      data = data,
      subset = NULL,
      na.action = na.omit,
      var.equal = var.equal
    )

  # creating a standardized dataframe with effect size and its confidence
  # intervals
  effsize_df <- lm_effsize_standardizer(
    object = stats::lm(
      formula = y ~ x,
      data = data,
      na.action = na.omit
    ),
    effsize = effsize,
    partial = partial,
    conf.level = conf.level,
    nboot = nboot
  )

  # preparing subtitle
  subtitle <- subtitle_template(
    no.parameters = 2L,
    stat.title = NULL,
    statistic.text = quote(italic("F")),
    statistic = stats_df$statistic[[1]],
    parameter = stats_df$parameter[[1]],
    parameter2 = stats_df$parameter[[2]],
    p.value = stats_df$p.value[[1]],
    effsize.text = effsize.text,
    effsize.estimate = effsize_df$estimate[[1]],
    effsize.LL = effsize_df$conf.low[[1]],
    effsize.UL = effsize_df$conf.high[[1]],
    n = sample_size,
    conf.level = conf.level,
    k = k,
    k.parameter = k.df
  )

  # message about effect size measure
  if (isTRUE(messages)) {
    effsize_ci_message(nboot = nboot, conf.level = conf.level)
  }

  # return the subtitle
  return(subtitle)
}

#' @title Making text subtitle for the Kruskal-Wallis test (nonparametric ANOVA)
#'   (between-subjects designs).
#' @name subtitle_kw_nonparametric
#' @author Indrajeet Patil
#'
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @param ... Additional arguments (ignored).
#' @inheritParams t1way_ci
#' @inheritParams subtitle_anova_parametric
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom stats kruskal.test
#'
#' @examples
#' subtitle_kw_nonparametric(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem
#' )
#' @export

# function body
subtitle_kw_nonparametric <-
  function(data,
             x,
             y,
             messages = TRUE,
             k = 2,
             nboot = 100,
             conf.level = 0.95,
             conf.type = "norm",
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

    # setting up the anova model and getting its summary
    stats_df <-
      stats::kruskal.test(
        formula = y ~ x,
        data = data,
        na.action = na.omit
      )

    # getting partial eta-squared based on H-statistic
    effsize_df <-
      suppressWarnings(kw_eta_h_ci(
        data = data,
        x = x,
        y = y,
        nboot = nboot,
        conf.level = conf.level,
        conf.type = conf.type
      ))

    # message about effect size measure
    if (isTRUE(messages)) {
      effsize_ci_message(nboot = nboot, conf.level = conf.level)
    }

    # preparing subtitle
    subtitle <- subtitle_template(
      no.parameters = 1L,
      stat.title = NULL,
      statistic.text = quote(italic(chi)^2),
      statistic = stats_df$statistic[[1]],
      parameter = stats_df$parameter[[1]],
      p.value = stats_df$p.value[[1]],
      effsize.text = quote(eta["H"]^2),
      effsize.estimate = effsize_df$eta_sq_H[[1]],
      effsize.LL = effsize_df$conf.low[[1]],
      effsize.UL = effsize_df$conf.high[[1]],
      n = sample_size,
      conf.level = conf.level,
      k = k,
      k.parameter = 0L
    )

    # return the subtitle
    return(subtitle)
  }

#' @title Making text subtitle for the Friedman Rank Sum Test (nonparametric
#'   ANOVA) (within-subjects designs).
#' @name subtitle_friedman_nonparametric
#' @author Indrajeet Patil
#'
#' @param ... Additional arguments (ignored).
#' @inheritParams subtitle_kw_nonparametric
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom stats friedman.test
#'
#' @examples
#' # setup
#' set.seed(123)
#' library(ggstatsplot)
#' library(jmv)
#' data("bugs", package = "jmv")
#'
#' # converting to long format
#' data_bugs <- bugs %>%
#'   tibble::as_tibble(.) %>%
#'   tidyr::gather(., key, value, LDLF:HDHF)
#'
#' # creating the subtitle
#' ggstatsplot::subtitle_friedman_nonparametric(
#'   data = data_bugs,
#'   x = key,
#'   y = value,
#'   k = 2
#' )
#' @export

# function body
subtitle_friedman_nonparametric <- function(data,
                                            x,
                                            y,
                                            messages = TRUE,
                                            k = 2,
                                            ...) {


  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    ) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
    tibble::as_tibble(x = .)

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
  no_measurements <- length(levels(data$x))

  # setting up the anova model and getting its summary
  friedman_stat <-
    stats::friedman.test(
      formula = value ~ key | rowid,
      data = data_within,
      na.action = na.omit
    )

  # calculating Kendall's W
  # ref: http://www.tss.awf.poznan.pl/files/3_Trends_Vol21_2014__no1_20.pdf
  kendall_w <- (friedman_stat$statistic[[1]]) /
    (sample_size * (no_measurements - 1))

  # preparing the subtitle
  subtitle <-
    base::substitute(
      expr =
        paste(
          "Friedman: ",
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
          italic("W")["Kendall"],
          " = ",
          kendall_w,
          ", ",
          italic("n"),
          " = ",
          n
        ),
      env = base::list(
        estimate = specify_decimal_p(
          x = friedman_stat$statistic[[1]],
          k = k,
          p.value = FALSE
        ),
        df = friedman_stat$parameter[[1]],
        pvalue = specify_decimal_p(
          x = friedman_stat$p.value[[1]],
          k,
          p.value = TRUE
        ),
        kendall_w = specify_decimal_p(
          x = kendall_w[[1]],
          k,
          p.value = FALSE
        ),
        n = sample_size
      )
    )

  # return the subtitle
  return(subtitle)
}

#' @title Making text subtitle for the robust ANOVA
#'   (between-subjects designs).
#' @name subtitle_anova_robust
#' @author Indrajeet Patil
#'
#' @inheritParams t1way_ci
#' @inheritParams subtitle_anova_parametric
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#'
#' @examples
#'
#' # examples not executed due to time constraints
#' \dontrun{
#' # for reproducibility
#' set.seed(123)
#'
#' # going with the defaults
#' subtitle_anova_robust(
#'   data = ggplot2::midwest,
#'   x = state,
#'   y = percbelowpoverty,
#'   nboot = 10
#' )
#'
#' # changing defaults
#' subtitle_anova_robust(
#'   data = ggplot2::midwest,
#'   x = state,
#'   y = percollege,
#'   tr = 0.2,
#'   nboot = 10
#' )
#' }
#' @export

# function body
subtitle_anova_robust <- function(data,
                                  x,
                                  y,
                                  tr = 0.1,
                                  nboot = 100,
                                  conf.level = 0.95,
                                  conf.type = "norm",
                                  messages = TRUE,
                                  k = 2,
                                  ...) {


  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    ) %>%
    dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
    tibble::as_tibble(x = .)

  # sample size
  sample_size <- nrow(data)

  # setting up the Bootstrap version of the heteroscedastic one-way ANOVA for
  # trimmed means
  stats_df <-
    t1way_ci(
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
    no.parameters = 2L,
    stat.title = NULL,
    statistic.text = quote(italic("F")),
    statistic = stats_df$F.value[[1]],
    parameter = stats_df$df1[[1]],
    parameter2 = stats_df$df2[[1]],
    p.value = stats_df$p.value[[1]],
    effsize.text = quote(italic(xi)),
    effsize.estimate = stats_df$xi[[1]][[1]],
    effsize.LL = stats_df$conf.low[[1]],
    effsize.UL = stats_df$conf.high[[1]],
    n = sample_size,
    conf.level = conf.level,
    k = k,
    k.parameter = k
  )


  # message about effect size measure
  if (isTRUE(messages)) {
    effsize_ci_message(nboot = nboot, conf.level = conf.level)
  }

  # return the subtitle
  return(subtitle)
}


#' @title Making text subtitle for the between-subject one-way anova designs.
#' @name subtitle_anova_bayes
#' @author Indrajeet Patil
#'
#' @inheritParams subtitle_anova_parametric
#' @inheritParams subtitle_t_bayes
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom stats lm oneway.test na.omit
#' @importFrom sjstats eta_sq omega_sq
#'
#' @examples
#' \dontrun{
#' # with defaults
#' subtitle_anova_bayes(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   k = 2,
#'   bf.prior = 0.8
#' )
#'
#' # modifying the defaults
#' subtitle_anova_bayes(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   effsize.type = "partial_eta",
#'   var.equal = TRUE
#' )
#' }
#' @export

# function body
subtitle_anova_bayes <- function(data,
                                 x,
                                 y,
                                 effsize.type = "unbiased",
                                 partial = TRUE,
                                 var.equal = FALSE,
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
    dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
    tibble::as_tibble(x = .)

  # sample size
  sample_size <- nrow(data)

  # Welch's ANOVA run by default
  stats_df <-
    stats::oneway.test(
      formula = y ~ x,
      data = data,
      subset = NULL,
      na.action = na.omit,
      var.equal = var.equal
    )

  # bayes factor results
  bf_results <-
    bf_oneway_anova(
      data = data,
      x = x,
      y = y,
      bf.prior = bf.prior,
      caption = NULL,
      output = "results"
    )

  # number of decimal places for degree of freedom
  if (!isTRUE(var.equal)) {
    k.df <- k
  } else if (isTRUE(var.equal)) {
    k.df <- 0
  }

  # figuring out which effect size to use
  effsize.type <- effsize_type_switch(effsize.type)

  # preparing the subtitles with appropriate effect sizes
  if (effsize.type == "unbiased") {
    effsize <- "omega"
    if (isTRUE(partial)) {
      effsize.text <- quote(omega["p"])
    } else {
      effsize.text <- quote(omega)
    }
  } else if (effsize.type == "biased") {
    effsize <- "eta"
    if (isTRUE(partial)) {
      effsize.text <- quote(eta["p"])
    } else {
      effsize.text <- quote(eta)
    }
  }

  # creating a standardized dataframe with effect size and its confidence
  # intervals
  effsize_df <- lm_effsize_standardizer(
    object = stats::lm(
      formula = y ~ x,
      data = data,
      na.action = na.omit
    ),
    effsize = effsize,
    partial = partial
  )

  # preparing the subtitle
  subtitle <-
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
          effsize.text^2,
          " = ",
          effsize,
          ", log"["e"],
          "(BF"["10"],
          ") = ",
          bf,
          ", ",
          italic("r")["Cauchy"],
          " = ",
          bf_prior,
          ", ",
          italic("n"),
          " = ",
          n
        ),
      env = base::list(
        effsize.text = effsize.text,
        estimate = specify_decimal_p(x = stats_df$statistic[[1]], k = k),
        df1 = stats_df$parameter[[1]],
        df2 = specify_decimal_p(x = stats_df$parameter[[2]], k = k.df),
        effsize = specify_decimal_p(x = effsize_df$estimate[[1]], k = k),
        bf = specify_decimal_p(x = bf_results$log_e_bf10[[1]], k = 1),
        bf_prior = specify_decimal_p(x = bf_results$bf.prior[[1]], k = 3),
        n = sample_size
      )
    )

  # return the subtitle
  return(subtitle)
}

#' @title Making text subtitle for the within-subject anova designs.
#' @name subtitle_anova_parametric_repeated
#' @author Chuck Powell, Indrajeet Patil
#'
#' @inheritParams t1way_ci
#' @param effsize.type Type of effect size for repeated *ANOVA* test. The
#'   argument can be `"biased"` (`"partial_eta"` for partial eta-squared)
#'   or `"unbiased"` (`"omega"` for omega-squared for **anova**)).
#' @param id.variable The subject identification variable from the dataframe
#'   `data`. See example
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: `k = 2`).
#' @param messages Decides whether messages references, notes, and warnings
#'   are to be displayed (Default: `TRUE`).
#' @param ... Additional arguments.
#' @inheritParams stats::oneway.test
#' @inheritParams subtitle_t_parametric
#' @inheritParams lm_effsize_standardizer
#'
#' @importFrom ez ezANOVA
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom sjstats eta_sq omega_sq
#'
#' @examples
#' # setup
#' set.seed(123)
#' library(ggstatsplot)
#' library(jmv)
#' data("bugs", package = "jmv")
#'
#' # converting to long format
#' data_bugs <- bugs %>%
#'   dplyr::filter(., !is.na(LDHF) & !is.na(LDLF) & !is.na(HDLF) & !is.na(HDHF)) %>%
#'   tibble::as_tibble(.) %>%
#'   tidyr::gather(., key, value, LDLF:HDHF)
#'
#' # creating the subtitle
#' ggstatsplot::subtitle_anova_parametric_repeated(
#'   data = data_bugs,
#'   x = key,
#'   y = value,
#'   id.variable = Subject,
#'   k = 2
#' )
#' @export

# function body
subtitle_anova_parametric_repeated <- function(data,
                                               x,
                                               y,
                                               id.variable,
                                               effsize.type = "unbiased",
                                               conf.level = 0.95,
                                               k = 2,
                                               messages = TRUE,
                                               ...) {

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y),
      id.variable = !!rlang::enquo(id.variable)
    ) %>%
    tidyr::drop_na(data = .) %>%
    dplyr::mutate(
      .data = .,
      x = droplevels(as.factor(x)),
      id.variable = droplevels(as.factor(id.variable))
    ) %>%
    tibble::as_tibble(x = .) %>%
    dplyr::mutate_if(is.character, as.factor)

  # test if we have complete within subjects data
  all_present <- table(data$id.variable, data$x)
  if (!all(all_present == 1)) {
    stop("Check your data you do not appear to have the right number of observations per id")
  }

  # run the ANOVA
  stats_df <-
    ez::ezANOVA(
      data = data,
      dv = y,
      wid = id.variable,
      within = x,
      detailed = TRUE,
      return_aov = TRUE
    )

  # get effect estimate and construct CIs
  #
  if (effsize.type == "unbiased") {
    effsize <- "omega"
    effsize.text <- quote(omega^2)
    effsize_df <- sjstats::omega_sq(stats_df$aov,
      ci.lvl = conf.level,
      partial = FALSE
    )
    effsize.estimate <- effsize_df[2, 2]
    effsize.LL <- effsize_df$conf.low[2]
    effsize.UL <- effsize_df$conf.high[2]
  } else if (effsize.type == "biased") {
    effsize <- "eta"
    effsize.text <- quote(eta["p"]^2)
    effsize_df <- sjstats::eta_sq(stats_df$aov,
      ci.lvl = conf.level,
      partial = TRUE
    )
    effsize.estimate <- effsize_df[2, 2]
    effsize.LL <- effsize_df$conf.low[2]
    effsize.UL <- effsize_df$conf.high[2]
  } else {
    effsize <- "omega"
    effsize.text <- quote(omega^2)
    effsize_df <- sjstats::omega_sq(stats_df$aov,
      ci.lvl = conf.level,
      partial = FALSE
    )
    effsize.estimate <- effsize_df[2, 2]
    effsize.LL <- effsize_df$conf.low[2]
    effsize.UL <- effsize_df$conf.high[2]
  }

  # preparing subtitle
  subtitle <- subtitle_template(
    no.parameters = 2L,
    stat.title = NULL,
    statistic.text = quote(italic("F")),
    statistic = stats_df$ANOVA$F[2],
    parameter = stats_df$ANOVA$DFn[2],
    parameter2 = stats_df$ANOVA$DFd[2],
    p.value = stats_df$ANOVA$p[2],
    effsize.text = effsize.text,
    effsize.estimate = effsize.estimate,
    effsize.LL = effsize.LL,
    effsize.UL = effsize.UL,
    n = stats_df$ANOVA$DFn[1] + stats_df$ANOVA$DFd[1],
    conf.level = conf.level,
    k = k,
    k.parameter = 0
  )

  # message about effect size measure
  if (isTRUE(messages)) {
    #    effsize_ci_message(nboot = nboot, conf.level = conf.level)
  }

  # return the subtitle
  return(subtitle)
}
