#' @title Making text subtitle for the between-subject anova designs.
#' @name subtitle_anova_parametric
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
    dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
    tibble::as_tibble(x = .)

  # convert the grouping variable to factor and drop unused levels
  data %<>%
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

  # number of decimal places for degree of freedom
  if (!isTRUE(var.equal)) {
    k.df2 <- k
  } else if (isTRUE(var.equal)) {
    k.df2 <- 0
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
  aov_effsize_ci <- lm_effsize_standardizer(
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

  # preparing the subtitle
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
          effsize.text^2,
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
          x = aov_stat$statistic[[1]],
          k = k,
          p.value = FALSE
        ),
        df1 = aov_stat$parameter[[1]],
        df2 = ggstatsplot::specify_decimal_p(
          x = aov_stat$parameter[[2]],
          k = k.df2,
          p.value = FALSE
        ),
        pvalue = ggstatsplot::specify_decimal_p(
          x = aov_stat$p.value[[1]],
          k = k,
          p.value = TRUE
        ),
        effsize.text = effsize.text,
        effsize = ggstatsplot::specify_decimal_p(
          x = aov_effsize_ci$estimate[[1]],
          k = k,
          p.value = FALSE
        ),
        conf.level = paste(conf.level * 100, "%", sep = ""),
        LL = ggstatsplot::specify_decimal_p(
          x = aov_effsize_ci$conf.low[[1]],
          k = k,
          p.value = FALSE
        ),
        UL = ggstatsplot::specify_decimal_p(
          x = aov_effsize_ci$conf.high[[1]],
          k = k,
          p.value = FALSE
        ),
        n = nrow(x = data)
      )
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
      dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
      tibble::as_tibble(x = .)

    # convert the grouping variable to factor and drop unused levels
    data %<>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "x",
        .funs = ~ base::droplevels(x = base::as.factor(x = .))
      )

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
    subtitle <- subtitle_template_1(
      stat.title = "Kruskal-Wallis: ",
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
    )

  # convert the grouping variable to factor and drop unused levels
  data %<>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "x",
      .funs = ~ base::droplevels(x = base::as.factor(x = .))
    ) %>%
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
          italic("W")["kendall"],
          " = ",
          kendall_w,
          ", ",
          italic("n"),
          " = ",
          n
        ),
      env = base::list(
        estimate = ggstatsplot::specify_decimal_p(
          x = friedman_stat$statistic[[1]],
          k = k,
          p.value = FALSE
        ),
        df = friedman_stat$parameter[[1]],
        pvalue = ggstatsplot::specify_decimal_p(
          x = friedman_stat$p.value[[1]],
          k,
          p.value = TRUE
        ),
        kendall_w = ggstatsplot::specify_decimal_p(
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
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @param ... Additional arguments (ignored).
#' @inheritParams t1way_ci
#' @inheritParams subtitle_t_parametric
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
subtitle_anova_robust <-
  function(data,
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
      dplyr::filter(.data = ., !is.na(x), !is.na(y))

    # convert the grouping variable to factor and drop unused levels
    data %<>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "x",
        .funs = ~ base::droplevels(x = base::as.factor(x = .))
      )

    # sample size
    sample_size <- nrow(data)

    # setting up the Bootstrap version of the heteroscedastic one-way ANOVA for
    # trimmed means
    robust_aov_stat <-
      t1way_ci(
        data = data,
        x = x,
        y = y,
        tr = tr,
        nboot = nboot,
        conf.level = conf.level,
        conf.type = conf.type
      )

    # message about effect size measure
    if (isTRUE(messages)) {
      effsize_ci_message(nboot = nboot, conf.level = conf.level)
    }

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
            x = robust_aov_stat$`F-value`[[1]],
            k = k,
            p.value = FALSE
          ),
          df1 = robust_aov_stat$df1[[1]],
          df2 = ggstatsplot::specify_decimal_p(
            x = robust_aov_stat$df2[[1]],
            k = k,
            p.value = FALSE
          ),
          pvalue = ggstatsplot::specify_decimal_p(
            x = robust_aov_stat$`p-value`[[1]],
            k = k,
            p.value = TRUE
          ),
          effsize = ggstatsplot::specify_decimal_p(
            x = robust_aov_stat$xi[[1]],
            k = k,
            p.value = FALSE
          ),
          conf.level = paste(conf.level * 100, "%", sep = ""),
          LL = ggstatsplot::specify_decimal_p(
            x = robust_aov_stat$conf.low[[1]],
            k = k,
            p.value = FALSE
          ),
          UL = ggstatsplot::specify_decimal_p(
            x = robust_aov_stat$conf.high[[1]],
            k = k,
            p.value = FALSE
          ),
          n = sample_size
        )
      )

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
    dplyr::filter(.data = ., !is.na(x), !is.na(y))

  # convert the grouping variable to factor and drop unused levels
  data %<>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "x",
      .funs = ~ base::droplevels(x = base::as.factor(x = .))
    )

  # sample size
  sample_size <- nrow(data)

  # Welch's ANOVA run by default
  aov_stat <-
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
    k.df2 <- k
  } else if (isTRUE(var.equal)) {
    k.df2 <- 0
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
  aov_effsize_ci <- lm_effsize_standardizer(
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
          ", Prior width = ",
          bf_prior,
          ", ",
          italic("n"),
          " = ",
          n
        ),
      env = base::list(
        effsize.text = effsize.text,
        estimate = ggstatsplot::specify_decimal_p(
          x = aov_stat$statistic[[1]],
          k = k,
          p.value = FALSE
        ),
        df1 = aov_stat$parameter[[1]],
        df2 = ggstatsplot::specify_decimal_p(
          x = aov_stat$parameter[[2]],
          k = k.df2,
          p.value = FALSE
        ),
        effsize = ggstatsplot::specify_decimal_p(
          x = aov_effsize_ci$estimate[[1]],
          k = k,
          p.value = FALSE
        ),
        bf = ggstatsplot::specify_decimal_p(
          x = bf_results$log_e_bf10[[1]],
          k = 1,
          p.value = FALSE
        ),
        bf_prior = ggstatsplot::specify_decimal_p(
          x = bf_results$bf.prior[[1]],
          k = 3,
          p.value = FALSE
        ),
        n = sample_size
      )
    )

  # return the subtitle
  return(subtitle)
}
