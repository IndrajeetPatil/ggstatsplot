#' @title Making text subtitle for parametric ANOVA.
#' @name subtitle_anova_parametric
#' @author Indrajeet Patil
#'
#' @note For repeated measures designs (`paired = TRUE`), only omega-squared and
#'   partial eta-squared effect sizes are supported.
#'
#' @inheritParams t1way_ci
#' @param paired Logical that decides whether the design is repeated
#'   measures/within-subjects (in which case one-way Friedman Rank Sum Test will
#'   be carried out) or between-subjects (in which case one-way Kruskal–Wallis H
#'   test will be carried out). The default is `FALSE`.
#' @param effsize.type Type of effect size needed for *parametric* tests. The
#'   argument can be `"biased"` (equivalent to `"d"` for Cohen's *d* for
#'   **t-test**; `"partial_eta"` for partial eta-squared for **anova**) or
#'   `"unbiased"` (equivalent to `"g"` Hedge's *g* for **t-test**;
#'   `"partial_omega"` for partial omega-squared for **anova**)).
#' @param sphericity.correction Logical that decides whether to apply correction
#'   to account for violation of sphericity in a repeated measures design ANOVA
#'   (Default: `TRUE`).
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: `k = 2`).
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @param ... Additional arguments.
#' @inheritParams stats::oneway.test
#' @inheritParams subtitle_t_parametric
#' @inheritParams groupedstats::lm_effsize_standardizer
#' @inheritParams subtitle_template
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom stats lm oneway.test na.omit
#' @importFrom ez ezANOVA
#' @importFrom groupedstats lm_effsize_standardizer
#'
#' @examples
#'
#' # for reproducibility
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # -------------------- between-subjects ------------------------------
#'
#' # with defaults
#' ggstatsplot::subtitle_anova_parametric(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   paired = FALSE,
#'   k = 3
#' )
#'
#' # modifying the defaults
#' ggstatsplot::subtitle_anova_parametric(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   paired = FALSE,
#'   effsize.type = "biased",
#'   partial = FALSE,
#'   var.equal = TRUE,
#'   nboot = 10
#' )
#'
#' # -------------------- repeated measures ------------------------------
#'
#' ggstatsplot::subtitle_anova_parametric(
#'   data = iris_long,
#'   x = condition,
#'   y = value,
#'   paired = TRUE,
#'   k = 4,
#'   nboot = 10
#' )
#' @export

# function body
subtitle_anova_parametric <- function(data,
                                      x,
                                      y,
                                      paired = FALSE,
                                      effsize.type = "unbiased",
                                      partial = TRUE,
                                      conf.level = 0.95,
                                      nboot = 100,
                                      var.equal = FALSE,
                                      sphericity.correction = TRUE,
                                      k = 2,
                                      stat.title = NULL,
                                      messages = TRUE,
                                      ...) {

  # for paired designs, variance is going to be equal across grouping levels
  if (isTRUE(paired)) {
    var.equal <- TRUE
  } else {
    sphericity.correction <- FALSE
  }

  # number of decimal places for degree of freedom
  if (isTRUE(var.equal)) {
    if (isTRUE(sphericity.correction)) {
      k.df2 <- k
    } else {
      k.df2 <- 0L
    }
  } else {
    k.df2 <- k
  }

  # denominator degrees of freedom
  if (isTRUE(paired) && isTRUE(sphericity.correction)) {
    k.df1 <- k
  } else {
    k.df1 <- 0L
  }

  # figuring out which effect size to use
  effsize.type <- effsize_type_switch(effsize.type)

  # some of the effect sizes don't work properly for paired designs
  if (isTRUE(paired)) {
    if (effsize.type == "unbiased") {
      partial <- FALSE
    } else {
      partial <- TRUE
    }
  }

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

  # creating a dataframe
  data <-
    dplyr::select(.data = data, x = {{ x }}, y = {{ y }}) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
    tibble::as_tibble(x = .)

  # properly removing NAs if it's a paired design
  if (isTRUE(paired)) {
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

    # warn the user if
    if (sample_size < nlevels(as.factor(data_within$key))) {
      # no sphericity correction applied
      sphericity.correction <- FALSE
      k.df1 <- 0L
      k.df2 <- 0L

      # inform the user
      message(cat(
        crayon::red("Warning: "),
        crayon::blue(
          "No. of factor levels is greater than number of observations per cell.\n"
        ),
        crayon::blue("No sphericity correction applied. Interpret the results with caution.\n")
      ),
      sep = ""
      )
    }

    # run the ANOVA
    ez_df <-
      ez::ezANOVA(
        data = data_within %>%
          dplyr::mutate_if(
            .tbl = .,
            .predicate = purrr::is_bare_character,
            .funs = as.factor
          ) %>%
          dplyr::mutate(.data = ., rowid = as.factor(rowid)),
        dv = value,
        wid = rowid,
        within = key,
        detailed = TRUE,
        return_aov = TRUE
      )

    # list with results
    if (isTRUE(sphericity.correction)) {
      epsilon_corr <- ez_df$`Sphericity Corrections`$GGe
      stats_df <-
        list(
          statistic = ez_df$ANOVA$F[2],
          parameter = c(
            epsilon_corr * ez_df$ANOVA$DFn[2],
            epsilon_corr * ez_df$ANOVA$DFd[2]
          ),
          p.value = ez_df$`Sphericity Corrections`$`p[GG]`[[1]]
        )
    } else {
      stats_df <-
        list(
          statistic = ez_df$ANOVA$F[2],
          parameter = c(ez_df$ANOVA$DFn[2], ez_df$ANOVA$DFd[2]),
          p.value = ez_df$ANOVA$p[2]
        )
    }

    # creating a standardized dataframe with effect size and its CIs
    effsize_object <- ez_df$aov
  } else {

    # remove NAs listwise for between-subjects design
    data %<>%
      tidyr::drop_na(data = .)

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

    # creating a standardized dataframe with effect size and its CIs
    effsize_object <-
      stats::lm(
        formula = y ~ x,
        data = data,
        na.action = na.omit
      )
  }

  # creating a standardized dataframe with effect size and its CIs
  effsize_df <-
    groupedstats::lm_effsize_standardizer(
      object = effsize_object,
      effsize = effsize,
      partial = partial,
      conf.level = conf.level,
      nboot = nboot
    )

  # preparing subtitle
  subtitle <- subtitle_template(
    no.parameters = 2L,
    stat.title = stat.title,
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
    k.parameter = k.df1,
    k.parameter2 = k.df2
  )

  # message about effect size measure
  if (isTRUE(messages)) {
    effsize_ci_message(nboot = nboot, conf.level = conf.level)
  }

  # return the subtitle
  return(subtitle)
}

#' @title Making text subtitle for nonparametric ANOVA.
#' @name subtitle_anova_nonparametric
#' @author Indrajeet Patil
#'
#' @description For paired designs, the effect size is Kendall's coefficient of
#'   concordance (*W*), while for between-subjects designs, the effect size is
#'   epsilon-squared (for more, see `?rcompanion::epsilonSquared`).
#'
#' @inheritParams t1way_ci
#' @inheritParams subtitle_anova_parametric
#' @inheritParams subtitle_template
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom stats friedman.test kruskal.test
#' @importFrom broomExtra tidy
#' @importFrom rcompanion epsilonSquared
#'
#' @examples
#' # setup
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # -------------- within-subjects design --------------------------------
#'
#' library(jmv)
#' data("bugs", package = "jmv")
#'
#' # converting to long format
#' data_bugs <- bugs %>%
#'   tibble::as_tibble(.) %>%
#'   tidyr::gather(., key, value, LDLF:HDHF)
#'
#' # creating the subtitle
#' ggstatsplot::subtitle_anova_nonparametric(
#'   data = data_bugs,
#'   x = key,
#'   y = value,
#'   paired = TRUE,
#'   conf.level = 0.99,
#'   k = 2
#' )
#'
#' # -------------- between-subjects design --------------------------------
#'
#' ggstatsplot::subtitle_anova_nonparametric(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   paired = FALSE,
#'   conf.level = 0.99,
#'   conf.type = "perc"
#' )
#' @export

# function body
subtitle_anova_nonparametric <- function(data,
                                         x,
                                         y,
                                         paired = FALSE,
                                         conf.type = "norm",
                                         conf.level = 0.95,
                                         k = 2,
                                         nboot = 100,
                                         stat.title = NULL,
                                         messages = TRUE,
                                         ...) {

  # creating a dataframe
  data <-
    dplyr::select(.data = data, x = {{ x }}, y = {{ y }}) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
    tibble::as_tibble(x = .)

  # properly removing NAs if it's a paired design
  if (isTRUE(paired)) {
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

    # setting up the anova model and getting its summary
    stats_df <-
      broomExtra::tidy(stats::friedman.test(
        formula = value ~ key | rowid,
        data = data_within,
        na.action = na.omit
      ))

    # calculating Kendall's W and its CI
    effsize_df <- kendall_w_ci(
      data = dplyr::select(long_to_wide_converter(data, x, y), -rowid),
      nboot = nboot,
      conf.type = conf.type,
      conf.level = conf.level
    )
  } else {
    # remove NAs listwise for between-subjects design
    data %<>%
      tidyr::drop_na(data = .)

    # sample size
    sample_size <- nrow(data)

    # setting up the anova model and getting its summary
    stats_df <-
      broomExtra::tidy(stats::kruskal.test(
        formula = y ~ x,
        data = data,
        na.action = na.omit
      ))

    # getting partial eta-squared based on H-statistic
    effsize_df <-
      rcompanion::epsilonSquared(
        x = data$y,
        g = data$x,
        group = "row",
        ci = TRUE,
        conf = conf.level,
        type = conf.type,
        R = nboot,
        histogram = FALSE,
        digits = 5
      ) %>%
      tibble::as_tibble(x = .) %>%
      dplyr::rename(
        .data = .,
        estimate = epsilon.squared,
        conf.low = lower.ci,
        conf.high = upper.ci
      )
  }

  # message about effect size measure
  if (isTRUE(messages)) {
    effsize_ci_message(nboot = nboot, conf.level = conf.level)
  }

  # choosing the appropriate effect size text
  if (isTRUE(paired)) {
    effsize.text <- quote(italic("W")["Kendall"])
  } else {
    effsize.text <- quote(epsilon^2)
  }

  # preparing subtitle
  subtitle <- subtitle_template(
    no.parameters = 1L,
    stat.title = stat.title,
    statistic.text = quote(italic(chi)^2),
    statistic = stats_df$statistic[[1]],
    parameter = stats_df$parameter[[1]],
    p.value = stats_df$p.value[[1]],
    effsize.text = effsize.text,
    effsize.estimate = effsize_df$estimate[[1]],
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

#' @title Making text subtitle for the robust ANOVA
#' @name subtitle_anova_robust
#' @author Indrajeet Patil
#'
#' @inheritParams t1way_ci
#' @inheritParams subtitle_anova_nonparametric
#' @inheritParams subtitle_template
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom WRS2 rmanova
#'
#' @examples
#'
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#'
#' # ------------------------ between-subjects -----------------------------
#'
#' # going with the defaults
#' ggstatsplot::subtitle_anova_robust(
#'   data = ggplot2::midwest,
#'   x = state,
#'   y = percbelowpoverty,
#'   paired = FALSE,
#'   nboot = 10
#' )
#'
#' # changing defaults
#' subtitle_anova_robust(
#'   data = ggplot2::midwest,
#'   x = state,
#'   y = percollege,
#'   paired = FALSE,
#'   conf.level = 0.99,
#'   tr = 0.2,
#'   nboot = 10
#' )
#'
#' # ------------------------ within-subjects -----------------------------
#'
#' ggstatsplot::subtitle_anova_robust(
#'   data = iris_long,
#'   x = condition,
#'   y = value,
#'   paired = TRUE,
#'   tr = 0.2,
#'   k = 3
#' )
#' }
#' @export

# function body
subtitle_anova_robust <- function(data,
                                  x,
                                  y,
                                  paired = FALSE,
                                  tr = 0.1,
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

  # properly removing NAs if it's a paired design
  if (isTRUE(paired)) {
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

    # test
    stats_df <-
      WRS2::rmanova(
        y = data_within$value,
        groups = data_within$key,
        blocks = data_within$rowid,
        tr = tr
      )

    # preparing the subtitle
    subtitle <-
      substitute(
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
            p.value,
            ", ",
            italic("n"),
            " = ",
            n
          ),
        env = list(
          estimate = specify_decimal_p(x = stats_df$test[[1]], k = k),
          df1 = specify_decimal_p(x = stats_df$df1[[1]], k = k),
          df2 = specify_decimal_p(x = stats_df$df2[[1]], k = k),
          p.value = specify_decimal_p(x = stats_df$p.value[[1]], k = k, p.value = TRUE),
          n = sample_size
        )
      )
  } else {
    # remove NAs listwise for between-subjects design
    data %<>%
      tidyr::drop_na(data = .)

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
      stat.title = stat.title,
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
      k.parameter = 0L,
      k.parameter2 = k
    )

    # message about effect size measure
    if (isTRUE(messages)) {
      effsize_ci_message(nboot = nboot, conf.level = conf.level)
    }
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
#'
#' @examples
#' \donttest{
#' set.seed(123)
#'
#' # between-subjects ---------------------------------------
#' # with defaults
#' ggstatsplot::subtitle_anova_bayes(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem
#' )
#'
#' # modifying the defaults
#' ggstatsplot::subtitle_anova_bayes(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   k = 3,
#'   bf.prior = 0.8
#' )
#'
#' # repeated measures ---------------------------------------
#' ggstatsplot::subtitle_anova_bayes(
#'   data = WRS2::WineTasting,
#'   x = Wine,
#'   y = Taste,
#'   paired = TRUE,
#'   k = 4
#' )
#' }
#' @export

# function body
subtitle_anova_bayes <- function(data,
                                 x,
                                 y,
                                 paired = FALSE,
                                 bf.prior = 0.707,
                                 k = 2,
                                 ...) {

  # creating a dataframe
  data <-
    dplyr::select(.data = data, x = {{ x }}, y = {{ y }}) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
    tibble::as_tibble(x = .)

  # properly removing NAs if it's a paired design
  if (isTRUE(paired)) {
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
  } else {

    # remove NAs listwise for between-subjects design
    data %<>%
      tidyr::drop_na(data = .)
  }

  # bayes factor results
  subtitle <-
    bf_oneway_anova(
      data = data,
      x = x,
      y = y,
      paired = paired,
      bf.prior = bf.prior,
      k = k,
      caption = NULL,
      output = "h1"
    )

  # return the subtitle
  return(subtitle)
}
