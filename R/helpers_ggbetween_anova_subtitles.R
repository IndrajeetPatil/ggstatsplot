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
#'   k = 2
#' )
#'
#' # modifying the defaults
#' subtitle_anova_parametric(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = sleep_rem,
#'   effsize.type = "partial_eta",
#'   var.equal = TRUE,
#'   nboot = 10
#' )
#' @export

# function body
subtitle_anova_parametric <-
  function(data,
             x,
             y,
             effsize.type = "partial_omega",
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

    # preparing the subtitles with appropriate effect sizes
    if (effsize.type %in% c("unbiased", "partial_omega", "partial.omega")) {
      # partial omega-squared is the biased estimate of effect size for
      # parametric ANOVA
      aov_effsize_ci <-
        sjstats::omega_sq(
          model = stats::lm(
            formula = y ~ x,
            data = data,
            na.action = na.omit
          ),
          partial = TRUE,
          ci.lvl = conf.level,
          n = nboot
        )

      # displaying message about bootstrap
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::green("Note: "),
          crayon::blue(
            crayon::yellow(paste(conf.level * 100, "%", sep = "")),
            "CI for partial omega-squared was computed with",
            crayon::yellow(nboot),
            "bootstrap samples.\n"
          ),
          sep = ""
        ))
      }

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
              omega["p"]^2,
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
            effsize = ggstatsplot::specify_decimal_p(
              x = aov_effsize_ci$partial.omegasq[[1]],
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
    } else if (effsize.type %in% c("biased", "partial_eta", "partial.eta")) {
      # getting confidence interval for partial eta-squared
      aov_effsize_ci <- sjstats::eta_sq(
        model = stats::lm(
          formula = y ~ x,
          data = data,
          na.action = na.omit
        ),
        partial = TRUE,
        ci.lvl = conf.level,
        n = nboot
      )

      # displaying message about bootstrap
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::green("Note: "),
          crayon::blue(
            crayon::yellow(paste(conf.level * 100, "%", sep = "")),
            "CI for partial eta-squared was computed with",
            crayon::yellow(nboot),
            "bootstrap samples.\n"
          ),
          sep = ""
        ))
      }

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
              eta["p"]^2,
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
              k,
              p.value = TRUE
            ),
            effsize = ggstatsplot::specify_decimal_p(
              x = aov_effsize_ci$partial.etasq[[1]],
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
    kw_stat <-
      stats::kruskal.test(
        formula = y ~ x,
        data = data,
        na.action = na.omit
      )

    # getting partial eta-squared based on H-statistic
    kw_effsize <-
      suppressWarnings(kw_eta_h_ci(
        data = data,
        x = x,
        y = y,
        nboot = nboot,
        conf.level = conf.level,
        conf.type = conf.type
      ))

    # displaying message about bootstrap
    if (isTRUE(messages)) {
      base::message(cat(
        crayon::green("Note: "),
        crayon::blue(
          crayon::yellow(paste(conf.level * 100, "%", sep = "")),
          "CI for effect size (H-statistic-based eta-squared) was computed with",
          crayon::yellow(nboot),
          "bootstrap samples \nfor Kruskal-Wallis Rank Sum Test.\n"
        ),
        sep = ""
      ))
    }

    # preparing the subtitle
    subtitle <-
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
            eta["H"]^2,
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
            x = kw_stat$statistic[[1]],
            k = k,
            p.value = FALSE
          ),
          df = kw_stat$parameter[[1]],
          pvalue = ggstatsplot::specify_decimal_p(
            x = kw_stat$p.value[[1]],
            k,
            p.value = TRUE
          ),
          effsize = ggstatsplot::specify_decimal_p(
            x = kw_effsize$eta_sq_H[[1]],
            k = k,
            p.value = FALSE
          ),
          conf.level = paste(conf.level * 100, "%", sep = ""),
          LL = ggstatsplot::specify_decimal_p(
            x = kw_effsize$conf.low[[1]],
            k = k,
            p.value = FALSE
          ),
          UL = ggstatsplot::specify_decimal_p(
            x = kw_effsize$conf.high[[1]],
            k = k,
            p.value = FALSE
          ),
          n = sample_size
        )
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
          italic("W"),
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

  # letting the user know that this test doesn't have agreed upon effect size
  if (isTRUE(messages)) {
    base::message(cat(
      crayon::red("Note: "),
      crayon::blue(
        "No effect size available for Friedman Rank Sum Test.\n"
      ),
      sep = ""
    ))
  }

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

    # displaying message about bootstrap
    if (isTRUE(messages)) {
      base::message(cat(
        crayon::green("Note: "),
        crayon::blue(
          crayon::yellow(paste(conf.level * 100, "%", sep = "")),
          "CI for explanatory measure of effect size was computed with",
          crayon::yellow(nboot),
          "bootstrap samples.\n"
        ),
        sep = ""
      ))
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
#'   var.equal = TRUE,
#'   nboot = 10
#' )
#' @export

# function body
subtitle_anova_bayes <- function(data,
                                 x,
                                 y,
                                 effsize.type = "partial_omega",
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

  # preparing the subtitles with appropriate effect sizes
  if (effsize.type %in% c("unbiased", "partial_omega", "partial.omega")) {
    # partial omega-squared is the biased estimate of effect size for
    # parametric ANOVA
    aov_effsize_ci <-
      sjstats::omega_sq(
        model = stats::lm(
          formula = y ~ x,
          data = data,
          na.action = na.omit
        ),
        partial = TRUE
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
            omega["p"]^2,
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
            x = aov_effsize_ci$partial.omegasq[[1]],
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
  } else if (effsize.type %in% c("biased", "partial_eta", "partial.eta")) {
    # getting confidence interval for partial eta-squared
    aov_effsize_ci <- sjstats::eta_sq(
      model = stats::lm(
        formula = y ~ x,
        data = data,
        na.action = na.omit
      ),
      partial = TRUE
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
            eta["p"]^2,
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
            x = aov_effsize_ci$partial.etasq[[1]],
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
  }

  # return the subtitle
  return(subtitle)
}
