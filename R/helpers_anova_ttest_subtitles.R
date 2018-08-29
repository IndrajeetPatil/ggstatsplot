#'
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
#' @inheritParams stats::oneway.test
#' @inheritParams specify_decimal_p
#'
#' @importFrom dplyr select
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @importFrom rlang enquo
#' @importFrom stats lm
#'
#' @keywords internal
#'

subtitle_ggbetween_anova_parametric <-
  function(data,
             x,
             y,
             effsize.type = "biased",
             nboot = 100,
             var.equal = FALSE,
             k = 3) {
    # creating a dataframe
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(x),
        y = !!rlang::enquo(y)
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
            pvalue = ggstatsplot::specify_decimal_p(x = aov_stat$p.value[[1]], k, p.value = TRUE),
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
