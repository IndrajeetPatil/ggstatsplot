#'
#' @title Making text subtitle for the correlation test.
#' @name subtitle_ggscatterstats
#' @author Indrajeet Patil
#'
#' @param type Type of association between paired samples required
#'   ("`"parametric"`: Pearson's product moment correlation coefficient" or
#'   "`"nonparametric"`: Spearman's rho" or "`"robust"`: percentage bend
#'   correlation coefficient"). Corresponding abbreviations are also accepted:
#'   `"p"` (for parametric/pearson's), `"np"` (nonparametric/spearman), `"r"`
#'   (robust), resp.
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @inheritParams robcor_ci
#' @inheritParams cor_tets_ci
#' @inheritParams specify_decimal_p
#'
#' @importFrom dplyr select
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @importFrom rlang enquo
#' @importFrom stats cor.test
#'
#' @keywords internal
#'

subtitle_ggscatterstats <-
  function(data,
             x,
             y,
             nboot = 100,
             beta = 0.1,
             type = "pearson",
             conf.level = 0.95,
             conf.type = "norm",
             messages = TRUE,
             k = 3) {
    # if dataframe is provided
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(x),
        y = !!rlang::enquo(y)
      )

    #-------------------------------------------------- Pearson's r -----------------------------------------------------
    #
    if (type == "pearson" || type == "p") {

      # Pearson's r
      c <-
        stats::cor.test(
          formula = ~x + y,
          data = data,
          method = "pearson",
          alternative = "two.sided",
          exact = FALSE,
          na.action = na.omit
        )

      # preparing the label
      subtitle <-
        base::substitute(
          expr =
            paste(
              "Pearson's ",
              italic("r"),
              "(",
              df,
              ")",
              " = ",
              estimate,
              ", 95% CI [",
              LL,
              ", ",
              UL,
              "], ",
              italic("p"),
              " = ",
              pvalue,
              ", ",
              italic("n"),
              " = ",
              n
            ),
          env = base::list(
            # degrees of freedom are always integer
            df = c$parameter[[1]],
            t = ggstatsplot::specify_decimal_p(x = c$statistic[[1]], k),
            estimate = ggstatsplot::specify_decimal_p(x = c$estimate[[1]], k),
            LL = ggstatsplot::specify_decimal_p(x = c$conf.int[1][[1]], k),
            UL = ggstatsplot::specify_decimal_p(x = c$conf.int[2][[1]], k),
            pvalue = ggstatsplot::specify_decimal_p(x = c$p.value[[1]], k, p.value = TRUE),
            n = nrow(x = data)
          )
        )

      #-------------------------------------------------- Spearnman's rho -----------------------------------------------------
    } else if (type == "spearman" || type == "np") {

      # note that stats::cor.test doesn't give degress of freedom; it's
      # calculated as df = (no. of pairs - 2)
      c <-
        stats::cor.test(
          formula = ~x + y,
          data = data,
          method = "spearman",
          alternative = "two.sided",
          exact = FALSE,
          na.action = na.omit
        )

      # getting confidence interval for rho using broom bootstrap
      c_ci <- cor_tets_ci(
        data = data,
        x = x,
        y = y,
        nboot = nboot,
        conf.level = conf.level,
        conf.type = conf.type
      )

      # displaying message about bootstrap
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::green("Note:"),
          crayon::blue(
            "95% CI for Spearman's rho was computed with ",
            crayon::yellow(nboot),
            "bootstrap samples."
          )
        ))
      }

      # preparing the label
      subtitle <-
        base::substitute(
          expr =
            paste(
              "Spearman's ",
              italic(rho),
              "(",
              df,
              ")",
              " = ",
              estimate,
              ", 95% CI [",
              LL,
              ", ",
              UL,
              "], ",
              italic("p"),
              " = ",
              pvalue,
              ", ",
              italic("n"),
              " = ",
              n
            ),
          env = base::list(
            df = (length(data$x) - 2),
            # degrees of freedom are always integer
            estimate = ggstatsplot::specify_decimal_p(x = c$estimate[[1]], k),
            LL = ggstatsplot::specify_decimal_p(x = c_ci$conf.low[[1]], k),
            UL = ggstatsplot::specify_decimal_p(x = c_ci$conf.high[[1]], k),
            pvalue = ggstatsplot::specify_decimal_p(
              x = c$p.value[[1]],
              k,
              p.value = TRUE
            ),
            n = nrow(x = data)
          )
        )

      #-------------------------------------------------- robust percentage bend --------------------------------------------------------
    } else if (type == "robust" || type == "r") {
      # running robust correlation
      rob_res <- robcor_ci(
        data = data,
        x = x,
        y = y,
        beta = beta,
        nboot = nboot,
        conf.level = conf.level,
        conf.type = conf.type
      )

      # preparing the subtitle
      subtitle <-
        base::substitute(
          expr =
            paste(
              "robust ",
              italic(r),
              " = ",
              estimate,
              ", 95% CI [",
              LL,
              ", ",
              UL,
              "], ",
              italic("p"),
              " = ",
              pvalue,
              ", ",
              italic("n"),
              " = ",
              n
            ),

          env = base::list(
            estimate = ggstatsplot::specify_decimal_p(x = rob_res$r[[1]], k),
            LL = ggstatsplot::specify_decimal_p(x = rob_res$conf.low[[1]], k),
            UL = ggstatsplot::specify_decimal_p(x = rob_res$conf.high[[1]], k),
            # degrees of freedom are always integer
            pvalue = ggstatsplot::specify_decimal_p(rob_res$`p-value`[[1]],
              k,
              p.value = TRUE
            ),
            n = rob_res$n[[1]]
          )
        )

      # displaying message about bootstrap
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::green("Note:"),
          crayon::blue(
            "95% CI for percentage bend correlation was computed with",
            crayon::yellow(nboot),
            "bootstrap samples."
          )
        ))
      }
    }

    # return the subtitle
    return(subtitle)
  }
