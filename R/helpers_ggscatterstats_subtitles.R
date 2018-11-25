#' @title Making text subtitle for the correlation test.
#' @name subtitle_ggscatterstats
#' @author Indrajeet Patil
#'
#' @param type Type of association between paired samples required
#'   ("`"parametric"`: Pearson's product moment correlation coefficient" or
#'   "`"nonparametric"`: Spearman's rho" or "`"robust"`: percentage bend
#'   correlation coefficient" or "`"bayes"`: Bayes Factor for Pearson's *r*").
#'   Corresponding abbreviations are also accepted: `"p"` (for
#'   parametric/pearson's), `"np"` (nonparametric/spearman), `"r"` (robust),
#'   `"bf"` (for bayes factor), resp.
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @inheritParams robcor_ci
#' @inheritParams cor_test_ci
#' @inheritParams bf_corr_test
#' @inheritParams subtitle_t_parametric
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom stats cor.test
#'
#' @examples
#' 
#' # without changing defaults
#' subtitle_ggscatterstats(
#'   data = ggplot2::midwest,
#'   x = area,
#'   y = percblack
#' )
#' 
#' # changing defaults
#' subtitle_ggscatterstats(
#'   data = ggplot2::midwest,
#'   x = area,
#'   y = percblack,
#'   nboot = 25,
#'   beta = 0.2,
#'   type = "r",
#'   k = 1
#' )
#' @export

# function body
subtitle_ggscatterstats <-
  function(data,
             x,
             y,
             nboot = 100,
             beta = 0.1,
             type = "pearson",
             bf.prior = 0.707,
             conf.level = 0.95,
             conf.type = "norm",
             messages = TRUE,
             k = 2) {

    #------------------------ dataframe -------------------------------------

    # if dataframe is provided
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(x),
        y = !!rlang::enquo(y)
      ) %>%
      stats::na.omit(.) %>%
      tibble::as_tibble(x = .)

    # the total sample size for analysis
    sample_size <- nrow(x = data)

    # Pearson's r (will also be used for Bayes tests)
    pearson_r_res <-
      stats::cor.test(
        formula = ~ x + y,
        data = data,
        method = "pearson",
        alternative = "two.sided",
        exact = FALSE,
        na.action = na.omit
      )

    #------------------------ Pearson's r -------------------------------------
    #
    if (type == "pearson" || type == "parametric" || type == "p") {

      # preparing the label
      subtitle <-
        base::substitute(
          expr =
            paste(
              italic("r")["pearson"],
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
            df = pearson_r_res$parameter[[1]],
            estimate = ggstatsplot::specify_decimal_p(
              x = pearson_r_res$estimate[[1]],
              k = k,
              p.value = FALSE
            ),
            LL = ggstatsplot::specify_decimal_p(
              x = pearson_r_res$conf.int[1][[1]],
              k = k,
              p.value = FALSE
            ),
            UL = ggstatsplot::specify_decimal_p(
              x = pearson_r_res$conf.int[2][[1]],
              k = k,
              p.value = FALSE
            ),
            pvalue = ggstatsplot::specify_decimal_p(
              x = pearson_r_res$p.value[[1]],
              k = k,
              p.value = TRUE
            ),
            n = sample_size
          )
        )

      #--------------------- Spearnman's rho ---------------------------------
    } else if (type == "spearman" || type == "nonparametric" || type == "np") {

      # note that stats::cor.test doesn't give degress of freedom; it's
      # calculated as df = (no. of pairs - 2)
      c <-
        stats::cor.test(
          formula = ~ x + y,
          data = data,
          method = "spearman",
          alternative = "two.sided",
          exact = FALSE,
          na.action = na.omit
        )

      # getting confidence interval for rho using broom bootstrap
      c_ci <-
        cor_test_ci(
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
          crayon::green("Note: "),
          crayon::blue(
            "95% CI for Spearman's rho was computed with",
            crayon::yellow(nboot),
            "bootstrap samples.\n"
          ),
          sep = ""
        ))
      }

      # preparing the label
      subtitle <-
        base::substitute(
          expr =
            paste(
              italic(rho)["spearman"],
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
            df = (sample_size - 2),
            estimate = ggstatsplot::specify_decimal_p(
              x = c$estimate[[1]],
              k = k,
              p.value = FALSE
            ),
            LL = ggstatsplot::specify_decimal_p(
              x = c_ci$conf.low[[1]],
              k = k,
              p.value = FALSE
            ),
            UL = ggstatsplot::specify_decimal_p(
              x = c_ci$conf.high[[1]],
              k = k,
              p.value = FALSE
            ),
            pvalue = ggstatsplot::specify_decimal_p(
              x = c$p.value[[1]],
              k,
              p.value = TRUE
            ),
            n = sample_size
          )
        )

      #---------------------- robust percentage bend --------------------------
    } else if (type == "robust" || type == "r") {
      # running robust correlation
      rob_res <-
        robcor_ci(
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
              italic(rho)["pb"],
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
            estimate = ggstatsplot::specify_decimal_p(
              x = rob_res$r[[1]],
              k = k,
              p.value = FALSE
            ),
            LL = ggstatsplot::specify_decimal_p(
              x = rob_res$conf.low[[1]],
              k = k,
              p.value = FALSE
            ),
            UL = ggstatsplot::specify_decimal_p(
              x = rob_res$conf.high[[1]],
              k = k,
              p.value = FALSE
            ),
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
          crayon::green("Note: "),
          crayon::blue(
            "95% CI for percentage bend correlation was computed with",
            crayon::yellow(nboot),
            "bootstrap samples.\n"
          ),
          sep = ""
        ))
      }
      #---------------------- bayes factor -----------------------------------
    } else if (type == "bayes" || type == "bf") {

      # bayes factor results
      bf_results <-
        bf_corr_test(
          data = data,
          x = x,
          y = y,
          bf.prior = bf.prior,
          caption = NULL,
          output = "results"
        )

      # preparing the subtitle
      subtitle <-
        base::substitute(
          expr =
            paste(
              italic("r")["pearson"],
              "(",
              df,
              ")",
              " = ",
              estimate,
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
            df = pearson_r_res$parameter[[1]],
            estimate = ggstatsplot::specify_decimal_p(
              x = pearson_r_res$estimate[[1]],
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
