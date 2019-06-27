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
#' @inheritParams robcor_ci
#' @inheritParams bf_corr_test
#' @inheritParams subtitle_anova_parametric
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo
#' @importFrom stats cor.test
#' @importFrom DescTools SpearmanRho
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
subtitle_ggscatterstats <- function(data,
                                    x,
                                    y,
                                    nboot = 100,
                                    beta = 0.1,
                                    type = "pearson",
                                    bf.prior = 0.707,
                                    conf.level = 0.95,
                                    conf.type = "norm",
                                    k = 2,
                                    stat.title = NULL,
                                    messages = TRUE,
                                    ...) {
  ellipsis::check_dots_used()

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
  sample_size <- nrow(data)

  # standardize the type of statistics
  stats.type <- stats_type_switch(stats.type = type)

  #------------------------ Pearson's r -------------------------------------

  if (stats.type == "parametric") {

    # Pearson's r
    stats_df <-
      broomExtra::tidy(
        stats::cor.test(
          formula = ~ x + y,
          data = data,
          method = "pearson",
          alternative = "two.sided",
          exact = FALSE,
          conf.level = conf.level,
          na.action = na.omit
        )
      )

    # stats object already contains effect size info
    effsize_df <- stats_df

    # subtitle parameters
    no.parameters <- 1L
    parameter <- stats_df$parameter[[1]]
    statistic.text <- quote(italic("t"))
    effsize.text <- quote(italic("r")["Pearson"])
  }

  #--------------------- Spearnman's rho ---------------------------------
  if (stats.type == "nonparametric") {

    # degrees of freedom calculated as df = (no. of pairs - 2)
    stats_df <-
      broomExtra::tidy(
        stats::cor.test(
          formula = ~ x + y,
          data = data,
          method = "spearman",
          alternative = "two.sided",
          exact = FALSE,
          na.action = na.omit
        )
      ) %>%
      dplyr::mutate(.data = ., statistic = log(statistic))

    # getting confidence interval for rho using broom bootstrap
    effsize_df <-
      DescTools::SpearmanRho(
        x = data$x,
        y = data$y,
        use = "pairwise.complete.obs",
        conf.level = conf.level
      ) %>%
      tibble::enframe(x = .) %>%
      tidyr::spread(data = ., key = "name", value = "value") %>%
      dplyr::select(
        .data = .,
        estimate = rho,
        conf.low = lwr.ci,
        conf.high = upr.ci
      )

    # subtitle parameters
    no.parameters <- 0L
    parameter <- NULL
    statistic.text <- quote("log"["e"](italic("S")))
    effsize.text <- quote(italic(rho)["Spearman"])
  }

  #---------------------- robust percentage bend --------------------------
  if (stats.type == "robust") {
    # running robust correlation
    stats_df <-
      robcor_ci(
        data = data,
        x = x,
        y = y,
        beta = beta,
        nboot = nboot,
        conf.level = conf.level,
        conf.type = conf.type
      )

    # stats object already contains effect size info
    effsize_df <- stats_df

    # subtitle parameters
    no.parameters <- 1L
    parameter <- sample_size - 2L
    statistic.text <- quote(italic("t"))
    effsize.text <- quote(italic(rho)["pb"])

    # message about effect size measure
    if (isTRUE(messages)) {
      effsize_ci_message(nboot = nboot, conf.level = conf.level)
    }
  }

  #---------------------- preparing subtitle ---------------------------------
  if (stats.type %in% c("parametric", "nonparametric", "robust")) {
    # preparing subtitle
    subtitle <- subtitle_template(
      no.parameters = no.parameters,
      stat.title = stat.title,
      statistic.text = statistic.text,
      statistic = stats_df$statistic[[1]],
      parameter = parameter,
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
  }

  #---------------------- bayes factor -----------------------------------
  if (stats.type == "bayes") {

    # bayes factor results
    subtitle <-
      bf_corr_test(
        data = data,
        x = x,
        y = y,
        bf.prior = bf.prior,
        caption = NULL,
        output = "h1",
        k = k
      )
  }

  # return the subtitle
  return(subtitle)
}
