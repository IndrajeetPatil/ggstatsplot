# custom function to write results from chi-square test into subtitle for the plot
# jmv_chi stands for the chi-square object from package jmv
# effect is the text label that needs to be entered to denote which interaction effect
# is being investigated in
# the chi-square test presented...if not entered, the default will be "Chi-square test"

chi_subtitle <- function(jmv_chi,
                         cramer_ci,
                         effect = NULL,
                         k = 3) {

  # preparing the subtitle
  results_subtitle <- base::substitute(
    expr =
      paste(
        y,
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
        italic(V),
        " = ",
        cramer,
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
      y = effect,
      estimate = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_chi$chiSq)[[2]], k),
      df = as.data.frame(jmv_chi$chiSq)[[3]],
      # df always an integer
      pvalue = ggstatsplot::specify_decimal_p(
        x = as.data.frame(jmv_chi$chiSq)[[4]],
        k,
        p.value = TRUE
      ),
      # select Cramer's V as effect size
      cramer = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_chi$nom)[[4]], k),
      LL = ggstatsplot::specify_decimal_p(x = cramer_ci$conf.low[[1]], k),
      UL = ggstatsplot::specify_decimal_p(x = cramer_ci$conf.high[[1]], k),
      n = as.data.frame(jmv_chi$chiSq)$`value[N]`[[1]]
    )
  )

  # return the subtitle
  return(results_subtitle)
}


# custom function to write results from chi-square test into subtitle for the plot
# jmv_chi stands for the chi-square object from paired contindency table analyses
# effect is the text label that needs to be entered to denote which interaction effect
# is being investigated in

mcnemar_subtitle <- function(jmv_chi,
                             effect = NULL,
                             k = 3) {
  # preparing the subtitle
  results_subtitle <- base::substitute(
    expr =
      paste(
        y,
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
        italic("n"),
        " = ",
        n
      ),
    env = base::list(
      y = effect,
      estimate = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_chi$test)$`value[mcn]`[[1]], k),
      df = as.data.frame(jmv_chi$test)$`df[mcn]`[[1]],
      # df always an integer
      pvalue = ggstatsplot::specify_decimal_p(
        x = as.data.frame(jmv_chi$test)$`p[mcn]`[[1]],
        k,
        p.value = TRUE
      ),
      n = as.data.frame(jmv_chi$test)$`value[n]`[[1]]
    )
  )

  # return the subtitle
  return(results_subtitle)
}


# adding bayes factor message for null hypothesis for t-test (one-sample,
# independent, or paired)

bf_message_ttest <- function(jmv_results,
                             bf.prior) {

  # prepare the bayes factor message
  bf_message <- base::substitute(
    expr =
      paste(
        "In favor of null: ",
        "log"["e"],
        "(BF"["01"],
        ") = ",
        bf,
        ", log"["e"],
        "(error) = ",
        bf_error,
        "%, Prior width = ",
        bf_prior
      ),
    env = base::list(
      bf = ggstatsplot::specify_decimal_p(x = log(
        x = (1 / as.data.frame(jmv_results$ttest)$`stat[bf]`),
        base = exp(1)
      ), k = 1),
      bf_error = ggstatsplot::specify_decimal_p(x = log(
        x = (1 / as.data.frame(jmv_results$ttest)$`err[bf]`),
        base = exp(1)
      ), k = 1),
      bf_prior = ggstatsplot::specify_decimal_p(x = bf.prior, k = 3)
    )
  )

  # return the message
  return(bf_message)
}
