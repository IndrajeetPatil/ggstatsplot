#' @title Extract Bayes Factors from `BayesFactor` model object.
#' @name bf_extractor
#'
#' @param bf.object An object from `BayesFactor` package test results.
#' @param ... Currently ignored.
#'
#' @importFrom BayesFactor extractBF
#' @importFrom groupedstats grouped_summary
#' @importFrom tibble as_tibble tribble enframe
#' @importFrom dplyr rename select mutate everything bind_cols
#'
#' @examples
#' # getting only bayes factors
#' ggstatsplot::bf_extractor(
#'   BayesFactor::anovaBF(
#'     formula = Sepal.Length ~ Species,
#'     data = iris,
#'     progress = FALSE
#'   )
#' )
#' @export

# function body
bf_extractor <- function(bf.object,
                         ...) {
  ellipsis::check_dots_used()

  # preparing the dataframe
  bf_df <-
    BayesFactor::extractBF(
      x = bf.object,
      logbf = FALSE,
      onlybf = FALSE
    ) %>%
    tibble::as_tibble(.) %>%
    dplyr::select(.data = ., -time, -code) %>%
    dplyr::rename(.data = ., bf10 = bf) %>%
    dplyr::mutate(
      .data = .,
      bf01 = 1 / bf10,
      log_e_bf10 = log(bf10),
      log_e_bf01 = log(bf01),
      log_10_bf10 = log10(bf10),
      log_10_bf01 = log10(bf01)
    ) %>%
    dplyr::select(
      .data = .,
      bf10,
      log_e_bf10,
      log_10_bf10,
      bf01,
      log_e_bf01,
      log_10_bf01,
      dplyr::everything()
    )

  # return the dataframe with bayes factors
  return(bf_df)
}

#' @title Prepare caption with bayes factor in favor of null
#' @name bf_caption_maker
#' @description Convenience function to write a caption message with bayes
#'   factors in favor of the null hypothesis.
#'
#' @param bf.df A dataframe containing two columns `log_e_bf01` (for evidence in
#'   favor of null hypothesis) and `bf.prior`. If dataframe contains more than
#'   two rows, only the first row will be used.
#' @param caption Text to display as caption (will be displayed on top of the
#'   bayes factor caption/message).
#' @param output Can either be `"null"` (or `"caption"` or `"H0"`, which will
#'   contain text for evidence in favor of the null hypothesis or H0)  or
#'   `"alternative"` (or `"title"` or `"H1"`) or `"results"`, which will return
#'   a dataframe with results all the details).
#' @param ... Additional arguments (ignored).
#' @inheritParams ggbetweenstats
#'
#' @examples
#'
#' set.seed(123)
#'
#' # dataframe containing results
#' bf_results <-
#'   ggstatsplot::bf_extractor(BayesFactor::correlationBF(
#'     x = iris$Sepal.Length,
#'     y = iris$Petal.Length
#'   )) %>%
#'   dplyr::mutate(.data = ., bf.prior = 0.707)
#'
#' # creating caption (for null)
#' ggstatsplot::bf_caption_maker(
#'   bf.df = bf_results,
#'   output = "null",
#'   k = 3,
#'   caption = "Note: Iris dataset"
#' )
#'
#' # creating caption (for alternative)
#' ggstatsplot::bf_caption_maker(
#'   bf.df = bf_results,
#'   output = "alternative"
#' )
#' @export

# function body
bf_caption_maker <- function(bf.df,
                             k = 2,
                             output = "null",
                             caption = NULL,
                             ...) {
  ellipsis::check_dots_used()

  # changing aspects of the caption based on what output is needed
  if (output %in% c("null", "caption", "H0", "h0")) {
    hypothesis.text <- "In favor of null: "
    bf.value <- bf.df$log_e_bf01[[1]]
    bf.subscript <- "01"
  } else {
    hypothesis.text <- "In favor of alternative: "
    bf.value <- -bf.df$log_e_bf01[[1]]
    bf.subscript <- "10"
  }

  # prepare the bayes factor message
  bf_text <-
    substitute(
      atop(displaystyle(top.text),
        expr =
          paste(
            hypothesis.text,
            "log"["e"],
            "(BF"[bf.subscript],
            ") = ",
            bf,
            ", ",
            italic("r")["Cauchy"],
            " = ",
            bf_prior
          )
      ),
      env = list(
        hypothesis.text = hypothesis.text,
        top.text = caption,
        bf.subscript = bf.subscript,
        bf = specify_decimal_p(x = bf.value, k = k),
        bf_prior = specify_decimal_p(x = bf.df$bf.prior[[1]], k = k)
      )
    )

  # return the caption
  return(bf_text)
}

#' @title Bayesian correlation test.
#' @name bf_corr_test
#' @author Indrajeet Patil
#'
#' @inheritParams BayesFactor::correlationBF
#' @inheritParams bf_caption_maker
#' @inheritParams ggscatterstats
#' @param bf.prior A number between 0.5 and 2 (default `0.707`), the prior width
#'   to use in calculating Bayes factors.
#'
#' @importFrom BayesFactor correlationBF
#'
#' @seealso \code{\link{bf_contingency_tab}}, \code{\link{bf_oneway_anova}},
#' \code{\link{bf_ttest}}
#'
#' @examples
#'
#' # for reproducibility
#' set.seed(123)
#'
#' # to get caption (default)
#' bf_corr_test(
#'   data = anscombe,
#'   x = x1,
#'   y = y4,
#'   bf.prior = 1
#' )
#'
#' # to see results
#' bf_corr_test(
#'   data = anscombe,
#'   x = x1,
#'   y = y4,
#'   bf.prior = 0.8,
#'   output = "results"
#' )
#' @export

# function body
bf_corr_test <- function(data,
                         x,
                         y,
                         bf.prior = 0.707,
                         caption = NULL,
                         output = "null",
                         k = 2,
                         ...) {

  # ============================ data preparation ==========================

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    ) %>%
    tidyr::drop_na(data = .) %>%
    tibble::as_tibble(.)

  # ========================= subtitle preparation ==========================

  # extracting results from bayesian test and creating a dataframe
  bf_results <-
    bf_extractor(
      BayesFactor::correlationBF(
        x = data$x,
        y = data$y,
        nullInterval = NULL,
        rscale = bf.prior,
        ...
      )
    ) %>% # adding prior width column
    dplyr::mutate(.data = ., bf.prior = bf.prior)

  # prepare the bayes factor message
  if (output != "results") {
    bf_message <-
      bf_caption_maker(
        bf.df = bf_results,
        output = output,
        k = k,
        caption = caption
      )
  }

  # ============================ return ==================================

  # return the text results or the dataframe with results
  return(switch(
    EXPR = output,
    "results" = bf_results,
    bf_message
  ))
}


#' @title Bayesian contingency table analysis.
#' @name bf_contingency_tab
#' @author Indrajeet Patil
#'
#' @inheritParams BayesFactor::contingencyTableBF
#' @inheritParams subtitle_contingency_tab
#' @inheritParams bf_corr_test
#' @param sampling.plan Character describing the sampling plan. Possible options
#'   are `"indepMulti"` (independent multinomial; default), `"poisson"`,
#'   `"jointMulti"` (joint multinomial), `"hypergeom"` (hypergeometric). For
#'   more, see `?BayesFactor::contingencyTableBF()`.
#' @param fixed.margin For the independent multinomial sampling plan, which
#'   margin is fixed (`"rows"` or `"cols"`). Defaults to `"rows"`.
#' @param prior.concentration Specifies the prior concentration parameter, set
#'   to `1` by default. It indexes the expected deviation from the null
#'   hypothesis under the alternative, and corresponds to Gunel and Dickey's
#'   (1974) `"a"` parameter.
#'
#' @importFrom BayesFactor contingencyTableBF logMeanExpLogs
#' @importFrom stats dmultinom
#' @importFrom MCMCpack rdirichlet
#'
#' @seealso \code{\link{bf_corr_test}}, \code{\link{bf_oneway_anova}},
#' \code{\link{bf_ttest}}
#'
#' @note Bayes Factor for goodness of fit test is based on gist provided by
#'   Richard Morey:
#'   \url{https://gist.github.com/richarddmorey/a4cd3a2051f373db917550d67131dba4}.
#'
#' @examples
#'
#' # ------------------ association tests --------------------------------
#'
#' # for reproducibility
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # to get caption (in favor of null)
#' bf_contingency_tab(
#'   data = mtcars,
#'   main = am,
#'   condition = cyl,
#'   fixed.margin = "cols"
#' )
#'
#' # to get caption (in favor of alternative)
#' bf_contingency_tab(
#'   data = mtcars,
#'   main = am,
#'   condition = cyl,
#'   fixed.margin = "rows",
#'   output = "alternative"
#' )
#'
#' # to see results
#' bf_contingency_tab(
#'   data = mtcars,
#'   main = am,
#'   condition = cyl,
#'   sampling.plan = "jointMulti",
#'   fixed.margin = "rows",
#'   prior.concentration = 1
#' )
#'
#' # ------------------ goodness of fit tests --------------------------------
#'
#' bf_contingency_tab(
#'   data = mtcars,
#'   main = am,
#'   prior.concentration = 10
#' )
#' @export

# function body
bf_contingency_tab <- function(data,
                               main,
                               condition = NULL,
                               counts = NULL,
                               ratio = NULL,
                               sampling.plan = "indepMulti",
                               fixed.margin = "rows",
                               prior.concentration = 1,
                               caption = NULL,
                               output = "null",
                               k = 2,
                               ...) {
  ellipsis::check_dots_used()

  # =============================== dataframe ================================

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      main = !!rlang::enquo(main),
      condition = !!rlang::enquo(condition),
      counts = !!rlang::enquo(counts)
    ) %>%
    tidyr::drop_na(data = .) %>%
    tibble::as_tibble(x = .)

  # =========================== converting counts ============================

  # untable the dataframe based on the count for each obervation
  if ("counts" %in% names(data)) {
    data %<>%
      tidyr::uncount(
        data = .,
        weights = counts,
        .remove = TRUE,
        .id = "id"
      )
  }

  # main and condition need to be a factor for this analysis
  # also drop the unused levels of the factors

  # main
  data %<>%
    dplyr::mutate(.data = ., main = droplevels(as.factor(main)))

  # ratio
  if (is.null(ratio)) {
    ratio <- rep(1 / length(table(data$main)), length(table(data$main)))
  }

  # ========================= caption preparation ==========================

  if ("condition" %in% names(data)) {

    # dropping unused levels
    data %<>%
      dplyr::mutate(.data = ., condition = droplevels(as.factor(condition)))

    # detailed text of sample plan
    sampling_plan_text <-
      switch(
        EXPR = sampling.plan,
        "jointMulti" = "joint multinomial",
        "poisson" = "poisson",
        "indepMulti" = "independent multinomial",
        "hypergeom" = "hypergeometric"
      )

    # extracting results from bayesian test and creating a dataframe
    bf_results <-
      bf_extractor(
        BayesFactor::contingencyTableBF(
          x = table(data$main, data$condition),
          sampleType = sampling.plan,
          fixedMargin = fixed.margin,
          priorConcentration = prior.concentration,
          ...
        )
      ) %>%
      dplyr::mutate(
        .data = .,
        sampling.plan = sampling_plan_text,
        fixed.margin = fixed.margin,
        prior.concentration = prior.concentration
      )
  } else {
    # no. of levels in `main` variable
    n_levels <- length(as.vector(table(data$main)))

    if (1 / n_levels == 0 || 1 / n_levels == 1) {
      return(NULL)
    }

    # one sample goodness of fit test for equal proportions
    y <- as.matrix(table(data$main))

    # (log) prob of data under null
    pr_y_h0 <- stats::dmultinom(x = y, prob = ratio, log = TRUE)

    # estimate log prob of data under null with Monte Carlo
    M <- 100000
    p1s <- MCMCpack::rdirichlet(n = M, alpha = prior.concentration * ratio)
    tmp_pr_h1 <-
      sapply(
        X = 1:M,
        FUN = function(i)
          stats::dmultinom(x = y, prob = p1s[i, ], log = TRUE)
      )

    # estimate log prob of data under alternative
    pr_y_h1 <- BayesFactor::logMeanExpLogs(v = tmp_pr_h1)

    # computing Bayes Factor
    bf_10 <- exp(pr_y_h1 - pr_y_h0)

    # dataframe with results
    bf_results <- tibble::enframe(bf_10) %>%
      dplyr::select(.data = ., bf10 = value) %>%
      dplyr::mutate(
        .data = .,
        bf01 = 1 / bf10,
        log_e_bf10 = log(bf10),
        log_e_bf01 = log(bf01),
        log_10_bf10 = log10(bf10),
        log_10_bf01 = log10(bf01)
      ) %>%
      dplyr::select(
        .data = .,
        bf10,
        log_e_bf10,
        log_10_bf10,
        bf01,
        log_e_bf01,
        log_10_bf01,
        dplyr::everything()
      ) %>%
      dplyr::mutate(
        .data = .,
        prior.concentration = prior.concentration
      )
  }

  # changing aspects of the caption based on what output is needed
  if (output %in% c("null", "caption", "H0", "h0")) {
    hypothesis.text <- "In favor of null: "
    bf.value <- bf_results$log_e_bf01[[1]]
    bf.subscript <- "01"
  } else {
    hypothesis.text <- "In favor of alternative: "
    bf.value <- -bf_results$log_e_bf01[[1]]
    bf.subscript <- "10"
  }

  # prepare the Bayes Factor message
  if ("condition" %in% names(data)) {
    bf_message <-
      substitute(
        atop(
          displaystyle(top.text),
          expr =
            paste(
              hypothesis.text,
              "log"["e"],
              "(BF"[bf.subscript],
              ") = ",
              bf,
              ", sampling = ",
              sampling.plan,
              ", ",
              italic("a"),
              " = ",
              a
            )
        ),
        env = list(
          hypothesis.text = hypothesis.text,
          top.text = caption,
          bf.subscript = bf.subscript,
          bf = specify_decimal_p(x = bf.value, k = k),
          sampling.plan = sampling_plan_text,
          a = specify_decimal_p(x = bf_results$prior.concentration[[1]], k = k)
        )
      )
  } else {
    bf_message <-
      substitute(
        atop(
          displaystyle(top.text),
          expr =
            paste(
              hypothesis.text,
              "log"["e"],
              "(BF"[bf.subscript],
              ") = ",
              bf,
              ", ",
              italic("a"),
              " = ",
              a
            )
        ),
        env = list(
          hypothesis.text = hypothesis.text,
          top.text = caption,
          bf.subscript = bf.subscript,
          bf = specify_decimal_p(x = bf.value, k = k),
          a = specify_decimal_p(x = bf_results$prior.concentration[[1]], k = k)
        )
      )
  }

  # ============================ return ==================================

  # return the text results or the dataframe with results
  return(switch(
    EXPR = output,
    "results" = bf_results,
    bf_message
  ))
}

#' @title Bayes Factor for *t*-test
#' @author Indrajeet Patil
#' @details If `y` is `NULL`, a one-sample *t*-test will be carried out,
#'   otherwise a two-sample *t*-test will be carried out.
#'
#' @importFrom BayesFactor ttestBF
#'
#' @param x Either the grouping variable from the dataframe `data` if it's a
#'   two-sample *t*-test or a numeric variable if it's a one-sample *t*-test.
#' @inheritParams ggbetweenstats
#' @inheritParams BayesFactor::ttestBF
#' @inheritParams bf_corr_test
#' @inheritParams subtitle_t_onesample
#'
#' @seealso \code{\link{bf_contingency_tab}}, \code{\link{bf_corr_test}},
#' \code{\link{bf_oneway_anova}}
#'
#' @examples
#'
#' # ------------------- two-samples tests -----------------------------------
#'
#' # for reproducibility
#' set.seed(123)
#'
#' # to get caption (default)
#' bf_ttest(
#'   data = mtcars,
#'   x = am,
#'   y = wt,
#'   paired = FALSE,
#'   bf.prior = 0.880
#' )
#'
#' # to see results
#' bf_ttest(
#'   data = mtcars,
#'   x = am,
#'   y = wt,
#'   paired = FALSE,
#'   output = "results"
#' )
#'
#' # for paired sample test
#' bf_ttest(
#'   data = dplyr::filter(
#'     ggstatsplot::intent_morality,
#'     condition %in% c("accidental", "attempted"),
#'     harm == "Poisoning"
#'   ),
#'   x = condition,
#'   y = rating,
#'   paired = TRUE,
#'   output = "results"
#' )
#'
#' # ------------------- one-samples test -----------------------------------
#'
#' # to get caption (default)
#' bf_ttest(
#'   data = iris,
#'   x = Sepal.Length,
#'   test.value = 5.85,
#'   bf.prior = 0.8,
#'   output = "caption", k = 2
#' )
#'
#' # to get results dataframe
#' bf_ttest(
#'   data = iris,
#'   x = Sepal.Length,
#'   test.value = 5.85,
#'   bf.prior = 0.8,
#'   output = "results"
#' )
#' @export

# function body
bf_ttest <- function(data,
                     x,
                     y = NULL,
                     test.value = 0,
                     paired = FALSE,
                     bf.prior = 0.707,
                     caption = NULL,
                     output = "null",
                     k = 2,
                     ...) {

  # ============================ data preparation ==========================

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    ) %>%
    tibble::as_tibble(.)

  # -------------------------- between-subjects design -------------------

  if ("y" %in% names(data)) {

    # dropping unused factor levels from `x` variable
    data %<>%
      dplyr::mutate(.data = ., x = droplevels(as.factor(x)))

    # running bayesian analysis
    if (!isTRUE(paired)) {

      # removing NAs
      data %<>%
        stats::na.omit(.)

      # extracting results from bayesian test and creating a dataframe
      bf_object <-
        BayesFactor::ttestBF(
          formula = y ~ x,
          data = as.data.frame(data),
          rscale = bf.prior,
          paired = FALSE,
          progress = FALSE,
          ...
        )
    } else {
      # the data needs to be in wide format
      data_wide <-
        long_to_wide_converter(
          data = data,
          x = x,
          y = y
        )

      # change names for convenience
      colnames(data_wide) <- c("rowid", "col1", "col2")

      # extracting results from Bayesian test and creating a dataframe
      bf_object <-
        BayesFactor::ttestBF(
          x = data_wide$col1,
          y = data_wide$col2,
          rscale = bf.prior,
          paired = TRUE,
          progress = FALSE,
          ...
        )
    }
  } else {
    bf_object <-
      BayesFactor::ttestBF(
        x = data$x,
        rscale = bf.prior,
        mu = test.value,
        nullInterval = NULL,
        ...
      )
  }

  # extracting the Bayes factors
  bf_results <- bf_extractor(bf.object = bf_object) %>%
    dplyr::mutate(.data = ., bf.prior = bf.prior)

  # prepare the Bayes factor message
  if (output != "results") {
    bf_message <-
      bf_caption_maker(
        bf.df = bf_results,
        output = output,
        k = k,
        caption = caption
      )
  }

  # ============================ return ==================================

  # return the text results or the dataframe with results
  return(switch(
    EXPR = output,
    "results" = bf_results,
    bf_message
  ))
}

#' @rdname bf_ttest
#' @aliases bf_ttest
#' @export

bf_one_sample_ttest <- bf_ttest

#' @rdname bf_ttest
#' @aliases bf_ttest
#' @export

bf_two_sample_ttest <- bf_ttest

#' @title Bayesian one-way analysis of variance.
#' @name bf_oneway_anova
#' @author Indrajeet Patil
#'
#' @importFrom BayesFactor anovaBF
#'
#' @inheritParams BayesFactor::anovaBF
#' @inheritParams ggbetweenstats
#' @inheritParams bf_corr_test
#' @inheritParams pairwise_p
#' @param ... Additional arguments.
#'
#' @seealso \code{\link{bf_contingency_tab}}, \code{\link{bf_corr_test}},
#' \code{\link{bf_ttest}}
#'
#' @examples
#'
#' # to get caption (default)
#' bf_oneway_anova(
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   bf.prior = 0.8
#' )
#'
#' # to get results dataframe
#' bf_oneway_anova(
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   bf.prior = 0.8,
#'   output = "results"
#' )
#' @export

# function body
bf_oneway_anova <- function(data,
                            x,
                            y,
                            bf.prior = 0.707,
                            caption = NULL,
                            output = "null",
                            paired = FALSE,
                            k = 2,
                            ...) {

  # ============================ data preparation ==========================

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    ) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
    tibble::as_tibble(.)

  # ========================= subtitle preparation ==========================

  if (isTRUE(paired)) {
    # converting to long format and then getting it back in wide so that the
    # rowid variable can be used as the block variable
    data <-
      long_to_wide_converter(
        data = data,
        x = x,
        y = y
      ) %>%
      tidyr::gather(data = ., key, value, -rowid) %>%
      dplyr::arrange(.data = ., rowid) %>%
      dplyr::mutate(.data = ., rowid = as.factor(rowid), key = as.factor(key))

    # extracting results from bayesian test and creating a dataframe
    bf_results <-
      bf_extractor(BayesFactor::anovaBF(
        value ~ key + rowid,
        data = as.data.frame(data),
        whichRandom = "rowid",
        rscaleFixed = bf.prior,
        progress = FALSE,
        rscaleRandom = 1,
        ...
      )) %>%
      dplyr::mutate(.data = ., bf.prior = bf.prior)
  } else {

    # remove NAs listwise for between-subjects design
    data %<>%
      tidyr::drop_na(data = .)

    # extracting results from bayesian test and creating a dataframe
    bf_results <-
      bf_extractor(
        BayesFactor::anovaBF(
          formula = y ~ x,
          data = as.data.frame(data),
          rscaleFixed = bf.prior,
          progress = FALSE,
          ...
        )
      ) %>%
      dplyr::mutate(.data = ., bf.prior = bf.prior)
  }

  # prepare the bayes factor message
  if (output != "results") {
    bf_message <-
      bf_caption_maker(
        bf.df = bf_results,
        output = output,
        k = k,
        caption = caption
      )
  }

  # ============================ return ==================================

  # return the text results or the dataframe with results
  return(switch(
    EXPR = output,
    "results" = bf_results,
    bf_message
  ))
}
