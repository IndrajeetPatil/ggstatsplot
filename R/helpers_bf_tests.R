#' @title Convenience function to extract bayes factors from `BayesFactor` model
#'   object.
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
#'   BayesFactor::anovaBF(Sepal.Length ~ Species,
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
#' @importFrom BayesFactor correlationBF extractBF
#'
#' @seealso \code{\link{bf_contingency_tab}}, \code{\link{bf_oneway_anova}},
#' \code{\link{bf_two_sample_ttest}}
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
#' @importFrom BayesFactor contingencyTableBF extractBF
#'
#' @seealso \code{\link{bf_corr_test}}, \code{\link{bf_oneway_anova}},
#' \code{\link{bf_two_sample_ttest}}
#'
#' @examples
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
#' @export

# function body
bf_contingency_tab <- function(data,
                               main,
                               condition,
                               sampling.plan = "indepMulti",
                               fixed.margin = "rows",
                               prior.concentration = 1,
                               caption = NULL,
                               output = "null",
                               k = 2,
                               ...) {

  # ============================ data preparation ==========================

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(main),
      y = !!rlang::enquo(condition)
    ) %>%
    tidyr::drop_na(data = .) %>%
    dplyr::mutate(
      .data = .,
      x = droplevels(as.factor(x)), y = droplevels(as.factor(y))
    ) %>%
    tibble::as_tibble(.)

  # ========================= subtitle preparation ==========================

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
        x = table(data$x, data$y),
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

  # prepare the bayes factor message
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

  # ============================ return ==================================

  # return the text results or the dataframe with results
  return(switch(
    EXPR = output,
    "results" = bf_results,
    bf_message
  ))
}

#' @title Bayesian one-sample *t*-test.
#' @name bf_one_sample_ttest
#' @author Indrajeet Patil
#'
#' @inheritParams BayesFactor::ttestBF
#' @inheritParams gghistostats
#' @inheritParams bf_corr_test
#'
#' @importFrom BayesFactor ttestBF extractBF
#'
#' @seealso \code{\link{bf_contingency_tab}}, \code{\link{bf_oneway_anova}},
#' \code{\link{bf_two_sample_ttest}}
#'
#' @examples
#'
#' # to get caption (default)
#' bf_one_sample_ttest(
#'   data = iris,
#'   x = Sepal.Length,
#'   test.value = 5.85,
#'   bf.prior = 0.8,
#'   output = "caption", k = 2
#' )
#'
#' # to get results dataframe
#' bf_one_sample_ttest(
#'   data = iris,
#'   x = Sepal.Length,
#'   test.value = 5.85,
#'   bf.prior = 0.8,
#'   output = "results"
#' )
#' @export

# function body
bf_one_sample_ttest <- function(data = NULL,
                                x,
                                test.value = 0,
                                bf.prior = 0.707,
                                caption = NULL,
                                output = "null",
                                k = 2,
                                ...) {

  # ================================= dataframe =============================

  # preparing a dataframe out of provided inputs
  if (!is.null(data)) {
    # if dataframe is provided
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(x)
      )
  } else {
    # if vectors are provided
    data <-
      cbind.data.frame(x = x)
  }

  # convert to a tibble
  data %<>%
    tibble::as_tibble(x = .)

  # ========================= subtitle preparation ==========================

  # extracting results from Bayesian test and creating a dataframe
  bf_results <-
    bf_extractor(
      BayesFactor::ttestBF(
        x = data$x,
        rscale = bf.prior,
        mu = test.value,
        nullInterval = NULL,
        ...
      )
    ) %>%
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

#' @title Bayesian two-samples *t*-test.
#' @name bf_two_sample_ttest
#' @author Indrajeet Patil
#'
#' @importFrom BayesFactor ttestBF extractBF
#'
#' @inheritParams BayesFactor::ttestBF
#' @inheritParams ggbetweenstats
#' @inheritParams bf_corr_test
#'
#' @seealso \code{\link{bf_contingency_tab}}, \code{\link{bf_corr_test}},
#' \code{\link{bf_oneway_anova}}
#'
#' @examples
#'
#' # for reproducibility
#' set.seed(123)
#'
#' # to get caption (default)
#' bf_two_sample_ttest(
#'   data = mtcars,
#'   x = am,
#'   y = wt,
#'   paired = FALSE,
#'   bf.prior = 0.880
#' )
#'
#' # to see results
#' bf_two_sample_ttest(
#'   data = mtcars,
#'   x = am,
#'   y = wt,
#'   paired = FALSE,
#'   output = "results"
#' )
#'
#' # for paired sample test
#' bf_two_sample_ttest(
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
#' @export

# function body
bf_two_sample_ttest <- function(data,
                                x,
                                y,
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
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
    tibble::as_tibble(.)

  # -------------------------- between-subjects design -------------------

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

    # extracting results from bayesian test and creating a dataframe
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

  # extracting the bayes factors
  bf_results <- bf_extractor(bf.object = bf_object) %>%
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

#' @title Bayesian one-way analysis of variance.
#' @name bf_oneway_anova
#' @author Indrajeet Patil
#'
#' @importFrom BayesFactor anovaBF extractBF
#'
#' @inheritParams BayesFactor::anovaBF
#' @inheritParams ggbetweenstats
#' @inheritParams bf_corr_test
#' @inheritParams pairwise_p
#' @param ... Additional arguments.
#'
#' @seealso \code{\link{bf_contingency_tab}}, \code{\link{bf_corr_test}},
#' \code{\link{bf_two_sample_ttest}}
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
