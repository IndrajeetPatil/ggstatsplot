#' @title Convenience function to extract bayes factors and details about
#'   posterior (mean, median, HDI, etc.) from `BayesFactor` model object.
#' @name bf_extractor
#'
#' @param bf.object An object from `BayesFactor` package test results.
#' @param posterior If `TRUE`, the dataframe will contain mean, median, standard
#'   deviation, and standard error for the posterior.
#' @param iterations The number of iterations to sample for computing posterior
#'   (default: `1000`).
#' @param cred.int A scalar between 0 and 1, indicating the mass within the
#'   credible interval that is to be estimated (default: `0.95`).
#'
#' @importFrom BayesFactor extractBF posterior
#' @importFrom sjstats hdi
#' @importFrom groupedstats grouped_summary
#' @importFrom tibble as_tibble tribble enframe
#' @importFrom dplyr rename select mutate everything bind_cols
#'
#' @examples
#'
#' # getting only bayes factors
#' ggstatsplot::bf_extractor(BayesFactor::anovaBF(Sepal.Length ~ Species,
#'   data = iris,
#'   progress = FALSE
#' ))
#' \dontrun{
#' # show all columns in a tibble
#' options(tibble.width = Inf)
#'
#' # getting bayes factors and posteriors
#' ggstatsplot::bf_extractor(
#'   BayesFactor::correlationBF(
#'     x = iris$Sepal.Length,
#'     y = iris$Sepal.Width
#'   ),
#'   posterior = TRUE,
#'   iterations = 1000,
#'   cred.int = 0.95
#' )
#' }
#' @export

# function body
bf_extractor <- function(bf.object,
                         posterior = FALSE,
                         iterations = 1000,
                         cred.int = 0.95) {

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
      log_e_bf10 = log(x = bf10, base = exp(1)),
      log_e_bf01 = log(x = bf01, base = exp(1)),
      log_10_bf10 = log10(x = bf10),
      log_10_bf01 = log10(x = bf01)
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

  if (isTRUE(posterior)) {

    # a vector posterior samples
    posterior_samples <-
      as.vector(suppressMessages(
        BayesFactor::posterior(
          model = bf.object,
          iterations = iterations,
          progress = FALSE
        )
      ))

    # dataframe with posteriors
    posterior_df <-
      posterior_samples %>%
      tibble::enframe(x = .) %>%
      dplyr::select(.data = ., -name) %>%
      dplyr::mutate(.data = ., group = "1") %>%
      groupedstats::grouped_summary(
        data = .,
        grouping.vars = group,
        measures = value
      ) %>%
      dplyr::select(
        .data = .,
        posterior.mean = mean,
        posterior.median = median,
        posterior.sd = sd,
        posterior.std.error = std.error
      )

    # computing HDI for posteriors
    hdi_df <- sjstats::hdi(x = posterior_samples, prob = cred.int)

    # creating a dataframe out of it
    hdi_df <- tibble::tribble(
      ~HDI.low, ~HDI.high, ~cred.int,
      hdi_df[1], hdi_df[2], cred.int
    )

    # combined dataframe with bayes factors and posterior
    bf_df <- dplyr::bind_cols(bf_df, posterior_df, hdi_df)
  }

  # return the dataframe with bayes factors
  return(bf_df)
}

#' @title Prepare caption with bayes factor in favor of null
#' @name bf_caption_maker
#' @description Convenience function to write a caption message with bayes
#'   factors in favor of the null hypothesis.
#'
#' @param bf.df A dataframe containing two columns `log_e_bf01` and `bf.prior`.
#'   If dataframe contains more than two rows, only the first row will be used.
#' @param caption Text to display as caption (will be displayed on top of the
#'   bayes factor caption/message).
#' @inheritParams ggbetweenstats
#'
#' @examples
#'
#' set.seed(123)
#'
#' # dataframe containing results
#' bf_results <-
#'   bf_extractor(BayesFactor::correlationBF(
#'     x = iris$Sepal.Length,
#'     y = iris$Petal.Length
#'   )) %>%
#'   dplyr::mutate(.data = ., bf.prior = 0.707)
#'
#' # creating caption
#' ggstatsplot::bf_caption_maker(
#'   bf.df = bf_results,
#'   k = 3,
#'   caption = "Note: Iris dataset"
#' )
#' @export

# function body
bf_caption_maker <- function(bf.df,
                             k = 2,
                             caption = NULL) {

  # prepare the bayes factor message
  bf_caption <-
    base::substitute(
      atop(displaystyle(top.text),
        expr =
          paste(
            "In favor of null: ",
            "log"["e"],
            "(BF"["01"],
            ") = ",
            bf,
            ", Prior width = ",
            bf_prior
          )
      ),
      env = base::list(
        top.text = caption,
        bf = specify_decimal_p(x = bf.df$log_e_bf01[[1]], k = k),
        bf_prior = specify_decimal_p(x = bf.df$bf.prior[[1]], k = k)
      )
    )

  # return the caption
  return(bf_caption)
}

#' @title Bayesian correlation test.
#' @name bf_corr_test
#' @aliases bf_corr_test
#' @author Indrajeet Patil
#'
#' @inheritParams ggscatterstats
#' @param bf.prior A number between 0.5 and 2 (default `0.707`), the prior width
#'   to use in calculating Bayes factors.
#' @param output Can either be `"caption"` (which will contain text for evidence
#'   in favor of null)  or `"results"` (which will return the dataframe with
#'   results).
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
                         output = "caption",
                         k = 2) {

  # ============================ data preparation ==========================

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    ) %>%
    dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
    tibble::as_tibble(.)

  # ========================= subtitle preparation ==========================

  # extracting results from bayesian test and creating a dataframe
  bf_results <-
    bf_extractor(bf.object = BayesFactor::correlationBF(
      x = data$x,
      y = data$y,
      nullInterval = NULL,
      rscale = bf.prior
    )) %>% # adding prior width column
    dplyr::mutate(.data = ., bf.prior = bf.prior)

  # prepare the bayes factor message
  bf_message <-
    bf_caption_maker(
      bf.df = bf_results,
      k = k,
      caption = caption
    )

  # ============================ return ==================================

  # return the text results or the dataframe with results
  if (output == "caption") {
    return(bf_message)
  } else if (output == "results") {
    return(bf_results)
  }
}


#' @title Bayesian contingency table analysis.
#' @name bf_contingency_tab
#' @aliases bf_contingency_tab
#' @author Indrajeet Patil
#'
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
#'
#' # to get caption (default)
#' bf_contingency_tab(
#'   data = mtcars,
#'   main = am,
#'   condition = cyl,
#'   fixed.margin = "cols"
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
                               output = "caption",
                               k = 2) {

  # ============================ data preparation ==========================

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(main),
      y = !!rlang::enquo(condition)
    ) %>%
    dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
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
    bf_extractor(bf.object = BayesFactor::contingencyTableBF(
      x = table(data$x, data$y),
      sampleType = sampling.plan,
      fixedMargin = fixed.margin,
      priorConcentration = prior.concentration
    )) %>%
    dplyr::mutate(
      .data = .,
      sampling.plan = sampling_plan_text,
      fixed.margin = fixed.margin,
      prior.concentration = prior.concentration
    )

  # prepare the bayes factor message
  bf_message <-
    base::substitute(
      atop(
        displaystyle(top.text),
        expr =
          paste(
            "In favor of null: ",
            "log"["e"],
            "(BF"["01"],
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
      env = base::list(
        top.text = caption,
        bf = specify_decimal_p(x = bf_results$log_e_bf01[[1]], k = k),
        sampling.plan = sampling_plan_text,
        a = specify_decimal_p(x = bf_results$prior.concentration[[1]], k = k)
      )
    )

  # ============================ return ==================================

  # return the text results or the dataframe with results
  if (output == "caption") {
    return(bf_message)
  } else if (output == "results") {
    return(bf_results)
  }
}


#' @title Bayesian two-samples *t*-test.
#' @name bf_two_sample_ttest
#' @aliases bf_two_sample_ttest
#' @author Indrajeet Patil
#'
#' @importFrom BayesFactor ttestBF extractBF
#'
#' @inheritParams ggbetweenstats
#' @inheritParams bf_corr_test
#' @inheritParams BayesFactor::ttestBF
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
                                output = "caption",
                                k = 2) {

  # ============================ data preparation ==========================

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
    )

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
        progress = FALSE
      )
  } else if (isTRUE(paired)) {
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
        progress = FALSE
      )
  }

  # extracting the bayes factors
  bf_results <- bf_extractor(
    bf.object = bf_object
  ) %>%
    dplyr::mutate(.data = ., bf.prior = bf.prior)

  # prepare the bayes factor message
  bf_message <-
    bf_caption_maker(
      bf.df = bf_results,
      k = k,
      caption = caption
    )

  # ============================ return ==================================

  # return the text results or the dataframe with results
  if (output == "caption") {
    return(bf_message)
  } else if (output == "results") {
    return(bf_results)
  }
}

#' @title Bayesian one-way analysis of variance.
#' @name bf_oneway_anova
#' @aliases bf_oneway_anova
#' @author Indrajeet Patil
#'
#' @importFrom BayesFactor anovaBF extractBF
#'
#' @inheritParams ggbetweenstats
#' @inheritParams bf_corr_test
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
                            output = "caption",
                            k = 2) {

  # ============================ data preparation ==========================

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    ) %>%
    stats::na.omit(.) %>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "x",
      .funs = ~ base::droplevels(x = base::as.factor(x = .))
    ) %>%
    tibble::as_tibble(.)

  # ========================= subtitle preparation ==========================

  # extracting results from bayesian test and creating a dataframe
  bf_results <-
    bf_extractor(bf.object = BayesFactor::anovaBF(
      formula = y ~ x,
      data = as.data.frame(data),
      rscaleFixed = bf.prior,
      progress = FALSE
    )) %>%
    dplyr::mutate(.data = ., bf.prior = bf.prior)

  # prepare the bayes factor message
  bf_message <-
    bf_caption_maker(
      bf.df = bf_results,
      k = k,
      caption = caption
    )

  # ============================ return ==================================

  # return the text results or the dataframe with results
  if (output == "caption") {
    return(bf_message)
  } else if (output == "results") {
    return(bf_results)
  }
}

#' @title Bayesian one-sample *t*-test.
#' @name bf_one_sample_ttest
#' @aliases bf_one_sample_ttest
#' @author Indrajeet Patil
#'
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
                                output = "caption",
                                k = 2) {

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
      base::cbind.data.frame(x = x)
  }

  # convert to a tibble
  data %<>%
    tibble::as_tibble(x = .)

  # ========================= subtitle preparation ==========================

  # extracting results from bayesian test and creating a dataframe
  bf_results <-
    bf_extractor(bf.object = BayesFactor::ttestBF(
      x = data$x,
      rscale = bf.prior,
      mu = test.value,
      nullInterval = NULL
    )) %>%
    dplyr::mutate(.data = ., bf.prior = bf.prior)

  # prepare the bayes factor message
  bf_message <-
    bf_caption_maker(
      bf.df = bf_results,
      k = k,
      caption = caption
    )

  # ============================ return ==================================

  # return the text results or the dataframe with results
  if (output == "caption") {
    return(bf_message)
  } else if (output == "results") {
    return(bf_results)
  }
}
