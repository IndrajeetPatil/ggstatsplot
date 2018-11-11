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
#' @seealso \code{\link{bf_contigency_tab}}, \code{\link{bf_oneway_anova}},
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
bf_corr_test <-
  function(data,
             x,
             y,
             bf.prior = 0.707,
             caption = NULL,
             output = "caption") {

    # ============================ data preparation ==========================

    # creating a dataframe
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(x),
        y = !!rlang::enquo(y)
      ) %>%
      dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
      tibble::as_data_frame(.)

    # ========================= subtitle preparation ==========================

    # extracting results from bayesian test and creating a dataframe
    bf_results <-
      BayesFactor::extractBF(
        x = BayesFactor::correlationBF(
          x = data$x,
          y = data$y,
          nullInterval = NULL,
          rscale = bf.prior
        ),
        logbf = FALSE,
        onlybf = FALSE
      ) %>% # converting to a tibble dataframe
      tibble::as_data_frame(.) %>% # removing unnecessary columns
      dplyr::select(.data = ., -time, -code) %>% # adding prior width column
      dplyr::mutate(.data = ., bf.prior = bf.prior)

    # prepare the bayes factor message
    bf_message <-
      base::substitute(
        atop(top.text,
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
          bf = ggstatsplot::specify_decimal_p(
            x = log(
              x = (1 / bf_results$bf[[1]]),
              base = exp(1)
            ),
            k = 1,
            p.value = FALSE
          ),
          bf_prior = ggstatsplot::specify_decimal_p(
            x = bf_results$bf.prior[[1]],
            k = 3,
            p.value = FALSE
          )
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


#' @title Bayesian contingency table analysis.
#' @name bf_contigency_tab
#' @aliases bf_contigency_tab
#' @author Indrajeet Patil
#'
#' @inheritParams subtitle_contigency_tab
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
#' bf_contigency_tab(
#'   data = mtcars,
#'   main = am,
#'   condition = cyl,
#'   fixed.margin = "cols"
#' )
#'
#' # to see results
#' bf_contigency_tab(
#'   data = mtcars,
#'   main = am,
#'   condition = cyl,
#'   sampling.plan = "jointMulti",
#'   fixed.margin = "rows",
#'   prior.concentration = 1
#' )
#' @export

# function body
bf_contigency_tab <-
  function(data,
             main,
             condition,
             sampling.plan = "indepMulti",
             fixed.margin = "rows",
             prior.concentration = 1,
             caption = NULL,
             output = "caption") {

    # ============================ data preparation ==========================

    # creating a dataframe
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(main),
        y = !!rlang::enquo(condition)
      ) %>%
      dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
      tibble::as_data_frame(.)

    # main and condition need to be a factor for this analysis
    # also drop the unused levels of the factors

    # main
    data %<>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "x",
        .funs = ~ base::droplevels(x = base::as.factor(x = .))
      )

    # condition
    data %<>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "y",
        .funs = ~ base::droplevels(x = base::as.factor(x = .))
      )

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
      BayesFactor::extractBF(
        BayesFactor::contingencyTableBF(
          x = table(data$x, data$y),
          sampleType = sampling.plan,
          fixedMargin = fixed.margin,
          priorConcentration = prior.concentration
        )
      ) %>%
      dplyr::select(.data = ., -time, -code) %>% # adding other columns
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
          bf = ggstatsplot::specify_decimal_p(
            x = log(
              x = (1 / bf_results$bf[[1]]),
              base = exp(1)
            ),
            k = 1,
            p.value = FALSE
          ),
          sampling.plan = sampling_plan_text,
          a = ggstatsplot::specify_decimal_p(
            x = bf_results$prior.concentration[[1]],
            k = 1,
            p.value = FALSE
          )
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


#' @title Bayesian t-test.
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
#' @seealso \code{\link{bf_contigency_tab}}, \code{\link{bf_corr_test}},
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
bf_two_sample_ttest <-
  function(data,
             x,
             y,
             paired = FALSE,
             bf.prior = 0.707,
             caption = NULL,
             output = "caption") {

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
      bf_results <-
        BayesFactor::extractBF(
          BayesFactor::ttestBF(
            formula = y ~ x,
            data = as.data.frame(data),
            rscale = bf.prior,
            paired = FALSE,
            progress = FALSE
          )
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
      bf_results <-
        BayesFactor::extractBF(
          BayesFactor::ttestBF(
            x = data_wide$col1,
            y = data_wide$col2,
            rscale = bf.prior,
            paired = TRUE,
            progress = FALSE
          )
        )
    }

    # cleaning the data further
    bf_results %<>% # converting to a tibble dataframe
      tibble::as_data_frame(.) %>% # removing unnecessary columns
      dplyr::select(.data = ., -time, -code) %>% # adding prior width column
      dplyr::mutate(.data = ., bf.prior = bf.prior)

    # prepare the bayes factor message
    bf_message <-
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
          bf = ggstatsplot::specify_decimal_p(
            x = log(
              x = (1 / bf_results$bf[[1]]),
              base = exp(1)
            ),
            k = 1,
            p.value = FALSE
          ),
          bf_prior = ggstatsplot::specify_decimal_p(
            x = bf_results$bf.prior[[1]],
            k = 3,
            p.value = FALSE
          )
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
#' @seealso \code{\link{bf_contigency_tab}}, \code{\link{bf_corr_test}},
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
bf_oneway_anova <-
  function(data,
             x,
             y,
             bf.prior = 0.707,
             caption = NULL,
             output = "caption") {

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
      tibble::as_data_frame(.)

    # ========================= subtitle preparation ==========================

    # extracting results from bayesian test and creating a dataframe
    bf_results <-
      BayesFactor::extractBF(
        BayesFactor::anovaBF(
          formula = y ~ x,
          data = as.data.frame(data),
          rscaleFixed = bf.prior,
          progress = FALSE
        )
      ) %>% # converting to a tibble dataframe
      tibble::as_data_frame(.) %>% # removing unnecessary columns
      dplyr::select(.data = ., -time, -code) %>% # adding prior width column
      dplyr::mutate(.data = ., bf.prior = bf.prior)

    # prepare the bayes factor message
    bf_message <-
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
          bf = ggstatsplot::specify_decimal_p(
            x = log(
              x = (1 / bf_results$bf[[1]]),
              base = exp(1)
            ),
            k = 1,
            p.value = FALSE
          ),
          bf_prior = ggstatsplot::specify_decimal_p(
            x = bf_results$bf.prior[[1]],
            k = 3,
            p.value = FALSE
          )
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
#' @seealso \code{\link{bf_contigency_tab}}, \code{\link{bf_oneway_anova}},
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
#'   output = "caption"
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
bf_one_sample_ttest <-
  function(data,
             x,
             test.value = 0,
             bf.prior = 0.707,
             caption = NULL,
             output = "caption") {

    # ================================= dataframe ==============================
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
      tibble::as_data_frame(x = .)


    # ========================= subtitle preparation ==========================

    # extracting results from bayesian test and creating a dataframe
    bf_results <-
      BayesFactor::extractBF(
        x = BayesFactor::ttestBF(
          x = data$x,
          rscale = bf.prior,
          mu = test.value,
          nullInterval = NULL
        )
      ) %>% # converting to a tibble dataframe
      tibble::as_data_frame(.) %>% # removing unnecessary columns
      dplyr::select(.data = ., -time, -code) %>% # adding prior width column
      dplyr::mutate(.data = ., bf.prior = bf.prior)

    # prepare the bayes factor message
    bf_message <-
      base::substitute(
        atop(top.text,
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
          bf = ggstatsplot::specify_decimal_p(
            x = log(
              x = (1 / bf_results$bf[[1]]),
              base = exp(1)
            ),
            k = 1,
            p.value = FALSE
          ),
          bf_prior = ggstatsplot::specify_decimal_p(
            x = bf_results$bf.prior[[1]],
            k = 3,
            p.value = FALSE
          )
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
