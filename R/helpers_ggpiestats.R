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
#' @seealso \code{\link{bf_corr_test}}, \code{\link{bf_oneway_anova}}
#'
#' @keywords internal

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
    # creating a dataframe
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(main),
        y = !!rlang::enquo(condition)
      ) %>%
      stats::na.omit(.) %>% # converting to a tibble dataframe
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

    # detailed text of sample plan
    sampling_plan_text <- switch(
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
              sampling.plan
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
          sampling.plan = sampling_plan_text
        )
      )

    # return the text results or the dataframe with results
    if (output == "caption") {
      return(bf_message)
    } else if (output == "results") {
      return(bf_results)
    }
  }
