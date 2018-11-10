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
#' @seealso \code{\link{bf_contigency_tab}}, \code{\link{bf_oneway_anova}}
#'
#' @keywords internal

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
      stats::na.omit(.) %>% # converting to a tibble dataframe
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
