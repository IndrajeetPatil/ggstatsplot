#' @title Switch function to use helper function to create subtitle for the
#'   `ggbetweenstats` plot.
#' @name ggbetweenstats_switch
#' @aliases ggbetweenstats_switch
#'
#' @inheritParams ggbetweenstats
#' @param test Decides which test to run (can be either `"t-test"` or
#'   `"anova"`).
#' @param ... Arguments for respective helper function.
#'
#' @author Indrajeet Patil
#'
#' @keywords internal

ggbetweenstats_switch <- function(type, test, ...) {
  # figuring out type of test needed to run
  test.type <- switch(
    EXPR = type,
    parametric = "p",
    p = "p",
    robust = "r",
    r = "r",
    nonparametric = "np",
    np = "np",
    bayes = "bf",
    bf = "bf"
  )

  # either t-test or anova will be run
  if (test == "t-test") {
    subtitle <- switch(
      EXPR = test.type,
      p = {
        ggstatsplot::subtitle_ggbetween_t_parametric(...)
      },
      np = {
        ggstatsplot::subtitle_ggbetween_mann_nonparametric(...)
      },
      r = {
        ggstatsplot::subtitle_ggbetween_t_rob(...)
      },
      bf = {
        ggstatsplot::subtitle_ggbetween_t_bayes(...)
      }
    )
  } else if (test == "anova") {
    subtitle <- switch(
      EXPR = test.type,
      p = {
        ggstatsplot::subtitle_ggbetween_anova_parametric(...)
      },
      np = {
        ggstatsplot::subtitle_ggbetween_kw_nonparametric(...)
      },
      r = {
        ggstatsplot::subtitle_ggbetween_rob_anova(...)
      },
      bf = {
        print("Bayes Factors currently not supported for ANOVA designs.")
      }
    )
  }

  # return the text for the subtitle
  return(subtitle)
}

#' @title Preparing text to describe which *p*-value adjustment method was used.
#' @name p.adjust.method.description
#' @return Standardized text description for what method was used.
#'
#' @inheritParams pairwise_p
#'
#' @keywords internal

p.adjust.method.description <- function(p.adjust.method) {
  base::switch(
    EXPR = p.adjust.method,
    none = "None",
    bonferroni = "Bonferroni",
    holm = "Holm",
    hochberg = "Hochberg",
    hommel = "Hommel",
    BH = "Benjamini & Hochberg",
    fdr = "Benjamini & Hochberg",
    BY = "Benjamini & Yekutieli"
  )
}
