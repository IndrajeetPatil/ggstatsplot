#' @title Switch function to use helper function to create subtitle for the
#'   `ggbetweenstats` plot.
#' @name ggbetweenstats_switch
#' @author Indrajeet Patil
#'
#' @inheritParams ggbetweenstats
#' @param test Decides which test to run (can be either `"t-test"` or
#'   `"anova"`).
#' @param ... Arguments passed to respective subtitle helper functions.
#'
#' @importFrom statsExpressions expr_t_parametric expr_t_nonparametric
#' @importFrom statsExpressions expr_t_robust expr_t_bayes
#' @importFrom statsExpressions expr_anova_parametric expr_anova_nonparametric
#' @importFrom statsExpressions expr_anova_robust expr_anova_bayes
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
        statsExpressions::expr_t_parametric(...)
      },
      np = {
        statsExpressions::expr_t_nonparametric(...)
      },
      r = {
        statsExpressions::expr_t_robust(...)
      },
      bf = {
        statsExpressions::expr_t_bayes(...)
      }
    )
  } else {
    subtitle <- switch(
      EXPR = test.type,
      p = {
        statsExpressions::expr_anova_parametric(...)
      },
      np = {
        statsExpressions::expr_anova_nonparametric(...)
      },
      r = {
        statsExpressions::expr_anova_robust(...)
      },
      bf = {
        statsExpressions::expr_anova_bayes(...)
      }
    )
  }

  # return the text for the subtitle
  return(subtitle)
}

#' @rdname ggbetweenstats_switch
#' @aliases ggbetweenstats_switch
#' @export

ggwithinstats_switch <- ggbetweenstats_switch


#' @title Switch function to determine which effect size is to computed.
#' @name effsize_type_switch
#' @description Takes in all allowed characters describing the needed effect
#'   size (e.g., `"d"`, `"partial_eta"`, etc.) and converts it into standard
#'   terms (`"biased"` or `"unbiased"`) to reduce the complexity of conditional
#'   statements.
#' @author Indrajeet Patil
#'
#' @param effsize.type Character describing the needed effect size.
#'
#' @keywords internal

effsize_type_switch <- function(effsize.type) {
  # figuring out which effect size to use
  switch(
    EXPR = effsize.type,
    d = "biased",
    g = "unbiased",
    eta = "biased",
    omega = "unbiased",
    partial_eta = "biased",
    partial_omega = "unbiased",
    partial.eta = "biased",
    partial.omega = "unbiased",
    p_eta = "biased",
    biased = "biased",
    p_omega = "unbiased",
    "unbiased"
  )
}
