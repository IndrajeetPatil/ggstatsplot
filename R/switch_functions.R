#' @title Switch function to use helper function to create subtitle for the
#'   `ggbetweenstats` plot.
#' @name ggbetweenstats_switch
#'
#' @inheritParams ggbetweenstats
#' @param test Decides which test to run (can be either `"t"` or
#'   `"anova"`).
#' @param ... Arguments passed to respective subtitle helper functions.
#'
#' @importFrom statsExpressions expr_t_parametric expr_t_nonparametric
#' @importFrom statsExpressions expr_t_robust expr_t_bayes
#' @importFrom statsExpressions expr_anova_parametric expr_anova_nonparametric
#' @importFrom statsExpressions expr_anova_robust expr_anova_bayes
#' @importFrom rlang eval_bare parse_expr
#'
#' @keywords internal

ggbetweenstats_switch <- function(type, test, ...) {
  # figuring out type of test needed to run
  type <-
    switch(
      EXPR = type,
      parametric = "parametric",
      p = "parametric",
      robust = "robust",
      r = "robust",
      nonparametric = "nonparametric",
      np = "nonparametric",
      bayes = "bayes",
      bf = "bayes"
    )

  # make a function character string
  .f_string <- paste("statsExpressions::expr_", test, "_", type, "(...)", sep = "")

  # evaluate it
  return(rlang::eval_bare(rlang::parse_expr(.f_string)))
}

#' @rdname ggbetweenstats_switch
#' @aliases ggbetweenstats_switch
#' @export

ggwithinstats_switch <- ggbetweenstats_switch
