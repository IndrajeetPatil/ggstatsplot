#' @title Switch subtitle making function
#' @description Switch function to use the appropriate helper function from
#'   `statsExpressions` to create subtitle.
#' @name subtitle_function_switch
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
#' @importFrom ipmisc stats_type_switch
#'
#' @keywords internal
#' @noRd

subtitle_function_switch <- function(test, type, ...) {
  # make a function character string
  .f_string <- paste0("statsExpressions::expr_", test, "_", type, "(...)")

  # evaluate it
  return(rlang::eval_bare(rlang::parse_expr(.f_string)))
}


#' @title Switch caption making function
#' @name caption_function_switch
#'
#' @inheritParams subtitle_function_switch
#'
#' @importFrom statsExpressions bf_ttest bf_oneway_anova
#' @importFrom rlang exec
#'
#' @keywords internal
#' @noRd

caption_function_switch <- function(test, ...) {
  # choosing the appropriate test
  if (test == "t") .f <- statsExpressions::bf_ttest
  if (test == "anova") .f <- statsExpressions::bf_oneway_anova

  # preparing the BF message
  rlang::exec(.fn = .f, ...)
}
