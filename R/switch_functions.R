#' @title Switch subtitle making function
#' @description Switch function to use the appropriate helper function from
#'   `statsExpressions` to create subtitle.
#' @name function_switch
#'
#' @param test Decides which test to run (can be either `"t"` or
#'   `"anova"`).
#' @param element Which expression is needed (`"subtitle"` or `"caption"`)
#' @param ... Arguments passed to respective subtitle helper functions.
#'
#' @importFrom statsExpressions expr_t_twosample expr_oneway_anova
#' @importFrom rlang exec
#'
#' @keywords internal
#' @noRd

function_switch <- function(test, element, ...) {
  # which function?
  if (test == "t") .f <- statsExpressions::expr_t_twosample
  if (test == "anova") .f <- statsExpressions::expr_oneway_anova

  # evaluate it
  suppressWarnings(suppressMessages(rlang::exec(.fn = .f, ...)))
}
