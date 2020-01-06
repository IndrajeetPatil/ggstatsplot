#' @title Switch function to use helper function to create subtitle for the
#'   `ggbetweenstats` plot.
#' @name ggbetweenstats_subtitle_switch
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
#' @importFrom dplyr case_when
#'
#' @keywords internal

ggbetweenstats_subtitle_switch <- function(type, test, ...) {
  # figuring out type of test needed to run
  type <- stats_type_switch(type)

  # make a function character string
  .f_string <- paste("statsExpressions::expr_", test, "_", type, "(...)", sep = "")

  # evaluate it
  return(rlang::eval_bare(rlang::parse_expr(.f_string)))
}

#' @rdname ggbetweenstats_subtitle_switch
#' @aliases ggbetweenstats_subtitle_switch
#' @keywords internal

ggwithinstats_subtitle_switch <- ggbetweenstats_subtitle_switch


#' @noRd

stats_type_switch <- function(type) {
  dplyr::case_when(
    grepl("^p", type, TRUE) ~ "parametric",
    grepl("^n", type, TRUE) ~ "nonparametric",
    grepl("^r", type, TRUE) ~ "robust",
    grepl("^b", type, TRUE) ~ "bayes",
    TRUE ~ "parametric"
  )
}

#' @title Switch function to use helper function to create caption for the
#'   `ggbetweenstats` plot.
#' @name ggbetweenstats_caption_switch
#'
#' @param test Decides which test to run (can be either `"t"` or
#'   `"anova"`).
#' @param ... Arguments passed to respective caption helper functions.
#'
#' @importFrom statsExpressions bf_ttest bf_oneway_anova
#' @importFrom rlang exec
#'
#' @keywords internal

ggbetweenstats_caption_switch <- function(test, ...) {
  # choosing the appropriate test
  if (test == "t") {
    .f <- statsExpressions::bf_ttest
  } else {
    .f <- statsExpressions::bf_oneway_anova
  }

  # preparing the BF message for null
  rlang::exec(.fn = .f, ...)
}

#' @rdname ggbetweenstats_caption_switch
#' @aliases ggbetweenstats_caption_switch
#' @keywords internal

ggwithinstats_caption_switch <- ggbetweenstats_caption_switch
