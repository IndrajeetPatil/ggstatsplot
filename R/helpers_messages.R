#'
#' @title Display normality test result as a message.
#' @name normality_message
#' @aliases normality_message
#' @param x A numeric vector.
#' @param lab A character describing label for the variable.
#' @param k Number of decimal places expected for results (Default: `3`).
#' @param output What output is desired: `"message"` (default) or `"stats"` objects.
#' @description A note to the user about the validity of assumptions for the
#'   default linear model.
#'
#' @importFrom stats shapiro.test
#' @importFrom crayon green
#' @importFrom crayon blue
#' @importFrom crayon yellow
#' @importFrom crayon red
#'
#' @keywords internal
#'
#'@note This is a helper function used internally in the package and not
#'  exported. In case you want to use it, you can do so by
#'  `ggstatsplot:::normality_message`. Note that it is `:::` and not `::`.
#'

normality_message <- function(x, lab, k = 3, output = "message") {
  if (length(x) > 3 && length(x) < 5000) {
    # for AD test of normality, sample size must be greater than 7
    sw_norm <- stats::shapiro.test(x = x)
    if (output == "message") {
    # exact message
    base::message(cat(
      crayon::green("Note: "),
      crayon::blue(
        "Shapuro-Wilk Normality Test for",
        crayon::yellow(lab),
        # entered y argument
        ": p-value = "
      ),
      crayon::yellow(
        ggstatsplot::specify_decimal_p(x = sw_norm$p.value[[1]],
                                       k = k,
                                       p.value = TRUE)
      )
    ))
    } else if (output == "stats") {
        print(sw_norm)
      }
  }
}
