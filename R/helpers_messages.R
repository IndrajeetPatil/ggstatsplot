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
          "Shapiro-Wilk Normality Test for",
          crayon::yellow(lab),
          # entered y argument
          ": p-value = "
        ),
        crayon::yellow(
          ggstatsplot::specify_decimal_p(
            x = sw_norm$p.value[[1]],
            k = k,
            p.value = TRUE
          )
        )
      ))
    } else if (output == "stats") {
      print(sw_norm)
    }
  }
}


#'
#' @title Display homogeneity of variance test as a message
#' @name bartlett_message
#' @aliases bartlett_message
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param lab A character describing label for the variable.
#' @param k Number of decimal places expected for results (Default: `3`).
#' @param output What output is desired: `"message"` (default) or `"stats"` objects.
#' @description A note to the user about the validity of assumptions for the
#'   default linear model.
#'
#' @importFrom stats bartlett.test
#' @importFrom crayon green
#' @importFrom crayon blue
#' @importFrom crayon yellow
#' @importFrom crayon red
#'
#' @keywords internal
#'
#'@note This is a helper function used internally in the package and not
#'  exported. In case you want to use it, you can do so by
#'  `ggstatsplot:::bartlett_message`. Note that it is `:::` and not `::`.
#'

bartlett_message <- function(data,
                             lab,
                             k = 3,
                             output = "message") {
  # running the test
  bartlett <- stats::bartlett.test(formula = y ~ x,
                                   data = data)
  if (output == "message") {
    # display homogeneity of variances test result as a message
    base::message(cat(
      crayon::green("Note: "),
      crayon::blue(
        "Bartlett's test for homogeneity of variances for factor",
        crayon::yellow(lab),
        # entered x argument
        ": p-value = "
      ),
      crayon::yellow(
        ggstatsplot::specify_decimal_p(x = bartlett$p.value[[1]],
                                       k,
                                       p.value = TRUE)
      )
    ))
  } else if (output == "stats") {
    print(bartlett)
  }
}
