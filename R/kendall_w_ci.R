#' @title Computing confidence intervals for the Kendall's coefficient of
#'   concordance (aka Kendall's *W*).
#' @name kendall_w_ci
#' @author Indrajeet Patil
#'
#' @importFrom dplyr tibble
#' @importFrom DescTools KendallW
#'
#' @param data Data in wide format with only numeric columns corresponding to
#'   raters' ratings.
#' @inheritParams t1way_ci
#' @param ... Additional arguments (currently ignored).
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' library(jmv)
#' data("bugs", package = "jmv")
#' ggstatsplot:::kendall_w_ci(dplyr::select(bugs, LDLF:HDHF))
#' }
#'
#' @keywords internal

kendall_w_ci <- function(data,
                         nboot = 100,
                         conf.type = "norm",
                         conf.level = 0.95,
                         ...) {
  ellipsis::check_dots_used()

  Function <- function(input, index) {
    Input <- input[index, ]
    r <-
      DescTools::KendallW(
        x = Input,
        correct = TRUE,
        test = FALSE,
        na.rm = TRUE
      )
    return(r)
  }
  r <-
    DescTools::KendallW(
      x = data,
      correct = TRUE,
      test = FALSE,
      na.rm = TRUE
    )
  Boot <- boot(data, Function, R = nboot)
  BCI <- boot.ci(Boot, conf = conf.level, type = conf.type)
  if (conf.type == "norm") {
    CI1 <- BCI$normal[2]
    CI2 <- BCI$normal[3]
  }
  if (conf.type == "basic") {
    CI1 <- BCI$basic[4]
    CI2 <- BCI$basic[5]
  }
  if (conf.type == "perc") {
    CI1 <- BCI$percent[4]
    CI2 <- BCI$percent[5]
  }
  if (conf.type == "bca") {
    CI1 <- BCI$bca[4]
    CI2 <- BCI$bca[5]
  }

  return(dplyr::tibble(estimate = r, conf.low = CI1, conf.high = CI2))
}
