#' @title Summary dataframe for categorical variables.
#' @name cat_label_df
#' @description Creating a dataframe with an added column corresponding to
#'   summary for categorical variables.
#' @author Indrajeet Patil
#'
#' @param data A dataframe containing summaries for categorical variables.
#'   Should contain columns named either `"perc"` or `"counts"` or both.
#' @param label.col.name Character that decides the column name containing
#'   summary label. This can either be `"slice.label"` (default) or
#'   `"data.label"`.
#' @param label.content Character decides what information needs to be displayed
#'   on the label in each pie or bar slice. Possible options are `"percentage"`
#'   (default), `"counts"`, `"both"`.
#' @param label.separator If `"both"` counts and proportion information is to be
#'   displayed in a label, this argument decides whether these two pieces of
#'   information are going to be on the same line (`" "`) or on separate lines
#'   (`"\n"`).
#' @inheritParams ggpiestats
#'
#' @importFrom dplyr mutate
#' @importFrom rlang !! :=
#' @importFrom stats pchisq
#'
#' @examples
#' \dontrun{
#'
#' # dataframe with label column
#' ggstatsplot:::cat_label_df(
#'   data = ggstatsplot:::cat_counter(mtcars, am, cyl),
#'   label.col.name = "slice.label",
#'   label.content = "both",
#'   perc.k = 1
#' )
#' }
#'
#' @keywords internal

# function body
cat_label_df <- function(data,
                         label.col.name = "slice.label",
                         label.content = "percentage",
                         label.separator = c("\n", " "),
                         perc.k = 1) {
  # checking what needs to be displayed in a label

  # only percentage
  if (label.content %in% c("percentage", "perc", "proportion", "prop", "%")) {
    data %<>%
      dplyr::mutate(
        .data = .,
        !!label.col.name := paste0(round(x = perc, digits = perc.k), "%")
      )
  }

  # only raw counts
  if (label.content %in% c("counts", "n", "count", "N")) {
    data %<>%
      dplyr::mutate(
        .data = .,
        !!label.col.name := paste0("n = ", counts)
      )
  }

  # both raw counts and percentages
  if (label.content %in% c("both", "mix", "all", "everything")) {
    data %<>%
      dplyr::mutate(
        .data = .,
        !!label.col.name := paste0(
          "n = ",
          counts,
          label.separator,
          "(",
          round(x = perc, digits = perc.k),
          "%)"
        )
      )
  }

  # return dataframe with label column
  return(data)
}


#' @title Preparing dataframe with counts and percentages for categorical
#'   variables.
#' @name cat_counter
#' @author Indrajeet Patil
#'
#' @inheritParams ggpiestats
#' @param ... Additional grouping variables.
#'
#' @importFrom rlang enquos !! quo_is_null
#' @importFrom purrr discard
#' @importFrom dplyr select group_by summarize n arrange if_else desc
#' @importFrom dplyr mutate mutate_at mutate_if group_by_at
#'
#' @examples
#' ggstatsplot:::cat_counter(data = ggplot2::mpg, "drv", cyl, "fl")
#' @keywords internal

# function body
cat_counter <- function(data, main, condition = NULL, ...) {
  # massaging the inputs
  dots <- rlang::enquos(condition, main, ..., .ignore_empty = "all")

  # discarding NULL arguments
  purrr::discard(.x = dots, .p = rlang::quo_is_null)

  # creating a dataframe with counts
  df <-
    data %>%
    dplyr::group_by_at(.tbl = ., .vars = dots, .drop = TRUE) %>%
    dplyr::summarize(.data = ., counts = dplyr::n()) %>%
    dplyr::mutate(.data = ., perc = (counts / sum(counts)) * 100) %>%
    dplyr::ungroup(x = .) %>%
    dplyr::arrange(.data = ., dplyr::desc(!!rlang::ensym(main))) %>%
    dplyr::filter(.data = ., counts != 0L)

  # return the final dataframe
  return(df)
}

#' @title Cramer's V and confidence intervals for a chi squared goodness of fit
#'   test using non central parameters.
#' @name cramer_v_ci
#' @description A customized version of `DescTools::CramerV` to allow for
#'   probabilities to be specified. Called from `helpers_ggpiestats_subtitle`
#'   after the appropriate `x` has been made into a `table`.
#' @author Chuck Powell
#'
#' @param x  table of counts.
#' @param conf.level confidence level of the interval around Cramer's *V*.
#' @param method string defining the method to calculate confidence intervals
#'   for Cramer's *V*. Either `"ncchisq"` (using noncentral chisquare), or the
#'   adjusted version `"ncchisqadj"`` with degrees of freedom added to both the
#'   numerator and denominator.  Default is `"ncchisq"`.
#' @param p (passed to `chisq.test`) a vector of probabilities of the same
#'   length of `x`. `p` is rescaled (if necessary and it is possible) to sum to
#'   1. An error is given if any entry of `p` is negative. Default is equal
#'   counts per category (factor level).
#'
#' @importFrom stats chisq.test pchisq
#'
#' @value A numeric vector with 3 elements for the estimate, the lower and
#'   the upper confidence interval.
#'
#' @examples
#'
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # simple function call with the defaults
#' x <- table(mtcars$am)
#' ggstatsplot:::cramer_v_ci(x)
#'
#' # raise confidence intervals specify proportions
#' ggstatsplot:::cramer_v_ci(x, conf.level = 0.999, p = c(0.6, 0.4))
#'
#' # notice order matters and it follows the levels of `x`
#' # in this case am = 0 is before am = 1
#' ggstatsplot:::cramer_v_ci(x, conf.level = 0.90, p = c(4, 6))
#'
#' # function body
cramer_v_ci <- function(x,
                        conf.level = .95,
                        method = c(
                          "ncchisq",
                          "ncchisqadj"
                        ),
                        p = rep(1 / length(x), length(x))) {

  # Make sure we were input a table
  if (!class(x) %in% c("table")) {
    # turn off pairwise comparisons
    stop("Input x must be a table")
  }

  # ------------------------- sub functions ----------------------------

  lochi <- function(chival, df, conf) {
    ulim <- 1 - (1 - conf) / 2
    lc <- c(0.001, chival / 2, chival)
    while (stats::pchisq(chival, df, lc[1]) < ulim) {
      if (stats::pchisq(chival, df) < ulim) {
        return(c(0, stats::pchisq(chival, df)))
      }
      lc <- c(lc[1] / 4, lc[1], lc[3])
    }
    diff <- 1
    while (diff > 1e-05) {
      if (stats::pchisq(chival, df, lc[2]) < ulim) {
        lc <- c(lc[1], (lc[1] + lc[2]) / 2, lc[2])
      } else {
        lc <- c(lc[2], (lc[2] + lc[3]) / 2, lc[3])
      }
      diff <- abs(stats::pchisq(chival, df, lc[2]) - ulim)
      ucdf <- stats::pchisq(chival, df, lc[2])
    }
    c(lc[2], ucdf)
  }

  hichi <- function(chival, df, conf) {
    uc <- c(chival, 2 * chival, 3 * chival)
    llim <- (1 - conf) / 2
    while (stats::pchisq(chival, df, uc[1]) < llim) {
      uc <- c(uc[1] / 4, uc[1], uc[3])
    }
    while (stats::pchisq(chival, df, uc[3]) > llim) {
      uc <- c(uc[1], uc[3], uc[3] + chival)
    }
    diff <- 1
    while (diff > 1e-05) {
      if (stats::pchisq(chival, df, uc[2]) < llim) {
        uc <- c(uc[1], (uc[1] + uc[2]) / 2, uc[2])
      } else {
        uc <- c(uc[2], (uc[2] + uc[3]) / 2, uc[3])
      }
      diff <- abs(stats::pchisq(chival, df, uc[2]) - llim)
      lcdf <- stats::pchisq(chival, df, uc[2])
    }
    c(uc[2], lcdf)
  }

  # --------------- initial chisq and V estimates ---------------------------

  chisq.hat <- suppressWarnings(
    stats::chisq.test(
      x = x,
      correct = FALSE,
      p = p,
      rescale.p = TRUE
    )$statistic
  )

  df <- prod(dim(x) - 1)
  n <- sum(x)
  v <- as.numeric(sqrt(chisq.hat / (n * (min(dim(x)) - 1))))

  # --------------- adjusted or unadjusted ------------------------

  switch(
    match.arg(method),
    ncchisq = {
      ci <- c(
        lochi(chisq.hat, df, conf.level)[1],
        hichi(chisq.hat, df, conf.level)[1]
      )
      ci <- unname(sqrt((ci) / (n * (min(dim(x)) - 1))))
    },
    ncchisqadj = {
      ci <- c(
        lochi(chisq.hat, df, conf.level)[1] + df,
        hichi(chisq.hat, df, conf.level)[1] + df
      )
      ci <- unname(sqrt((ci) / (n * (min(dim(x)) - 1))))
    }
  )

  # --------------- return results ---------------------------

  return(c(
    `Cramer V` = v,
    lwr.ci = max(0, ci[1]),
    upr.ci = min(1, ci[2])
  ))
}
