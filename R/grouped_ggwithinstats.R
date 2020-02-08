#' @title Violin plots for group or condition comparisons in within-subjects
#'   designs repeated across all levels of a grouping variable.
#' @name grouped_ggwithinstats
#' @description A combined plot of comparison plot created for levels of a
#'   grouping variable.
#'
#' @inheritParams ggwithinstats
#' @inheritDotParams ggwithinstats -title
#' @inheritParams grouped_ggbetweenstats
#'
#' @import ggplot2
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo quo_name ensym !!!
#' @importFrom purrr pmap
#'
#' @seealso \code{\link{ggwithinstats}}, \code{\link{ggbetweenstats}},
#' \code{\link{grouped_ggbetweenstats}}
#'
#' @inherit ggwithinstats return references
#' @inherit ggwithinstats return details
#'
#' @examples
#' \donttest{
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # the most basic function call
#' ggstatsplot::grouped_ggwithinstats(
#'   data = VR_dilemma,
#'   x = modality,
#'   y = score,
#'   grouping.var = order,
#'   ggplot.component = ggplot2::scale_y_continuous(
#'     breaks = seq(0, 1, 0.1),
#'     limits = c(0, 1)
#'   ),
#'   messages = TRUE
#' )
#' }
#' @export

# defining the function
grouped_ggwithinstats <- function(data,
                                  x,
                                  y,
                                  grouping.var,
                                  outlier.label = NULL,
                                  title.prefix = NULL,
                                  output = "plot",
                                  ...,
                                  plotgrid.args = list(),
                                  title.text = NULL,
                                  title.args = list(size = 16, fontface = "bold"),
                                  caption.text = NULL,
                                  caption.args = list(size = 10),
                                  sub.text = NULL,
                                  sub.args = list(size = 12)) {

  # =================== check user input and prep =========================

  # create a list of function call to check
  param_list <- as.list(match.call())

  # check that there is a grouping.var
  if (!"grouping.var" %in% names(param_list)) {
    stop("You must specify a grouping variable")
  }

  # check that conditioning and grouping.var are different
  if (as.character(param_list$x) == as.character(param_list$grouping.var)) {
    message(cat(
      crayon::red("\nError: "),
      crayon::blue(
        "Identical variable (",
        crayon::yellow(param_list$x),
        ") was used for both grouping and x axis, which is not allowed.\n"
      ),
      sep = ""
    ))
    return(invisible(param_list$x))
  }

  # ensure the grouping variable works quoted or unquoted
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  grouping.var <- rlang::ensym(grouping.var)
  outlier.label <- if (!rlang::quo_is_null(rlang::enquo(outlier.label))) {
    rlang::ensym(outlier.label)
  }

  # if `title.prefix` is not provided, use the variable `grouping.var` name
  if (is.null(title.prefix)) title.prefix <- rlang::as_name(grouping.var)

  # ======================== preparing dataframe =============================

  # creating a dataframe
  df <-
    data %>%
    dplyr::select(
      .data = .,
      {{ grouping.var }},
      {{ x }},
      {{ y }},
      {{ outlier.label }}
    ) %>%
    tidyr::drop_na(data = .) %>% # creating a list for grouped analysis
    grouped_list(data = ., grouping.var = {{ grouping.var }})

  # ============== creating a list of plots using `pmap`======================

  plotlist_purrr <-
    purrr::pmap(
      .l = list(data = df, title = paste(title.prefix, ": ", names(df), sep = "")),
      .f = ggstatsplot::ggwithinstats,
      # put common parameters here
      x = {{ x }},
      y = {{ y }},
      outlier.label = {{ outlier.label }},
      output = output,
      ...
    )

  # combining the list of plots into a single plot
  if (output == "plot") {
    return(ggstatsplot::combine_plots2(
      plotlist = plotlist_purrr,
      plotgrid.args = plotgrid.args,
      title.text = title.text,
      title.args = title.args,
      caption.text = caption.text,
      caption.args = caption.args,
      sub.text = sub.text,
      sub.args = sub.args
    ))
  } else {
    # subtitle list
    return(plotlist_purrr)
  }
}
