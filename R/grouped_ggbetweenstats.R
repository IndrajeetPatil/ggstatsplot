#' @title Violin plots for group or condition comparisons in between-subjects
#'   designs repeated across all levels of a grouping variable.
#' @name grouped_ggbetweenstats
#' @description A combined plot of comparison plot created for levels of a
#'   grouping variable.
#'
#' @param title.prefix Character string specifying the prefix text for the fixed
#'   plot title (name of each factor level) (Default: `NULL`). If `NULL`, the
#'   variable name entered for `grouping.var` will be used.
#' @inheritParams ggbetweenstats
#' @inheritParams grouped_list
#' @inheritParams combine_plots2
#' @inheritDotParams ggbetweenstats -title
#'
#' @import ggplot2
#'
#' @importFrom dplyr select bind_rows summarize mutate mutate_at mutate_if
#' @importFrom dplyr group_by n arrange
#' @importFrom rlang !! enquo quo_name ensym
#' @importFrom purrr pmap
#'
#' @seealso \code{\link{ggbetweenstats}}, \code{\link{ggwithinstats}},
#'  \code{\link{grouped_ggwithinstats}}
#'
#' @inherit ggbetweenstats return references
#' @inherit ggbetweenstats return details
#'
#' @examples
#' \donttest{
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#'
#' # the most basic function call
#' ggstatsplot::grouped_ggbetweenstats(
#'   data = dplyr::filter(ggplot2::mpg, drv != "4"),
#'   x = year,
#'   y = hwy,
#'   grouping.var = drv,
#'   conf.level = 0.99
#' )
#'
#' # modifying individual plots using `ggplot.component` argument
#' ggstatsplot::grouped_ggbetweenstats(
#'   data = dplyr::filter(
#'     ggstatsplot::movies_long,
#'     genre %in% c("Action", "Comedy"),
#'     mpaa %in% c("R", "PG")
#'   ),
#'   x = genre,
#'   y = rating,
#'   grouping.var = mpaa,
#'   results.subtitle = FALSE,
#'   ggplot.component = ggplot2::scale_y_continuous(
#'     breaks = seq(1, 9, 1),
#'     limits = (c(1, 9))
#'   ),
#'   messages = FALSE
#' )
#' }
#' @export

# defining the function
grouped_ggbetweenstats <- function(data,
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

  # ensure the grouping variable works quoted or unquoted
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  grouping.var <- rlang::ensym(grouping.var)
  outlier.label <- if (!rlang::quo_is_null(rlang::enquo(outlier.label))) {
    rlang::ensym(outlier.label)
  }

  # if `title.prefix` is not provided, use the variable `grouping.var` name
  if (is.null(title.prefix)) title.prefix <- rlang::as_name(grouping.var)

  # ======================== preparing dataframe ==========================

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
    grouped_list(data = ., grouping.var = {{ grouping.var }})

  # ============== creating a list of plots using `pmap`=======================

  plotlist_purrr <-
    purrr::pmap(
      .l = list(data = df, title = paste(title.prefix, ": ", names(df), sep = "")),
      .f = ggstatsplot::ggbetweenstats,
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
    return(plotlist_purrr) # subtitle list
  }
}
