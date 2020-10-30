#' @title Grouped pie charts with statistical tests
#' @name grouped_ggpiestats
#' @description Helper function for `ggstatsplot::ggpiestats` to apply this
#'   function across multiple levels of a given factor and combining the
#'   resulting plots using `ggstatsplot::combine_plots`.
#'
#' @inheritParams ggpiestats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggpiestats -title
#'
#' @importFrom dplyr select
#' @importFrom rlang !! enquo quo_name ensym
#' @importFrom purrr map
#'
#' @seealso \code{\link{ggbarstats}}, \code{\link{ggpiestats}},
#'  \code{\link{grouped_ggbarstats}}
#'
#' @inherit ggpiestats return references
#' @inherit ggpiestats return details
#' @inherit ggpiestats return return
#'
#' @examples
#' \donttest{
#' # grouped one-sample proportion tests
#' ggstatsplot::grouped_ggpiestats(
#'   data = mtcars,
#'   grouping.var = am,
#'   x = cyl
#' )
#'
#' # the following will take slightly more amount of time
#' # for reproducibility
#' set.seed(123)
#'
#' # let's create a smaller dataframe
#' diamonds_short <- ggplot2::diamonds %>%
#'   dplyr::filter(.data = ., cut %in% c("Fair", "Very Good", "Ideal")) %>%
#'   dplyr::sample_frac(tbl = ., size = 0.10)
#'
#' # plot
#' ggstatsplot::grouped_ggpiestats(
#'   data = diamonds_short,
#'   x = color,
#'   y = clarity,
#'   grouping.var = cut,
#'   sampling.plan = "poisson",
#'   title.prefix = "Quality",
#'   slice.label = "both",
#'   perc.k = 1,
#'   plotgrid.args = list(nrow = 3)
#' )
#' }
#' @export

# defining the function
grouped_ggpiestats <- function(data,
                               x,
                               y = NULL,
                               counts = NULL,
                               grouping.var,
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

  # ======================== check user input =============================

  # ensure the grouping variable works quoted or unquoted
  grouping.var <- rlang::ensym(grouping.var)
  x <- rlang::ensym(x)
  y <- if (!rlang::quo_is_null(rlang::enquo(y))) rlang::ensym(y)
  counts <- if (!rlang::quo_is_null(rlang::enquo(counts))) rlang::ensym(counts)

  # if `title.prefix` is not provided, use the variable `grouping.var` name
  if (is.null(title.prefix)) title.prefix <- rlang::as_name(grouping.var)

  # ======================== preparing dataframe =============================

  # creating a dataframe
  df <-
    dplyr::select(.data = data, {{ grouping.var }}, {{ x }}, {{ y }}, {{ counts }}) %>%
    grouped_list(data = ., grouping.var = {{ grouping.var }})

  # ==================== creating a list of return objects ===================

  # creating a list of plots using `pmap`
  plotlist_purrr <-
    purrr::pmap(
      .l = list(data = df, title = paste0(title.prefix, ": ", names(df))),
      .f = ggstatsplot::ggpiestats,
      # put common parameters here
      x = {{ x }},
      y = {{ y }},
      counts = {{ counts }},
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
