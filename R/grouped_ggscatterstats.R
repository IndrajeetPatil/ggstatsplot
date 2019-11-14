#' @title Scatterplot with marginal distributions for all levels of a grouping
#'   variable
#' @name grouped_ggscatterstats
#' @description Grouped scatterplots from `ggplot2` combined with marginal
#'   histograms/boxplots/density plots with statistical details added as a
#'   subtitle.
#'
#' @inheritParams ggscatterstats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams combine_plots
#'
#' @import ggplot2
#'
#' @importFrom dplyr select bind_rows summarize mutate mutate_at mutate_if
#' @importFrom dplyr group_by n arrange
#' @importFrom rlang !! enquo enexpr ensym
#' @importFrom purrr pmap
#'
#' @seealso \code{\link{ggscatterstats}}, \code{\link{ggcorrmat}},
#' \code{\link{grouped_ggcorrmat}}
#'
#' @inherit ggscatterstats return references
#' @inherit ggscatterstats return details
#'
#' @examples
#' # to ensure reproducibility
#' set.seed(123)
#'
#' # basic function call
#' ggstatsplot::grouped_ggscatterstats(
#'   data = dplyr::filter(
#'     ggstatsplot::movies_long,
#'     genre == "Comedy" |
#'       genre == "Drama"
#'   ),
#'   x = length,
#'   y = rating,
#'   method = "lm",
#'   formula = y ~ x + I(x^3),
#'   grouping.var = genre
#' )
#'
#' # using labeling
#' # (also show how to modify basic plot from within function call)
#' ggstatsplot::grouped_ggscatterstats(
#'   data = dplyr::filter(ggplot2::mpg, cyl != 5),
#'   x = displ,
#'   y = hwy,
#'   grouping.var = cyl,
#'   title.prefix = "Cylinder count",
#'   type = "robust",
#'   label.var = manufacturer,
#'   label.expression = hwy > 25 & displ > 2.5,
#'   xfill = NULL,
#'   ggplot.component = ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis()),
#'   package = "yarrr",
#'   palette = "appletv",
#'   messages = FALSE
#' )
#'
#' # labeling without expression
#'
#' ggstatsplot::grouped_ggscatterstats(
#'   data = dplyr::filter(
#'     .data = ggstatsplot::movies_long,
#'     rating == 7,
#'     genre %in% c("Drama", "Comedy")
#'   ),
#'   x = budget,
#'   y = length,
#'   grouping.var = genre,
#'   bf.message = FALSE,
#'   label.var = "title",
#'   marginal = FALSE,
#'   title.prefix = "Genre",
#'   caption.text = "All movies have IMDB rating equal to 7."
#' )
#' @export

# defining the function
grouped_ggscatterstats <- function(data,
                                   x,
                                   y,
                                   grouping.var,
                                   type = "pearson",
                                   conf.level = 0.95,
                                   bf.prior = 0.707,
                                   bf.message = TRUE,
                                   label.var = NULL,
                                   label.expression = NULL,
                                   title.prefix = NULL,
                                   xlab = NULL,
                                   ylab = NULL,
                                   method = "lm",
                                   method.args = list(),
                                   formula = y ~ x,
                                   point.color = "black",
                                   point.size = 3,
                                   point.alpha = 0.4,
                                   line.size = 1.5,
                                   point.width.jitter = 0,
                                   point.height.jitter = 0,
                                   line.color = "blue",
                                   marginal = TRUE,
                                   marginal.type = "histogram",
                                   marginal.size = 5,
                                   margins = c("both", "x", "y"),
                                   package = "wesanderson",
                                   palette = "Royal1",
                                   direction = 1,
                                   xfill = "#009E73",
                                   yfill = "#D55E00",
                                   xalpha = 1,
                                   yalpha = 1,
                                   xsize = 0.7,
                                   ysize = 0.7,
                                   centrality.para = NULL,
                                   results.subtitle = TRUE,
                                   stat.title = NULL,
                                   caption = NULL,
                                   subtitle = NULL,
                                   nboot = 100,
                                   beta = 0.1,
                                   k = 2,
                                   axes.range.restrict = FALSE,
                                   ggtheme = ggplot2::theme_bw(),
                                   ggstatsplot.layer = TRUE,
                                   ggplot.component = NULL,
                                   return = "plot",
                                   messages = TRUE,
                                   ...) {

  # check that there is a grouping.var
  if (!"grouping.var" %in% names(as.list(match.call()))) {
    stop("You must specify a grouping variable")
  }

  # ensure the grouping variable works quoted or unquoted
  grouping.var <- rlang::ensym(grouping.var)
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  label.var <- if (!rlang::quo_is_null(rlang::enquo(label.var))) rlang::ensym(label.var)

  # ======================== preparing dataframe =============================

  # if `title.prefix` is not provided, use the variable `grouping.var` name
  if (is.null(title.prefix)) title.prefix <- rlang::as_name(grouping.var)

  # getting the dataframe ready
  df <- grouped_list(data = data, grouping.var = {{ grouping.var }})

  # ==================== creating a list of plots =======================

  # creating a list of plots using `pmap`
  plotlist_purrr <-
    purrr::pmap(
      .l = list(data = df, title = paste(title.prefix, ": ", names(df), sep = "")),
      .f = ggstatsplot::ggscatterstats,
      # put common parameters here
      x = {{ x }},
      y = {{ y }},
      label.var = {{ label.var }},
      label.expression = !!rlang::enexpr(label.expression),
      type = type,
      conf.level = conf.level,
      bf.prior = bf.prior,
      bf.message = bf.message,
      method = method,
      xlab = xlab,
      ylab = ylab,
      method.args = method.args,
      formula = formula,
      point.color = point.color,
      point.size = point.size,
      point.alpha = point.alpha,
      line.size = line.size,
      point.width.jitter = point.width.jitter,
      point.height.jitter = point.height.jitter,
      line.color = line.color,
      marginal = marginal,
      marginal.type = marginal.type,
      marginal.size = marginal.size,
      margins = margins,
      package = package,
      palette = palette,
      direction = direction,
      xfill = xfill,
      yfill = yfill,
      xalpha = xalpha,
      yalpha = yalpha,
      xsize = xsize,
      ysize = ysize,
      centrality.para = centrality.para,
      results.subtitle = results.subtitle,
      stat.title = stat.title,
      caption = caption,
      subtitle = subtitle,
      nboot = nboot,
      beta = beta,
      k = k,
      axes.range.restrict = axes.range.restrict,
      ggtheme = ggtheme,
      ggstatsplot.layer = ggstatsplot.layer,
      ggplot.component = ggplot.component,
      return = return,
      messages = messages
    )

  # combining the list of plots into a single plot
  # inform user this can't be modified further with ggplot commands
  if (return == "plot") {
    if (isTRUE(messages)) grouped_message()
    return(ggstatsplot::combine_plots(plotlist = plotlist_purrr, ...))
  } else {
    return(plotlist_purrr)
  }
}
