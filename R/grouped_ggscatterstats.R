#' @title Scatterplot with marginal distributions for all levels of a grouping
#'   variable
#' @name grouped_ggscatterstats
#' @aliases grouped_ggscatterstats
#' @author Indrajeet Patil, Chuck Powell
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
#' @importFrom rlang !! enquo quo_name ensym as_quosure parse_expr quo_text
#' @importFrom glue glue
#' @importFrom purrr map set_names pmap
#'
#' @seealso \code{\link{ggscatterstats}}, \code{\link{ggcorrmat}},
#' \code{\link{grouped_ggcorrmat}}
#'
#' @inherit ggscatterstats return references
#' @inherit ggscatterstats return details
#'
#' @examples
#'
#' \dontrun{
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
#' }
#' @export

# defining the function
grouped_ggscatterstats <- function(data,
                                   x,
                                   y,
                                   type = "pearson",
                                   conf.level = 0.95,
                                   bf.prior = 0.707,
                                   bf.message = TRUE,
                                   label.var = NULL,
                                   label.expression = NULL,
                                   grouping.var,
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

  # create a list of function call to check for label.expression
  param_list <- as.list(match.call())

  # check that there is a grouping.var
  if (!"grouping.var" %in% names(param_list)) {
    stop("You must specify a grouping variable")
  }

  # check that label.var and grouping.var are different
  if ("label.var" %in% names(param_list)) {
    if (as.character(param_list$label.var) == as.character(param_list$grouping.var)) {
      message(cat(
        crayon::red("\nError: "),
        crayon::blue(
          "Identical variable (",
          crayon::yellow(param_list$label.var),
          ") was used for both grouping and labeling, which is not allowed.\n"
        ),
        sep = ""
      ))
      return(invisible(param_list$label.var))
    }
  }

  # check if labeling variable has been specified
  if ("label.var" %in% names(param_list)) {
    labelvar.present <- TRUE
    label.var <- rlang::ensym(label.var)
    label.var <- deparse(substitute(label.var))
  } else {
    labelvar.present <- FALSE
  }

  # check if labeling expression has been specified
  if ("label.expression" %in% names(param_list)) {
    expression.present <- TRUE
  } else {
    expression.present <- FALSE
  }

  # ======================== preparing dataframe =============================

  # ensure the grouping variable works quoted or unquoted
  grouping.var <- rlang::ensym(grouping.var)

  # if `title.prefix` is not provided, use the variable `grouping.var` name
  if (is.null(title.prefix)) {
    title.prefix <- rlang::as_name(grouping.var)
  }

  # getting the dataframe ready
  # note that `dplyr::everything` is used because point labelling can involve
  # any of the data columns
  df <-
    dplyr::select(
      .data = data,
      !!rlang::enquo(grouping.var),
      !!rlang::enquo(x),
      !!rlang::enquo(y),
      dplyr::everything()
    )

  # creating a list for grouped analysis
  df %<>%
    grouped_list(data = ., grouping.var = !!rlang::enquo(grouping.var))

  # if labeling expression has been specified, format the arguments accordingly
  if (isTRUE(expression.present)) {
    if (typeof(param_list$label.expression) == "language") {
      # unquoted case
      label.expression <- rlang::enquo(label.expression)
    } else {
      # quoted case
      label.expression <- rlang::parse_expr(x = label.expression)
      # the environment is essential
      label.expression <- rlang::as_quosure(
        x = label.expression,
        env = sys.frame(which = 0)
      )
    }
  }

  # unquoted case
  if (typeof(param_list$x) == "symbol") {
    x <- deparse(substitute(x))
  }

  # unquoted case
  if (typeof(param_list$y) == "symbol") {
    y <- deparse(substitute(y))
  }

  # ============== build pmap list based on conditions =====================

  if (isTRUE(expression.present) && isTRUE(labelvar.present)) {
    flexiblelist <- list(
      data = df,
      x = x,
      y = y,
      title = glue::glue("{title.prefix}: {names(df)}"),
      label.var = label.var,
      label.expression = rlang::quo_text(label.expression)
    )
  }

  if (isTRUE(expression.present) && isFALSE(labelvar.present)) {
    flexiblelist <- list(
      data = df,
      x = x,
      y = y,
      title = glue::glue("{title.prefix}: {names(df)}"),
      label.expression = rlang::quo_text(label.expression)
    )
  }

  if (isFALSE(expression.present) && isTRUE(labelvar.present)) {
    flexiblelist <- list(
      data = df,
      x = x,
      y = y,
      title = glue::glue("{title.prefix}: {names(df)}"),
      label.var = label.var
    )
  }

  if (isFALSE(expression.present) && isFALSE(labelvar.present)) {
    flexiblelist <- list(
      data = df,
      x = x,
      y = y,
      title = glue::glue("{title.prefix}: {names(df)}")
    )
  }

  # ==================== creating a list of plots =======================

  # creating a list of plots using `pmap`
  plotlist_purrr <-
    purrr::pmap(
      .l = flexiblelist,
      .f = ggstatsplot::ggscatterstats,
      # put common parameters here
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
  if (return == "plot") {
    combined_object <-
      ggstatsplot::combine_plots(
        plotlist = plotlist_purrr,
        ...
      )

    # inform user this can't be modified further with ggplot commands
    if (isTRUE(messages)) {
      grouped_message()
    }
  } else {
    combined_object <- plotlist_purrr
  }

  # return the combined plot
  return(combined_object)
}
