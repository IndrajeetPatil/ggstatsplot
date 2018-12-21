#' @title Scatterplot with marginal distributions for all levels of a grouping
#'   variable
#' @name grouped_ggscatterstats
#' @aliases grouped_ggscatterstats
#' @author Indrajeet Patil, Chuck Powell
#' @description Grouped scatterplots from `ggplot2` combined with marginal
#'   histograms/boxplots/density plots with statistical details added as a
#'   subtitle.
#'
#' @param grouping.var Grouping variable. Can be entered either as
#'   a character string (e.g., `"group"`) or as a bare expression (e.g, `group`).
#' @param title.prefix Character specifying the prefix text for the fixed plot
#'   title (name of each factor level) (Default: `"Group"`).
#' @inheritParams ggscatterstats
#' @inheritDotParams combine_plots
#'
#' @import ggplot2
#'
#' @importFrom dplyr select bind_rows summarize mutate mutate_at mutate_if
#' @importFrom dplyr group_by n arrange
#' @importFrom rlang !! enquo quo_name ensym as_quosure parse_expr
#' @importFrom glue glue
#' @importFrom purrr map set_names
#' @importFrom tidyr nest
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
#'   bf.message = TRUE,
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
                                   bf.message = FALSE,
                                   label.var = NULL,
                                   label.expression = NULL,
                                   grouping.var,
                                   title.prefix = "Group",
                                   xlab = NULL,
                                   ylab = NULL,
                                   method = "lm",
                                   method.args = list(),
                                   formula = y ~ x,
                                   point.color = "black",
                                   point.size = 3,
                                   point.alpha = 0.4,
                                   line.size = 1.5,
                                   point.width.jitter = NULL,
                                   point.height.jitter = NULL,
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
                                   caption = NULL,
                                   subtitle = NULL,
                                   nboot = 100,
                                   beta = 0.1,
                                   k = 2,
                                   axes.range.restrict = FALSE,
                                   ggtheme = ggplot2::theme_bw(),
                                   ggstatsplot.layer = TRUE,
                                   messages = TRUE,
                                   ...) {

  # create a list of function call to check for label.expression
  param_list <- base::as.list(base::match.call())

  # check that there is a grouping.var
  if(!"grouping.var" %in% names(param_list)) {
    base::stop("You must specify a grouping variable")
  }

  # check that label.var and grouping.var are different
  if ("label.var" %in% names(param_list)) {
    if (as.character(param_list$label.var) == as.character(param_list$grouping.var)) {
      base::message(cat(
        crayon::red("\nError: "),
        crayon::blue(
          "Identical variable (",
          crayon::yellow(param_list$label.var),
          ") was used for both grouping and labeling, which is not allowed.\n"
        ),
        sep = ""
      ))
      base::return(base::invisible(param_list$label.var))
    }
  }

  # check labeling variable has been entered
  if ("label.var" %in% names(param_list)) {
    labelvar.present <- TRUE
    label.var <- rlang::ensym(label.var)
    label.var <- deparse(substitute(label.var))
  } else {
    labelvar.present <- FALSE
  }

  # check labeling expression has been specified
  if ("label.expression" %in% names(param_list)) {
    expression.present <- TRUE
  } else {
    expression.present <- FALSE
  }

  # ======================== preparing dataframe =============================

  # ensure the grouping variable works quoted or unquoted
  grouping.var <- rlang::ensym(grouping.var)

  # getting the dataframe ready
  df <-
    dplyr::select(
      .data = data,
      !!rlang::enquo(grouping.var),
      !!rlang::enquo(x),
      !!rlang::enquo(y),
      dplyr::everything()
    ) %>%
    dplyr::mutate(
      .data = .,
      title.text = !!rlang::enquo(grouping.var)
    )

  # creating dataframe per level of grouping
  df %<>%
    dplyr::mutate_if(
      .tbl = .,
      .predicate = purrr::is_bare_character,
      .funs = ~ as.factor(.)
    ) %>%
    dplyr::mutate_if(
      .tbl = .,
      .predicate = is.factor,
      .funs = ~ base::droplevels(.)
    ) %>%
    dplyr::filter(.data = ., !is.na(!!rlang::enquo(grouping.var))) %>%
    base::split(.[[rlang::quo_text(grouping.var)]])

  if (isTRUE(expression.present)) {
    if (typeof(param_list$label.expression) == "language") {
      # unquoted case
      label.expression <- rlang::enquo(label.expression)
    } else {
      # quoted case
      label.expression <- rlang::parse_expr(x = label.expression)
      # the environment is essential
      label.expression <- rlang::as_quosure(x = label.expression,
                                            env = base::sys.frame(which = 0))
    }
  }


  # ==================== build pmap list based on conditions =======================

  if (isTRUE(expression.present) && isTRUE(labelvar.present)) {
  flexiblelist <- list(data = df,
                   x = rlang::quo_text(enquo(x)),
                   y = rlang::quo_text(enquo(y)),
                   #  title = glue::glue("{title.prefix}: {rlang::quo_text(enquo(title.text))}"),
                   label.var = label.var,
                   label.expression = rlang::quo_text(label.expression))
  }

  if (isTRUE(expression.present) && isFALSE(labelvar.present)) {
    flexiblelist <- list(data = df,
                         x = rlang::quo_text(enquo(x)),
                         y = rlang::quo_text(enquo(y)),
                         #  title = glue::glue("{title.prefix}: {rlang::quo_text(enquo(title.text))}"),
                         label.expression = rlang::quo_text(label.expression))
  }

  if (isFALSE(expression.present) && isTRUE(labelvar.present)) {
    flexiblelist <- list(data = df,
                         x = rlang::quo_text(enquo(x)),
                         y = rlang::quo_text(enquo(y)),
                         #  title = glue::glue("{title.prefix}: {rlang::quo_text(enquo(title.text))}"),
                         label.var = label.var)
  }

  if (isFALSE(expression.present) && isFALSE(labelvar.present)) {
    flexiblelist <- list(data = df,
                         x = rlang::quo_text(enquo(x)),
                         y = rlang::quo_text(enquo(y))
                         #  title = glue::glue("{title.prefix}: {rlang::quo_text(enquo(title.text))}"),
                          )
  }


  # ==================== creating a list of plots =======================

      plotlist_purrr <-
            purrr::pmap(
              .l = flexiblelist, #end of lists
              .f = ggstatsplot::ggscatterstats,
                # put common parameters here
                type = type,
                conf.level = conf.level,
                bf.prior = bf.prior,
                bf.message = bf.message,
                title = title.prefix,
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
                caption = caption,
                subtitle = subtitle,
                nboot = nboot,
                beta = beta,
                k = k,
                axes.range.restrict = axes.range.restrict,
                ggtheme = ggplot2::theme_bw(),
                ggstatsplot.layer = ggstatsplot.layer,
                messages = messages
            )


  # combining the list of plots into a single plot
  combined_plot <-
    ggstatsplot::combine_plots(
      plotlist = plotlist_purrr,
      ...
    )

  # show the note about grouped_ variant producing object which is not of
  # class ggplot
  if (isTRUE(messages)) {
    grouped_message()
  }

  # return the combined plot
  return(combined_plot)
}
