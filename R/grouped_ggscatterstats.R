#'
#' @title Scatterplot with marginal distributions for all levels of a grouping
#'   variable
#' @name grouped_ggscatterstats
#' @aliases grouped_ggscatterstats
#' @author Indrajeet Patil
#' @description Grouped scatterplots from `ggplot2` combined with marginal
#'   histograms/boxplots/density plots with statistical details added as a
#'   subtitle.
#'
#' @param grouping.var Grouping variable.
#' @param title.prefix Character specifying the prefix text for the fixed plot
#'   title (name of each factor level) (Default: `"Group"`).
#' @inheritParams ggscatterstats
#' @inheritDotParams combine_plots
#'
#' @import ggplot2
#'
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr mutate_if
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom purrr set_names
#'
#' @seealso \code{\link{ggscatterstats}} \code{\link{ggcorrmat}} \code{\link{grouped_ggcorrmat}}
#'
#' @references
#' \url{https://cran.r-project.org/web/packages/ggstatsplot/vignettes/ggscatterstats.html}
#'
#' @examples
#' 
#' # to ensure reproducibility
#' set.seed(123)
#' 
#' # basic functio call
#' ggstatsplot::grouped_ggscatterstats(
#'   data = datasets::iris,
#'   x = Sepal.Length,
#'   y = Sepal.Width,
#'   grouping.var = Species,
#'   messages = FALSE
#' )
#' @export
#'

# defining the function
grouped_ggscatterstats <- function(data,
                                   x,
                                   y,
                                   grouping.var,
                                   title.prefix = "Group",
                                   xlab = NULL,
                                   ylab = NULL,
                                   method = "lm",
                                   method.args = list(),
                                   formula = y ~ x,
                                   line.size = 1.5,
                                   line.color = "blue",
                                   marginal = TRUE,
                                   marginal.type = "histogram",
                                   marginal.size = 5,
                                   margins = c("both", "x", "y"),
                                   width.jitter = NULL,
                                   height.jitter = NULL,
                                   xfill = "#009E73",
                                   yfill = "#D55E00",
                                   xalpha = 1,
                                   yalpha = 1,
                                   xsize = 0.7,
                                   ysize = 0.7,
                                   centrality.para = NULL,
                                   type = "pearson",
                                   results.subtitle = TRUE,
                                   caption = NULL,
                                   subtitle = NULL,
                                   nboot = 100,
                                   beta = 0.1,
                                   k = 3,
                                   axes.range.restrict = FALSE,
                                   ggtheme = ggplot2::theme_bw(),
                                   ggstatsplot.layer = TRUE,
                                   messages = TRUE,
                                   ...) {
  # ========================================= preparing dataframe =======================================================

  # getting the dataframe ready
  df <- dplyr::select(
    .data = data,
    !!rlang::enquo(grouping.var),
    !!rlang::enquo(x),
    !!rlang::enquo(y)
  ) %>%
    dplyr::mutate(
      .data = .,
      title.text = !!rlang::enquo(grouping.var)
    )

  # creating a nested dataframe
  df %<>%
    dplyr::mutate_if(
      .tbl = .,
      .predicate = purrr::is_bare_character,
      .funs = ~as.factor(.)
    ) %>%
    dplyr::mutate_if(
      .tbl = .,
      .predicate = is.factor,
      .funs = ~base::droplevels(.)
    ) %>%
    dplyr::arrange(.data = ., !!rlang::enquo(grouping.var)) %>%
    dplyr::group_by(.data = ., !!rlang::enquo(grouping.var)) %>%
    tidyr::nest(data = .)

  # creating a list of plots
  plotlist_purrr <- df %>%
    dplyr::mutate(
      .data = .,
      plots = data %>%
        purrr::set_names(!!rlang::enquo(grouping.var)) %>%
        purrr::map(
          .x = .,
          .f = ~ggstatsplot::ggscatterstats(
            data = .,
            x = !!rlang::enquo(x),
            y = !!rlang::enquo(y),
            title = glue::glue("{title.prefix}: {as.character(.$title.text)}"),
            xlab = xlab,
            ylab = ylab,
            method = method,
            method.args = method.args,
            formula = formula,
            line.size = line.size,
            line.color = line.color,
            marginal = marginal,
            marginal.type = marginal.type,
            marginal.size = marginal.size,
            margins = margins,
            width.jitter = width.jitter,
            height.jitter = height.jitter,
            xfill = xfill,
            yfill = yfill,
            xalpha = xalpha,
            yalpha = yalpha,
            xsize = xsize,
            ysize = ysize,
            centrality.para = centrality.para,
            type = type,
            results.subtitle = results.subtitle,
            subtitle = subtitle,
            caption = caption,
            nboot = nboot,
            beta = beta,
            k = k,
            axes.range.restrict = axes.range.restrict,
            ggtheme = ggtheme,
            ggstatsplot.layer = ggstatsplot.layer,
            messages = messages
          )
        )
    )

  # combining the list of plots into a single plot
  combined_plot <-
    ggstatsplot::combine_plots(
      plotlist = plotlist_purrr$plots,
      ...
    )

  # return the combined plot
  return(combined_plot)
}
