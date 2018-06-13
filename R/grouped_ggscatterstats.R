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
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param x A vector containing the explanatory variable.
#' @param y The response - a vector of length the number of rows of `x`.
#' @param xlab Label for `x` axis variable.
#' @param ylab Label for `y` axis variable.
#' @param line.color color for the regression line.
#' @param line.size Size for the regression line.
#' @param marginal Decides whether `ggExtra::ggMarginal()` plots will be
#'   displayed; the default is `TRUE`.
#' @param marginal.type Type of marginal distribution to be plotted on the axes
#'   (`"histogram"`, `"boxplot"`, `"density"`, `"violin"`).
#' @param marginal.size Integer describing the relative size of the marginal
#'   plots compared to the main plot. A size of `5` means that the main plot is
#'   5x wider and 5x taller than the marginal plots.
#' @param margins Character describing along which margins to show the plots.
#'   Any of the following arguments are accepted: `"both"`, `"x"`, `"y"`.
#' @param xfill color fill for x axis distribution (default: `"#009E73"`).
#' @param yfill color fill for y axis distribution (default: `"#D55E00"`).
#' @param type Type of association between paired samples required
#'   ("`"parametric"`: Pearson's product moment correlation coefficient" or
#'   "`"nonparametric"`: Spearman's rho" or "`"robust"`: Robust regression using
#'   an M estimator"). Corresponding abbreviations are also accepted: `"p"` (for
#'   parametric/pearson's), `"np"` (nonparametric/spearman), `"r"` (robust),
#'   resp.
#' @param results.subtitle Decides whether the results of statistical tests are
#'   to be displayed as subtitle.
#' @param centrality.para Decides *which* measure of central tendency (`"mean"`
#'   or `"median"`) is to be displayed as vertical (for `x`) and horizontal (for
#'   `y`) lines.
#' @param caption The text for the plot caption.
#' @param maxit Maximum number of iterations for robust linear regression or
#'   bootstrap samples to compute Spearman's rho confidence intervals (Default:
#'   `500`).
#' @param k Number of decimal places expected for results.
#' @param width.jitter Degree of jitter in `x` direction. Defaults to 40\% of
#'   the resolution of the data.
#' @param height.jitter Degree of jitter in `y` direction. Defaults to 40\% of
#'   the resolution of the data.
#' @param axes.range.restrict Logical decides whether to restrict the axes values
#'   ranges to min and max values of the `x` and `y` variables (Default: `FALSE`).
#' @param ggtheme A function, `ggplot2` theme name. Default value is
#'   `ggplot2::theme_grey()`. Allowed values are the official `ggplot2` themes,
#'   including `theme_bw()`, `theme_minimal()`, `theme_classic()`,
#'   `theme_void()`, etc.
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
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
#' @importFrom MASS rlm
#' @importFrom sfsmisc f.robftest
#' @importFrom broom bootstrap
#' @importFrom broom tidy
#' @importFrom ggExtra ggMarginal
#' @importFrom stats cor.test
#' @importFrom stats na.omit
#' @importFrom stats confint.default
#'
#' @seealso \code{\link{ggscatterstats}} \code{\link{ggcorrmat}} \code{\link{grouped_ggcorrmat}}
#'
#' @examples
#'
#' # to ensure reproducibility
#' set.seed(123)
#'
#' # basic functio call
#' ggstatsplot::grouped_ggscatterstats(
#'  data = datasets::iris,
#'  x = Sepal.Length,
#'  y = Sepal.Width,
#'  grouping.var = Species,
#'  messages = FALSE
#' )
#'
#' @export
#'

# defining the function
grouped_ggscatterstats <- function(grouping.var,
                                   title.prefix = "Group",
                                   data,
                                   x,
                                   y,
                                   xlab = NULL,
                                   ylab = NULL,
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
                                   centrality.para = NULL,
                                   type = "pearson",
                                   results.subtitle = NULL,
                                   caption = NULL,
                                   maxit = 500,
                                   k = 3,
                                   axes.range.restrict = FALSE,
                                   ggtheme = ggplot2::theme_grey(),
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
            centrality.para = centrality.para,
            type = type,
            results.subtitle = results.subtitle,
            caption = caption,
            maxit = maxit,
            k = k,
            axes.range.restrict = axes.range.restrict,
            ggtheme = ggtheme,
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
