#'
#' @title Violin plots for group or condition comparisons repeated across all
#'   levels of a grouping variable.
#' @name grouped_ggbetweenstats
#' @aliases grouped_ggbetweenstats
#' @description A combined plot of comparison plot created for levels of a
#'   grouping variable.
#' @author Indrajeet Patil
#'
#' @param grouping.var Grouping variable.
#' @param title.prefix Character specifying the prefix text for the fixed plot
#'   title (name of each factor level) (Default: `"Group"`).
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param x The grouping variable.
#' @param y The response - a vector of length the number of rows of `x`.
#' @param plot.type Character describing the *type* of plot. Currently supported
#'   plots are `"box"` (for pure boxplots), `"violin"` (for pure violin plots),
#'   and `"boxviolin"` (for a mix of box and violin plots; default).
#' @param xlab Label for `x` axis variable.
#' @param ylab Label for `y` axis variable.
#' @param type Type of statistic expected (`"parametric"` or `"nonparametric"`
#'   or `"robust"`).Corresponding abbreviations are also accepted: `"p"` (for
#'   parametric), `"np"` (nonparametric), `"r"` (robust), resp.
#' @param effsize.type Type of effect size needed for *parametric* tests
#'   (`"biased"` (Cohen's *d* for **t-test**; partial eta-squared for **anova**)
#'   or `"unbiased"` (Hedge's *g* for **t-test**; partial omega-squared for
#'   **anova**)).
#' @param caption The text for the plot caption.
#' @param k Number of decimal places expected for results.
#' @param var.equal A logical variable indicating whether to treat the two
#'   variances as being equal (Default: `FALSE`).
#' @param nboot Number of bootstrap samples for computing effect size (Default:
#'   `100`).
#' @param tr Trim level for the mean when carrying out `robust` tests. If you
#'   get error stating "Standard error cannot be computed because of Winsorized
#'   variance of 0 (e.g., due to ties). Try to decrease the trimming level.",
#'   try to play around with the value of `tr`, which is by default set to
#'   `0.1`. Lowering the value might help.
#' @param conf.type A vector of character strings representing the type of
#'   intervals required. The value should be any subset of the values `"norm"`,
#'   `"basic"`, `"perc"`, `"bca"`. For more, see `?boot::boot.ci`.
#' @param conf.level Scalar between 0 and 1. If `NULL`, the defaults return 95%
#'   lower and upper confidence intervals (`0.95`).
#' @param notch A logical. If `FALSE` (default), a standard box plot will be
#'   displayed. If `TRUE`, a notched box plot will be used. Notches are used to
#'   compare groups; if the notches of two boxes do not overlap, this suggests
#'   that the medians are significantly different. In a notched box plot, the
#'   notches extend 1.58 * IQR / sqrt(n). This gives a roughly 95% confidence
#'   interval for comparing medians. IQR: Inter-Quartile Range.
#' @param notchwidth For a notched box plot, width of the notch relative to the
#'   body (default `0.5`).
#' @param linetype Character strings (`"blank"`, `"solid"`, `"dashed"`,
#'   `"dotted"`, `"dotdash"`, `"longdash"`, and `"twodash"`) specifying the
#'   type of line to draw box plots (Default: `"solid"`). Alternatively, the
#'   numbers `0` to `6` can be used (`0` for "blank", `1` for "solid", etc.).
#' @param outlier.color Default aesthetics for outliers (Default: `"black"`).
#' @param outlier.tagging Decides whether outliers should be tagged (Default:
#'   `FALSE`).
#' @param outlier.label Label to put on the outliers that have been tagged.
#' @param outlier.label.color Color for the label to to put on the outliers that
#'   have been tagged (Default: `"black"`).
#' @param outlier.coef Coefficient for outlier detection using Tukey's method.
#'   With Tukey's method, outliers are below (1st Quartile) or above (3rd
#'   Quartile) `outlier.coef` times the Inter-Quartile Range (IQR) (Default:
#'   `1.5`).
#' @param mean.plotting Decides whether mean is to be highlighted and its value
#'   to be displayed (Default: `TRUE`).
#' @param mean.color Color for the data point corresponding to mean (Default:
#'   `"darkred"`).
#' @param mean.size Point size for the data point corresponding to mean
#'   (Default: `5`).
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
#' @importFrom ggrepel geom_label_repel
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @importFrom WRS2 t1way
#' @importFrom WRS2 yuen
#' @importFrom WRS2 yuen.effect.ci
#' @importFrom effsize cohen.d
#' @importFrom sjstats eta_sq
#' @importFrom sjstats omega_sq
#' @importFrom stats aov
#' @importFrom stats na.omit
#' @importFrom stats t.test
#' @importFrom stats var.test
#' @importFrom stats bartlett.test
#' @importFrom stats kruskal.test
#' @importFrom stats aov
#' @importFrom stats quantile
#' @importFrom stats oneway.test
#' @importFrom coin wilcox_test
#' @importFrom coin statistic
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom ggrepel geom_label_repel
#' @importFrom crayon green
#' @importFrom crayon blue
#' @importFrom crayon yellow
#' @importFrom crayon red
#' @importFrom jmv anova
#'
#' @seealso \code{\link{ggbetweenstats}}
#'
#' @examples
#'
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#'
#' # the most basic function call
#' ggstatsplot::grouped_ggbetweenstats(
#' data = mtcars,
#' x = cyl,
#' y = wt,
#' grouping.var = am
#' )
#'
#' @export
#'

# defining the function
grouped_ggbetweenstats <- function(grouping.var,
                                   title.prefix = "Group",
                                   data = NULL,
                                   x,
                                   y,
                                   plot.type = "boxviolin",
                                   type = "parametric",
                                   effsize.type = "unbiased",
                                   xlab = NULL,
                                   ylab = NULL,
                                   caption = NULL,
                                   k = 3,
                                   var.equal = FALSE,
                                   nboot = 100,
                                   tr = 0.1,
                                   conf.level = 0.95,
                                   conf.type = "norm",
                                   notch = FALSE,
                                   notchwidth = 0.5,
                                   linetype = "solid",
                                   outlier.tagging = NULL,
                                   outlier.label = NULL,
                                   outlier.label.color = "black",
                                   outlier.color = "black",
                                   outlier.coef = 1.5,
                                   mean.plotting = TRUE,
                                   mean.size = 5,
                                   mean.color = "darkred",
                                   messages = TRUE,
                                   ...) {
  # ================== preparing dataframe ==================

  if (!base::missing(outlier.label)) {
    # getting the dataframe ready
    df <- dplyr::select(
      .data = data,
      !!rlang::enquo(grouping.var),
      !!rlang::enquo(x),
      !!rlang::enquo(y),
      !!rlang::enquo(outlier.label)
    ) %>%
      dplyr::mutate(
        .data = .,
        title.text = !!rlang::enquo(grouping.var)
      )
  } else {
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
  }

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

  if (!base::missing(outlier.label)) {
    # creating a list of plots
    plotlist_purrr <- df %>%
      dplyr::mutate(
        .data = .,
        plots = data %>%
          purrr::set_names(!!rlang::enquo(grouping.var)) %>%
          purrr::map(
            .x = .,
            .f = ~ggstatsplot::ggbetweenstats(
              data = .,
              x = !!rlang::enquo(x),
              y = !!rlang::enquo(y),
              title = glue::glue("{title.prefix}: {as.character(.$title.text)}"),
              plot.type = plot.type,
              type = type,
              effsize.type = effsize.type,
              xlab = xlab,
              ylab = ylab,
              caption = caption,
              k = k,
              var.equal = var.equal,
              nboot = nboot,
              tr = tr,
              conf.level = conf.level,
              conf.type = conf.type,
              notch = notch,
              notchwidth = notchwidth,
              linetype = linetype,
              outlier.tagging = outlier.tagging,
              outlier.label = !!rlang::enquo(outlier.label),
              outlier.label.color = outlier.label.color,
              outlier.color = outlier.color,
              outlier.coef = outlier.coef,
              mean.plotting = mean.plotting,
              mean.size = mean.size,
              mean.color = mean.color,
              messages = messages
            )
          )
      )
  } else {
    # creating a list of plots
    plotlist_purrr <- df %>%
      dplyr::mutate(
        .data = .,
        plots = data %>%
          purrr::set_names(!!rlang::enquo(grouping.var)) %>%
          purrr::map(
            .x = .,
            .f = ~ggstatsplot::ggbetweenstats(
              data = .,
              x = !!rlang::enquo(x),
              y = !!rlang::enquo(y),
              title = glue::glue("{title.prefix}: {as.character(.$title.text)}"),
              plot.type = plot.type,
              type = type,
              effsize.type = effsize.type,
              xlab = xlab,
              ylab = ylab,
              caption = caption,
              k = k,
              var.equal = var.equal,
              nboot = nboot,
              tr = tr,
              conf.level = conf.level,
              conf.type = conf.type,
              notch = notch,
              notchwidth = notchwidth,
              linetype = linetype,
              outlier.tagging = outlier.tagging,
              outlier.label.color = outlier.label.color,
              outlier.color = outlier.color,
              outlier.coef = outlier.coef,
              mean.plotting = mean.plotting,
              mean.size = mean.size,
              mean.color = mean.color,
              messages = messages
            )
          )
      )
  }

  # combining the list of plots into a single plot
  combined_plot <-
    ggstatsplot::combine_plots(
      plotlist = plotlist_purrr$plots,
      ...
    )

  # return the combined plot
  return(combined_plot)
}
