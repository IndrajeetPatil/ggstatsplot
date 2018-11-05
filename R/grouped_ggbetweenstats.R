#' @title Violin plots for group or condition comparisons in between-subjects
#'   designs repeated across all levels of a grouping variable.
#' @name grouped_ggbetweenstats
#' @aliases grouped_ggbetweenstats
#' @description A combined plot of comparison plot created for levels of a
#'   grouping variable.
#' @author Indrajeet Patil
#'
#' @param grouping.var Grouping variable.
#' @param title.prefix Character specifying the prefix text for the fixed plot
#'   title (name of each factor level) (Default: `"Group"`).
#' @inheritParams ggbetweenstats
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
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom tidyr nest
#'
#'
#' @seealso \code{\link{ggbetweenstats}}
#'
#' @inherit ggbetweenstats return references
#' @inherit ggbetweenstats return details
#'
#' @examples
#' 
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#' 
#' # the most basic function call
#' ggstatsplot::grouped_ggbetweenstats(
#'   data = dplyr::filter(ggplot2::mpg, drv != "4"),
#'   x = year,
#'   y = hwy,
#'   grouping.var = drv,
#'   bf.message = TRUE
#' )
#' @export

# defining the function
grouped_ggbetweenstats <- function(data,
                                   x,
                                   y,
                                   grouping.var,
                                   title.prefix = "Group",
                                   plot.type = "boxviolin",
                                   type = "parametric",
                                   effsize.type = "unbiased",
                                   effsize.noncentral = FALSE,
                                   bf.prior = 0.707,
                                   bf.message = FALSE,
                                   results.subtitle = TRUE,
                                   xlab = NULL,
                                   ylab = NULL,
                                   subtitle = NULL,
                                   caption = NULL,
                                   sample.size.label = TRUE,
                                   k = 3,
                                   var.equal = FALSE,
                                   nboot = 100,
                                   tr = 0.1,
                                   mean.label.size = 3,
                                   mean.label.fontface = "bold",
                                   mean.label.color = "black",
                                   notch = FALSE,
                                   notchwidth = 0.5,
                                   linetype = "solid",
                                   outlier.tagging = NULL,
                                   outlier.label = NULL,
                                   outlier.label.color = "black",
                                   outlier.color = "black",
                                   outlier.coef = 1.5,
                                   mean.plotting = TRUE,
                                   mean.ci = FALSE,
                                   mean.size = 5,
                                   mean.color = "darkred",
                                   point.jitter.width = NULL,
                                   point.jitter.height = 0.2,
                                   point.dodge.width = 0.75,
                                   ggtheme = ggplot2::theme_bw(),
                                   ggstatsplot.layer = TRUE,
                                   package = "RColorBrewer",
                                   palette = "Dark2",
                                   direction = 1,
                                   messages = TRUE,
                                   ...) {
  # ======================== preparing dataframe ==========================

  if (!base::missing(outlier.label)) {
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
      ) %>%
      stats::na.omit(.)
  } else {
    df <- dplyr::select(
      .data = data,
      !!rlang::enquo(grouping.var),
      !!rlang::enquo(x),
      !!rlang::enquo(y)
    ) %>%
      dplyr::mutate(
        .data = .,
        title.text = !!rlang::enquo(grouping.var)
      ) %>%
      stats::na.omit(.)
  }

  # creating a nested dataframe
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
    dplyr::arrange(.data = ., !!rlang::enquo(grouping.var)) %>%
    dplyr::group_by(.data = ., !!rlang::enquo(grouping.var)) %>%
    tidyr::nest(data = .)

  # creating a list of plots
  if (!base::missing(outlier.label)) {
    plotlist_purrr <-
      df %>%
      dplyr::mutate(
        .data = .,
        plots = data %>%
          purrr::set_names(!!rlang::enquo(grouping.var)) %>%
          purrr::map(
            .x = .,
            .f = ~ ggstatsplot::ggbetweenstats(
              data = .,
              x = !!rlang::enquo(x),
              y = !!rlang::enquo(y),
              title = glue::glue("{title.prefix}: {as.character(.$title.text)}"),
              plot.type = plot.type,
              type = type,
              effsize.type = effsize.type,
              effsize.noncentral = effsize.noncentral,
              bf.prior = bf.prior,
              bf.message = bf.message,
              results.subtitle = results.subtitle,
              xlab = xlab,
              ylab = ylab,
              subtitle = subtitle,
              caption = caption,
              sample.size.label = sample.size.label,
              k = k,
              var.equal = var.equal,
              nboot = nboot,
              tr = tr,
              mean.label.size = mean.label.size,
              mean.label.fontface = mean.label.fontface,
              mean.label.color = mean.label.color,
              notch = notch,
              notchwidth = notchwidth,
              linetype = linetype,
              outlier.tagging = outlier.tagging,
              outlier.label = !!rlang::enquo(outlier.label),
              outlier.label.color = outlier.label.color,
              outlier.color = outlier.color,
              outlier.coef = outlier.coef,
              mean.plotting = mean.plotting,
              mean.ci = mean.ci,
              mean.size = mean.size,
              mean.color = mean.color,
              ggtheme = ggtheme,
              ggstatsplot.layer = ggstatsplot.layer,
              package = package,
              palette = palette,
              direction = direction,
              messages = messages,
              point.jitter.width = point.jitter.width,
              point.dodge.width = point.dodge.width,
              point.jitter.height = point.jitter.height
            )
          )
      )
  } else {
    plotlist_purrr <-
      df %>%
      dplyr::mutate(
        .data = .,
        plots = data %>%
          purrr::set_names(!!rlang::enquo(grouping.var)) %>%
          purrr::map(
            .x = .,
            .f = ~ ggstatsplot::ggbetweenstats(
              data = .,
              x = !!rlang::enquo(x),
              y = !!rlang::enquo(y),
              title = glue::glue("{title.prefix}: {as.character(.$title.text)}"),
              plot.type = plot.type,
              type = type,
              effsize.type = effsize.type,
              effsize.noncentral = effsize.noncentral,
              bf.prior = bf.prior,
              bf.message = bf.message,
              results.subtitle = results.subtitle,
              xlab = xlab,
              ylab = ylab,
              subtitle = subtitle,
              caption = caption,
              sample.size.label = sample.size.label,
              k = k,
              var.equal = var.equal,
              nboot = nboot,
              tr = tr,
              mean.label.size = mean.label.size,
              mean.label.fontface = mean.label.fontface,
              mean.label.color = mean.label.color,
              notch = notch,
              notchwidth = notchwidth,
              linetype = linetype,
              outlier.tagging = outlier.tagging,
              outlier.label.color = outlier.label.color,
              outlier.color = outlier.color,
              outlier.coef = outlier.coef,
              mean.plotting = mean.plotting,
              mean.ci = mean.ci,
              mean.size = mean.size,
              mean.color = mean.color,
              ggtheme = ggtheme,
              ggstatsplot.layer = ggstatsplot.layer,
              package = package,
              palette = palette,
              direction = direction,
              messages = messages,
              point.jitter.width = point.jitter.width,
              point.dodge.width = point.dodge.width,
              point.jitter.height = point.jitter.height
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

  # show the note about grouped_ variant producing object which is not of
  # class ggplot
  if (isTRUE(messages)) {
    grouped_message()
  }

  # return the combined plot
  return(combined_plot)
}
