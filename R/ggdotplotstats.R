#' @title Dot chart for labelled numeric data.
#' @name ggdotplotstats
#' @aliases ggdotplotstats
#' @description A dot chart with statistical details from one-sample test
#'   included in the plot as a subtitle.
#' @author Indrajeet Patil
#'
#' @param x A numeric variable.
#' @param y Label or grouping variable.
#' @param xlab Label for `x` axis variable.
#' @param ylab Label for `y` axis variable.
#' @param summarize Logical that decides whether `x` is to be summarized by a
#'   grouping variable `y` (Default: `TRUE`).
#' @inheritParams gghistostats
#'
#' @examples
#' ggdotplotstats(
#'   data = ggplot2::mpg,
#'   x = cty,
#'   y = manufacturer
#' )
#' @export

# function body
ggdotplotstats <- function(data,
                           x,
                           y,
                           summarize = TRUE,
                           xlab = NULL,
                           ylab = NULL,
                           title = NULL,
                           subtitle = NULL,
                           caption = NULL,
                           ggtheme = ggplot2::theme_bw(),
                           ggstatsplot.layer = TRUE) {
  # ------------------------------ variable names ----------------------------

  # preparing a dataframe with variable names
  lab.df <- colnames(x = dplyr::select(
    .data = data,
    !!rlang::enquo(x),
    !!rlang::enquo(y)
  ))

  # if `xlab` is not provided, use the variable `x` name
  if (is.null(xlab)) {
    xlab <- lab.df[1]
  }

  # if `ylab` is not provided, use the variable `y` name
  if (is.null(ylab)) {
    ylab <- lab.df[2]
  }

  # --------------------------- data preparation ----------------------------

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    ) %>%
    dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
    tibble::as.tibble(x = .)

  # if the data hasn't already been summarized, do so
  if (isTRUE(summarize)) {
    data %<>%
      dplyr::group_by(.data = ., y) %>%
      dplyr::summarise(.data = ., x = mean(x = x, na.rm = TRUE)) %>%
      dplyr::ungroup(x = .)
  }

  # rank ordering the data
  data %<>%
    dplyr::arrange(.data = ., x) %>%
    dplyr::mutate(
      .data = .,
      y = factor(x = y, levels = .$y)
    ) %>%
    dplyr::mutate(
      .data = .,
      percent_rank = (trunc(rank(x)) / length(x)) * 100,
      rank = 1:nrow(.)
    )

  # ------------------------------ basic plot ----------------------------

  # creating the basic plot
  plot <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(x = x, y = rank)
  ) +
    ggplot2::geom_point(size = 3, color = "black", na.rm = TRUE) +
    ggplot2::scale_y_continuous(
      name = ylab,
      labels = data$y,
      breaks = data$rank,
      sec.axis = ggplot2::dup_axis(
        name = "percentile",
        breaks = seq(
          from = 1,
          to = nrow(data),
          by = (nrow(data) - 1) / 4
        ),
        labels = 25 * 0:4
      )
    ) +
    ggplot2::scale_x_continuous(
      name = xlab,
      sec.axis = ggplot2::dup_axis(name = ggplot2::element_blank())
    ) +
    ggplot2::geom_vline(
      xintercept = mean(x = data$x, na.rm = TRUE),
      # linetype = centrality.linetype,
      # color = centrality.color,
      # size = centrality.size,
      na.rm = TRUE
    )

  # ------------------------ annotations and themes -------------------------

  # specifying theme and labels for the final plot
  plot <- plot +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    ggstatsplot::theme_ggstatsplot(
      ggtheme = ggtheme,
      ggstatsplot.layer = ggstatsplot.layer
    ) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_line(
        color = "black",
        size = 0.1,
        linetype = "dashed"
      )
    )

  # return the plot
  return(plot)
}
