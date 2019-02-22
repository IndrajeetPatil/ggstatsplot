#' @title Making aesthetic modifications to the plot.
#' @name aesthetic_addon
#' @author Indrajeet Patil
#'
#' @param plot Plot to be aesthetically modified.
#' @param x A numeric vector for `x` axis.
#' @inheritParams ggbetweenstats
#' @param ... Additional arguments.
#'
#' @keywords internal

aesthetic_addon <- function(plot,
                            x,
                            xlab = NULL,
                            ylab = NULL,
                            title = NULL,
                            subtitle = NULL,
                            caption = NULL,
                            ggtheme = ggplot2::theme_bw(),
                            ggstatsplot.layer = TRUE,
                            package = "RColorBrewer",
                            palette = "Dark2",
                            direction = 1,
                            ggplot.component = NULL,
                            ...) {
  ellipsis::check_dots_used()

  # if no. of factor levels is greater than the default palette color count
  palette_message(
    package = package,
    palette = palette,
    min_length = length(unique(levels(x)))[[1]]
  )

  # modifying the plot
  plot <- plot +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      color = xlab
    ) +
    ggstatsplot::theme_mprl(
      ggtheme = ggtheme,
      ggstatsplot.layer = ggstatsplot.layer
    ) +
    ggplot2::theme(legend.position = "none") +
    paletteer::scale_color_paletteer_d(
      package = !!package,
      palette = !!palette,
      direction = direction
    ) +
    paletteer::scale_fill_paletteer_d(
      package = !!package,
      palette = !!palette,
      direction = direction
    )

  # ---------------- adding ggplot component ---------------------------------

  # if any additional modification needs to be made to the plot
  # this is primarily useful for grouped_ variant of this function
  plot <- plot + ggplot.component

  # return the changed plot
  return(plot)
}
