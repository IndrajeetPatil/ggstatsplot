#' @title Custom function to set upper and lower margins to legend title in
#'   ggplot2
#' @name legend_title_margin
#' @aliases legend_title_margin
#' @return A plot with desired margins between the legend title and the legend.
#'
#' @param plot Plot with the legend title whose margins need to be modified.
#' @param t.margin,b.margin Margins in grid units.
#'
#' @import grid
#' @import ggplot2
#' @import gtable
#'
#' @importFrom cowplot ggdraw
#'
#' @keywords internal
#'
#' @note This is a helper function used internally in the package and not
#' exported. In case you want to use it, you can do so by
#' `ggstatsplot:::legend_title_margin`. Note that it is `:::` and not `::`.
#'

legend_title_margin <- function(plot,
                                t.margin = unit(0, "mm"),
                                b.margin = unit(3, "mm")) {
  # get the plot grob
  g <- ggplot2::ggplotGrob(x = plot)

  # get the legend
  index <- base::which(x = g$layout$name == "guide-box")
  leg <- g$grobs[[index]][[1]][[1]]

  # get the legend title
  title <- leg$grobs[[4]]

  # set up the heights: for the two margins and the original title
  # unit.c produces a new unit object by combining the unit objects specified as arguments
  heights <-
    grid::unit.c(t.margin,
                 grid::unit(x = 1, units = "grobheight", data = title),
                 b.margin)

  # set up a column of three viewports
  vp <- grid::viewport(
    layout = grid::grid.layout(
      nrow = 3,
      ncol = 1,
      heights = heights
    ),
    name = "vp1"
  )

  # the middle row, where the title text will appear, is named as 'child_vp'.
  child_vp <-
    grid::viewport(layout.pos.row = 2,
                   clip = "off",
                   name = "child_vp")

  # put the title into a gTree containing one grob (the title) and the three viewports
  TitleText <- grid::gTree(
    children = grid::gList(title),
    vp = grid::vpTree(parent = vp, children = grid::vpList(child_vp))
  )

  # back to the legend: Set height for row 2 of legend to new height of TitleText
  leg$heights[2] <- sum(heights)

  # Add the new TitleText grob to row 2 of legend
  leg <- gtable::gtable_add_grob(
    x = leg,
    grobs = TitleText,
    t = 2,
    l = 2,
    r = 5,
    name = "TitleText"
  )

  # remove the original title
  leg$grobs <- leg$grobs[-4]
  leg$layout <- leg$layout[-4, ]

  # put the legend back into the plot
  g$grobs[[index]][[1]][[1]] <- leg

  class(g) <- c("legend_title_margin", class(g))

  # draw the plot
  g <- cowplot::ggdraw(g)

  # return the final plot
  return(g)
}

#'
#' @title Default theme used for pie chart
#' @name theme_pie
#' @author Indrajeet Patil
#' #'
#' @return A `ggplot2` object with the `theme_mprl` theme.
#'
#' @import ggplot2
#' @import grid
#'
#' @keywords internal
#'
#' @note This is a helper function used internally in the package and not
#' exported. In case you want to use it, you can do so by
#' `ggstatsplot:::theme_pie`. Note that it is `:::` and not `::`.
#'

theme_pie <- function() {
  ggplot2::theme_grey() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(size = 14, face = "bold"),
      strip.text.y = ggplot2::element_text(size = 14, face = "bold"),
      strip.text = ggplot2::element_text(size = 14, face = "bold"),
      legend.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.title.align = 0.5,
      legend.text.align = 0.5,
      legend.direction = "horizontal",
      legend.position = "bottom",
      legend.key = ggplot2::element_rect(size = 5),
      legend.key.size = grid::unit(x = 1.5, units = "lines"),
      legend.margin = ggplot2::margin(
        t = 5,
        r = 5,
        b = 5,
        l = 5,
        unit = "pt"
      ),
      legend.box.margin = ggplot2::margin(
        t = 5,
        r = 5,
        b = 5,
        l = 5,
        unit = "pt"
      ),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        size = 1
      ),
      plot.subtitle = ggplot2::element_text(
        color = "black",
        size = 14,
        hjust = 0.5
      ),
      plot.title = ggplot2::element_text(
        color = "black",
        size = 16,
        face = "bold",
        hjust = 0.5
      )
    )
}
