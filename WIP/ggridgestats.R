#' @title Ridgeline plots for comparisons
#' @name ggridgestats
#'
#' @seealso
#' \code{\link{ggbetweenstats}}, \code{\link{grouped_ggbetweenstats}},
#' \code{\link{ggwithinstats}}, \code{\link{grouped_ggwithinstats}}
#'
#' @inheritParams ggbetweenstats
#' @inheritParams ggpiestats
#'
#' @examples
#'
#' # default function call
#' ggridgestats(mtcars, am, wt)
#' @export

ggridgestats <- function(data,
                         x,
                         y,
                         type = "parametric",
                         paired = FALSE,
                         pairwise.display = "significant",
                         p.adjust.method = "holm",
                         effsize.type = "unbiased",
                         bf.prior = 0.707,
                         bf.message = TRUE,
                         results.subtitle = TRUE,
                         xlab = NULL,
                         ylab = NULL,
                         caption = NULL,
                         title = NULL,
                         subtitle = NULL,
                         digits = 2L,
                         var.equal = FALSE,
                         conf.level = 0.95,
                         nboot = 100L,
                         tr = 0.2,
                         centrality.plotting = TRUE,
                         centrality.type = type,
                         centrality.point.args = list(
                           size = 5,
                           color = "darkred"
                         ),
                         centrality.label.args = list(
                           size = 3,
                           nudge_x = 0.4,
                           segment.linetype = 4,
                           min.segment.length = 0
                         ),
                         ggsignif.args = list(
                           textsize = 3,
                           tip_length = 0.01
                         ),
                         ggtheme = ggstatsplot::theme_ggstatsplot(),
                         package = "RColorBrewer",
                         palette = "Dark2",
                         ggplot.component = NULL,

                         ...) {
  # data -----------------------------------


  type <- statsExpressions::stats_type_switch(type)

  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))


  data %<>%
    dplyr::select({{ x }}, {{ y }}) %>%
    tidyr::drop_na() %>%
    dplyr::mutate({{ x }} := droplevels(as.factor({{ x }})))

  # statistical analysis ------------------------------------------

  # test to run; depends on the no. of levels of the independent variable
  test <- ifelse(nlevels(data %>% dplyr::pull({{ x }})) < 3, "t", "anova")

  if (results.subtitle) {

    .f.args <- list(
      data = data,
      x = rlang::as_string(x),
      y = rlang::as_string(y),
      paired = paired,
      effsize.type = effsize.type,
      conf.level = conf.level,
      var.equal = var.equal,
      digits = digits,
      tr = tr,
      bf.prior = bf.prior,
      nboot = nboot
    )

    .f <- .f_switch(test)
    subtitle_df <- .eval_f(.f, !!!.f.args, type = type)
    subtitle <- if (!is.null(subtitle_df)) subtitle_df$expression[[1L]]

    # Bayes factor message
    if (type == "parametric" && bf.message) {
      caption_df <- .eval_f(.f, !!!.f.args, type = "bayes")
      caption <- if (!is.null(caption_df)) caption_df$expression[[1L]]
    }
  }

  # plot -----------------------------------

  descriptive_df <- centrality_description(
    data,
    x = {{ x }},
    y = {{ y }},
    type = type,
    tr = tr,
    digits = digits
  )


  plot <- ggplot(data, aes(x = {{ y }}, y = {{ x }}, fill = {{ x }})) +
    ggridges::geom_density_ridges(
      # points
      jittered_points = TRUE,
      position = ggridges::position_raincloud(
        adjust_vlines = TRUE,
        width = 0.02,
        height = 0.2
      ),
      point_size = 2,
      point_alpha = 0.5,
      quantile_lines = TRUE,
      # density
      scale = 0.7,
      alpha = 0.5,
      # quantile lines
      vline_size = 1,
      vline_color = "red"
    ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_errorbarh(
      data = descriptive_df,
      mapping = ggplot2::aes(xmin = conf.low, xmax = conf.high),
      height = 0
    ) +
    ggplot2::geom_point(
      data = descriptive_df,
      mapping = ggplot2::aes(x = {{ y }}, y = {{ x }}),
      size = 3,
      color = "darkred"
    ) +
    ggrepel::geom_text_repel(
      data = descriptive_df,
      x = descriptive_df %>% dplyr::pull({{ y }}),
      y = descriptive_df %>% dplyr::pull({{ x }}),
      direction = "x",
      label = descriptive_df$expression[[1L]],
      # min.segment.length = 0,
      hjust = "right",
      segment.linetype = 4,
      inherit.aes = FALSE,
      parse = TRUE
    ) +
    ggplot2::scale_y_discrete(labels = c(unique(descriptive_df$n.expression))) +
    ggplot2::coord_flip()

  # ggsignif labels -------------------------------------

  if (pairwise.display != "none" && test == "anova") {
    # creating data frame with pairwise comparison results
    mpc_df <- pairwise_comparisons(
      data = data,
      x = {{ x }},
      y = {{ y }},
      type = type,
      tr = tr,
      paired = paired,
      var.equal = var.equal,
      p.adjust.method = p.adjust.method,
      digits = digits
    )

    # adding the layer for pairwise comparisons
    plot <- .ggsignif_adder(
      plot = plot,
      mpc_df = mpc_df,
      data = data,
      x = {{ x }},
      y = {{ y }},
      pairwise.display = pairwise.display,
      ggsignif.args = ggsignif.args,
      y_start_jitter = 0.2
    )

    # caption for pairwise comparisons test
    caption <- .pairwise_seclabel(
      caption,
      unique(mpc_df$test),
      ifelse(type == "bayes", "all", pairwise.display)
    )
  }

  # annotations ------------------------

  # specifying annotations and other aesthetic aspects for the plot
  .aesthetic_addon(
    plot = plot,
    x = data %>% dplyr::pull({{ x }}),
    xlab = xlab %||% rlang::as_name(x),
    ylab = ylab %||% rlang::as_name(y),
    title = title,
    subtitle = subtitle,
    caption = caption,
    ggtheme = ggtheme,
    package = package,
    palette = palette,
    ggplot.component = ggplot.component
  )
}
