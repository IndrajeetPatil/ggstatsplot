#' @title Adding labels for mean values.
#' @name centrality_ggrepel
#'
#' @param plot A `ggplot` object for which means are to be displayed.
#' @param ... Additional arguments.
#' @inheritParams ggbetweenstats
#' @inheritParams ggwithinstats
#' @inheritParams ggrepel::geom_label_repel
#'
#' @importFrom ggrepel geom_label_repel
#' @importFrom rlang !! enquo ensym exec
#' @importFrom insight standardize_names format_value
#' @importFrom statsExpressions format_num centrality_description
#'
#' @examples
#' # this internal function may not have much utility outside of the package
#' set.seed(123)
#' library(ggplot2)
#'
#' # make a plot
#' p <- ggplot(data = iris, aes(x = Species, y = Sepal.Length)) +
#'   geom_boxplot()
#'
#' # add means
#' ggstatsplot:::centrality_ggrepel(
#'   data = iris,
#'   plot = p,
#'   x = Species,
#'   y = Sepal.Length
#' )
#' @noRd

# function body
centrality_ggrepel <- function(plot,
                               data,
                               x,
                               y,
                               centrality.path = FALSE,
                               centrality.path.args = list(
                                 color = "red",
                                 size = 1,
                                 alpha = 0.5
                               ),
                               centrality.point.args = list(size = 5, color = "darkred"),
                               centrality.label.args = list(
                                 size = 3,
                                 nudge_x = 0.4,
                                 segment.linetype = 4,
                                 min.segment.length = 0
                               ),
                               ...) {
  # creating the dataframe
  centrality_df <- suppressWarnings(centrality_description(data, {{ x }}, {{ y }}, ...))

  # if there should be lines connecting mean values across groups
  if (isTRUE(centrality.path)) {
    plot <- plot +
      rlang::exec(
        ggplot2::geom_path,
        data = centrality_df,
        mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}, group = 1),
        inherit.aes = FALSE,
        !!!centrality.path.args
      )
  }

  # highlight the mean of each group
  plot +
    rlang::exec(
      ggplot2::geom_point,
      mapping = ggplot2::aes({{ x }}, {{ y }}),
      data = centrality_df,
      inherit.aes = FALSE,
      !!!centrality.point.args
    ) + # attach the labels with means to the plot
    rlang::exec(
      ggrepel::geom_label_repel,
      data = centrality_df,
      mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}, label = expression),
      inherit.aes = FALSE,
      parse = TRUE,
      !!!centrality.label.args
    ) + # adding sample size labels to the x axes
    ggplot2::scale_x_discrete(labels = c(unique(centrality_df$n_label)))
}

#' @title Adding `geom_signif` to `ggplot`
#' @name ggsignif_adder
#'
#' @param ... Currently ignored.
#' @param plot A `ggplot` object on which `geom_signif` needed to be added.
#' @param mpc_df A dataframe containing results from pairwise comparisons
#'   (produced by `pairwiseComparisons::pairwise_comparisons()` function).
#' @inheritParams ggbetweenstats
#'
#' @importFrom purrr pmap
#' @importFrom dplyr mutate filter arrange pull
#' @importFrom ggsignif geom_signif
#'
#' @examples
#' set.seed(123)
#' library(ggplot2)
#'
#' # plot
#' p <- ggplot(iris, aes(Species, Sepal.Length)) +
#'   geom_boxplot()
#'
#' # dataframe with pairwise comparison test results
#' df_pair <- pairwiseComparisons::pairwise_comparisons(
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length
#' )
#'
#' # adding a geom for pairwise comparisons
#' ggstatsplot:::ggsignif_adder(
#'   plot = p,
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   mpc_df = df_pair
#' )
#' @noRd

ggsignif_adder <- function(plot,
                           data,
                           x,
                           y,
                           mpc_df,
                           pairwise.display = "significant",
                           ggsignif.args = list(textsize = 3, tip_length = 0.01),
                           ...) {
  # creating a column for group combinations
  mpc_df %<>% dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c))

  # for Bayes Factor, there will be no "p.value" column
  if ("p.value" %in% names(mpc_df)) {
    # decide what needs to be displayed
    if (grepl("^s", pairwise.display)) mpc_df %<>% dplyr::filter(p.value < 0.05)
    if (grepl("^n", pairwise.display)) mpc_df %<>% dplyr::filter(p.value >= 0.05)

    # proceed only if there are any significant comparisons to display
    if (dim(mpc_df)[[1]] == 0L) {
      return(plot)
    }
  }

  # arrange the dataframe so that annotations are properly aligned
  mpc_df %<>% dplyr::arrange(group1, group2)

  # adding ggsignif comparisons to the plot
  plot +
    rlang::exec(
      ggsignif::geom_signif,
      comparisons = mpc_df$groups,
      map_signif_level = TRUE,
      y_position = ggsignif_xy(
        data %>% dplyr::pull({{ x }}),
        data %>% dplyr::pull({{ y }})
      ),
      annotations = mpc_df$label,
      test = NULL,
      parse = TRUE,
      !!!ggsignif.args
    )
}

#' @name ggsignif_xy
#' @importFrom utils combn
#'
#' @inheritParams ggbetweenstats
#'
#' @keywords internal
#' @noRd

ggsignif_xy <- function(x, y) {
  # number of comparisons
  n_comparions <- length(utils::combn(x = unique(x), m = 2L, simplify = FALSE))

  # start position on `y`-axis for the `ggsignif` lines
  y_start <- max(y, na.rm = TRUE) * (1 + 0.025)

  # steps in which the y values need to increase
  step_length <- (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)) / 20

  # end position on `y`-axis for the `ggsignif` lines
  y_end <- y_start + (step_length * n_comparions)

  # creating a vector of positions for the `ggsignif` lines
  seq(y_start, y_end, length.out = n_comparions)
}


#' @title Making aesthetic modifications to the plot
#' @name aesthetic_addon
#'
#' @param plot Plot to be aesthetically modified.
#' @param x A numeric vector for `x` axis.
#' @inheritParams ggbetweenstats
#' @param ... Additional arguments.
#'
#' @noRd

aesthetic_addon <- function(plot,
                            x,
                            xlab = NULL,
                            ylab = NULL,
                            title = NULL,
                            subtitle = NULL,
                            caption = NULL,
                            ggtheme = ggstatsplot::theme_ggstatsplot(),
                            package = "RColorBrewer",
                            palette = "Dark2",
                            ggplot.component = NULL,
                            ...) {
  # if no. of factor levels is greater than the default palette color count
  palette_message(package, palette, length(unique(levels(x)))[[1]])

  # modifying the plot
  plot +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      color = xlab
    ) +
    ggtheme +
    ggplot2::theme(legend.position = "none") +
    paletteer::scale_color_paletteer_d(paste0(package, "::", palette)) +
    ggplot.component
}


#' @title Adding a column to dataframe describing outlier status
#' @name outlier_df
#'
#' @inheritParams ggbetweenstats
#' @param ... Additional arguments.
#'
#' @return The dataframe entered as `data` argument is returned with two
#'   additional columns: `isanoutlier` and `outlier` denoting which observation
#'   are outliers and their corresponding labels.
#'
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom statsExpressions %$%
#' @importFrom performance check_outliers
#'
#' @examples
#' # adding column for outlier and a label for that outlier
#' ggstatsplot:::outlier_df(
#'   data = morley,
#'   x = Expt,
#'   y = Speed,
#'   outlier.label = Run,
#'   outlier.coef = 2
#' ) %>%
#'   dplyr::arrange(outlier)
#' @noRd

# add a logical column indicating whether a point is or isn't an outlier
outlier_df <- function(data, x, y, outlier.label, outlier.coef = 1.5, ...) {
  dplyr::group_by(data, {{ x }}) %>%
    dplyr::mutate(
      isanoutlier = ifelse((.) %$% as.vector(performance::check_outliers({{ y }},
        method = "iqr", threshold = list("iqr" = outlier.coef)
      )), TRUE, FALSE),
      outlier = ifelse(isanoutlier, {{ outlier.label }}, NA)
    ) %>%
    dplyr::ungroup(.)
}


#' @title Switch expression making function
#' @name function_switch
#'
#' @param test Decides which test to run (can be either `"t"` or
#'   `"anova"`).
#' @param element Which expression is needed (`"subtitle"` or `"caption"`)
#' @param ... Arguments passed to respective subtitle helper functions.
#'
#' @importFrom statsExpressions two_sample_test oneway_anova
#' @importFrom rlang exec
#'
#' @noRd

function_switch <- function(test, element, ...) {
  # which function?
  if (test == "t") .f <- statsExpressions::two_sample_test
  if (test == "anova") .f <- statsExpressions::oneway_anova

  # return it
  .f
}

#' @title Message if palette doesn't have enough number of colors.
#' @name palette_message
#' @description Informs the user about not using the default color palette
#'   when the number of factor levels is greater than 8, the maximum number of
#'   colors allowed by `"Dark2"` palette from the `RColorBrewer` package.
#'
#' @importFrom dplyr filter select
#' @importFrom rlang !!
#' @importFrom statsExpressions %$%
#'
#' @noRd

# function body
palette_message <- function(package, palette, min_length) {
  # computing the palette length
  dplyr::filter(paletteer::palettes_d_names, package == !!package, palette == !!palette) %$%
    length[[1]] -> pl

  # check if insufficient number of colors are available in a given palette
  pl_message <- ifelse(pl < min_length, FALSE, TRUE)

  # inform the user
  if (isFALSE(pl_message)) {
    message(cat(
      "Warning: Number of labels is greater than default palette color count.\n",
      "Try using another color `palette` (and/or `package`).\n"
    ))
  }

  invisible(pl_message)
}
