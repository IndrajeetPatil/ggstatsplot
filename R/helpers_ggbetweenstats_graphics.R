#' @title Adding labels for mean values.
#' @name centrality_ggrepel
#'
#' @param plot A `ggplot` object for which means are to be displayed.
#' @param ... Additional arguments.
#' @inheritParams ggbetweenstats
#' @inheritParams ggwithinstats
#' @inheritParams ggrepel::geom_label_repel
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
                               ...) {
  # creating the dataframe
  centrality_df <- suppressWarnings(centrality_description(data, {{ x }}, {{ y }}, ...))

  # if there should be lines connecting mean values across groups
  if (isTRUE(centrality.path)) {
    plot <- plot +
      exec(
        geom_path,
        data = centrality_df,
        mapping = aes(x = {{ x }}, y = {{ y }}, group = 1),
        inherit.aes = FALSE,
        !!!centrality.path.args
      )
  }

  # highlight the mean of each group
  plot +
    exec(
      geom_point,
      mapping = aes({{ x }}, {{ y }}),
      data = centrality_df,
      inherit.aes = FALSE,
      !!!centrality.point.args
    ) + # attach the labels with means to the plot
    exec(
      ggrepel::geom_label_repel,
      data = centrality_df,
      mapping = aes(x = {{ x }}, y = {{ y }}, label = expression),
      inherit.aes = FALSE,
      parse = TRUE,
      !!!centrality.label.args
    ) + # adding sample size labels to the x axes
    scale_x_discrete(labels = c(unique(centrality_df$n_label)))
}

#' @title Adding `geom_signif` to `ggplot`
#' @name ggsignif_adder
#'
#' @param ... Currently ignored.
#' @param plot A `ggplot` object on which `geom_signif` needed to be added.
#' @param mpc_df A dataframe containing results from pairwise comparisons
#'   (produced by `pairwise_comparisons()` function).
#' @inheritParams ggbetweenstats
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
#' df_pair <- pairwise_comparisons(
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
  mpc_df %<>% mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c))

  # for Bayes Factor, there will be no "p.value" column
  if ("p.value" %in% names(mpc_df)) {
    # decide what needs to be displayed
    if (grepl("^s", pairwise.display)) mpc_df %<>% filter(p.value < 0.05)
    if (grepl("^n", pairwise.display)) mpc_df %<>% filter(p.value >= 0.05)

    # proceed only if there are any significant comparisons to display
    if (dim(mpc_df)[[1]] == 0L) {
      return(plot)
    }
  }

  # arrange the dataframe so that annotations are properly aligned
  mpc_df %<>% arrange(group1, group2)

  # adding ggsignif comparisons to the plot
  plot +
    exec(
      ggsignif::geom_signif,
      comparisons = mpc_df$groups,
      map_signif_level = TRUE,
      y_position = ggsignif_xy(
        data %>% pull({{ x }}),
        data %>% pull({{ y }})
      ),
      annotations = mpc_df$label,
      test = NULL,
      parse = TRUE,
      !!!ggsignif.args
    )
}

#' @name ggsignif_xy
#'
#' @inheritParams ggbetweenstats
#'
#' @keywords internal
#' @noRd
ggsignif_xy <- function(x, y) {
  # number of comparisons and size of each step
  n_comps <- length(utils::combn(x = unique(x), m = 2L, simplify = FALSE))
  step_length <- (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)) / 20

  # start and end position on `y`-axis for the `ggsignif` lines
  y_start <- max(y, na.rm = TRUE) * (1 + 0.025)
  y_end <- y_start + (step_length * n_comps)

  # creating a vector of positions for the `ggsignif` lines
  seq(y_start, y_end, length.out = n_comps)
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
    labs(
      x        = xlab,
      y        = ylab,
      title    = title,
      subtitle = subtitle,
      caption  = caption,
      color    = xlab
    ) +
    ggtheme +
    theme(legend.position = "none") +
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
#'   data          = morley,
#'   x             = Expt,
#'   y             = Speed,
#'   outlier.label = Run,
#'   outlier.coef  = 2
#' ) %>%
#'   arrange(outlier)
#' @noRd
outlier_df <- function(data, x, y, outlier.label, outlier.coef = 1.5, ...) {
  group_by(data, {{ x }}) %>%
    mutate(
      isanoutlier = ifelse((.) %$% as.vector(performance::check_outliers({{ y }}, method = "iqr", threshold = list("iqr" = outlier.coef))), TRUE, FALSE),
      outlier = ifelse(isanoutlier, {{ outlier.label }}, NA)
    ) %>%
    ungroup(.)
}


#' @title Switch expression making function
#' @name function_switch
#'
#' @param test Decides which test to run (can be either `"t"` or
#'   `"anova"`).
#' @param element Which expression is needed (`"subtitle"` or `"caption"`)
#' @param ... Arguments passed to respective subtitle helper functions.
#'
#' @noRd
function_switch <- function(test, element, ...) {
  # which function?
  if (test == "t") .f <- statsExpressions::two_sample_test
  if (test == "anova") .f <- statsExpressions::oneway_anova

  # return it
  .f
}
