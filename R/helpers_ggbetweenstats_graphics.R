#' @name mean_labeller
#' @title Dataframe with mean per group and a formatted label for display in
#'   `ggbetweenstats` plot.
#'
#' @inheritParams ggbetweenstats
#' @param ... Currently ignored.
#'
#' @importFrom parameters describe_distribution
#' @importFrom broomExtra easystats_to_tidy_names
#' @importFrom dplyr select group_by matches mutate rowwise group_modify arrange ungroup
#' @importFrom rlang !! enquo ensym :=
#' @importFrom tidyr drop_na
#'
#' @examples
#' ggstatsplot:::mean_labeller(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = brainwt,
#'   mean.ci = TRUE,
#'   k = 3
#' )
#' @keywords internal

# function body
mean_labeller <- function(data,
                          x,
                          y,
                          mean.ci = FALSE,
                          k = 3L,
                          ...) {
  # creating the dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}) %>%
    tidyr::drop_na(.) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    as_tibble(.) %>%
    dplyr::group_by(.data = ., {{ x }}) %>%
    dplyr::group_modify(
      .f = ~ broomExtra::easystats_to_tidy_names(
        parameters::describe_distribution(x = ., centrality = "mean", ci = 0.95)
      )
    ) %>%
    dplyr::ungroup(.) %>%
    dplyr::rowwise()

  # prepare label
  if (isTRUE(mean.ci)) {
    data %<>%
      dplyr::mutate(
        label = paste0(
          "list(~italic(widehat(mu))==",
          specify_decimal_p(mean, k),
          ",",
          "CI[95*'%']",
          "*'['*",
          specify_decimal_p(conf.low, k),
          ",",
          specify_decimal_p(conf.high, k),
          "*']')"
        )
      )
  } else {
    data %<>%
      dplyr::mutate(
        label = paste0(
          "list(~italic(widehat(mu))==",
          specify_decimal_p(mean, k),
          ")"
        )
      )
  }

  # add label about sample size
  data %>%
    dplyr::ungroup(.) %>%
    dplyr::mutate(n_label = paste0({{ x }}, "\n(n = ", n, ")")) %>%
    dplyr::arrange({{ x }}) %>%
    dplyr::select({{ x }}, !!as.character(rlang::ensym(y)) := mean, dplyr::matches("label"))
}


#' @title Adding labels for mean values.
#' @name mean_ggrepel
#'
#' @param mean.data A dataframe containing means for each level of the factor.
#'   The columns should be titled `x`, `y`, and `label`.
#' @param plot A `ggplot` object for which means are to be displayed.
#' @param ... Additional arguments.
#' @inheritParams ggbetweenstats
#' @inheritParams ggrepel::geom_label_repel
#'
#' @importFrom ggrepel geom_label_repel
#' @importFrom rlang !! enquo ensym exec
#'
#' @examples
#'
#' # this internal function may not have much utility outside of the package
#' set.seed(123)
#' library(ggplot2)
#'
#' # make a plot
#' p <- ggplot(data = iris, aes(x = Species, y = Sepal.Length)) +
#'   geom_boxplot()
#'
#' # get a dataframe with means
#' mean_dat <- ggstatsplot:::mean_labeller(
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   mean.ci = TRUE,
#'   k = 3
#' )
#'
#' # add means
#' ggstatsplot:::mean_ggrepel(
#'   plot = p,
#'   x = Species,
#'   y = Sepal.Length,
#'   mean.data = mean_dat,
#'   mean.color = "darkgreen"
#' )
#' @keywords internal

# function body
mean_ggrepel <- function(plot,
                         x,
                         y,
                         mean.data,
                         mean.point.args = list(size = 5, color = "darkred"),
                         mean.label.args = list(size = 3),
                         inherit.aes = TRUE,
                         ...) {
  # highlight the mean of each group
  plot <- plot +
    rlang::exec(
      .fn = ggplot2::stat_summary,
      mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}),
      fun = mean,
      geom = "point",
      inherit.aes = inherit.aes,
      na.rm = TRUE,
      !!!mean.point.args
    )

  # attach the labels with means to the plot
  plot +
    rlang::exec(
      .fn = ggrepel::geom_label_repel,
      data = mean.data,
      mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}, label = label),
      show.legend = FALSE,
      min.segment.length = 0,
      inherit.aes = FALSE,
      parse = TRUE,
      na.rm = TRUE,
      !!!mean.label.args
    )
}

#' @title Adding `geom_signif` to `ggplot`
#' @name ggsignif_adder
#'
#' @param ... Currently ignored.
#' @param plot A `ggplot` object on which `geom_signif` needed to be added.
#' @param df_pairwise A dataframe containing results from pairwise comparisons
#'   (produced by `pairwiseComparisons::pairwise_comparisons()` function).
#' @inheritParams ggbetweenstats
#'
#' @importFrom purrr pmap
#' @importFrom dplyr mutate rename filter arrange pull
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
#' df_pair <-
#'   pairwiseComparisons::pairwise_comparisons(
#'     data = iris,
#'     x = Species,
#'     y = Sepal.Length
#'   )
#'
#' # adding a geom for pairwise comparisons
#' ggstatsplot:::ggsignif_adder(
#'   plot = p,
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   df_pairwise = df_pair
#' )
#' @keywords internal

ggsignif_adder <- function(plot,
                           df_pairwise,
                           data,
                           x,
                           y,
                           pairwise.display = "significant",
                           ...) {
  # creating a column for group combinations
  df_pairwise %<>% dplyr::mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c))

  # for Bayes Factor, there will be no "significance" column
  if ("significance" %in% names(df_pairwise)) {
    # decide what needs to be displayed:
    # only significant comparisons shown
    if (pairwise.display %in% c("s", "significant")) {
      df_pairwise %<>% dplyr::filter(.data = ., significance != "ns")
    }

    # only non-significant comparisons shown
    if (pairwise.display %in% c("ns", "nonsignificant", "non-significant")) {
      df_pairwise %<>% dplyr::filter(.data = ., significance == "ns")
    }

    # proceed only if there are any significant comparisons to display
    if (dim(df_pairwise)[[1]] == 0L) {
      return(plot)
    }
  }

  # arrange the dataframe so that annotations are properly aligned
  df_pairwise %<>% dplyr::arrange(.data = ., group1, group2)

  # adding ggsignif comparisons to the plot
  plot +
    ggsignif::geom_signif(
      comparisons = df_pairwise$groups,
      map_signif_level = TRUE,
      textsize = 3,
      tip_length = 0.01,
      vjust = 0,
      y_position = ggsignif_xy(
        data %>% dplyr::pull({{ x }}),
        data %>% dplyr::pull({{ y }})
      ),
      annotations = df_pairwise$label,
      test = NULL,
      na.rm = TRUE,
      parse = TRUE
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
                            ggplot.component = NULL,
                            ...) {

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
    ggstatsplot::theme_ggstatsplot(
      ggtheme = ggtheme,
      ggstatsplot.layer = ggstatsplot.layer
    ) +
    ggplot2::theme(legend.position = "none") +
    paletteer::scale_color_paletteer_d(paste0(package, "::", palette)) +
    paletteer::scale_fill_paletteer_d(paste0(package, "::", palette))

  # ---------------- adding ggplot component ---------------------------------

  # return with any additional modification that needs to be made to the plot
  return(plot + ggplot.component)
}
