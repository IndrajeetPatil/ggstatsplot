#' @title Adding labels for mean values.
#' @name .centrality_ggrepel
#'
#' @param plot A `ggplot` object for which means are to be displayed.
#' @param ... Additional arguments.
#' @inheritParams ggbetweenstats
#' @inheritParams ggwithinstats
#' @inheritParams ggrepel::geom_label_repel
#'
#' @autoglobal
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
#' ggstatsplot:::.centrality_ggrepel(
#'   data = iris,
#'   plot = p,
#'   x = Species,
#'   y = Sepal.Length
#' )
#' @noRd
.centrality_ggrepel <- function(
    plot,
    data,
    x,
    y,
    centrality.path = FALSE,
    centrality.path.args = list(
      linewidth = 1.0,
      color = "red",
      alpha = 0.5
    ),
    centrality.point.args = list(size = 5.0, color = "darkred"),
    centrality.label.args = list(
      size = 3.0,
      nudge_x = 0.4,
      segment.linetype = 4.0
    ),
    ...) {
  centrality_df <- suppressWarnings(centrality_description(data, {{ x }}, {{ y }}, ...))

  # lines connecting mean values across groups
  if (isTRUE(centrality.path)) {
    plot <- plot +
      exec(
        geom_path,
        data = centrality_df,
        mapping = aes({{ x }}, {{ y }}, group = 1L),
        inherit.aes = FALSE,
        !!!centrality.path.args
      )
  }

  plot + # highlight the mean of each group
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
      mapping = aes({{ x }}, {{ y }}, label = expression),
      inherit.aes = FALSE,
      parse = TRUE,
      !!!centrality.label.args
    ) + # adding sample size labels to the x axes
    scale_x_discrete(labels = unique(centrality_df$n.expression))
}

#' @title Adding `geom_signif` to `ggplot`
#' @name .ggsignif_adder
#'
#' @param ... Currently ignored.
#' @param plot A `ggplot` object on which `geom_signif` needed to be added.
#' @param mpc_df A data frame containing results from pairwise comparisons
#'   (produced by [`pairwise_comparisons()`] function).
#' @inheritParams ggbetweenstats
#'
#' @autoglobal
#'
#' @examples
#' set.seed(123)
#' library(ggplot2)
#'
#' # plot
#' p <- ggplot(iris, aes(Species, Sepal.Length)) +
#'   geom_boxplot()
#'
#' # data frame with pairwise comparison test results
#' df_pair <- pairwise_comparisons(
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length
#' )
#'
#' # adding a geom for pairwise comparisons
#' ggstatsplot:::.ggsignif_adder(
#'   plot = p,
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   mpc_df = df_pair
#' )
#' @noRd
.ggsignif_adder <- function(
    plot,
    data,
    x,
    y,
    mpc_df,
    pairwise.display = "significant",
    ggsignif.args = list(textsize = 3, tip_length = 0.01, na.rm = TRUE),
    ...) {
  # creating a column for group combinations
  mpc_df %<>% mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c))

  # for Bayes Factor, there will be no "p.value" column
  if ("p.value" %in% names(mpc_df)) {
    if (startsWith(pairwise.display, "s")) mpc_df %<>% filter(p.value < 0.05) # sig
    if (startsWith(pairwise.display, "n")) mpc_df %<>% filter(p.value >= 0.05) # non-sig

    # proceed only if there are any significant comparisons to display
    if (nrow(mpc_df) == 0L) {
      return(plot)
    }
  }

  # arrange the data frame so that annotations are properly aligned
  mpc_df %<>% arrange(group1, group2)

  # adding ggsignif comparisons to the plot
  plot +
    exec(
      ggsignif::geom_signif,
      comparisons = mpc_df$groups,
      map_signif_level = TRUE,
      y_position = .ggsignif_xy(pull(data, {{ x }}), pull(data, {{ y }})),
      annotations = as.character(mpc_df$expression),
      test = NULL,
      parse = TRUE,
      !!!ggsignif.args
    )
}

#' @name .ggsignif_xy
#'
#' @inheritParams ggbetweenstats
#'
#' @keywords internal
#' @autoglobal
#' @noRd
.ggsignif_xy <- function(x, y) {
  # number of comparisons and size of each step
  n_comps <- length(utils::combn(x = unique(x), m = 2L, simplify = FALSE))
  step_length <- (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)) / 20

  # start and end position on `y`-axis for the `ggsignif` lines
  y_start <- max(y, na.rm = TRUE) * (1 + 0.025)
  y_end <- y_start + (step_length * n_comps)

  # creating a vector of positions for the `ggsignif` lines
  seq(y_start, y_end, length.out = n_comps)
}

#' @name .pairwise_seclabel
#' @title Pairwise comparison test expression
#'
#' @description
#'
#' This returns an expression containing details about the pairwise comparison
#' test and the *p*-value adjustment method. These details are typically
#' included in the `{ggstatsplot}` package plots as a caption.
#'
#' @param test.description Text describing the details of the test.
#' @param pairwise.display Decides *which* pairwise comparisons to display.
#'   Available options are:
#'   - `"significant"` (abbreviation accepted: `"s"`)
#'   - `"non-significant"` (abbreviation accepted: `"ns"`)
#'   - `"all"`
#'
#'   You can use this argument to make sure that your plot is not uber-cluttered
#'   when you have multiple groups being compared and scores of pairwise
#'   comparisons being displayed.
#'
#' @examples
#' .pairwise_seclabel("my caption", "Student's t-test")
#' @keywords internal
#' @autoglobal
#' @noRd
.pairwise_seclabel <- function(test.description, pairwise.display = "significant") {
  # single quote (') needs to be escaped inside glue expressions
  test <- sub("'", "\\'", test.description, fixed = TRUE)

  # which comparisons were displayed?
  display <- case_when(
    startsWith(pairwise.display, "s") ~ "significant",
    startsWith(pairwise.display, "n") ~ "non-significant",
    .default = "all"
  )

  parse(text = glue("list('Pairwise test:'~bold('{test}'), 'Bars shown:'~bold('{display}'))"))
}



#' @title Making aesthetic modifications to the plot
#' @name .aesthetic_addon
#'
#' @param plot Plot to be aesthetically modified.
#' @param x A numeric vector for `x` axis.
#' @param seclabel A label for secondary axis.
#' @inheritParams ggbetweenstats
#' @param ... Additional arguments.
#'
#' @noRd
.aesthetic_addon <- function(
    plot,
    x,
    xlab = NULL,
    ylab = NULL,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    seclabel = NULL,
    ggtheme = ggstatsplot::theme_ggstatsplot(),
    package = "RColorBrewer",
    palette = "Dark2",
    ggplot.component = NULL,
    ...) {
  # if no. of factor levels is greater than the default palette color count
  .is_palette_sufficient(package, palette, nlevels(x))

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
    # no matter the theme, the following ought to be part of a ggstatsplot plot
    theme(legend.position = "none") +
    paletteer::scale_color_paletteer_d(paste0(package, "::", palette)) +
    scale_y_continuous(sec.axis = dup_axis(name = seclabel, breaks = NULL, labels = NULL)) +
    # this is the hail mary way for users to override these defaults
    ggplot.component
}

#' @title Switch expression making function
#' @noRd
.f_switch <- function(test) ifelse(test == "t", two_sample_test, oneway_anova)
