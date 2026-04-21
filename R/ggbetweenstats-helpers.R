#' @title Compute subtitle and caption for box/violin plots
#' @name .bw_subtitle_caption
#'
#' @description
#'
#' Shared helper for `ggbetweenstats()` and `ggwithinstats()` that builds the
#' argument list for the statistical test function and optionally computes a
#' Bayes Factor caption.
#'
#' @param data Data frame for the statistical test.
#' @param x,y Column name symbols for the grouping and response variables.
#' @param test Character: `"t"` or `"anova"`.
#' @param type Character: statistical test type (e.g. `"parametric"`).
#' @param bf.message Logical: include Bayes Factor caption?
#' @param paired Logical: whether the test is paired (within-subjects).
#' @param var.equal Logical: assume equal variances? Only used for
#'   between-subjects tests (`NULL` by default, omitted when `NULL`).
#' @param subject.id Optional symbol for the subject identifier column
#'   (within-subjects designs only; `NULL` by default, omitted when `NULL`).
#' @inheritParams ggbetweenstats
#'
#' @return A list with elements `subtitle`, `caption`, `subtitle_df`, and
#'   `caption_df`.
#'
#' @autoglobal
#' @noRd
.bw_subtitle_caption <- function(
  data,
  x,
  y,
  test,
  type,
  bf.message,
  effsize.type,
  conf.level,
  digits,
  tr,
  bf.prior,
  nboot,
  alternative,
  paired,
  var.equal = NULL,
  subject.id = NULL
) {
  .f.args <- list(
    data = data,
    x = as_string(x),
    y = as_string(y),
    effsize.type = effsize.type,
    conf.level = conf.level,
    digits = digits,
    tr = tr,
    paired = paired,
    bf.prior = bf.prior,
    nboot = nboot
  )

  if (!is.null(var.equal)) {
    .f.args$var.equal <- var.equal
  }
  if (!is.null(subject.id)) {
    .f.args$subject.id <- subject.id
  }
  if (test == "t") {
    .f.args$alternative <- alternative
  }

  .subtitle_caption(.f_switch(test), .f.args, type, bf.message)
}


#' @title Decorate a box/violin comparison plot
#' @name .bw_decorate
#'
#' @description
#'
#' Adds centrality labels, sample-size x-axis labels, pairwise-comparison
#' annotations (ggsignif), and final aesthetic theming shared by
#' `ggbetweenstats()` and `ggwithinstats()`.
#'
#' @param plot A `ggplot` object to decorate.
#' @param data Data frame used for plotting.
#' @param pairwise_args A named list of extra arguments forwarded to
#'   [pairwise_comparisons()], typically containing `data`, `paired`,
#'   `p.adjust.method`, and optionally `var.equal` or `subject.id`.
#' @inheritParams ggbetweenstats
#' @inheritParams ggwithinstats
#'
#' @return A decorated `ggplot` object.
#'
#' @autoglobal
#' @noRd
.bw_decorate <- function(
  plot,
  data,
  x,
  y,
  type,
  test,
  centrality.plotting,
  centrality.type,
  digits,
  tr,
  centrality.point.args,
  centrality.label.args,
  pairwise.display,
  pairwise.alpha,
  pairwise_args,
  ggsignif.args,
  xlab,
  ylab,
  title,
  subtitle,
  caption,
  ggtheme,
  palette,
  ggplot.component,
  centrality.path = FALSE,
  centrality.path.args = list()
) {
  x <- ensym(x)
  y <- ensym(y)

  # centrality tagging
  if (isTRUE(centrality.plotting)) {
    centrality_df <- suppressWarnings(centrality_description(
      data,
      !!x,
      !!y,
      type = stats_type_switch(centrality.type),
      digits = digits,
      tr = tr
    ))

    plot <- suppressWarnings(.centrality_ggrepel(
      plot = plot,
      centrality_df = centrality_df,
      x = !!x,
      y = !!y,
      centrality.path = centrality.path,
      centrality.path.args = centrality.path.args,
      centrality.point.args = centrality.point.args,
      centrality.label.args = centrality.label.args
    ))
  } else {
    centrality_df <- suppressWarnings(centrality_description(data, !!x, !!y)) # nocov
  }

  # sample size labels on x-axis
  plot <- plot +
    scale_x_discrete(labels = unique(centrality_df$n.expression))

  # ggsignif labels
  seclabel <- NULL

  if (pairwise.display != "none" && test == "anova") {
    pw_args <- c(
      list(x = x, y = y, type = type, tr = tr, digits = digits),
      pairwise_args
    )
    mpc_df <- suppressWarnings(inject(pairwise_comparisons(!!!pw_args)))

    assign("mpc_df", mpc_df, envir = plot$plot_env)

    plot <- .ggsignif_adder(
      plot = plot,
      mpc_df = mpc_df,
      data = data,
      x = !!x,
      y = !!y,
      pairwise.display = pairwise.display,
      pairwise.alpha = pairwise.alpha,
      ggsignif.args = ggsignif.args
    )

    seclabel <- .pairwise_seclabel(
      test.description = unique(mpc_df$test),
      pairwise.display = ifelse(type == "bayes", "all", pairwise.display),
      pairwise.alpha = pairwise.alpha
    )
  }

  # annotations
  .aesthetic_addon(
    plot = plot,
    x = pull(data, !!x),
    xlab = xlab,
    ylab = ylab,
    title = title,
    subtitle = subtitle,
    caption = caption,
    seclabel = seclabel,
    ggtheme = ggtheme,
    palette = palette,
    ggplot.component = ggplot.component
  )
}


#' @title Adding labels for mean values.
#' @name .centrality_ggrepel
#'
#' @param plot A `ggplot` object for which means are to be displayed.
#' @param centrality_df A data frame produced by
#'   [statsExpressions::centrality_description()].
#' @inheritParams ggbetweenstats
#' @inheritParams ggwithinstats
#' @inheritParams ggrepel::geom_label_repel
#'
#' @autoglobal
#' @noRd
.centrality_ggrepel <- function(
  plot,
  centrality_df,
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
  )
) {
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
    )
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
  pairwise.alpha = 0.05,
  ggsignif.args = list(textsize = 3, tip_length = 0.01, na.rm = TRUE),
  ...
) {
  # creating a column for group combinations
  mpc_df %<>% mutate(groups = purrr::pmap(.l = list(group1, group2), .f = c))

  # for Bayes Factor, there will be no "p.value" column
  if ("p.value" %in% names(mpc_df)) {
    if (startsWith(pairwise.display, "s")) {
      mpc_df %<>% filter(p.value < pairwise.alpha)
    } # sig
    if (startsWith(pairwise.display, "n")) {
      mpc_df %<>% filter(p.value >= pairwise.alpha)
    } # non-sig

    # proceed only if there are any significant comparisons to display
    if (nrow(mpc_df) == 0L) {
      return(plot)
    } # nocov
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
#' @param pairwise.alpha Numeric alpha threshold used to decide which pairwise
#'   comparisons are displayed.
#'
#' @examples
#' .pairwise_seclabel("Student's t-test")
#'
#' # non-significant pairwise comparisons
#' .pairwise_seclabel("Student's t-test", pairwise.display = "non-significant")
#'
#' # all pairwise comparisons
#' .pairwise_seclabel("Student's t-test", pairwise.display = "all")
#'
#' # custom alpha threshold
#' .pairwise_seclabel("Student's t-test", pairwise.alpha = 0.01)
#' @keywords internal
#' @autoglobal
#' @noRd
.pairwise_seclabel <- function(
  test.description,
  pairwise.display = "significant",
  pairwise.alpha = 0.05
) {
  # single quote (') needs to be escaped inside glue expressions
  test <- sub("'", "\\'", test.description, fixed = TRUE)
  alpha_label <- format(pairwise.alpha, scientific = FALSE, trim = TRUE)

  # which comparisons were displayed?
  display <- if (startsWith(pairwise.display, "s")) {
    "significant"
  } else if (startsWith(pairwise.display, "n")) {
    "non-significant"
  } else {
    "all"
  }

  parse(
    text = glue(
      "list('Pairwise test:'~bold('{test}'), 'Bars shown:'~bold('{display}'), alpha == {alpha_label})"
    )
  )
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
  palette = "ggthemes::gdoc",
  ggplot.component = NULL,
  ...
) {
  # if no. of factor levels is greater than the default palette color count
  .is_palette_sufficient(palette, nlevels(x))

  plot +
    labs(
      x = xlab,
      y = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      color = xlab
    ) +
    ggtheme +
    # no matter the theme, the following ought to be part of a ggstatsplot plot
    theme(legend.position = "none", panel.grid.major.x = element_blank()) +
    paletteer::scale_color_paletteer_d(palette) +
    scale_y_continuous(
      sec.axis = dup_axis(name = seclabel, breaks = NULL, labels = NULL)
    ) +
    # this is the hail mary way for users to override these defaults
    ggplot.component
}

#' @title Switch expression making function
#' @noRd
.f_switch <- function(test) ifelse(test == "t", two_sample_test, oneway_anova)
