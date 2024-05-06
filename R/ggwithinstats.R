#' @title Box/Violin plots for repeated measures comparisons
#' @name ggwithinstats
#'
#' @description
#'
#' A combination of box and violin plots along with raw (unjittered) data points
#' for within-subjects designs with statistical details included in the plot as
#' a subtitle.
#'
#' @section Summary of graphics:
#'
#' ```{r child="man/rmd-fragments/ggwithinstats_graphics.Rmd"}
#' ```
#'
#' @inheritParams ggbetweenstats
#' @param point.path,centrality.path Logical that decides whether individual
#'   data points and means, respectively, should be connected using
#'   `ggplot2::geom_path()`. Both default to `TRUE`. Note that `point.path`
#'   argument is relevant only when there are two groups (i.e., in case of a
#'   *t*-test). In case of large number of data points, it is advisable to set
#'   `point.path = FALSE` as these lines can overwhelm the plot.
#' @param centrality.path.args,point.path.args A list of additional aesthetic
#'   arguments passed on to `ggplot2::geom_path()` connecting raw data points
#'   and mean points.
#' @inheritParams statsExpressions::oneway_anova
#'
#' @inheritSection statsExpressions::centrality_description Centrality measures
#' @inheritSection statsExpressions::two_sample_test Two-sample tests
#' @inheritSection statsExpressions::oneway_anova One-way ANOVA
#' @inheritSection statsExpressions::pairwise_comparisons Pairwise comparison tests
#'
#' @seealso \code{\link{grouped_ggbetweenstats}}, \code{\link{ggbetweenstats}},
#'  \code{\link{grouped_ggwithinstats}}
#'
#' @autoglobal
#'
#' @details For details, see:
#' <https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggwithinstats.html>
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("afex", quietly = TRUE)
#' # for reproducibility
#' set.seed(123)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # create a plot
#' p <- ggwithinstats(
#'   data = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
#'   x    = condition,
#'   y    = desire,
#'   type = "np"
#' )
#'
#'
#' # looking at the plot
#' p
#'
#' # extracting details from statistical tests
#' extract_stats(p)
#'
#' # modifying defaults
#' ggwithinstats(
#'   data = bugs_long,
#'   x    = condition,
#'   y    = desire,
#'   type = "robust"
#' )
#'
#' # you can remove a specific geom by setting `width` to `0` for that geom
#' ggbetweenstats(
#'   data = bugs_long,
#'   x = condition,
#'   y = desire,
#'   # to remove violin plot
#'   violin.args = list(width = 0, linewidth = 0),
#'   # to remove boxplot
#'   boxplot.args = list(width = 0),
#'   # to remove points
#'   point.args = list(alpha = 0)
#' )
#' @export
ggwithinstats <- function(
    data,
    x,
    y,
    type = "parametric",
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
    conf.level = 0.95,
    nboot = 100L,
    tr = 0.2,
    centrality.plotting = TRUE,
    centrality.type = type,
    centrality.point.args = list(size = 5, color = "darkred"),
    centrality.label.args = list(size = 3, nudge_x = 0.4, segment.linetype = 4),
    centrality.path = TRUE,
    centrality.path.args = list(linewidth = 1, color = "red", alpha = 0.5),
    point.args = list(size = 3, alpha = 0.5, na.rm = TRUE),
    point.path = TRUE,
    point.path.args = list(alpha = 0.5, linetype = "dashed"),
    boxplot.args = list(width = 0.2, alpha = 0.5, na.rm = TRUE),
    violin.args = list(width = 0.5, alpha = 0.2, na.rm = TRUE),
    ggsignif.args = list(textsize = 3, tip_length = 0.01, na.rm = TRUE),
    ggtheme = ggstatsplot::theme_ggstatsplot(),
    package = "RColorBrewer",
    palette = "Dark2",
    ggplot.component = NULL,
    ...) {
  # data -----------------------------------

  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(ensym(x), ensym(y))
  type <- stats_type_switch(type)

  data %<>%
    select({{ x }}, {{ y }}) %>%
    mutate({{ x }} := droplevels(as.factor({{ x }}))) %>%
    mutate(.rowid = row_number(), .by = {{ x }}) %>%
    anti_join(x = ., y = filter(., is.na({{ y }})), by = ".rowid")

  # statistical analysis ------------------------------------------

  test <- ifelse(nlevels(pull(data, {{ x }})) < 3L, "t", "anova")

  if (results.subtitle) {
    .f.args <- list(
      data = data,
      x = as_string(x),
      y = as_string(y),
      effsize.type = effsize.type,
      conf.level = conf.level,
      digits = digits,
      tr = tr,
      paired = TRUE,
      bf.prior = bf.prior,
      nboot = nboot
    )

    # styler: off
    .f          <- .f_switch(test)
    subtitle_df <- .eval_f(.f, !!!.f.args, type = type)
    subtitle    <- .extract_expression(subtitle_df)
    # styler: on

    if (type == "parametric" && bf.message) {
      # styler: off
      caption_df <- .eval_f(.f, !!!.f.args, type = "bayes")
      caption    <- .extract_expression(caption_df)
      # styler: on
    }
  }

  # plot -------------------------------------------

  plot_comparison <- ggplot(data, aes({{ x }}, {{ y }}, group = .rowid)) +
    exec(geom_point, aes(color = {{ x }}), !!!point.args) +
    exec(geom_boxplot, aes({{ x }}, {{ y }}), inherit.aes = FALSE, !!!boxplot.args, outlier.shape = NA) +
    exec(geom_violin, aes({{ x }}, {{ y }}), inherit.aes = FALSE, !!!violin.args)

  # add a connecting path only if there are only two groups
  if (test == "t" && point.path) plot_comparison <- plot_comparison + exec(geom_path, !!!point.path.args)

  # centrality tagging -------------------------------------

  if (isTRUE(centrality.plotting)) {
    plot_comparison <- suppressWarnings(.centrality_ggrepel(
      plot = plot_comparison,
      data = data,
      x = {{ x }},
      y = {{ y }},
      digits = digits,
      type = stats_type_switch(centrality.type),
      tr = tr,
      centrality.path = centrality.path,
      centrality.path.args = centrality.path.args,
      centrality.point.args = centrality.point.args,
      centrality.label.args = centrality.label.args
    ))
  }

  # ggsignif labels -------------------------------------

  # initialize
  seclabel <- NULL

  if (pairwise.display != "none" && test == "anova") {
    mpc_df <- pairwise_comparisons(
      data = data,
      x = {{ x }},
      y = {{ y }},
      type = type,
      tr = tr,
      paired = TRUE,
      p.adjust.method = p.adjust.method,
      digits = digits
    )

    # adding the layer for pairwise comparisons
    plot_comparison <- .ggsignif_adder(
      plot             = plot_comparison,
      mpc_df           = mpc_df,
      data             = data,
      x                = {{ x }},
      y                = {{ y }},
      pairwise.display = pairwise.display,
      ggsignif.args    = ggsignif.args
    )

    # secondary label axis to give pairwise comparisons test details
    seclabel <- .pairwise_seclabel(unique(mpc_df$test), ifelse(type == "bayes", "all", pairwise.display))
  }

  # annotations -------------------------

  .aesthetic_addon(
    plot             = plot_comparison,
    x                = pull(data, {{ x }}),
    xlab             = xlab %||% as_name(x),
    ylab             = ylab %||% as_name(y),
    title            = title,
    subtitle         = subtitle,
    caption          = caption,
    seclabel         = seclabel,
    ggtheme          = ggtheme,
    package          = package,
    palette          = palette,
    ggplot.component = ggplot.component
  )
}


#' @title Violin plots for group or condition comparisons in within-subjects
#'   designs repeated across all levels of a grouping variable.
#' @name grouped_ggwithinstats
#'
#' @description
#'
#' A combined plot of comparison plot created for levels of a grouping variable.
#'
#' @inheritParams ggwithinstats
#' @inheritDotParams ggwithinstats -title
#' @inheritParams grouped_ggbetweenstats
#'
#' @seealso \code{\link{ggwithinstats}}, \code{\link{ggbetweenstats}},
#' \code{\link{grouped_ggbetweenstats}}
#'
#' @autoglobal
#'
#' @inherit ggwithinstats return references
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("afex", quietly = TRUE)
#' # for reproducibility
#' set.seed(123)
#' library(dplyr, warn.conflicts = FALSE)
#' library(ggplot2)
#'
#' # the most basic function call
#' grouped_ggwithinstats(
#'   data             = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
#'   x                = condition,
#'   y                = desire,
#'   grouping.var     = gender,
#'   type             = "np",
#'   # additional modifications for **each** plot using `{ggplot2}` functions
#'   ggplot.component = scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10))
#' )
#' @export
grouped_ggwithinstats <- function(
    data,
    ...,
    grouping.var,
    plotgrid.args = list(),
    annotation.args = list()) {
  .grouped_list(data, {{ grouping.var }}) %>%
    purrr::pmap(.f = ggwithinstats, ...) %>%
    combine_plots(plotgrid.args, annotation.args)
}
