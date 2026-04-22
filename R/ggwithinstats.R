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
#'   [`ggplot2::geom_path()`]. Both default to `TRUE`. Note that `point.path`
#'   argument is relevant only when there are two groups (i.e., in case of a
#'   *t*-test). In case of large number of data points, it is advisable to set
#'   `point.path = FALSE` as these lines can overwhelm the plot.
#' @param centrality.path.args,point.path.args A list of additional aesthetic
#'   arguments passed on to [`ggplot2::geom_path()`] connecting raw data points
#'   and mean points.
#' @param subject.id Across repeated measures conditions, each row in the
#'   dataset must correspond to a unique unit (e.g., subject or participant).
#'   If your data frame is already in such a format, you can ignore the
#'   `subject.id` argument (the function will use row number to pair
#'   observations). **But if you are not sure, it is always better to specify
#'   this argument.** Note that if there are any missing values (i.e., `NA`) in
#'   the dependent variable and the `subject.id` is not specified, they will be
#'   dropped using a list-wise approach. If you specify `subject.id`, partially
#'   observed subjects will still be shown in the plot, but inferential
#'   statistics will be computed using only complete repeated-measures pairs.
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
#' <https://www.indrapatil.com/ggstatsplot/articles/web_only/ggwithinstats.html>
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("afex", quietly = TRUE)
#' # for reproducibility
#' set.seed(123)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # create a plot
#' p <- ggwithinstats(
#'   data       = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
#'   x          = condition,
#'   y          = desire,
#'   type       = "np",
#'   subject.id = subject
#' )
#'
#'
#' # looking at the plot
#' p
#'
#' # if the data are already arranged in repeated-measures order, `subject.id`
#' # can be omitted
#' ggwithinstats(
#'   data             = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
#'   x                = condition,
#'   y                = desire,
#'   pairwise.display = "none",
#'   results.subtitle = FALSE
#' )
#'
#' # extracting details from statistical tests
#' extract_stats(p)
#'
#' # use a stricter alpha threshold for significant pairwise comparisons
#' ggwithinstats(
#'   data = bugs_long,
#'   x = condition,
#'   y = desire,
#'   subject.id = subject,
#'   pairwise.alpha = 0.001
#' )
#'
#' # modifying defaults
#' ggwithinstats(
#'   data       = bugs_long,
#'   x          = condition,
#'   y          = desire,
#'   type       = "robust",
#'   subject.id = subject
#' )
#'
#' # you can remove a specific geom to reduce complexity of the plot
#' ggwithinstats(
#'   data = bugs_long,
#'   x = condition,
#'   y = desire,
#'   subject.id = subject,
#'   # to remove violin plot
#'   violin.args = list(width = 0, linewidth = 0, colour = NA),
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
  subject.id = NULL,
  pairwise.display = "significant",
  pairwise.alpha = 0.05,
  p.adjust.method = "holm",
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
  tr = 0.2,
  alternative = "two.sided",
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
  palette = "ggthemes::gdoc",
  ggplot.component = NULL,
  ...
) {
  palette <- .validate_palette(palette)

  # data -----------------------------------

  # make sure both quoted and unquoted arguments are allowed
  x <- ensym(x)
  y <- ensym(y)
  type <- extract_stats_type(type)

  subject.id <- if (!quo_is_null(enquo(subject.id))) ensym(subject.id)
  sid_str <- if (!is.null(subject.id)) as_string(subject.id)

  data <- data |>
    select({{ x }}, {{ y }}, any_of(sid_str %||% character(0))) |>
    mutate({{ x }} := droplevels(as.factor({{ x }})))

  if (is.null(sid_str)) {
    stats_data <- data

    data <- mutate(data, .rowid = row_number(), .by = {{ x }})
    data <- anti_join(x = data, y = filter(data, is.na({{ y }})), by = ".rowid")
  } else {
    data <- filter(data, !is.na(.data[[sid_str]]))

    stats_data <- data

    data <- data |>
      mutate(.rowid = .data[[sid_str]]) |>
      filter(!is.na({{ y }}))
  }

  data <- mutate(data, {{ x }} := droplevels({{ x }}))

  stats_data <- mutate(stats_data, {{ x }} := droplevels({{ x }}))

  # statistical analysis ------------------------------------------

  test <- ifelse(nlevels(pull(data, {{ x }})) < 3L, "t", "anova")

  if (results.subtitle) {
    stats_output <- .bw_subtitle_caption(
      data = stats_data,
      x = x,
      y = y,
      test = test,
      type = type,
      bf.message = bf.message,
      bf.prior = bf.prior,
      conf.level = conf.level,
      digits = digits,
      tr = tr,
      alternative = alternative,
      paired = TRUE,
      subject.id = subject.id
    )
    subtitle <- stats_output$subtitle
    caption <- stats_output$caption
    subtitle_df <- stats_output$subtitle_df
    caption_df <- stats_output$caption_df
  }

  # plot -------------------------------------------

  plot_comparison <- ggplot(data, aes({{ x }}, {{ y }}, group = .rowid)) +
    exec(geom_point, aes(color = {{ x }}), !!!point.args) +
    exec(
      geom_boxplot,
      aes({{ x }}, {{ y }}),
      inherit.aes = FALSE,
      !!!boxplot.args,
      outlier.shape = NA
    ) +
    exec(
      geom_violin,
      aes({{ x }}, {{ y }}),
      inherit.aes = FALSE,
      !!!violin.args
    )

  # add a connecting path only if there are only two groups
  if (test == "t" && point.path) {
    plot_comparison <- plot_comparison + exec(geom_path, !!!point.path.args)
  }

  # decorate and return -------------------------

  .bw_decorate(
    plot = plot_comparison,
    data = data,
    x = {{ x }},
    y = {{ y }},
    type = type,
    test = test,
    centrality.plotting = centrality.plotting,
    centrality.type = centrality.type,
    digits = digits,
    tr = tr,
    centrality.point.args = centrality.point.args,
    centrality.label.args = centrality.label.args,
    centrality.path = centrality.path,
    centrality.path.args = centrality.path.args,
    pairwise.display = pairwise.display,
    pairwise.alpha = pairwise.alpha,
    pairwise_args = list(
      data = stats_data,
      paired = TRUE,
      subject.id = subject.id,
      p.adjust.method = p.adjust.method
    ),
    ggsignif.args = ggsignif.args,
    xlab = xlab %||% as_name(x),
    ylab = ylab %||% as_name(y),
    title = title,
    subtitle = subtitle,
    caption = caption,
    ggtheme = ggtheme,
    palette = palette,
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
#'   subject.id       = subject,
#'   grouping.var     = gender,
#'   type             = "np",
#'   # additional modifications for **each** plot using `{ggplot2}` functions
#'   ggplot.component = scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10)),
#'   annotation.args  = list(title = "Desire ratings by condition for each gender")
#' )
#' @export
grouped_ggwithinstats <- .make_grouped_fn(ggwithinstats)
