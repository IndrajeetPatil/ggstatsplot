#' @title Box/Violin plots for group or condition comparisons in
#'   between-subjects designs.
#' @name ggbetweenstats
#' @description A combination of box and violin plots along with jittered data
#'   points for between-subjects designs with statistical details included in
#'   the plot as a subtitle.
#'
#' @param plot.type Character describing the *type* of plot. Currently supported
#'   plots are `"box"` (for pure boxplots), `"violin"` (for pure violin plots),
#'   and `"boxviolin"` (for a combination of box and violin plots; default).
#' @param xlab,ylab Labels for `x` and `y` axis variables. If `NULL` (default),
#'   variable names for `x` and `y` will be used.
#' @param pairwise.comparisons Logical that decides whether pairwise comparisons
#'   are to be displayed (default: `TRUE`). Please note that only
#'   **significant** comparisons will be shown by default. To change this
#'   behavior, select appropriate option with `pairwise.display` argument. The
#'   pairwise comparison dataframes are prepared using the
#'   `pairwiseComparisons::pairwise_comparisons` function. For more details
#'   about pairwise comparisons, see the documentation for that function.
#' @param p.adjust.method Adjustment method for *p*-values for multiple
#'   comparisons. Possible methods are: `"holm"` (default), `"hochberg"`,
#'   `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.
#' @param pairwise.display Decides which pairwise comparisons to display.
#'   Available options are `"significant"` (abbreviation accepted: `"s"`) or
#'   `"non-significant"` (abbreviation accepted: `"ns"`) or
#'   `"everything"`/`"all"`. The default is `"significant"`. You can use this
#'   argument to make sure that your plot is not uber-cluttered when you have
#'   multiple groups being compared and scores of pairwise comparisons being
#'   displayed.
#' @param bf.prior A number between `0.5` and `2` (default `0.707`), the prior
#'   width to use in calculating Bayes factors.
#' @param bf.message Logical that decides whether to display Bayes Factor in
#'   favor of the *null* hypothesis. This argument is relevant only **for
#'   parametric test** (Default: `TRUE`).
#' @param results.subtitle Decides whether the results of statistical tests are
#'   to be displayed as a subtitle (Default: `TRUE`). If set to `FALSE`, only
#'   the plot will be returned.
#' @param title The text for the plot title.
#' @param subtitle The text for the plot subtitle. Will work only if
#'   `results.subtitle = FALSE`.
#' @param caption The text for the plot caption.
#' @param sample.size.label Logical that decides whether sample size information
#'   should be displayed for each level of the grouping variable `x` (Default:
#'   `TRUE`).
#' @param notch A logical. If `FALSE` (default), a standard box plot will be
#'   displayed. If `TRUE`, a notched box plot will be used. Notches are used to
#'   compare groups; if the notches of two boxes do not overlap, this suggests
#'   that the medians are significantly different. In a notched box plot, the
#'   notches extend `1.58 * IQR / sqrt(n)`, where IQR: Inter-Quartile Range.
#'   This gives a roughly `95%` confidence interval for comparing medians.
#' @param notchwidth For a notched box plot, width of the notch relative to the
#'   body (default `0.5`).
#' @param outlier.color Default aesthetics for outliers (Default: `"black"`).
#' @param outlier.tagging Decides whether outliers should be tagged (Default:
#'   `FALSE`).
#' @param outlier.label Label to put on the outliers that have been tagged. This
#'   **can't** be the same as `x` argument.
#' @param outlier.shape Hiding the outliers can be achieved by setting
#'   `outlier.shape = NA`. Importantly, this does not remove the outliers,
#'   it only hides them, so the range calculated for the `y`-axis will be
#'   the same with outliers shown and outliers hidden.
#' @param outlier.label.args A list of additional aesthetic arguments to be
#'   passed to `ggrepel::geom_label_repel` for outlier label plotting.
#' @param outlier.coef Coefficient for outlier detection using Tukey's method.
#'   With Tukey's method, outliers are below (1st Quartile) or above (3rd
#'   Quartile) `outlier.coef` times the Inter-Quartile Range (IQR) (Default:
#'   `1.5`).
#' @param mean.plotting Logical that decides whether centrality tendency measure
#'   is to be displayed as a point with a label (Default: `TRUE`).
#' @param point.args A list of additional aesthetic arguments to be passed to
#'   the `geom_point` displaying the raw data.
#' @param violin.args A list of additional aesthetic arguments to be passed to
#'   the `geom_violin`.
#' @param ggplot.component A `ggplot` component to be added to the plot prepared
#'   by `ggstatsplot`. This argument is primarily helpful for `grouped_`
#'   variants of all primary functions. Default is `NULL`. The argument should
#'   be entered as a `ggplot2` function or a list of `ggplot2` functions.
#' @param package,palette Name of the package from which the given palette is to
#'   be extracted. The available palettes and packages can be checked by running
#'   `View(paletteer::palettes_d_names)`.
#' @param output Character that describes what is to be returned: can be
#'   `"plot"` (default) or `"subtitle"` or `"caption"`. Setting this to
#'   `"subtitle"` will return the expression containing statistical results. If
#'   you have set `results.subtitle = FALSE`, then this will return a `NULL`.
#'   Setting this to `"caption"` will return the expression containing details
#'   about Bayes Factor analysis, but valid only when `type = "parametric"` and
#'   `bf.message = TRUE`, otherwise this will return a `NULL`.
#' @param ... Currently ignored.
#' @inheritParams theme_ggstatsplot
#' @param mean.point.args,mean.label.args A list of additional aesthetic
#'   arguments to be passed to `ggplot2::geom_point` and
#'   `ggrepel::geom_label_repel` geoms, which are involved in mean plotting.
#' @param  ggsignif.args A list of additional aesthetic
#'   arguments to be passed to `ggsignif::geom_signif`.
#' @inheritParams statsExpressions::expr_anova_parametric
#' @inheritParams statsExpressions::expr_t_parametric
#' @inheritParams statsExpressions::expr_t_onesample
#' @inheritParams statsExpressions::expr_anova_robust
#'
#' @import ggplot2
#'
#' @importFrom dplyr select group_by arrange mutate mutate_at mutate_if
#' @importFrom ggrepel geom_label_repel
#' @importFrom stats t.test oneway.test
#' @importFrom rlang enquo quo_name as_name !! as_string
#' @importFrom ggrepel geom_label_repel
#' @importFrom paletteer scale_color_paletteer_d scale_fill_paletteer_d
#' @importFrom ggsignif geom_signif
#' @importFrom statsExpressions bf_ttest bf_oneway_anova
#' @importFrom pairwiseComparisons pairwise_comparisons pairwise_caption
#'
#' @seealso \code{\link{grouped_ggbetweenstats}}, \code{\link{ggwithinstats}},
#'  \code{\link{grouped_ggwithinstats}}
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggbetweenstats.html}
#'
#' @examples
#' \donttest{
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # simple function call with the defaults
#' ggstatsplot::ggbetweenstats(
#'   data = mtcars,
#'   x = am,
#'   y = mpg,
#'   title = "Fuel efficiency by type of car transmission",
#'   caption = "Transmission (0 = automatic, 1 = manual)"
#' )
#'
#' # more detailed function call
#' ggstatsplot::ggbetweenstats(
#'   data = datasets::morley,
#'   x = Expt,
#'   y = Speed,
#'   type = "nonparametric",
#'   plot.type = "box",
#'   xlab = "The experiment number",
#'   ylab = "Speed-of-light measurement",
#'   pairwise.comparisons = TRUE,
#'   p.adjust.method = "fdr",
#'   outlier.tagging = TRUE,
#'   outlier.label = Run,
#'   ggtheme = ggplot2::theme_grey(),
#'   ggstatsplot.layer = FALSE
#' )
#' }
#' @export

# defining the function
ggbetweenstats <- function(data,
                           x,
                           y,
                           plot.type = "boxviolin",
                           type = "parametric",
                           pairwise.comparisons = TRUE,
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
                           sample.size.label = TRUE,
                           k = 2L,
                           var.equal = FALSE,
                           conf.level = 0.95,
                           nboot = 100L,
                           tr = 0.1,
                           mean.plotting = TRUE,
                           mean.point.args = list(size = 5, color = "darkred"),
                           mean.label.args = list(size = 3, nudge_x = 0.4, segment.linetype = 4),
                           notch = FALSE,
                           notchwidth = 0.5,
                           outlier.tagging = FALSE,
                           outlier.label = NULL,
                           outlier.coef = 1.5,
                           outlier.shape = 19,
                           outlier.color = "black",
                           outlier.label.args = list(size = 3),
                           point.args = list(
                             position = ggplot2::position_jitterdodge(dodge.width = 0.60),
                             alpha = 0.4,
                             size = 3,
                             stroke = 0
                           ),
                           violin.args = list(width = 0.5, alpha = 0.2),
                           ggsignif.args = list(textsize = 3, tip_length = 0.01),
                           ggtheme = ggplot2::theme_bw(),
                           ggstatsplot.layer = TRUE,
                           package = "RColorBrewer",
                           palette = "Dark2",
                           ggplot.component = NULL,
                           output = "plot",
                           ...) {

  # convert entered stats type to a standard notation
  type <- ipmisc::stats_type_switch(type)

  # ------------------------------ variable names ----------------------------

  # ensure the variables work quoted or unquoted
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  outlier.label <- if (!rlang::quo_is_null(rlang::enquo(outlier.label))) {
    rlang::ensym(outlier.label)
  }

  # if `xlab` and `ylab` is not provided, use the variable `x` and `y` name
  if (is.null(xlab)) xlab <- rlang::as_name(x)
  if (is.null(ylab)) ylab <- rlang::as_name(y)

  # --------------------------------- data -----------------------------------

  # creating a dataframe
  data %<>%
    dplyr::select(.data = ., {{ x }}, {{ y }}, outlier.label = {{ outlier.label }}) %>%
    tidyr::drop_na(data = .) %>%
    dplyr::mutate(.data = ., {{ x }} := droplevels(as.factor({{ x }}))) %>%
    as_tibble(x = .)

  # if outlier.label column is not present, just use the values from `y` column
  if (rlang::quo_is_null(rlang::enquo(outlier.label))) {
    data %<>% dplyr::mutate(.data = ., outlier.label = {{ y }})
  }

  # add a logical column indicating whether a point is or is not an outlier
  data %<>%
    outlier_df(
      data = .,
      x = {{ x }},
      y = {{ y }},
      outlier.coef = outlier.coef,
      outlier.label = outlier.label
    )

  # figure out which test to run based on the number of levels of the
  # independent variables
  test <- ifelse(nlevels(data %>% dplyr::pull({{ x }}))[[1]] < 3, "t", "anova")

  # --------------------- subtitle/caption preparation ------------------------

  if (isTRUE(results.subtitle)) {
    # preparing the Bayes factor message
    if (type == "parametric" && isTRUE(bf.message)) {
      caption <-
        caption_function_switch(
          test = test,
          data = data,
          x = rlang::as_string(x),
          y = rlang::as_string(y),
          bf.prior = bf.prior,
          top.text = caption,
          paired = FALSE,
          output = "caption",
          k = k
        )
    }

    # extracting the subtitle using the switch function
    subtitle <-
      subtitle_function_switch(
        # switch based on
        type = type,
        test = test,
        # arguments relevant for expression helper functions
        data = data,
        x = {{ x }},
        y = {{ y }},
        paired = FALSE,
        effsize.type = effsize.type,
        var.equal = var.equal,
        bf.prior = bf.prior,
        tr = tr,
        nboot = nboot,
        conf.level = conf.level,
        k = k
      )
  }

  # return early if anything other than plot
  if (output != "plot") {
    return(switch(EXPR = output, "caption" = caption, subtitle))
  }

  # -------------------------- basic plot -----------------------------------

  # first add only the points which are *not* outliers
  plot <-
    ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = {{ x }}, y = {{ y }})) +
    rlang::exec(
      .fn = ggplot2::geom_point,
      data = dplyr::filter(.data = data, !isanoutlier),
      na.rm = TRUE,
      ggplot2::aes(color = {{ x }}),
      !!!point.args
    )

  # if outliers are not being tagged, then add the points that were previously left out
  if (isFALSE(outlier.tagging)) {
    plot <- plot +
      rlang::exec(
        .fn = ggplot2::geom_point,
        data = dplyr::filter(.data = data, isanoutlier),
        na.rm = TRUE,
        ggplot2::aes(color = {{ x }}),
        !!!point.args
      )
  }

  # if outlier tagging is happening, decide how those points should be displayed
  if (plot.type == "violin" && isTRUE(outlier.tagging)) {
    plot <- plot +
      # add all outliers in
      ggplot2::geom_point(
        data = dplyr::filter(.data = data, isanoutlier),
        size = 3,
        stroke = 0,
        alpha = 0.7,
        na.rm = TRUE,
        color = outlier.color,
        shape = outlier.shape
      )
  }

  # adding a boxplot
  if (plot.type %in% c("box", "boxviolin")) {
    if (isTRUE(outlier.tagging)) {
      .f <- ggplot2::stat_boxplot
      outlier_list <- list(
        outlier.shape = outlier.shape,
        outlier.size = 3,
        outlier.alpha = 0.7,
        outlier.color = outlier.color
      )
    } else {
      .f <- ggplot2::geom_boxplot
      outlier_list <- list(
        outlier.shape = NA,
        position = ggplot2::position_dodge(width = NULL)
      )
    }

    # add a boxplot
    suppressWarnings(plot <- plot +
      rlang::exec(
        .fn = .f,
        notch = notch,
        notchwidth = notchwidth,
        width = 0.3,
        alpha = 0.2,
        fill = "white",
        na.rm = TRUE,
        geom = "boxplot",
        coef = outlier.coef,
        !!!outlier_list
      ))
  }

  # add violin geom
  if (plot.type %in% c("violin", "boxviolin")) {
    plot <- plot +
      rlang::exec(
        .fn = ggplot2::geom_violin,
        fill = "white",
        na.rm = TRUE,
        !!!violin.args
      )
  }

  # ---------------------------- outlier labeling -----------------------------

  # If `outlier.label` is not provided, outlier labels will just be values of
  # the `y` vector. If the outlier tag has been provided, just use the dataframe
  # already created.

  # applying the labels to tagged outliers with `ggrepel`
  if (isTRUE(outlier.tagging)) {
    plot <- plot +
      rlang::exec(
        .fn = ggrepel::geom_label_repel,
        data = dplyr::filter(.data = data, isanoutlier),
        mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}, label = outlier.label),
        show.legend = FALSE,
        min.segment.length = 0,
        inherit.aes = FALSE,
        na.rm = TRUE,
        !!!outlier.label.args
      )
  }

  # ---------------- mean value tagging -------------------------------------

  # add labels for mean values
  if (isTRUE(mean.plotting)) {
    plot <-
      mean_ggrepel(
        plot = plot,
        data = data,
        x = {{ x }},
        y = {{ y }},
        k = k,
        inherit.aes = TRUE,
        sample.size.label = sample.size.label,
        mean.point.args = mean.point.args,
        mean.label.args = mean.label.args
      )
  }

  # ggsignif labels -----------------------------------------------------------

  if (isTRUE(pairwise.comparisons) && test == "anova") {
    # creating dataframe with pairwise comparison results
    df_pairwise <-
      pairwiseComparisons::pairwise_comparisons(
        data = data,
        x = {{ x }},
        y = {{ y }},
        type = type,
        tr = tr,
        paired = FALSE,
        var.equal = var.equal,
        p.adjust.method = p.adjust.method,
        k = k
      )

    # adding the layer for pairwise comparisons
    plot <-
      ggsignif_adder(
        plot = plot,
        df_pairwise = df_pairwise,
        data = data,
        x = {{ x }},
        y = {{ y }},
        pairwise.display = pairwise.display,
        ggsignif.args = ggsignif.args
      )

    # preparing the caption for pairwise comparisons test
    if (type != "bayes") {
      caption <-
        pairwiseComparisons::pairwise_caption(
          caption,
          unique(df_pairwise$test.details),
          pairwise.display
        )
    }
  }

  # ------------------------ annotations and themes -------------------------

  # specifying annotations and other aesthetic aspects for the plot
  aesthetic_addon(
    plot = plot,
    x = data %>% dplyr::pull({{ x }}),
    xlab = xlab,
    ylab = ylab,
    title = title,
    subtitle = subtitle,
    caption = caption,
    ggtheme = ggtheme,
    ggstatsplot.layer = ggstatsplot.layer,
    package = package,
    palette = palette,
    ggplot.component = ggplot.component
  )
}
