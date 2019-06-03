#' @title Box/Violin plots for group or condition comparisons in
#'   between-subjects designs.
#' @name ggbetweenstats
#' @description A combination of box and violin plots along with jittered data
#'   points for between-subjects designs with statistical details included in
#'   the plot as a subtitle.
#' @author Indrajeet Patil
#'
#' @param plot.type Character describing the *type* of plot. Currently supported
#'   plots are `"box"` (for pure boxplots), `"violin"` (for pure violin plots),
#'   and `"boxviolin"` (for a combination of box and violin plots; default).
#' @param xlab,ylab Labels for `x` and `y` axis variables. If `NULL` (default),
#'   variable names for `x` and `y` will be used.
#' @param type Type of statistic expected (`"parametric"` or `"nonparametric"`
#'   or `"robust"` or `"bayes"`).Corresponding abbreviations are also accepted:
#'   `"p"` (for parametric), `"np"` (nonparametric), `"r"` (robust), or
#'   `"bf"`resp.
#' @param pairwise.comparisons Logical that decides whether pairwise comparisons
#'   are to be displayed. **Only significant comparisons** will be shown by
#'   default. (default: `FALSE`). To change this behavior, select appropriate
#'   option with `pairwise.display` argument.
#' @param p.adjust.method Adjustment method for *p*-values for multiple
#'   comparisons. Possible methods are: `"holm"` (default), `"hochberg"`,
#'   `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.
#' @param pairwise.annotation Character that decides the annotations to use for
#'   pairwise comparisons. Either `"p.value"` or `"asterisk"` (default).
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
#' @param mean.label.size,mean.label.fontface,mean.label.color Aesthetics for
#' the label displaying mean. Defaults: `3`, `"bold"`,`"black"`, respectively.
#' @param notch A logical. If `FALSE` (default), a standard box plot will be
#'   displayed. If `TRUE`, a notched box plot will be used. Notches are used to
#'   compare groups; if the notches of two boxes do not overlap, this suggests
#'   that the medians are significantly different. In a notched box plot, the
#'   notches extend `1.58 * IQR / sqrt(n)`. This gives a roughly `95%`
#'   confidence interval for comparing medians. IQR: Inter-Quartile Range.
#' @param notchwidth For a notched box plot, width of the notch relative to the
#'   body (default `0.5`).
#' @param linetype Character strings (`"blank"`, `"solid"`, `"dashed"`,
#'   `"dotted"`, `"dotdash"`, `"longdash"`, and `"twodash"`) specifying the type
#'   of line to draw box plots (Default: `"solid"`). Alternatively, the numbers
#'   `0` to `6` can be used (`0` for "blank", `1` for "solid", etc.).
#' @param outlier.color Default aesthetics for outliers (Default: `"black"`).
#' @param outlier.tagging Decides whether outliers should be tagged (Default:
#'   `FALSE`).
#' @param outlier.label Label to put on the outliers that have been tagged.
#' @param outlier.shape Hiding the outliers can be achieved by setting
#'   outlier.shape = NA. Importantly, this does not remove the outliers,
#'   it only hides them, so the range calculated for the y-axis will be
#'   the same with outliers shown and outliers hidden.
#' @param outlier.label.color Color for the label to to put on the outliers that
#'   have been tagged (Default: `"black"`).
#' @param outlier.coef Coefficient for outlier detection using Tukey's method.
#'   With Tukey's method, outliers are below (1st Quartile) or above (3rd
#'   Quartile) `outlier.coef` times the Inter-Quartile Range (IQR) (Default:
#'   `1.5`).
#' @param mean.plotting Logical that decides whether mean is to be highlighted
#'   and its value to be displayed (Default: `TRUE`).
#' @param mean.ci Logical that decides whether 95% confidence interval for mean
#'   is to be displayed (Default: `FALSE`).
#' @param mean.color Color for the data point corresponding to mean (Default:
#'   `"darkred"`).
#' @param mean.size Point size for the data point corresponding to mean
#'   (Default: `5`).
#' @param palette If a character string (e.g., `"Set1"`), will use that named
#'   palette. If a number, will index into the list of palettes of appropriate
#'   type. Default palette is `"Dark2"`.
#' @param point.jitter.width Numeric specifying the degree of jitter in `x`
#'   direction. Defaults to `40%` of the resolution of the data.
#' @param point.jitter.height Numeric specifying the degree of jitter in `y`
#'   direction. Defaults to `0.1`.
#' @param point.dodge.width Numeric specifying the amount to dodge in the `x`
#'   direction. Defaults to `0.60`.
#' @param ggplot.component A `ggplot` component to be added to the plot prepared
#'   by `ggstatsplot`. This argument is primarily helpful for `grouped_` variant
#'   of the current function. Default is `NULL`. The argument should be entered
#'   as a function. If the given function has an argument `axes.range.restrict`
#'   and if it has been set to `TRUE`, the added ggplot component *might* not
#'   work as expected.
#' @param axes.range.restrict Logical that decides whether to restrict the axes
#'   values ranges to `min` and `max` values of the axes variables (Default:
#'   `FALSE`), only relevant for functions where axes variables are of numeric
#'   type.
#' @param sort If `"ascending"` (default), `x`-axis variable factor levels will
#'   be sorted based on increasing values of `y`-axis variable. If
#'   `"descending"`, the opposite. If `"none"`, no sorting will happen.
#' @param sort.fun The function used to sort (default: `mean`).
#' @inheritParams paletteer::scale_color_paletteer_d
#' @inheritParams theme_ggstatsplot
#' @inheritParams t1way_ci
#' @inheritParams subtitle_anova_parametric
#' @inheritParams subtitle_t_parametric
#'
#' @import ggplot2
#'
#' @importFrom dplyr select group_by arrange mutate mutate_at mutate_if
#' @importFrom ggrepel geom_label_repel
#' @importFrom stats na.omit t.test oneway.test
#' @importFrom rlang enquo quo_name as_name !!
#' @importFrom ggrepel geom_label_repel
#' @importFrom crayon blue green red yellow
#' @importFrom paletteer scale_color_paletteer_d scale_fill_paletteer_d
#' @importFrom ggsignif geom_signif
#' @importFrom purrrlyr by_row
#'
#' @seealso \code{\link{grouped_ggbetweenstats}}, \code{\link{ggwithinstats}},
#'  \code{\link{grouped_ggwithinstats}}, \code{\link{pairwise_p}}
#'
#' @details
#' For parametric tests, Welch's ANOVA/*t*-test are used as a default (i.e.,
#' `var.equal = FALSE`).
#' References:
#' \itemize{
#'  \item ANOVA: Delacre, Leys, Mora, & Lakens, *PsyArXiv*, 2018
#'  \item *t*-test: Delacre, Lakens, & Leys,
#'  *International Review of Social Psychology*, 2017
#'  }
#'
#'  If robust tests are selected, following tests are used is .
#' \itemize{
#'  \item ANOVA: one-way ANOVA on trimmed means (see `?WRS2::t1way`)
#'  \item *t*-test: Yuen's test for trimmed means (see `?WRS2::yuen`)
#'  }
#'
#'  For more about how the effect size measures (for nonparametric tests) and
#'  their confidence intervals are computed, see `?rcompanion::wilcoxonR`.
#'
#'  For repeated measures designs, use `ggwithinstats`.
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggbetweenstats.html}
#'
#' @examples
#'
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
#' \dontrun{
#' # more detailed function call
#' ggstatsplot::ggbetweenstats(
#'   data = datasets::morley,
#'   x = Expt,
#'   y = Speed,
#'   plot.type = "box",
#'   conf.level = 0.99,
#'   xlab = "The experiment number",
#'   ylab = "Speed-of-light measurement",
#'   pairwise.comparisons = TRUE,
#'   pairwise.annotation = "p.value",
#'   p.adjust.method = "fdr",
#'   outlier.tagging = TRUE,
#'   outlier.label = Run,
#'   nboot = 10,
#'   ggtheme = ggplot2::theme_grey(),
#'   ggstatsplot.layer = FALSE,
#'   bf.message = FALSE
#' )
#' }
#' @export

# defining the function
ggbetweenstats <- function(data,
                           x,
                           y,
                           plot.type = "boxviolin",
                           type = "parametric",
                           pairwise.comparisons = FALSE,
                           pairwise.annotation = "asterisk",
                           pairwise.display = "significant",
                           p.adjust.method = "holm",
                           effsize.type = "unbiased",
                           partial = TRUE,
                           effsize.noncentral = TRUE,
                           bf.prior = 0.707,
                           bf.message = TRUE,
                           results.subtitle = TRUE,
                           xlab = NULL,
                           ylab = NULL,
                           caption = NULL,
                           title = NULL,
                           subtitle = NULL,
                           sample.size.label = TRUE,
                           k = 2,
                           var.equal = FALSE,
                           conf.level = 0.95,
                           nboot = 100,
                           tr = 0.1,
                           sort = "none",
                           sort.fun = mean,
                           axes.range.restrict = FALSE,
                           mean.label.size = 3,
                           mean.label.fontface = "bold",
                           mean.label.color = "black",
                           notch = FALSE,
                           notchwidth = 0.5,
                           linetype = "solid",
                           outlier.tagging = FALSE,
                           outlier.shape = 19,
                           outlier.label = NULL,
                           outlier.label.color = "black",
                           outlier.color = "black",
                           outlier.coef = 1.5,
                           mean.plotting = TRUE,
                           mean.ci = FALSE,
                           mean.size = 5,
                           mean.color = "darkred",
                           point.jitter.width = NULL,
                           point.jitter.height = 0,
                           point.dodge.width = 0.60,
                           ggtheme = ggplot2::theme_bw(),
                           ggstatsplot.layer = TRUE,
                           package = "RColorBrewer",
                           palette = "Dark2",
                           direction = 1,
                           ggplot.component = NULL,
                           messages = TRUE) {

  # no pairwise comparisons are available for bayesian t-tests
  if (type %in% c("bf", "bayes") && isTRUE(pairwise.comparisons)) {
    # turn off pairwise comparisons
    pairwise.comparisons <- FALSE
  }

  # ------------------------------ variable names ----------------------------

  # if `xlab` is not provided, use the variable `x` name
  if (is.null(xlab)) {
    xlab <- rlang::as_name(rlang::ensym(x))
  }

  # if `ylab` is not provided, use the variable `y` name
  if (is.null(ylab)) {
    ylab <- rlang::as_name(rlang::ensym(y))
  }

  # --------------------------------- data -----------------------------------

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y),
      outlier.label = !!rlang::enquo(outlier.label)
    ) %>%
    tidyr::drop_na(data = .) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x))) %>%
    tibble::as_tibble(x = .)

  # if outlier.label column is not present, just use the values from `y` column
  if (!"outlier.label" %in% names(data)) {
    data %<>%
      dplyr::mutate(.data = ., outlier.label = y)
  }

  # add a logical column indicating whether a point is or is not an outlier
  data %<>%
    outlier_df(
      data = .,
      x = x,
      y = y,
      outlier.coef = outlier.coef,
      outlier.label = outlier.label
    )

  # figure out which test to run based on the number of levels of the
  # independent variables
  if (length(levels(as.factor(data$x))) < 3) {
    test <- "t-test"
  } else {
    test <- "anova"
  }

  # --------------------------------- sorting --------------------------------

  # if sorting is happening
  if (sort != "none") {
    data %<>%
      sort_xy(
        data = .,
        x = x,
        y = y,
        sort = sort,
        sort.fun = sort.fun
      )
  }

  # -------------------------- basic plot -----------------------------------

  # create the basic plot
  plot <-
    ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes(x = x, y = y)
    ) +
    # add all points which are not outliers
    ggplot2::geom_point(
      data = dplyr::filter(.data = data, !isanoutlier),
      position = ggplot2::position_jitterdodge(
        jitter.width = point.jitter.width,
        dodge.width = point.dodge.width,
        jitter.height = point.jitter.height
      ),
      alpha = 0.4,
      size = 3,
      stroke = 0,
      na.rm = TRUE,
      ggplot2::aes(color = factor(x))
    )

  # decide how to plot outliers if it's desired
  if (isFALSE(outlier.tagging)) {
    plot <- plot +
      # add all outliers in using same method
      ggplot2::geom_point(
        data = dplyr::filter(.data = data, isanoutlier),
        position = ggplot2::position_jitterdodge(
          jitter.width = point.jitter.width,
          dodge.width = point.dodge.width,
          jitter.height = point.jitter.height
        ),
        alpha = 0.4,
        size = 3,
        stroke = 0,
        na.rm = TRUE,
        ggplot2::aes(color = factor(x))
      )
  } else {
    if (plot.type == "violin") {
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
  }

  # single component for creating geom_violin
  ggbetweenstats_geom_violin <-
    ggplot2::geom_violin(
      width = 0.5,
      alpha = 0.2,
      fill = "white",
      na.rm = TRUE
    )

  if (plot.type %in% c("box", "boxviolin")) {
    # adding a boxplot
    if (isTRUE(outlier.tagging)) {
      plot <- plot +
        ggplot2::stat_boxplot(
          notch = notch,
          notchwidth = notchwidth,
          linetype = linetype,
          geom = "boxplot",
          width = 0.3,
          alpha = 0.2,
          fill = "white",
          outlier.shape = outlier.shape,
          outlier.size = 3,
          outlier.alpha = 0.7,
          outlier.color = outlier.color,
          coef = outlier.coef,
          na.rm = TRUE
        )
    } else {
      plot <- plot +
        ggplot2::geom_boxplot(
          notch = notch,
          notchwidth = notchwidth,
          linetype = linetype,
          width = 0.3,
          alpha = 0.2,
          fill = "white",
          outlier.shape = NA,
          position = ggplot2::position_dodge(width = NULL),
          na.rm = TRUE
        )
    }
    if (plot.type == "boxviolin") {
      plot <- plot +
        ggbetweenstats_geom_violin
    }
  } else if (plot.type == "violin") {
    plot <- plot +
      ggbetweenstats_geom_violin
  }

  # --------------------- subtitle/caption preparation ------------------------

  if (isTRUE(results.subtitle)) {

    # figuring out which effect size to use
    effsize.type <- effsize_type_switch(effsize.type)

    # preparing the bayes factor message
    if (type %in% c("parametric", "p") && isTRUE(bf.message)) {
      # preparing the BF message for null
      if (test == "t-test") {
        caption <-
          bf_two_sample_ttest(
            data = data,
            x = x,
            y = y,
            bf.prior = bf.prior,
            caption = caption,
            paired = FALSE,
            output = "caption",
            k = k
          )
      } else if (test == "anova") {
        # preparing the BF message for null
        caption <-
          bf_oneway_anova(
            data = data,
            x = x,
            y = y,
            bf.prior = bf.prior,
            caption = caption,
            output = "caption",
            k = k
          )
      }
    }

    # extracting the subtitle using the switch function
    subtitle <-
      ggbetweenstats_switch(
        # switch based on
        type = type,
        test = test,
        # arguments relevant for subtitle helper functions
        data = data,
        x = x,
        y = y,
        paired = FALSE,
        effsize.type = effsize.type,
        partial = partial,
        effsize.noncentral = effsize.noncentral,
        var.equal = var.equal,
        bf.prior = bf.prior,
        tr = tr,
        nboot = nboot,
        conf.level = conf.level,
        k = k,
        messages = messages
      )
  } else {
    test <- "none"
  }

  # ---------------------------- outlier tagging -----------------------------

  # If `outlier.label` is not provided, outlier labels will just be values of
  # the `y` vector. If the outlier tag has been provided, just use the dataframe
  # already created.

  if (isTRUE(outlier.tagging)) {
    # finding and tagging the outliers
    data_outlier_label <- data %>%
      dplyr::filter(.data = ., isanoutlier) %>%
      dplyr::select(.data = ., -outlier)

    # applying the labels to tagged outliers with ggrepel
    plot <-
      plot +
      ggrepel::geom_label_repel(
        data = data_outlier_label,
        mapping = ggplot2::aes(x = x, y = y, label = outlier.label),
        fontface = "bold",
        color = outlier.label.color,
        max.iter = 3e2,
        box.padding = 0.35,
        point.padding = 0.5,
        segment.color = "black",
        force = 2,
        na.rm = TRUE,
        seed = 123
      )
  }

  # ---------------- mean value tagging -------------------------------------

  # computing mean and confidence interval for mean using helper function
  # creating label column based on whether just mean is to be displayed or
  # mean plus its CI
  mean_dat <-
    mean_labeller(
      data = data,
      x = x,
      y = y,
      mean.ci = mean.ci,
      k = k
    )

  # add labels for mean values
  if (isTRUE(mean.plotting)) {
    plot <- mean_ggrepel(
      plot = plot,
      mean.data = mean_dat,
      mean.size = mean.size,
      mean.color = mean.color,
      mean.label.size = mean.label.size,
      mean.label.fontface = mean.label.fontface,
      mean.label.color = mean.label.color
    )
  }

  # ----------------- sample size labels --------------------------------------

  # adding sample size labels to the x axes
  if (isTRUE(sample.size.label)) {
    plot <- plot +
      ggplot2::scale_x_discrete(labels = c(unique(mean_dat$n_label)))
  }

  # ggsignif labels -----------------------------------------------------------

  if (isTRUE(pairwise.comparisons) && test == "anova") {
    # creating dataframe with pairwise comparison results
    df_pairwise <-
      pairwise_p(
        data = data,
        x = x,
        y = y,
        type = type,
        tr = tr,
        paired = FALSE,
        var.equal = var.equal,
        p.adjust.method = p.adjust.method,
        k = k,
        messages = FALSE
      )

    # display the results if needed
    if (isTRUE(messages)) {
      print(df_pairwise)
    }

    # adding the layer for pairwise comparisons
    plot <- ggsignif_adder(
      plot = plot,
      df_pairwise = df_pairwise,
      data = data,
      pairwise.annotation = pairwise.annotation,
      pairwise.display = pairwise.display
    )

    # preparing the caption for pairwise comparisons test
    caption <-
      pairwise_p_caption(
        type = type,
        var.equal = var.equal,
        paired = FALSE,
        p.adjust.method = p.adjust.method,
        caption = caption
      )
  }

  # ------------------------ annotations and themes -------------------------

  # specifiying annotations and other aesthetic aspects for the plot
  plot <-
    aesthetic_addon(
      plot = plot,
      x = data$x,
      xlab = xlab,
      ylab = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      ggtheme = ggtheme,
      ggstatsplot.layer = ggstatsplot.layer,
      package = package,
      palette = palette,
      direction = direction,
      ggplot.component = ggplot.component
    )

  # don't do scale restriction in case of post hoc comparisons
  if (isTRUE(axes.range.restrict) && !isTRUE(pairwise.comparisons)) {
    plot <- plot +
      ggplot2::coord_cartesian(ylim = c(min(data$y), max(data$y))) +
      ggplot2::scale_y_continuous(limits = c(min(data$y), max(data$y)))
  }

  # --------------------- messages ------------------------------------------

  if (isTRUE(messages)) {

    # display normality test result as a message
    normality_message(
      x = data$y,
      lab = ylab,
      k = k,
      output = "message"
    )

    # display homogeneity of variance test as a message
    bartlett_message(
      data = data,
      x = x,
      y = y,
      lab = xlab,
      k = k,
      output = "message"
    )
  }

  # return the final plot
  return(plot)
}
