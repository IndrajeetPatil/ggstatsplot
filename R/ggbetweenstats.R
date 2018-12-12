#' @title Box/Violin plots for group or condition comparisons in
#'   between-subjects designs.
#' @name ggbetweenstats
#' @aliases ggbetweenstats
#' @description A combination of box and violin plots along with jittered data
#'   points for between-subjects designs with statistical details included in
#'   the plot as a subtitle.
#' @author Indrajeet Patil
#'
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param x The grouping variable.
#' @param y The response - a vector of length the number of rows of `x`.
#' @param plot.type Character describing the *type* of plot. Currently supported
#'   plots are `"box"` (for pure boxplots), `"violin"` (for pure violin plots),
#'   and `"boxviolin"` (for a mix of box and violin plots; default).
#' @param xlab Label for `x` axis variable.
#' @param ylab Label for `y` axis variable.
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
#' @param bf.prior A number between 0.5 and 2 (default `0.707`), the prior width
#'   to use in calculating Bayes factors.
#' @param bf.message Logical that decides whether to display Bayes Factor in
#'   favor of the *null* hypothesis **for parametric test** (Default: `FALSE`).
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
#' @inheritParams paletteer::scale_color_paletteer_d
#' @inheritParams theme_ggstatsplot
#' @inheritParams subtitle_anova_parametric
#' @inheritParams subtitle_t_parametric
#' @inheritParams t1way_ci
#'
#' @import ggplot2
#'
#' @importFrom dplyr select group_by arrange mutate mutate_at mutate_if
#' @importFrom ggrepel geom_label_repel
#' @importFrom WRS2 t1way yuen yuen.effect.ci
#' @importFrom effsize cohen.d
#' @importFrom sjstats eta_sq omega_sq
#' @importFrom stats na.omit t.test oneway.test
#' @importFrom coin wilcox_test statistic
#' @importFrom rlang enquo quo_name !!
#' @importFrom ggrepel geom_label_repel
#' @importFrom crayon blue green red yellow
#' @importFrom paletteer scale_color_paletteer_d scale_fill_paletteer_d
#' @importFrom ggsignif geom_signif
#' @importFrom purrrlyr by_row
#'
#' @seealso \code{\link{grouped_ggbetweenstats}},
#'  \code{\link{pairwise_p}}
#'
#' @details
#'
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
#' Variant of this function `ggwithinstats` is currently under work. You *can*
#' still use this function just to prepare the **plot** for exploratory data
#' analysis, but the statistical details displayed in the subtitle will be
#' incorrect. You can remove them by adding `+ ggplot2::labs(subtitle = NULL)`.
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggbetweenstats.html}
#'
#' @examples
#'
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#'
#' # simple function call with the defaults
#' ggstatsplot::ggbetweenstats(
#'   data = mtcars,
#'   x = am,
#'   y = mpg,
#'   title = "Fuel efficiency by type of car transmission",
#'   caption = "Transmission (0 = automatic, 1 = manual)",
#'   bf.message = TRUE
#' )
#'
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
#'   ggtheme = ggthemes::theme_few(),
#'   ggstatsplot.layer = FALSE,
#'   bf.message = TRUE
#' )
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
                           effsize.noncentral = FALSE,
                           bf.prior = 0.707,
                           bf.message = FALSE,
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
                           point.jitter.height = 0.1,
                           point.dodge.width = 0.60,
                           ggtheme = ggplot2::theme_bw(),
                           ggstatsplot.layer = TRUE,
                           package = "RColorBrewer",
                           palette = "Dark2",
                           direction = 1,
                           messages = TRUE) {

  # no pairwise comparisons are available for bayesian t-tests
  if (type %in% c("bf", "bayes") && isTRUE(pairwise.comparisons)) {
    # turn off pairwise comparisons
    pairwise.comparisons <- FALSE
  }

  # ------------------------------ variable names ----------------------------

  # preparing a dataframe with variable names
  lab.df <- colnames(x = dplyr::select(
    .data = data,
    !!rlang::enquo(x),
    !!rlang::enquo(y)
  ))

  # if `xlab` is not provided, use the variable `x` name
  if (is.null(xlab)) {
    xlab <- lab.df[1]
  }

  # if `ylab` is not provided, use the variable `y` name
  if (is.null(ylab)) {
    ylab <- lab.df[2]
  }

  # --------------------------------- data -----------------------------------

  # if outlier label is provided then include it in the dataframe
  if (base::missing(outlier.label)) {

    # if outlier label is not provided then only include the two arguments
    # provided
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(x),
        y = !!rlang::enquo(y)
      ) %>%
      dplyr::mutate(
        .data = .,
        outlier.label = y
      )
  } else {

    # if outlier label is provided then include it to make a dataframe
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(x),
        y = !!rlang::enquo(y),
        outlier.label = !!rlang::quo_name(rlang::enquo(outlier.label))
      )
  }

  # convert the grouping variable to factor and drop unused levels
  data %<>%
    dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "x",
      .funs = ~ base::droplevels(x = base::as.factor(x = .))
    ) %>%
    tibble::as_tibble(x = .)

  # if no. of factor levels is greater than the default palette color count
  palette_message(
    package = package,
    palette = palette,
    min_length = length(unique(levels(data$x)))[[1]]
  )

  # -------------------------------- plot -----------------------------------

  # create the basic plot
  plot <-
    ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes(x = x, y = y)
    ) +
    ggplot2::geom_point(
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
#        ggplot2::geom_boxplot(
#          notch = notch,
#          notchwidth = notchwidth,
#          linetype = linetype,
#          width = 0.3,
#          alpha = 0.2,
#          fill = "white",
#          position = ggplot2::position_dodge(width = NULL),
#          na.rm = TRUE,
#          outlier.color = outlier.color
#        ) +
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

  # --------------------- subtitle preparation -------------------------------

  if (isTRUE(results.subtitle)) {
    # figure out which test to run based on the number of levels of the
    # independent variables
    if (length(levels(as.factor(data$x))) < 3) {
      test <- "t-test"
    } else {
      test <- "anova"
    }

    # figuring out which effect size to use
    effsize.type <- effsize_type_switch(effsize.type)

    # preparing the bayes factor message
    if (test == "t-test") {

      # preparing the BF message for null
      if (isTRUE(bf.message)) {
        bf.caption.text <-
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
      }
    } else if (test == "anova") {
      # preparing the BF message for null
      if (isTRUE(bf.message)) {
        bf.caption.text <-
          bf_oneway_anova(
            data = data,
            x = x,
            y = y,
            bf.prior = bf.prior,
            caption = caption,
            output = "caption"
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

    # if bayes factor message needs to be displayed
    if (type %in% c("parametric", "p") && isTRUE(bf.message)) {
      caption <- bf.caption.text
    }
  }

  # ---------------------------- outlier tagging -----------------------------

  # If `outlier.label` is not provided, outlier labels will just be values of
  # the `y` vector. If the outlier tag has been provided, just use the dataframe
  # already created.

  if (isTRUE(outlier.tagging)) {
    # finding and tagging the outliers
    data_outlier_label <- data %>%
      dplyr::group_by(.data = ., x) %>%
      dplyr::mutate(
        .data = .,
        outlier = base::ifelse(
          test = check_outlier(
            var = y,
            coef = outlier.coef
          ),
          yes = outlier.label,
          no = NA
        )
      ) %>%
      dplyr::ungroup(x = .) %>%
      stats::na.omit(.) %>%
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

  # highlight the mean of each group
  if (isTRUE(mean.plotting)) {
    plot <- plot +
      ggplot2::stat_summary(
        fun.y = mean,
        geom = "point",
        color = mean.color,
        size = mean.size,
        na.rm = TRUE
      )

    # attach the labels with means to the plot
    plot <- plot +
      ggrepel::geom_label_repel(
        data = mean_dat,
        mapping = ggplot2::aes(x = x, y = y, label = label),
        size = mean.label.size,
        fontface = mean.label.fontface,
        color = mean.label.color,
        direction = "both",
        max.iter = 3e2,
        box.padding = 0.35,
        point.padding = 0.5,
        segment.color = "black",
        force = 2,
        na.rm = TRUE,
        seed = 123
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
      ggstatsplot::pairwise_p(
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

    # creating a column for group combinations
    df_pairwise %<>%
      purrrlyr::by_row(
        .d = .,
        ..f = ~ c(.$group1, .$group2),
        .collate = "list",
        .to = "groups"
      )

    # decide what needs to be displayed:
    # only significant or non-significant comparisons
    if (pairwise.display %in% c("s", "significant")) {
      df_pairwise %<>%
        dplyr::filter(.data = ., significance != "ns")
    } else if (pairwise.display %in% c("ns", "nonsignificant", "non-significant")) {
      df_pairwise %<>%
        dplyr::filter(.data = ., significance == "ns")
    }

    # proceed only if there are any significant comparisons to display
    if (dim(df_pairwise)[[1]] != 0L) {

      # deciding what needs to be displayed
      if (pairwise.annotation %in% c("p", "p-value", "p.value")) {
        # if p-values are to be displayed
        df_pairwise %<>%
          dplyr::rename(.data = ., label = p.value.label)

        # for ggsignif
        textsize <- 3
        vjust <- 0
      } else {
        # otherwise just show the asterisks
        df_pairwise %<>%
          dplyr::rename(.data = ., label = significance)

        # for ggsignif
        textsize <- 4
        vjust <- 0.2
      }

      # arrange the dataframe so that annotations are properly aligned
      df_pairwise %<>%
        dplyr::arrange(.data = ., group1)

      # retaining data corresponding to the levels of the grouping variable for
      # which the comparisons are to be drawn
      data_ggsignif <-
        dplyr::filter(.data = data, x %in%
          unique(x = c(levels(
            as.factor(df_pairwise$group1)
          ), levels(
            as.factor(df_pairwise$group2)
          )))) %>%
        dplyr::mutate_if(
          .tbl = .,
          .predicate = base::is.factor,
          .funs = ~ as.character(.)
        ) %>%
        dplyr::arrange(.data = ., x) %>%
        tibble::as_tibble(x = .)

      # computing y coordinates for ggsgnif bars
      ggsignif_y_position <-
        ggsignif_position_calculator(x = data$x, y = data$y)

      # adding ggsignif comparisons to the plot
      plot <- plot +
        ggsignif::geom_signif(
          comparisons = df_pairwise$groups,
          map_signif_level = TRUE,
          textsize = textsize,
          tip_length = 0.01,
          vjust = vjust,
          y_position = ggsignif_y_position,
          annotations = df_pairwise$label,
          test = NULL,
          na.rm = TRUE
        )
    }

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

  # specifying theme and labels for the final plot
  plot <- plot +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption,
      color = lab.df[1]
    ) +
    ggstatsplot::theme_mprl(
      ggtheme = ggtheme,
      ggstatsplot.layer = ggstatsplot.layer
    ) +
    ggplot2::theme(legend.position = "none")

  # don't do scale restriction in case of post hoc comparisons
  if (!isTRUE(pairwise.comparisons)) {
    plot <- plot +
      ggplot2::coord_cartesian(ylim = c(min(data$y), max(data$y))) +
      ggplot2::scale_y_continuous(limits = c(min(data$y), max(data$y)))
  }

  # choosing palette
  plot <- plot +
    paletteer::scale_color_paletteer_d(
      package = !!package,
      palette = !!palette,
      direction = direction
    ) +
    paletteer::scale_fill_paletteer_d(
      package = !!package,
      palette = !!palette,
      direction = direction
    )

  # --------------------- messages ------------------------------------------

  if (isTRUE(messages)) {

    # display normality test result as a message
    normality_message(
      x = data$y,
      lab = lab.df[2],
      k = k,
      output = "message"
    )

    # display homogeneity of variance test as a message
    bartlett_message(
      data = data,
      x = x,
      y = y,
      lab = lab.df[1],
      k = k,
      output = "message"
    )
  }

  # return the final plot
  return(plot)
}
