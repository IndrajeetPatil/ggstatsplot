#'
#' @title Violin plots for group or condition comparisons in between-subjects
#'   designs.
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
#'   or `"robust"`).Corresponding abbreviations are also accepted: `"p"` (for
#'   parametric), `"np"` (nonparametric), `"r"` (robust), resp.
#' @param effsize.type Type of effect size needed for *parametric* tests
#'   (`"biased"` (Cohen's *d* for **t-test**; partial eta-squared for **anova**)
#'   or `"unbiased"` (Hedge's *g* for **t-test**; partial omega-squared for
#'   **anova**)).
#' @param effsize.noncentral Logical indicating whether to use non-central
#'   *t*-distributions for computing the 95% confidence interval for Cohen's *d*
#'   or Hedge's *g* (Default: `FALSE`).
#' @param title The text for the plot title.
#' @param caption The text for the plot caption.
#' @param sample.size.label Logical that decides whether sample size information
#'   should be displayed for each level of the grouping variable `x` (Default:
#'   `TRUE`).
#' @param k Number of decimal places expected for results.
#' @param var.equal A logical variable indicating whether to treat the two
#'   variances as being equal (Default: `FALSE`).
#' @param nboot Number of bootstrap samples for computing effect size (Default:
#'   `100`).
#' @param tr Trim level for the mean when carrying out `robust` tests. If you
#'   get error stating "Standard error cannot be computed because of Winsorized
#'   variance of 0 (e.g., due to ties). Try to decrease the trimming level.",
#'   try to play around with the value of `tr`, which is by default set to
#'   `0.1`. Lowering the value might help.
#' @param conf.type A vector of character strings representing the type of
#'   confidence intervals required from bootstrapping for partial eta- and
#'   omega-squared. The value should be any subset of the values `"norm"`,
#'   `"basic"`, `"perc"`, `"bca"`. For more, see `?boot::boot.ci`.
#' @param mean.label.size,mean.label.fontface,mean.label.color Aesthetics for
#'   the label displaying mean. Defaults: `3`, `"bold"`,`"black"`, respectively.
#' @param conf.level Scalar between 0 and 1. If `NULL`, the defaults return
#'   `95%` lower and upper confidence intervals (`0.95`).
#' @param notch A logical. If `FALSE` (default), a standard box plot will be
#'   displayed. If `TRUE`, a notched box plot will be used. Notches are used to
#'   compare groups; if the notches of two boxes do not overlap, this suggests
#'   that the medians are significantly different. In a notched box plot, the
#'   notches extend `1.58 * IQR / sqrt(n)`. This gives a roughly `95%` confidence
#'   interval for comparing medians. IQR: Inter-Quartile Range.
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
#' @param ggtheme A function, `ggplot2` theme name. Default value is
#'   `ggplot2::theme_bw()`. Allowed values are the official `ggplot2` themes,
#'   including `ggplot2::theme_grey()`, `ggplot2::theme_minimal()`,
#' `ggplot2::theme_classic()`, `ggplot2::theme_void()`, etc.
#' @param palette If a character string (e.g., `"Set1"`), will use that named
#'   palette. If a number, will index into the list of palettes of appropriate
#'   type. Default palette is `"Dark2"`.
#' @param point.jitter.width Numeric specifying the degree of jitter in `x`
#'   direction. Defaults to `40%` of the resolution of the data.
#' @param point.jitter.height Numeric specifying the degree of jitter in `y`
#'   direction. Defaults to `0.1`.
#' @param point.dodge.width Numeric specifying the amount to dodge in the `x`
#'   direction. Defaults to `0.60`.
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#'
#' @import ggplot2
#'
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr mutate_if
#' @importFrom ggrepel geom_label_repel
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @importFrom WRS2 t1way
#' @importFrom WRS2 yuen
#' @importFrom WRS2 yuen.effect.ci
#' @importFrom effsize cohen.d
#' @importFrom sjstats eta_sq
#' @importFrom sjstats omega_sq
#' @importFrom stats aov
#' @importFrom stats sd
#' @importFrom stats na.omit
#' @importFrom stats t.test
#' @importFrom stats var.test
#' @importFrom stats bartlett.test
#' @importFrom stats kruskal.test
#' @importFrom stats aov
#' @importFrom stats quantile
#' @importFrom stats oneway.test
#' @importFrom stats qt
#' @importFrom coin wilcox_test
#' @importFrom coin statistic
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom ggrepel geom_label_repel
#' @importFrom crayon green
#' @importFrom crayon blue
#' @importFrom crayon yellow
#' @importFrom crayon red
#'
#' @seealso \code{\link{grouped_ggbetweenstats}}
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/ggbetweenstats.html}
#'
#' @examples
#'
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#'
#' # simple function call with the defaults
#' ggstatsplot::ggbetweenstats(
#' data = datasets::iris,
#' x = Species,
#' y = Sepal.Length
#' )
#'
#' # more detailed function call
#' ggstatsplot::ggbetweenstats(
#' data = datasets::ToothGrowth,
#' x = supp,
#' y = len,
#' plot.type = "box",
#' xlab = "Supplement type",
#' ylab = "Tooth length")
#'
#' @export
#'

# defining the function
ggbetweenstats <- function(data,
                           x,
                           y,
                           plot.type = "boxviolin",
                           type = "parametric",
                           effsize.type = "unbiased",
                           effsize.noncentral = FALSE,
                           xlab = NULL,
                           ylab = NULL,
                           caption = NULL,
                           title = NULL,
                           sample.size.label = TRUE,
                           k = 3,
                           var.equal = FALSE,
                           nboot = 100,
                           tr = 0.1,
                           conf.level = 0.95,
                           conf.type = "norm",
                           mean.label.size = 3,
                           mean.label.fontface = "bold",
                           mean.label.color = "black",
                           notch = FALSE,
                           notchwidth = 0.5,
                           linetype = "solid",
                           outlier.tagging = FALSE,
                           outlier.label = NULL,
                           outlier.label.color = "black",
                           outlier.color = "black",
                           outlier.coef = 1.5,
                           mean.plotting = TRUE,
                           mean.ci = FALSE,
                           mean.size = 5,
                           mean.color = "darkred",
                           ggtheme = ggplot2::theme_bw(),
                           palette = "Dark2",
                           point.jitter.width = NULL,
                           point.jitter.height = 0.1,
                           point.dodge.width = 0.60,
                           messages = TRUE) {
  ####################################### creating a dataframe #################################################

  # preparing labels from given dataframe
  lab.df <- colnames(dplyr::select(.data = data,
                                   !!rlang::enquo(x),
                                   !!rlang::enquo(y)))
  # if xlab is not provided, use the variable x name
  if (is.null(xlab)) {
    xlab <- lab.df[1]
  }
  # if ylab is not provided, use the variable y name
  if (is.null(ylab)) {
    ylab <- lab.df[2]
  }
  # if outlier label is provided then include it in the dataframe
  if (base::missing(outlier.label)) {
    # if outlier label is not provided then only include the two arguments provided
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(x),
        y = !!rlang::enquo(y)
      ) %>%
      dplyr::mutate(.data = .,
                    outlier.label = y)
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

  # it is possible that sometimes the variable hasn't been converted to factor
  # class and this will produce an error unused levels of the factor need to be
  # dropped otherwise anova will be run instead of a t-test
  data %<>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "x",
      .funs = ~ base::droplevels(x = base::as.factor(x = .))
    )

  ################################################### plot ##############################################################

  # create the basic plot
  plot <-
    ggplot2::ggplot(data = data,
                    mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(
      position = ggplot2::position_jitterdodge(
        jitter.width = point.jitter.width,
        dodge.width = point.dodge.width,
        jitter.height = point.jitter.height
      ),
      alpha = 0.5,
      size = 3,
      na.rm = TRUE,
      ggplot2::aes(color = factor(x))
    )

  if (plot.type == "box" || plot.type == "boxviolin") {
    # adding a boxplot
    if (isTRUE(outlier.tagging)) {
      plot <- plot +
        ggplot2::geom_boxplot(
          notch = notch,
          notchwidth = notchwidth,
          linetype = linetype,
          width = 0.3,
          alpha = 0.2,
          fill = "white",
          position = ggplot2::position_dodge(width = NULL),
          na.rm = TRUE,
          outlier.color = outlier.color
        ) +
        ggplot2::stat_boxplot(
          notch = notch,
          notchwidth = notchwidth,
          linetype = linetype,
          geom = "boxplot",
          width = 0.3,
          alpha = 0.2,
          fill = "white",
          outlier.shape = 16,
          outlier.size = 3,
          outlier.alpha = 0.7,
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
          position = ggplot2::position_dodge(width = NULL),
          na.rm = TRUE
        )
    }
    if (plot.type == "boxviolin") {
      plot <- plot +
        ggplot2::geom_violin(
          width = 0.5,
          alpha = 0.2,
          fill = "white",
          na.rm = TRUE
        )
    }
  } else if (plot.type == "violin") {
    plot <- plot +
      ggplot2::geom_violin(
        width = 0.5,
        alpha = 0.2,
        fill = "white",
        na.rm = TRUE
      )
  }
  # specifying theme and labels for the final plot
  plot <- plot +
    ggstatsplot::theme_mprl(ggtheme = ggtheme) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      caption = caption
    ) +
    ggplot2::coord_cartesian(ylim = c(min(data$y), max(data$y))) +
    ggplot2::scale_y_continuous(limits = c(min(data$y), max(data$y)))

  # choosing palette
  plot <- plot +
    ggplot2::scale_fill_brewer(palette = palette) +
    ggplot2::scale_color_brewer(palette = palette)

  ################################################  preparing stats subtitles #########################################

  # figure out which test to run based on the number of levels of the independent variables
  if (length(levels(as.factor(data$x))) < 3) {
    test <- "t-test"
  } else {
    test <- "anova"
  }

  # running anova
  if (test == "anova") {
    ##################################### parametric ANOVA ############################################################

    # running parametric ANOVA
    if (type == "parametric" || type == "p") {
      # Welch's ANOVA run by default
      aov_stat <-
        stats::oneway.test(
          formula = y ~ x,
          data = data,
          subset = NULL,
          na.action = na.omit,
          var.equal = var.equal
        )

      # preparing the subtitles with appropriate effect sizes
      if (effsize.type == "unbiased") {
        # partial omega-squared is the biased estimate of effect size for parametric ANOVA
        aov_effsize_ci <- sjstats::omega_sq(
          model = stats::lm(
            formula = y ~ x,
            data = data,
            na.action = na.omit
          ),
          partial = TRUE,
          ci.lvl = 0.95,
          n = nboot
        )

        # aov_stat input represents the anova object summary derived from car library
        rsubtitle_omega <-
          # extracting the elements of the statistical object
          base::substitute(
            expr =
              paste(
                italic("F"),
                "(",
                df1,
                ",",
                df2,
                ") = ",
                estimate,
                ", ",
                italic("p"),
                " = ",
                pvalue,
                ", p",
                omega ^ 2,
                " = ",
                effsize,
                ", 95% CI [",
                LL,
                ", ",
                UL,
                "]",
                ", ",
                italic("n"),
                " = ",
                n
              ),
            env = base::list(
              estimate = ggstatsplot::specify_decimal_p(x = aov_stat$statistic[[1]], k),
              df1 = aov_stat$parameter[[1]],
              # numerator degrees of freedom are always integer
              df2 = ggstatsplot::specify_decimal_p(x = aov_stat$parameter[[2]], k),
              pvalue = ggstatsplot::specify_decimal_p(x = aov_stat$p.value[[1]], k, p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$partial.omegasq[[1]], k),
              LL = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$conf.low[[1]], k),
              UL = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$conf.high[[1]], k),
              n = nrow(x = data)
            )
          )

        # adding the subtitle to the plot
        plot <-
          plot +
          ggplot2::labs(subtitle = rsubtitle_omega)

      } else if (effsize.type == "biased") {

        # getting confidence interval for partial eta-squared
        aov_effsize_ci <- sjstats::eta_sq(
          model = stats::lm(
            formula = y ~ x,
            data = data,
            na.action = na.omit
          ),
          partial = TRUE,
          ci.lvl = 0.95,
          n = nboot
        )
        # aov_stat input represents the anova object summary derived from car library
        rsubtitle_peta <-
          # extracting the elements of the statistical object
          base::substitute(
            expr =
              paste(
                italic("F"),
                "(",
                df1,
                ",",
                df2,
                ") = ",
                estimate,
                ", ",
                italic("p"),
                " = ",
                pvalue,
                ", p",
                eta ^ 2,
                " = ",
                effsize,
                ", 95% CI [",
                LL,
                ", ",
                UL,
                "]",
                ", ",
                italic("n"),
                " = ",
                n
              ),
            env = base::list(
              estimate = ggstatsplot::specify_decimal_p(x = aov_stat$statistic[[1]], k),
              df1 = aov_stat$parameter[[1]],
              # numerator degrees of freedom are always integer
              df2 = ggstatsplot::specify_decimal_p(x = aov_stat$parameter[[2]], k),
              pvalue = ggstatsplot::specify_decimal_p(x = aov_stat$p.value[[1]], k, p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$partial.etasq[[1]], k),
              LL = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$conf.low[[1]], k),
              UL = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$conf.high[[1]], k),
              n = nrow(x = data)
            )
          )

        # adding the subtitle to the plot
        plot <-
          plot +
          ggplot2::labs(subtitle = rsubtitle_peta)
      }

      # displaying the details of the test that was run
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::green("Reference: "),
          crayon::blue("Welch's ANOVA is used as a default."),
          crayon::yellow("(Delacre, Leys, Mora, & Lakens, PsyArXiv, 2018).")
        ))
      }
    } else if (type == "nonparametric" || type == "np") {
      ############################ Kruskal-Wallis (nonparametric ANOVA) #################################################
      # setting up the anova model and getting its summary
      kw_stat <- stats::kruskal.test(formula = y ~ x,
                                     data = data,
                                     na.action = na.omit)

      # aov_stat input represents the anova object summary derived from car library
      rsubtitle_kw <- function(kw_stat) {
        # extracting the elements of the statistical object
        base::substitute(
          expr =
            paste(
              "Kruskal-Wallis: ",
              italic(chi) ^ 2,
              "(",
              df,
              ") = ",
              estimate,
              ", ",
              italic("p"),
              " = ",
              pvalue,
              ", ",
              italic("n"),
              " = ",
              n
            ),
          env = base::list(
            estimate = ggstatsplot::specify_decimal_p(x = kw_stat$statistic[[1]], k),
            df = kw_stat$parameter[[1]],
            # degrees of freedom are always integer
            pvalue = ggstatsplot::specify_decimal_p(x = kw_stat$p.value[[1]],
                                                    k,
                                                    p.value = TRUE),
            n = nrow(x = data)
          )
        )
      }

      # adding the subtitle to the plot
      plot <-
        plot +
        ggplot2::labs(subtitle = rsubtitle_kw(kw_stat = kw_stat))

      # letting the user know that this test doesn't have agreed upon effect size
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::red("Note: "),
          crayon::blue(
            "No effect size available for Kruskal-Wallis Rank Sum Test."
          )
        ))
      }
    } else if (type == "robust" || type == "r") {
      ######################################### robust ANOVA ############################################################

      # setting up the Bootstrap version of the heteroscedastic one-way ANOVA for trimmed means
      robust_aov_stat <- t1way_ci(
        data = data,
        x = x,
        y = y,
        tr = tr,
        nboot = nboot,
        conf.level = conf.level,
        conf.type = conf.type
      )

      # robust_aov_stat input represents the robust anova object summary derived from WRS2 library
      rsubtitle_robaov <-
        # extracting the elements of the statistical object
        base::substitute(
          expr =
            paste(
              italic("F"),
              "(",
              df1,
              ",",
              df2,
              ") = ",
              estimate,
              ", ",
              italic("p"),
              " = ",
              pvalue,
              ", ",
              italic(xi),
              " = ",
              effsize,
              ", 95% CI [",
              LL,
              ", ",
              UL,
              "]",
              ", ",
              italic("n"),
              " = ",
              n
            ),
          env = base::list(
            estimate = ggstatsplot::specify_decimal_p(x = robust_aov_stat$`F-value`[[1]], k),
            df1 = robust_aov_stat$df1[[1]],
            # degrees of freedom are always integer
            df2 = ggstatsplot::specify_decimal_p(x = robust_aov_stat$df2[[1]], k),
            pvalue = ggstatsplot::specify_decimal_p(x = robust_aov_stat$`p-value`[[1]],
                                                    k,
                                                    p.value = TRUE),
            effsize = ggstatsplot::specify_decimal_p(x = robust_aov_stat$xi[[1]], k),
            LL = ggstatsplot::specify_decimal_p(x = robust_aov_stat$conf.low[[1]], k),
            UL = ggstatsplot::specify_decimal_p(x = robust_aov_stat$conf.high[[1]], k),
            n = nrow(x = data)
          )
        )

      # displaying the details of the test that was run
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::green("Note:"),
          crayon::blue(
            "In case of error, try reducing the trimming level",
            crayon::yellow(tr),
            "and/or increasing the number of bootstrap samples",
            crayon::yellow(nboot)
          )
        ))
      }

      # adding the label to the plot
      plot <-
        plot +
        ggplot2::labs(subtitle = rsubtitle_robaov)
    }
  } else if (test == "t-test") {
    # if type of test is not specified, then use the default, which is parametric test
    if (is.null(type)) {
      type <- "parametric"
    }

    ##################################### parametric t-test ############################################################

    if (type == "parametric" || type == "p") {
      # setting up the anova model and getting its summary and effect size
      t_stat <-
        stats::t.test(
          formula = y ~ x,
          data = data,
          alternative = "two.sided",
          var.equal = var.equal,
          na.action = na.omit
        )

      if (effsize.type == "unbiased") {
        # t_stat input represents the t-test object summary derived from stats library
        rsubtitle_g <- function(t_stat, t_effsize) {
          # extracting the elements of the statistical object
          base::substitute(
            expr =
              paste(
                italic("t"),
                "(",
                df,
                ") = ",
                estimate,
                ", ",
                italic("p"),
                " = ",
                pvalue,
                ", ",
                italic("g"),
                " = ",
                effsize,
                ", 95% CI [",
                LL,
                ", ",
                UL,
                "]",
                ", ",
                italic("n"),
                " = ",
                n
              ),
            env = base::list(
              estimate = ggstatsplot::specify_decimal_p(x = t_stat[[1]], k),
              df = ggstatsplot::specify_decimal_p(x = t_stat[[2]], k),
              pvalue = ggstatsplot::specify_decimal_p(x = t_stat[[3]], k, p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(x = t_effsize[[3]], k),
              LL = ggstatsplot::specify_decimal_p(x = t_effsize$conf.int[[1]], k),
              UL = ggstatsplot::specify_decimal_p(x = t_effsize$conf.int[[2]], k),
              n = nrow(x = data)
            )
          )
        }

        # Hedge's g is an unbiased estimate of the effect size
        t_effsize <-
          effsize::cohen.d(
            formula = y ~ x,
            data = data,
            hedges.correction = TRUE,
            na.rm = TRUE,
            conf.level = 0.95,
            noncentral = effsize.noncentral
          )

        # adding subtitle to the plot
        plot <-
          plot +
          ggplot2::labs(subtitle = rsubtitle_g(t_stat = t_stat,
                                               t_effsize = t_effsize))
      } else if (effsize.type == "biased") {
        # t_stat input represents the t-test object summary derived from stats library
        rsubtitle_d <- function(t_stat, t_effsize) {
          # extracting the elements of the statistical object
          base::substitute(
            expr =
              paste(
                italic("t"),
                "(",
                df,
                ") = ",
                estimate,
                ", ",
                italic("p"),
                " = ",
                pvalue,
                ", ",
                italic("d"),
                " = ",
                effsize,
                ", 95% CI [",
                LL,
                ", ",
                UL,
                "]",
                ", ",
                italic("n"),
                " = ",
                n
              ),
            env = base::list(
              estimate = ggstatsplot::specify_decimal_p(x = t_stat[[1]], k),
              df = ggstatsplot::specify_decimal_p(x = t_stat[[2]], k),
              pvalue = ggstatsplot::specify_decimal_p(x = t_stat[[3]], k, p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(x = t_effsize[[3]], k),
              LL = ggstatsplot::specify_decimal_p(x = t_effsize$conf.int[[1]], k),
              UL = ggstatsplot::specify_decimal_p(x = t_effsize$conf.int[[2]], k),
              n = nrow(x = data)
            )
          )
        }

        # Cohen's d is a biased estimate of the effect size
        t_effsize <-
          effsize::cohen.d(
            formula = y ~ x,
            data = data,
            hedges.correction = FALSE,
            na.rm = TRUE,
            conf.level = 0.95,
            noncentral = effsize.noncentral
          )

        # adding subtitle to the plot
        plot <-
          plot +
          ggplot2::labs(subtitle = rsubtitle_d(t_stat = t_stat,
                                               t_effsize = t_effsize))
      }

      # displaying the details of the test that was run
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::green("Reference: "),
          crayon::blue("Welch's t-test is used as a default."),
          crayon::yellow(
            "(Delacre, Lakens, & Leys, International Review of Social Psychology, 2017)."
          )
        ))
      }
    }
    else if (type == "nonparametric" || type == "np") {
      ######################################### Mann-Whitney U test ######################################################
      # setting up the Mann-Whitney U-test and getting its summary
      mann_stat <- stats::wilcox.test(
        formula = y ~ x,
        data = data,
        paired = FALSE,
        alternative = "two.sided",
        na.action = na.omit,
        exact = FALSE,
        # asymptotic
        correct = TRUE,
        conf.int = TRUE,
        conf.level = 0.95
      )

      # computing Z score
      z_stat <- coin::wilcox_test(
        formula = y ~ x,
        data = data,
        distribution = "asymptotic",
        alternative = "two.sided",
        conf.int = TRUE
      )

      # mann_stat input represents the U-test summary derived from stats library, while Z is
      # from Exact Wilcoxon-Pratt Signed-Rank Test from coin library
      rsubtitle_mann <- function(mann_stat, z_stat) {
        # extracting the elements of the statistical object
        base::substitute(
          expr =
            paste(
              "Mann-Whitney: ",
              italic(U),
              " = ",
              estimate,
              ", ",
              italic(Z),
              " = ",
              z_value,
              ", ",
              italic(" p"),
              " = ",
              pvalue,
              ", ",
              italic("r"),
              " = ",
              r,
              ", ",
              italic("n"),
              " = ",
              n
            ),
          env = base::list(
            estimate = ggstatsplot::specify_decimal_p(x = mann_stat$statistic[[1]], k),
            z_value = ggstatsplot::specify_decimal_p(x = coin::statistic(z_stat)[[1]], k),
            pvalue = ggstatsplot::specify_decimal_p(x = mann_stat$p.value[[1]], k, p.value = TRUE),
            # effect size is r = z/sqrt(n)
            r = ggstatsplot::specify_decimal_p(x = (
              coin::statistic(z_stat)[[1]] / sqrt(length(data$y))
            ), k),
            n = nrow(x = data)
          )
        )
      }
      # adding subtitle to the plot
      plot <-
        plot +
        ggplot2::labs(subtitle = rsubtitle_mann(mann_stat = mann_stat,
                                                z_stat = z_stat))
    } else if (type == "robust" || type == "r") {
      ######################################### robust t-test ############################################################

      # t_robust_stat input represents the t-test object summary derived from WRS2 library
      rsubtitle_rob <-
        function(t_robust_stat, t_robust_effsize) {
          # extracting the elements of the statistical object
          base::substitute(
            expr =
              paste(
                italic("t"),
                "(",
                df,
                ") = ",
                estimate,
                ", ",
                italic("p"),
                " = ",
                pvalue,
                ", ",
                italic(xi),
                " = ",
                effsize,
                ", 95% CI [",
                LL,
                ", ",
                UL,
                "]",
                ", ",
                italic("n"),
                " = ",
                n
              ),
            env = base::list(
              estimate = ggstatsplot::specify_decimal_p(x = t_robust_stat$test[[1]], k),
              df = ggstatsplot::specify_decimal_p(x = t_robust_stat$df[[1]], k),
              pvalue = ggstatsplot::specify_decimal_p(x = t_robust_stat$p.value[[1]],
                                                      k,
                                                      p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(x = t_robust_effsize$effsize[[1]], k),
              LL = ggstatsplot::specify_decimal_p(x = t_robust_effsize$CI[[1]][[1]], k),
              UL = ggstatsplot::specify_decimal_p(x = t_robust_effsize$CI[[2]][[1]], k),
              n = nrow(x = data)
            )
          )
        }

      # setting up the independent samples t-tests on robust location measures (without bootstraps)
      t_robust_stat <-
        WRS2::yuen(formula = y ~ x,
                   data = data)
      # computing effect sizes
      t_robust_effsize <-
        WRS2::yuen.effect.ci(formula = y ~ x,
                             data = data)

      # adding the label to the plot
      plot <-
        plot +
        ggplot2::labs(subtitle = rsubtitle_rob(t_robust_stat = t_robust_stat,
                                               t_robust_effsize = t_robust_effsize))
    }
  }

  ########################################### outlier tagging #########################################################

  # if outlier.tagging is set to TRUE, first figure out what labels need to be
  # attached to the outlier if outlier label is not provided, outlier labels
  # will just be values of the y vector if the outlier tag has been provided,
  # just use the dataframe already created
  if (isTRUE(outlier.tagging)) {
    # finding and tagging the outliers
    data_df <- data %>%
      dplyr::group_by(.data = ., x) %>%
      dplyr::mutate(
        .data = .,
        outlier = base::ifelse(
          test = check_outlier(var = y,
                               coef = outlier.coef),
          yes = outlier.label,
          no = NA
        )
      )

    # converting outlier column to a numeric value that can be attached to
    data_df$outlier[which(is.na(data_df$outlier))] <- NA
    # if outlier.label is in character format, convert it to factor
    if (is.character(data_df$outlier.label)) {
      data_df$outlier.label <- as.factor(data_df$outlier.label)
    }

    # if outlier labels are words or other types of characters, you want these characters to be diaplyed and not the values
    if (is.factor(data_df$outlier.label)) {
      data_df$outlier.label <- as.character(data_df$outlier.label)

      # convert outlier.label to NAs when outlier is also NA
      data_df %<>%
        dplyr::mutate(
          .data = .,
          outlier = base::ifelse(
            test = !is.na(outlier),
            yes = outlier.label,
            no = NA
          )
        )

      # applying the labels to tagged outliers with ggrepel
      plot <-
        plot +
        ggrepel::geom_label_repel(
          mapping = ggplot2::aes(label = data_df$outlier),
          fontface = "bold",
          color = outlier.label.color,
          max.iter = 3e2,
          box.padding = 0.35,
          point.padding = 0.5,
          segment.color = "black",
          force = 2,
          na.rm = TRUE
        )
    } else {
      # if the value for outliers are to be displated, no need to convert outlier labels to character vector
      # applying the labels to tagged outliers with ggrepel
      plot <-
        plot +
        ggrepel::geom_label_repel(
          mapping = ggplot2::aes(label = data_df$outlier),
          fontface = "bold",
          color = outlier.label.color,
          max.iter = 3e2,
          box.padding = 0.35,
          point.padding = 0.5,
          segment.color = "black",
          force = 2,
          na.rm = TRUE
        )
    }
  }

  ####################################################### mean plotting ################################################

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

    if (!isTRUE(mean.ci)) {
      # use ggrepel to attach text label to each mean
      # create a dataframe with means
      mean_dat <- data %>%
        # in case outlier.label is present, remove it since it's of no utility here
        dplyr::select(.data = ., -dplyr::contains("outlier")) %>%
        dplyr::group_by(.data = ., x) %>%
        dplyr::summarise(.data = ., y = mean(y, na.rm = TRUE)) %>%
        dplyr::mutate(.data = ., label = y) %>%
        dplyr::ungroup(x = .)
    } else {
      mean_dat <- data %>%
        # in case outlier.label is present, remove it since it's of no utility here
        dplyr::select(.data = ., -dplyr::contains("outlier")) %>%
        dplyr::group_by(.data = ., x) %>%
        dplyr::summarise(
          .data = .,
          mean.y = base::mean(x = y, na.rm = TRUE),
          sd.y = stats::sd(x = y, na.rm = TRUE),
          n.y = dplyr::n()
        ) %>%
        dplyr::mutate(
          .data = .,
          se.y = sd.y / base::sqrt(n.y),
          lower.ci.y = mean.y - stats::qt(p = 1 - (0.05 / 2), df = n.y - 1, lower.tail = TRUE) * se.y,
          upper.ci.y = mean.y + stats::qt(p = 1 - (0.05 / 2), df = n.y - 1, lower.tail = TRUE) * se.y
        ) %>%
        dplyr::ungroup(x = .)
    }

    # format the numeric values
    mean_dat %<>%
      dplyr::mutate_if(
        .tbl = .,
        .predicate = purrr::is_bare_numeric,
        .funs = ~ as.numeric(as.character(
          ggstatsplot::specify_decimal_p(x = ., k = k)
        )) # format the values for printing
      )

    if (isTRUE(mean.ci)) {
      mean_dat %<>%
        purrrlyr::by_row(
          .d = .,
          ..f = ~ paste(.$mean.y,
                      ", 95% CI [",
                      .$lower.ci.y,
                      ", ",
                      .$upper.ci.y,
                      "]",
                      sep = "",
                      collapse = ""),
          .collate = "rows",
          .to = "label",
          .labels = TRUE
        ) %>%
        dplyr::rename(.data = ., y = mean.y)
    }

    # attach the labels to the plot
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
        na.rm = TRUE
      )
  }

  #============================================= sample sizes ================================================

  # adding sample size labels to the x axes
  if (isTRUE(sample.size.label)) {
    data_label <- data %>%
      dplyr::group_by(.data = ., x) %>%
      dplyr::mutate(.data = ., n = dplyr::n()) %>%
      dplyr::ungroup(.data = ., x = .) %>%
      dplyr::mutate(.data = ., label = paste0(x, "\n(n = ", n, ")", sep = "")) %>%
      dplyr::arrange(.data = ., x)

    # adding new labels to the plot
    plot <- plot +
      ggplot2::scale_x_discrete(labels = c(unique(data_label$label)))

  }

  #============================================= messages ===================================================

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
      lab = lab.df[1],
      k = k,
      output = "message"
    )
  }

  # return the final plot
  return(plot)
}
