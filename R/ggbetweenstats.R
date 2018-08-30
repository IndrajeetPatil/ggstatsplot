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
#'   or `"robust"` or `"bayes"`).Corresponding abbreviations are also accepted:
#'   `"p"` (for parametric), `"np"` (nonparametric), `"r"` (robust), or
#'   `"bf"`resp.
#' @param bf.prior A number between 0.5 and 2 (default `0.707`), the prior width
#'   to use in calculating Bayes factors.
#' @param bf.message Logical. Decides whether to display Bayes Factor in favor
#'   of *null* hypothesis **for parametric test** (Default: `bf.message = FALSE`).
#' @param title The text for the plot title.
#' @param caption The text for the plot caption.
#' @param sample.size.label Logical that decides whether sample size information
#'   should be displayed for each level of the grouping variable `x` (Default:
#'   `TRUE`).
#' @param mean.label.size,mean.label.fontface,mean.label.color Aesthetics for
#'   the label displaying mean. Defaults: `3`, `"bold"`,`"black"`, respectively.
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
#' @inheritParams paletteer::scale_color_paletteer_d
#' @inheritParams theme_ggstatsplot
#' @inheritParams subtitle_ggbetween_anova_parametric
#' @inheritParams subtitle_ggbetween_t_parametric
#' @inheritParams t1way_ci
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
#' @importFrom stats sd
#' @importFrom stats na.omit
#' @importFrom stats t.test
#' @importFrom stats var.test
#' @importFrom stats bartlett.test
#' @importFrom stats kruskal.test
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
#' @importFrom paletteer scale_color_paletteer_d
#' @importFrom paletteer scale_fill_paletteer_d
#'
#' @seealso \code{\link{grouped_ggbetweenstats}}
#'
#' @details
#'
#' For parametric tests, Welch's ANOVA/t-test are used as a default. References:
#' \itemize{
#'  \item ANOVA: Delacre, Leys, Mora, & Lakens, *PsyArXiv*, 2018
#'  \item t-test: Delacre, Lakens, & Leys, *International Review of Social Psychology*, 2017
#'  }
#'
#'  If robust tests are selected, following tests are used is .
#' \itemize{
#'  \item ANOVA: one-way ANOVA on trimmed means (see `?WRS2::t1way`)
#'  \item t-test: Yuen's test for trimmed means (see `?WRS2::yuen`)
#'  }
#'
#' Variant of this function `ggwithinstats` is currently under work. You *can*
#' still use this function just to prepare the **plot** for exploratory data
#' analysis, but the statistical details displayed in the subtitle will be
#' incorrect. You can remove them by adding `+ ggplot2::labs(subtitle = NULL)`.
#'
#' @references
#' \url{https://cran.r-project.org/web/packages/ggstatsplot/vignettes/ggbetweenstats.html}
#'
#' @examples
#'
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#'
#' # simple function call with the defaults
#' ggstatsplot::ggbetweenstats(
#'   data = datasets::iris,
#'   x = Species,
#'   y = Sepal.Length
#' )
#'
#' # more detailed function call
#' ggstatsplot::ggbetweenstats(
#'   data = datasets::ToothGrowth,
#'   x = supp,
#'   y = len,
#'   plot.type = "box",
#'   xlab = "Supplement type",
#'   ylab = "Tooth length"
#' )
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
                           bf.prior = 0.707,
                           bf.message = FALSE,
                           xlab = NULL,
                           ylab = NULL,
                           caption = NULL,
                           title = NULL,
                           sample.size.label = TRUE,
                           k = 3,
                           var.equal = FALSE,
                           nboot = 100,
                           tr = 0.1,
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
                           ggstatsplot.layer = TRUE,
                           package = "RColorBrewer",
                           palette = "Dark2",
                           direction = 1,
                           point.jitter.width = NULL,
                           point.jitter.height = 0.1,
                           point.dodge.width = 0.60,
                           messages = TRUE) {
  #
  #------------------------------------------------------ variable names ---------------------------------------------------------------

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

  #------------------------------------------------------ data --------------------------------------------------------------------------

  # if outlier label is provided then include it in the dataframe
  if (base::missing(outlier.label)) {

    # if outlier label is not provided then only include the two arguments provided
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
    dplyr::mutate_at(
      .tbl = .,
      .vars = "x",
      .funs = ~base::droplevels(x = base::as.factor(x = .))
    )

  #------------------------------------------------------ plot ---------------------------------------------------------------

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

  #------------------------------------------------ subtitle preparation -------------------------------------------------------------------

  # figure out which test to run based on the number of levels of the independent variables
  if (length(levels(as.factor(data$x))) < 3) {
    test <- "t-test"
  } else {
    test <- "anova"
  }

  #---------------------------------------------- parametric anova -----------------------------------------------------------------------------

  # running anova
  if (test == "anova") {
    # running parametric ANOVA
    if (type == "parametric" || type == "p") {
      subtitle <- subtitle_ggbetween_anova_parametric(
        data = data,
        x = x,
        y = y,
        effsize.type = effsize.type,
        nboot = nboot,
        var.equal = var.equal,
        k = k
      )

      #--------------------------------------------- Kruskal-Wallis (nonparametric ANOVA) --------------------------------------------------
    } else if (type == "nonparametric" || type == "np") {
      subtitle <- subtitle_ggbetween_kw_nonparametric(
        data = data,
        x = x,
        y = y,
        k = k,
        messages = messages
      )

      #---------------------------------------------------------- robust ANOVA --------------------------------------------------------
    } else if (type == "robust" || type == "r") {
      subtitle <- subtitle_ggbetween_rob_anova(
        data = data,
        x = x,
        y = y,
        k = k,
        messages = messages,
        tr = tr,
        nboot = nboot
      )
    }
  } else if (test == "t-test") {

    # running bayesian analysis
    jmv_results <- jmv::ttestIS(
      data = data,
      vars = "y",
      group = "x",
      students = TRUE,
      effectSize = TRUE,
      bf = TRUE,
      bfPrior = bf.prior,
      hypothesis = "different",
      miss = "listwise"
    )

    # preparing the BF message for NULL
    if (isTRUE(bf.message)) {
      bf.caption.text <-
        bf_message_ttest(jmv_results = jmv_results, bf.prior = bf.prior)
    }

    #------------------------------------------------- parametric t-test ------------------------------------------------------------

    if (type == "parametric" || type == "p") {
      # Welch's t-test run by default
      subtitle <- subtitle_ggbetween_t_parametric(
        data = data,
        x = x,
        y = y,
        paired = FALSE,
        effsize.type = effsize.type,
        effsize.noncentral = effsize.noncentral,
        var.equal = var.equal,
        k = k
      )

      #------------------------------------------------- Mann-Whitney U test ------------------------------------------------------------
    } else if (type == "nonparametric" || type == "np") {
      subtitle <- subtitle_ggbetween_mann_nonparametric(
        data = data,
        x = x,
        y = y,
        paired = FALSE,
        k = k
      )

      #------------------------------------------------- robust t-test ------------------------------------------------------------
    } else if (type == "robust" || type == "r") {
      subtitle <- subtitle_ggbetween_t_rob(
        data = data,
        x = x,
        y = y,
        k = k,
        tr = tr,
        nboot = nboot
      )

      #------------------------------------------------- bayesian t-test ------------------------------------------------------------
    } else if (type == "bayes" || type == "bf") {
      subtitle <- subtitle_ggbetween_t_bayes(
        data = data,
        x = x,
        y = y,
        bf.prior = bf.prior,
        k = k
      )
    }
  }

  #------------------------------------------------- annotations and themes ------------------------------------------------------------

  # add message with bayes factor
  if (test == "t-test") {
    if (type %in% c("parametric", "p")) {
      if (isTRUE(bf.message)) {
        if (is.null(caption)) {
          caption.text <- bf.caption.text
        } else {
          caption.text <- caption
        }
      } else {
        caption.text <- caption
      }
    } else {
      caption.text <- caption
    }
  } else {
    caption.text <- caption
  }

  # specifying theme and labels for the final plot
  plot <- plot +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      subtitle = subtitle,
      caption = caption.text,
      color = lab.df[1]
    ) +
    ggstatsplot::theme_mprl(ggtheme = ggtheme, ggstatsplot.layer = ggstatsplot.layer) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_cartesian(ylim = c(min(data$y), max(data$y))) +
    ggplot2::scale_y_continuous(limits = c(min(data$y), max(data$y)))

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

  #------------------------------------------------- outlier tagging ------------------------------------------------------------

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
          test = check_outlier(
            var = y,
            coef = outlier.coef
          ),
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

  #------------------------------------------------- labels with mean values ------------------------------------------------------------

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
        .funs = ~as.numeric(as.character(
          ggstatsplot::specify_decimal_p(x = ., k = k)
        )) # format the values for printing
      )

    # adding confidence intervals to the label for mean
    if (isTRUE(mean.ci)) {
      mean_dat %<>%
        purrrlyr::by_row(
          .d = .,
          ..f = ~paste(.$mean.y,
            ", 95% CI [",
            .$lower.ci.y,
            ", ",
            .$upper.ci.y,
            "]",
            sep = "",
            collapse = ""
          ),
          .collate = "rows",
          .to = "label",
          .labels = TRUE
        ) %>%
        dplyr::rename(.data = ., y = mean.y)
    }

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
        na.rm = TRUE
      )
  }

  #------------------------------------------------- sample size labels ------------------------------------------------------------

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

  # if caption is provided then use combine_plots function later on to add this caption
  # add caption with bayes factor
  if (test == "t-test") {
    if (type %in% c("parametric", "p")) {
      if (isTRUE(bf.message)) {
        if (!is.null(caption)) {
          # adding bayes factor result
          plot <-
            ggstatsplot::combine_plots(plot,
              caption.text = bf.caption.text
            )

          # producing warning
          base::message(cat(
            crayon::red("Warning:"),
            crayon::blue(
              "You are simultaneously setting `bf.message = TRUE` and using a `caption`. \nThis produces a fixed plot whose *internal* elements can no longer be modified with `ggplot2` functions."
            )
          ))
        }
      }
    }
  }

  #------------------------------------------------- messages -------------------------------------------------------------------

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
