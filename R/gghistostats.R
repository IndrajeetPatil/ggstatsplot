#'
#' @title Histogram for distribution of a numeric variable
#' @name gghistostats
#' @aliases gghistostats
#' @description Histogram with statistical details from one-sample test included
#'   in the plot as a subtitle.
#' @author Indrajeet Patil
#'
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken. This argument is optional.
#' @param x A numeric variable.
#' @param bar.measure Character describing what value needs to be represented as
#'   height in the bar chart. This can either be `"count"`, which shows number
#'   of points in bin, or `"density"`, which density of points in bin, scaled to
#'   integrate to 1, or "`proportion`", which shows relative frequencies of
#'   observations in each bin, or "`mix`", which shows both count and proportion
#'   in the same plot.
#' @param xlab Label for `x` axis variable.
#' @param title The text for the plot title.
#' @param subtitle The text for the plot subtitle *if* you don't want results
#'   from one sample test to be displayed.
#' @param caption The text for the plot caption.
#' @param type Type of statistic expected (`"parametric"` or `"nonparametric"`
#'   or `"robust"` or `"bayes"`). Abbreviations accepted are `"p"` or `"np"` or
#'   `"r"` or `"bf"`, respectively.
#' @param test.value A number specifying the value of the null hypothesis.
#' @param test.value.size Decides size for the vertical line for test value
#'   (Default: `1.2`).
#' @param test.value.linetype Decides linetype for the vertical line for test
#'   value (Default: `"dashed"`).
#' @param bf.prior A number between 0.5 and 2 (default 0.707), the prior width
#'   to use in calculating Bayes factors.
#' @param bf.message Logical. Decides whether to display Bayes Factor in favor
#'   of null hypothesis for parametric test if the null hypothesis can't be
#'   rejected (Default: `bf.message = TRUE`).
#' @param robust.estimator If `test = "robust"` robust estimator to be used
#'   (`"onestep"` (Default), `"mom"`, or `"median"`). For more, see
#'   `?WRS2::onesampb`.
#' @param nboot Number of bootstrap samples for robust one-sample location test.
#' @param k Number of decimal places expected for results.
#' @param fill.gradient Logical decides whether color fill gradient is to be
#'   displayed (Default: `FALSE`). If `FALSE`, the legend and the color gradient
#'   will also be removed. The default is set to `FALSE` because the gradient
#'   provides redundant information in light of y-axis labels.
#' @param low.color,high.color Colors for low and high ends of the gradient.
#'   Defaults are colorblind-friendly.
#' @param bar.fill If `fill.gradient = FALSE`, then `bar.fill` decides which
#'   color will uniformly fill all the bars in the histogram (Default: `"white"`).
#' @param results.subtitle Decides whether the results of statistical tests are
#'   to be displayed as subtitle (Default: `results.subtitle = TRUE`). If set to
#'   `FALSE`, no statistical tests will be run.
#' @param legend.title.margin Adjusting the margin between legend title and the
#'   colorbar (Default: `FALSE`).
#' @param t.margin,b.margin Margins in grid units. For more details, see
#'   `?grid::unit()`.
#' @param centrality.para Decides *which* measure of central tendency (`"mean"`
#'   or `"median"`) is to be displayed as a vertical line.
#' @param centrality.color Decides color for the vertical line for centrality
#'   parameter (Default: `"blue"`).
#' @param centrality.size Decides size for the vertical line for centrality
#'   parameter (Default: `1.2`).
#' @param centrality.linetype Decides linetype for the vertical line for
#'   centrality parameter (Default: `"dashed"`).
#' @param test.value.line Decides whether test value is to be displayed as a
#'   vertical line (Default: `FALSE`).
#' @param test.value.color Decides color for the vertical line denoting test
#'   value (Default: `"black"`).
#' @param line.labeller A logical that decides whether line labels should be
#'   displayed (Default: `FALSE`).
#' @param line.labeller.y A numeric denoting the y-coordinate for displaying
#'   line labels (Default: `-2`).
#' @param binwidth The width of the bins. Can be specified as a numeric value,
#'   or a function that calculates width from `x`. The default is to use bins
#'   bins that cover the range of the data. You should always override this
#'   value, exploring multiple widths to find the best to illustrate the stories
#'   in your data.
#' @param ggtheme A function, `ggplot2` theme name. Default value is
#'   `ggplot2::theme_bw()`. Allowed values are the official `ggplot2` themes,
#'   including `theme_grey()`, `theme_minimal()`, `theme_classic()`,
#'   `theme_void()`, etc.
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
#' @importFrom jmv ttestOneS
#' @importFrom WRS2 onesampb
#' @importFrom scales percent
#' @importFrom crayon green
#' @importFrom crayon blue
#' @importFrom crayon yellow
#' @importFrom crayon red
#'
#' @examples
#'
#' # most basic function call with the defaults
#' ggstatsplot::gghistostats(
#'   data = datasets::ToothGrowth,
#'   x = len,
#'   xlab = "Tooth length"
#' )
#'
#' # a detailed function call
#' ggstatsplot::gghistostats(
#'   data = datasets::iris,
#'   x = Sepal.Length,
#'   bar.measure = "count",
#'   type = "bf",
#'   bf.prior = 0.8,
#'   test.value = 3,
#'   centrality.para = "mean",
#'   test.value.line = TRUE,
#'   binwidth = 0.10,
#'   bar.fill = "grey50
#' )
#' @note If you are using R Notebook and see a blank image being inserted when a
#'   chunk is executed, this behavior can be turned off by setting
#'   `legend.title.margin = FALSE`.
#'
#' @seealso \code{\link{grouped_gghistostats}}
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/gghistostats.html}
#'
#' @export
#'

# function body
gghistostats <-
  function(data = NULL,
             x,
             binwidth = NULL,
             bar.measure = "mix",
             xlab = NULL,
             title = NULL,
             subtitle = NULL,
             caption = NULL,
             type = "parametric",
             test.value = 0,
             bf.prior = 0.707,
             bf.message = TRUE,
             robust.estimator = "onestep",
             nboot = 500,
             k = 3,
             ggtheme = ggplot2::theme_bw(),
             fill.gradient = FALSE,
             low.color = "#0072B2",
             high.color = "#D55E00",
           bar.fill = "white",
             results.subtitle = TRUE,
             centrality.para = NULL,
             centrality.color = "blue",
             centrality.size = 1.2,
             centrality.linetype = "dashed",
             test.value.line = FALSE,
             test.value.color = "black",
             test.value.size = 1.2,
             test.value.linetype = "dashed",
             line.labeller = FALSE,
             line.labeller.y = -2,
             legend.title.margin = FALSE,
             t.margin = unit(0, "mm"),
             b.margin = unit(3, "mm"),
             messages = TRUE) {
    # if data is not available then don't display any messages
    if (is.null(data)) {
      messages <- FALSE
    }

    # save the value of caption in another variable because caption is going to be modified in the function body
    if (is.null(caption)) {
      bf.caption <- caption
    } else {
      bf.caption <- NULL
    }

    # if no color fill is to be displayed, set low and high color to white
    if (!isTRUE(fill.gradient)) {
      low.color <- bar.fill
      high.color <- bar.fill
    }

    # ========================================== dataframe ==============================================================
    # preparing a dataframe out of provided inputs
    if (!is.null(data)) {
      # preparing labels from given dataframe
      lab.df <- colnames(dplyr::select(
        .data = data,
        !!rlang::enquo(x)
      ))
      # if xlab is not provided, use the variable x name
      if (is.null(xlab)) {
        xlab <- lab.df[1]
      }

      # if dataframe is provided
      data <-
        dplyr::select(
          .data = data,
          x = !!rlang::enquo(x)
        )
    } else {
      # if vectors are provided
      data <-
        base::cbind.data.frame(x = x)
    }

    # convert to a tibble
    data %<>%
      tibble::as_data_frame(x = .)

    # ========================================== stats ==================================================================

    if (isTRUE(results.subtitle)) {
      # model
      jmv_os <- jmv::ttestOneS(
        data = data,
        vars = "x",
        students = TRUE,
        bf = TRUE,
        bfPrior = bf.prior,
        wilcoxon = TRUE,
        # Mann-Whitney U test
        testValue = test.value,
        hypothesis = "dt",
        # two-sided hypothesis-testing
        effectSize = TRUE,
        miss = "listwise"
        # excludes a row from all analyses if one of its entries is missing
      )

      # ========================================== parametric ==================================================================
      if (type == "parametric" || type == "p") {
        # preparing the subtitle
        subtitle <- base::substitute(
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
              ", ",
              italic("n"),
              " = ",
              n
            ),
          env = base::list(
            estimate = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_os$ttest)$`stat[stud]`, k),
            # df is integer value for Student's t-test
            df = as.data.frame(jmv_os$ttest)$`df[stud]`,
            pvalue = ggstatsplot::specify_decimal_p(
              x = as.data.frame(jmv_os$ttest)$`p[stud]`,
              k,
              p.value = TRUE
            ),
            effsize = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_os$ttest)$`es[stud]`, k),
            n = nrow(x = data)
          )
        )

        # if effect is not significant, display Bayes Factor in favor of the NULL
        # save it as text if bf.message has not been disabled
        if (as.data.frame(jmv_os$ttest)$`p[stud]` > 0.05) {
          if (isTRUE(bf.message)) {
            bf.caption.text <-
              paste(
                "Note: Evidence in favor of the null hypothesis:",
                ggstatsplot::specify_decimal_p(x = 1 / as.data.frame(jmv_os$ttest)$`stat[bf]`, k),
                "with prior width =",
                ggstatsplot::specify_decimal_p(x = bf.prior, k)
              )
          } else {
            # display a note about prior used to compute Bayes Factor
            if (isTRUE(messages)) {
              base::message(cat(
                crayon::green("Note: "),
                crayon::blue(
                  "Prior width used to compute Bayes Factor:",
                  crayon::yellow(bf.prior)
                ),
                crayon::blue("\nEvidence in favor of the null hypothesis (H0):"),
                crayon::yellow(1 / as.data.frame(jmv_os$ttest)$`stat[bf]`)
              ))
            }
          }
        }

        # ========================================== non-parametric =====================================================
      } else if (type == "nonparametric" || type == "np") {
        # preparing the subtitle
        subtitle <- base::substitute(
          expr =
            paste(
              italic("U"),
              " = ",
              estimate,
              ", ",
              italic("p"),
              " = ",
              pvalue,
              ", ",
              italic("d"),
              " = ",
              effsize,
              ", ",
              italic("n"),
              " = ",
              n
            ),
          env = base::list(
            estimate = as.data.frame(jmv_os$ttest)$`stat[wilc]`,
            pvalue = ggstatsplot::specify_decimal_p(
              x = as.data.frame(jmv_os$ttest)$`p[wilc]`,
              k,
              p.value = TRUE
            ),
            effsize = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_os$ttest)$`es[wilc]`, k),
            n = nrow(x = data)
          )
        )
        # ========================================== robust ==================================================================
      } else if (type == "robust" || type == "r") {

        # running one-sample percentile bootstrap
        rob_os <- WRS2::onesampb(
          x = data$x,
          est = robust.estimator,
          nboot = nboot,
          nv = test.value
        )

        # preparing the subtitle
        subtitle <- base::substitute(
          expr =
            paste(
              "M"[robust],
              " = ",
              estimate,
              ", 95% CI [",
              LL,
              ", ",
              UL,
              "], ",
              italic("p"),
              " = ",
              pvalue,
              ", ",
              italic("n"),
              " = ",
              n
            ),
          env = base::list(
            estimate = ggstatsplot::specify_decimal_p(x = rob_os$estimate[[1]], k),
            LL = ggstatsplot::specify_decimal_p(x = rob_os$ci[[1]], k),
            UL = ggstatsplot::specify_decimal_p(x = rob_os$ci[[2]], k),
            pvalue = ggstatsplot::specify_decimal_p(
              x = rob_os$p.value[[1]],
              k,
              p.value = TRUE
            ),
            n = rob_os$n[[1]]
          )
        )
        # ========================================== bayes ==================================================================
      } else if (type == "bayes" || type == "bf") {
        # preparing the subtitle
        subtitle <- base::substitute(
          expr =
            paste(
              italic("t"),
              "(",
              df,
              ") = ",
              estimate,
              ", ",
              "log(BF"[10],
              ") = ",
              bf,
              ", log(error) = ",
              bf_error,
              ", ",
              italic("d"),
              " = ",
              effsize,
              ", ",
              italic("n"),
              " = ",
              n
            ),
          env = base::list(
            # df is integer value for Student's t-test
            df = as.data.frame(jmv_os$ttest)$`df[stud]`,
            estimate = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_os$ttest)$`stat[stud]`, k),
            bf = ggstatsplot::specify_decimal_p(x = log10(x = as.data.frame(jmv_os$ttest)$`stat[bf]`), k = 0),
            bf_error = ggstatsplot::specify_decimal_p(x = log10(x = as.data.frame(jmv_os$ttest)$`err[bf]`), k = 0),
            effsize = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_os$ttest)$`es[stud]`, k),
            n = nrow(x = data)
          )
        )

        # display a note about prior used to compute Bayes Factor
        if (isTRUE(messages)) {
          base::message(cat(
            crayon::green("Note: "),
            crayon::blue(
              "Prior width used to compute Bayes Factor:",
              crayon::yellow(bf.prior)
            ),
            crayon::blue("\nEvidence in favor of the null hypothesis (H0):"),
            crayon::yellow(1 / as.data.frame(jmv_os$ttest)$`stat[bf]`)
          ))
        }
      } else {
        subtitle <- subtitle
      }

      # preparing caption
      # if caption is not provided, then use bf.caption.text as caption
      if (type == "parametric") {
        if (as.data.frame(jmv_os$ttest)$`p[stud]` > 0.05) {
          if (isTRUE(bf.message)) {
            if (is.null(caption)) {
              caption <- bf.caption.text
            }
          }
        }
      }
    }
    # ========================================== plot ===================================================================

    # preparing the basic layout of the plot based on whether counts or density information is needed
    # only counts
    if (bar.measure == "count") {
      plot <- ggplot2::ggplot(
        data = data,
        mapping = ggplot2::aes(x = x)
      ) +
        ggplot2::stat_bin(
          col = "black",
          alpha = 0.7,
          binwidth = binwidth,
          na.rm = TRUE,
          mapping = ggplot2::aes(
            y = ..count..,
            fill = ..count..
          )
        ) +
        ggplot2::scale_fill_gradient(
          name = "count",
          low = low.color,
          high = high.color
        )
    } else if (bar.measure == "proportion") {
      # only proportion
      plot <- ggplot2::ggplot(
        data = data,
        mapping = ggplot2::aes(x = x)
      ) +
        ggplot2::stat_bin(
          col = "black",
          alpha = 0.7,
          binwidth = binwidth,
          na.rm = TRUE,
          mapping = ggplot2::aes(
            y = ..count.. / sum(..count..),
            fill = ..count.. / sum(..count..)
          )
        ) +
        ggplot2::scale_fill_gradient(
          name = "proportion",
          low = low.color,
          high = high.color,
          labels = percent
        ) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::ylab("proportion")
    } else if (bar.measure == "density") {
      # only density
      plot <- ggplot2::ggplot(
        data = data,
        mapping = ggplot2::aes(x = x)
      ) +
        ggplot2::stat_bin(
          col = "black",
          alpha = 0.7,
          binwidth = binwidth,
          na.rm = TRUE,
          mapping = ggplot2::aes(
            y = ..density..,
            fill = ..density..
          )
        ) +
        ggplot2::scale_fill_gradient(
          name = "density",
          low = low.color,
          high = high.color
        )
    } else if (bar.measure == "mix") {

      # this works only with the development version of ggplot2
      # all things combined
      plot <- ggplot2::ggplot(
        data = data,
        mapping = ggplot2::aes(x = x)
      ) +
        ggplot2::stat_bin(
          col = "black",
          alpha = 0.7,
          binwidth = binwidth,
          na.rm = TRUE,
          mapping = ggplot2::aes(
            y = ..count..,
            fill = ..count..
          )
        ) +
        ggplot2::scale_fill_gradient(
          name = "count",
          low = low.color,
          high = high.color
        ) +
        ggplot2::scale_y_continuous(
          sec.axis = ggplot2::sec_axis(
            trans = ~. / nrow(x = data),
            labels = scales::percent,
            name = "proportion"
          )
        ) +
        ggplot2::ylab("count") +
        ggplot2::guides(fill = FALSE)
    }

    # adding the theme and labels
    plot <- plot +
      ggstatsplot::theme_mprl(ggtheme = ggtheme) +
      ggplot2::labs(
        x = xlab,
        title = title,
        subtitle = subtitle,
        caption = caption
      )

    # if central tendency parameter is to be added
    if (!is.null(centrality.para)) {
      if (isTRUE(centrality.para) || centrality.para == "mean") {
        plot <- plot +
          ggplot2::geom_vline(
            xintercept = mean(x = data$x, na.rm = TRUE),
            linetype = centrality.linetype,
            color = centrality.color,
            size = centrality.size,
            na.rm = TRUE
          )

        if (isTRUE(line.labeller)) {
          # this can be used to label the vertical lines, but leave it out since it makes for an ugly plot
          plot <- plot +
            ggplot2::geom_text(
              mapping = ggplot2::aes(
                x = mean(x = data$x, na.rm = TRUE),
                label = "mean",
                y = line.labeller.y
              ),
              color = centrality.color,
              angle = 0,
              size = 6
            )
        }
      } else if (centrality.para == "median") {
        plot <- plot +
          ggplot2::geom_vline(
            xintercept = median(x = data$x, na.rm = TRUE),
            linetype = centrality.linetype,
            color = centrality.color,
            size = centrality.size,
            na.rm = TRUE
          )
        # this can be used to label the vertical lines, but makes for an ugly plot
        if (isTRUE(line.labeller)) {
          plot <- plot +
            ggplot2::geom_text(
              mapping = ggplot2::aes(
                x = median(x = data$x, na.rm = TRUE),
                label = "median",
                y = line.labeller.y
              ),
              color = centrality.color,
              angle = 0,
              size = 6
            )
        }
      }

      # if central tendency parameter is to be added
      if (isTRUE(test.value.line)) {
        plot <- plot +
          ggplot2::geom_vline(
            xintercept = test.value,
            linetype = test.value.linetype,
            color = test.value.color,
            size = test.value.size,
            na.rm = TRUE
          )
        # if a text label is to be attached the line
        if (isTRUE(line.labeller)) {
          plot <- plot +
            ggplot2::geom_text(
              mapping = ggplot2::aes(
                x = test.value,
                label = "test",
                y = line.labeller.y
              ),
              color = "black",
              angle = 0,
              size = 6
            )
        }
      }
    }
    # if caption is provided then use combine_plots function later on to add this caption
    # add caption with bayes factor
    if (isTRUE(results.subtitle)) {
      if (type == "parametric") {
        if (as.data.frame(jmv_os$ttest)$`p[stud]` > 0.05) {
          if (isTRUE(bf.message)) {
            if (!is.null(bf.caption)) {
              plot <-
                ggstatsplot::combine_plots(plot,
                  caption.text = bf.caption.text
                )
            }
          }
        }
      }
    }

    # creating proper spacing between the legend.title and the colorbar
    if (isTRUE(legend.title.margin)) {
      plot <- legend_title_margin(
        plot = plot,
        t.margin = t.margin,
        b.margin = b.margin
      )
    }

    # if no color fill gradient is used, then remove the legend
    if (!isTRUE(fill.gradient)) {
      plot <- plot +
        ggplot2::theme(legend.position = "none")
    }

    # ========================================== messages ==================================================================
    if (isTRUE(messages)) {
      # display normality test result as a message
      normality_message <- normality_message(x = data$x, lab = lab.df[1], k = k)
      print(normality_message)
    }

    # return the final plot
    return(plot)
  }
