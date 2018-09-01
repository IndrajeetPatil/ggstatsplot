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
#'   in the same plot (only available from `ggplot2 3.0.1` onward).
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
#' @param bf.prior A number between 0.5 and 2 (default `0.707`), the prior width
#'   to use in calculating Bayes factors.
#' @param bf.message Logical. Decides whether to display Bayes Factor in favor
#'   of *null* hypothesis **for parametric test** (Default: `bf.message = FALSE`).
#'   This will work only if `results.subtitle = TRUE`.
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
#'   color will uniformly fill all the bars in the histogram (Default: `"grey50"`).
#' @param results.subtitle Decides whether the results of statistical tests are
#'   to be displayed as subtitle (Default: `results.subtitle = TRUE`). If set to
#'   `FALSE`, no statistical tests will be run.
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
#' @param test.line.labeller,centrality.line.labeller A logical that decides
#'   whether line labels should be displayed (Default: `TRUE`).
#' @param test.k,centrality.k Integer denoting the number of decimal places
#'   expected for test and centrality parameters. (Default: `0` and `2`,
#'   respectively).
#' @param binwidth The width of the bins. Can be specified as a numeric value,
#'   or a function that calculates width from `x`. The default is to use bins
#'   bins that cover the range of the data. You should always override this
#'   value, exploring multiple widths to find the best to illustrate the stories
#'   in your data.
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @inheritParams theme_ggstatsplot
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
#'   bar.fill = "grey50"
#' )
#' @seealso \code{\link{grouped_gghistostats}}
#'
#' @references
#' \url{https://cran.r-project.org/package=ggstatsplot/vignettes/gghistostats.html}
#'
#' @export
#'

# function body
gghistostats <-
  function(data = NULL,
             x,
             binwidth = NULL,
             bar.measure = "count",
             xlab = NULL,
             title = NULL,
             subtitle = NULL,
             caption = NULL,
             type = "parametric",
             test.value = 0,
             bf.prior = 0.707,
             bf.message = FALSE,
             robust.estimator = "onestep",
             nboot = 500,
             k = 3,
             ggtheme = ggplot2::theme_bw(),
             ggstatsplot.layer = TRUE,
             fill.gradient = FALSE,
             low.color = "#0072B2",
             high.color = "#D55E00",
             bar.fill = "grey50",
             results.subtitle = TRUE,
             centrality.para = "mean",
             centrality.color = "blue",
             centrality.size = 1.2,
             centrality.linetype = "dashed",
             centrality.line.labeller = TRUE,
             centrality.k = 2,
             test.value.line = FALSE,
             test.value.color = "black",
             test.value.size = 1.2,
             test.value.linetype = "dashed",
             test.line.labeller = TRUE,
             test.k = 0,
             messages = TRUE) {
    # if data is not available then don't display any messages
    if (is.null(data)) {
      messages <- FALSE
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
      jmv_results <- jmv::ttestOneS(
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

      # preparing the BF message for NULL
      if (isTRUE(bf.message)) {
        bf.caption.text <-
          bf_message_ttest(
            jmv_results = jmv_results,
            # caption = caption,
            bf.prior = bf.prior
          )
      }
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
            estimate = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_results$ttest)$`stat[stud]`, k),
            # df is integer value for Student's t-test
            df = as.data.frame(jmv_results$ttest)$`df[stud]`,
            pvalue = ggstatsplot::specify_decimal_p(
              x = as.data.frame(jmv_results$ttest)$`p[stud]`,
              k,
              p.value = TRUE
            ),
            effsize = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_results$ttest)$`es[stud]`, k),
            n = nrow(x = data)
          )
        )


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
            estimate = as.data.frame(jmv_results$ttest)$`stat[wilc]`,
            pvalue = ggstatsplot::specify_decimal_p(
              x = as.data.frame(jmv_results$ttest)$`p[wilc]`,
              k,
              p.value = TRUE
            ),
            effsize = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_results$ttest)$`es[wilc]`, k),
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
              ", log"["e"],
              "(BF"["10"],
              ") = ",
              bf,
              ", log"["e"],
              "(error) = ",
              bf_error,
              "% , ",
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
            df = as.data.frame(jmv_results$ttest)$`df[stud]`,
            estimate = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_results$ttest)$`stat[stud]`, k),
            bf = ggstatsplot::specify_decimal_p(x = log(x = as.data.frame(jmv_results$ttest)$`stat[bf]`, base = exp(1)), k = 1),
            bf_error = ggstatsplot::specify_decimal_p(x = log(x = as.data.frame(jmv_results$ttest)$`err[bf]`, base = exp(1)), k = 1),
            effsize = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_results$ttest)$`es[stud]`, k),
            n = nrow(x = data)
          )
        )
      }
    }
    # ========================================== plot ===================================================================

    # preparing the basic layout of the plot based on whether counts or density information is needed

    if (bar.measure == "count") {

      # only counts
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

    # add message with bayes factor
    if (isTRUE(results.subtitle)) {
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

    # adding the theme and labels
    plot <- plot +
      ggstatsplot::theme_mprl(ggtheme = ggtheme, ggstatsplot.layer = ggstatsplot.layer) +
      ggplot2::labs(
        x = xlab,
        title = title,
        subtitle = subtitle,
        caption = caption.text
      )

    # ========================================== line and label ===================================================================

    # computing summary statistics needed for displaying labels
    x_mean <- mean(x = data$x, na.rm = TRUE)
    x_median <- median(x = data$x, na.rm = TRUE)
    y_label_median <- median(x = ggplot2::layer_scales(plot)$y$range$range, na.rm = TRUE)

    # if test value is to be added
    if (isTRUE(test.value.line)) {
      plot <- plot +
        ggplot2::geom_vline(
          xintercept = test.value,
          linetype = test.value.linetype,
          color = test.value.color,
          size = test.value.size,
          na.rm = TRUE
        )

      if (isTRUE(test.line.labeller)) {
        # adding a text label with test value
        plot <-
          plot +
          ggplot2::geom_label(
            mapping = ggplot2::aes(
              label = list(bquote(
                "test" == .(ggstatsplot::specify_decimal_p(
                  x = test.value, k = test.k
                ))
              )),
              x = test.value,
              y = y_label_median * (1 - 0.25)
            ),
            show.legend = FALSE,
            parse = TRUE,
            color = test.value.color
          )
      }
    }

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

        if (isTRUE(centrality.line.labeller)) {
          # adding a text label with mean value
          plot <-
            plot +
            ggplot2::geom_label(
              mapping = ggplot2::aes(
                label = list(bquote(
                  "mean" == .(ggstatsplot::specify_decimal_p(
                    x = x_mean, k = centrality.k
                  ))
                )),
                x = x_mean,
                y = y_label_median * (1 + 0.25)
              ),
              show.legend = FALSE,
              parse = TRUE,
              color = centrality.color
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

        # adding a text label with median value
        if (isTRUE(centrality.line.labeller)) {
          plot <-
            plot +
            ggplot2::geom_label(
              mapping = ggplot2::aes(
                label = list(bquote(
                  "median" == .(ggstatsplot::specify_decimal_p(
                    x = x_median, k = centrality.k
                  ))
                )),
                x = x_median,
                y = y_label_median * (1 + 0.25)
              ),
              show.legend = FALSE,
              parse = TRUE,
              color = centrality.color
            )
        }
      }
    }


    # if no color fill gradient is used, then remove the legend
    if (!isTRUE(fill.gradient)) {
      plot <- plot +
        ggplot2::theme(legend.position = "none")
    }

    # if caption is provided then use combine_plots function later on to add this caption
    # add caption with bayes factor
    if (isTRUE(results.subtitle)) {
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

    # ========================================== messages ==================================================================
    #
    # display normality test result as a message
    if (isTRUE(messages)) {
      normality_message(x = data$x, lab = lab.df[1], k = k)
    }

    # return the final plot
    return(plot)
  }
