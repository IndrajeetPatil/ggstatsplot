#'
#' @title histogram for distribution of a numeric variable
#' @name gghistostats
#' @aliases gghistostats
#' @description Histogram with statistical details from one-sample test included
#'   in the plot as a subtitle.
#' @author Indrajeet Patil
#'
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param x A numeric variable.
#' @param xlab Label for `x` axis variable.
#' @param title The text for the plot title.
#' @param subtitle The text for the plot subtitle *if* you don't want results
#'   from one sample test to be displayed.
#' @param caption The text for the plot caption.
#' @param type Type of statistic expected ("parametric" or "nonparametric" or
#'   "bayes"). Abbreviations accepted are "p" or "np" or "bf", respectively.
#' @param test.value A number specifying the value of the null hypothesis.
#' @param bf.prior A number between 0.5 and 2 (default 0.707), the prior width
#'   to use in calculating Bayes factors.
#' @param bf.message Logical. Decides whether to display Bayes Factor in favor
#'   of null hypothesis for parametric test if the null hypothesis can't be
#'   rejected (Default: `bf.message = TRUE`).
#' @param k Number of decimal places expected for results.
#' @param results.subtitle Decides whether the results of statistical tests are
#'   to be displayed as subtitle.
#' @param density.plot Decides whether kernel density estimate, which is a
#'   smoothed version of the histogram, is to be overlayed on top of the
#'   histogram.
#' @param density.colour Decides colour for the density plot.
#' @param centrality.para Decides *which* measure of central tendency ("mean" or
#'   "median") is to be displayed as a vertical line.
#' @param centrality.colour Decides colour for the vertical line.
#' @param binwidth.adjust If set to `TRUE`, you can use it to pick better value
#'   with the `binwidth` argument to `stat_bin()`.
#' @param binwidth The width of the bins. Can be specified as a numeric value,
#'   or a function that calculates width from `x`. The default is to use bins
#'   bins that cover the range of the data. You should always override this
#'   value, exploring multiple widths to find the best to illustrate the stories
#'   in your data.
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#'
#' @import ggplot2
#'
#' @importFrom jmv ttestOneS
#' @importFrom stats dnorm
#' @importFrom nortest ad.test
#' @importFrom crayon green
#' @importFrom crayon blue
#' @importFrom crayon yellow
#' @importFrom crayon red
#'
#' @examples
#'
#' # most basic function call with the defaults
#' ggstatsplot::gghistostats(
#' data = datasets::ToothGrowth,
#' x = len,
#' xlab = "Tooth length")
#'
#'# another example
#' ggstatsplot::gghistostats(
#' data = NULL,
#' x = stats::rnorm(n = 1000, mean = 0, sd = 1),
#' centrality.para = "mean",
#' type = "np")
#'
#' # more detailed function call
#' ggstatsplot::gghistostats(
#' data = datasets::iris,
#' x = Sepal.Length,
#' type = "bf",
#' bf.prior = 0.8,
#' test.value = 3,
#' centrality.para = "mean",
#' density.plot = TRUE,
#' binwidth.adjust = TRUE,
#' binwidth = 0.10
#' )
#'
#' @export
#'

# defining global variables and functions to quient the R CMD check notes
utils::globalVariables(
  c(
    "U",
    "V",
    "Z",
    "chi",
    "counts",
    "df",
    "df1",
    "df2",
    "effsize",
    "estimate",
    "eta",
    "omega",
    "perc",
    "cramer",
    "pvalue",
    "r",
    "rho",
    "xi",
    "y",
    "z_value",
    "italic",
    "rsubtitle",
    "stats_subtitle",
    "chi_subtitle",
    "proptest_subtitle",
    "LL",
    "UL",
    "..count..",
    "..density..",
    "dnorm",
    "mean",
    "median",
    "sd",
    "bf",
    "bf_error"
  )
)

# function body
gghistostats <-
  function(data = NULL,
           x,
           xlab = NULL,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           type = "parametric",
           test.value = 0,
           bf.prior = 0.707,
           bf.message = TRUE,
           k = 3,
           results.subtitle = TRUE,
           density.plot = FALSE,
           density.colour = "black",
           centrality.para = NULL,
           centrality.colour = "blue",
           binwidth.adjust = FALSE,
           binwidth = NULL,
           messages = TRUE) {
    # if data is not available then don't display any messages
    if (is.null(data))
      messages <- FALSE
    # save the value of caption in another variable because caption is going to be modified in the function body
    bf.caption <- caption
    # ========================================== dataframe ==============================================================
    # preparing a dataframe out of provided inputs
    if (!is.null(data)) {
      # preparing labels from given dataframe
      lab.df <- colnames(dplyr::select(.data = data,
                                       !!rlang::enquo(x)))
      # if xlab is not provided, use the variable x name
      if (is.null(xlab)) {
        xlab <- lab.df[1]
      }
      # if dataframe is provided
      data <-
        dplyr::select(.data = data,
                      x = !!rlang::enquo(x))
    } else {
      # if vectors are provided
      data <-
        base::cbind.data.frame(x = x)
    }
    # ========================================== stats ==================================================================
    if (isTRUE(results.subtitle)) {
      # model
      jmv_os <- jmv::ttestOneS(
        data = data,
        vars = "x",
        students = TRUE,
        bf = TRUE,
        bfPrior = bf.prior,
        mann = TRUE,
        # Mann-Whitney U test
        testValue = test.value,
        hypothesis = "dt",
        # two-sided hypothesis-testing
        effectSize = TRUE
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
              effsize
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
            effsize = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_os$ttest)$`es[stud]`, k)
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
                "with prior =",
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
              effsize
            ),
          env = base::list(
            estimate = as.data.frame(jmv_os$ttest)$`stat[mann]`,
            pvalue = ggstatsplot::specify_decimal_p(
              x = as.data.frame(jmv_os$ttest)$`p[mann]`,
              k,
              p.value = TRUE
            ),
            effsize = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_os$ttest)$`es[mann]`, k)
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
              "BF"[10],
              " = ",
              bf,
              ", error = ",
              bf_error,
              ", ",
              italic("d"),
              " = ",
              effsize
            ),
          env = base::list(
            # df is integer value for Student's t-test
            df = as.data.frame(jmv_os$ttest)$`df[stud]`,
            estimate = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_os$ttest)$`stat[stud]`, k),
            bf = as.data.frame(jmv_os$ttest)$`stat[bf]`,
            bf_error = as.data.frame(jmv_os$ttest)$`err[bf]`,
            effsize = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_os$ttest)$`es[stud]`, k)
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
    }
    # ========================================== plot ===================================================================

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

    # if the user wants to adjust the binwidth
    if (isTRUE(binwidth.adjust)) {
      plot <- ggplot2::ggplot(data = data,
                              mapping = ggplot2::aes(x = x)) +
        ggplot2::stat_bin(
          col = "black",
          alpha = 0.7,
          binwidth = binwidth,
          na.rm = TRUE,
          mapping = ggplot2::aes(y = ..density..,
                                 fill = ..count..)
        ) +
        ggplot2::scale_fill_gradient("count",
                                     low = "green",
                                     high = "red") +
        ggstatsplot::theme_mprl() +
        ggplot2::labs(
          x = xlab,
          title = title,
          subtitle = subtitle,
          caption = caption
        )
    } else {
      # if not, use the defaults
      plot <- ggplot2::ggplot(data = data,
                              mapping = aes(x = x)) +
        ggplot2::geom_histogram(
          col = "black",
          alpha = 0.7,
          mapping = ggplot2::aes(y = ..density.., fill = ..count..),
          na.rm = TRUE
        ) +
        ggplot2::scale_fill_gradient("count",
                                     low = "green",
                                     high = "red") +
        ggstatsplot::theme_mprl() +
        ggplot2::labs(
          x = xlab,
          title = title,
          subtitle = subtitle,
          caption = caption
        )
    }

    # if central tendency parameter is to be added
    if (!is.null(centrality.para)) {
      if (centrality.para == "mean") {
        plot <- plot +
          ggplot2::geom_vline(
            xintercept = mean(data$x),
            linetype = "dashed",
            color = centrality.colour,
            size = 1.2,
            na.rm = TRUE
          )
        # this can be used to label the vertical lines, but leave it out since it makes for an ugly plot
        # + ggplot2::geom_text(
        #   mapping = aes(
        #     x = mean(data$x) + 0.10,
        #     label = "mean",
        #     y = -0.05
        #   ),
        #   colour = "black",
        #   angle = 0,
        #   size = 11
        # )
      } else if (centrality.para == "median") {
        plot <- plot +
          ggplot2::geom_vline(
            xintercept = median(data$x),
            linetype = "dashed",
            color = "blue",
            size = 1.2,
            na.rm = TRUE
          )
        # this can be used to label the vertical lines, but leave it out since it makes for an ugly plot
        # + ggplot2::geom_text(
        #     mapping = aes(
        #       x = median(data$x) + 0.13,
        #       label = "median",
        #       y = -0.05
        #     ),
        #     colour = "black",
        #     angle = 0,
        #     size = 11
        #   )
      }
    }

    # if normal distribution plot is to be added
    if (isTRUE(density.plot)) {
      plot <- plot +
        ggplot2::geom_density(colour = density.colour,
                              size = 1.0,
                              na.rm = TRUE)
    }

    # if caption is provided then use combine_plots function later on to add this caption
    # add caption with bayes factor
    if (type == "parametric") {
      if (as.data.frame(jmv_os$ttest)$`p[stud]` > 0.05) {
        if (isTRUE(bf.message)) {
          if (!is.null(bf.caption)) {
            plot <-
              ggstatsplot::combine_plots(plot,
                                         caption.text = bf.caption.text)
          }
        }
      }
    }
    # ========================================== messages ==================================================================
    if (isTRUE(messages)) {
      # display normality test result as a message
      # # for AD test of normality, sample size must be greater than 7
      if (length(data$x) > 7) {
        ad_norm <- nortest::ad.test(x = data$x)
        base::message(cat(
          crayon::green("Note: "),
          crayon::blue(
            "Anderson-Darling Normality Test for",
            crayon::yellow(lab.df[1]),
            # entered x argument
            ": p-value = "
          ),
          crayon::yellow(
            ggstatsplot::specify_decimal_p(x = ad_norm$p.value,
                                           k,
                                           p.value = TRUE)
          )
        ))
      }
    }
    # return the final plot
    return(plot)
  }
