#'
#' @title histogram for distribution of a numeric variable
#' @name gghistostats
#' @aliases gghistostats
#' @description Histogram with statistical details from one-sample test included in the plot as a subtitle.
#' @author Indrajeet Patil
#'
#' @param data Dataframe from which variables specified are preferentially to be taken.
#' @param x A numeric variable.
#' @param xlab Label for `x` axis variable.
#' @param title The text for the plot title.
#' @param subtitle The text for the plot subtitle *if* you don't want results from one sample test to be displayed.
#' @param caption The text for the plot caption.
#' @param type Type of statistic expected ("parametric" or "nonparametric").
#' @param test.value A number specifying the value of the null hypothesis.
#' @param k Number of decimal places expected for results.
#' @param results.subtitle Decides whether the results of statistical tests are to be displayed as subtitle.
#' @param normality.plot Decides whether normal distribution plot is overlayed on top of the underlying histogram.
#' @param centrality.plot Decides *whether* measure of central tendency is to be displayed (in the form of a vertical line).
#' @param centrality.colour Decides colour for the vertical line.
#' @param centrality.para Decides *which* measure of central tendency is used ("mean" or "median").
#' @param binwidth.adjust If set to `TRUE`, you can use it to pick better value with the `binwidth` argument to `stat_bin()`.
#' @param binwidth The width of the bins. Can be specified as a numeric value, or a function that calculates width from `x`.
#' The default is to use bins bins that cover the range of the data. You should always override this value,
#' exploring multiple widths to find the best to illustrate the stories in your data.
#'
#' @import ggplot2
#'
#' @importFrom jmv ttestOneS
#' @importFrom stats dnorm
#' @importFrom stats shapiro.test
#'
#' @examples
#' library(ggplot2)
#'
#' ggstatsplot::gghistostats(
#' data = iris,
#' x = Sepal.Length,
#' type = "parametric",
#' test.value = 3,
#' centrality.plot = TRUE,
#' centrality.para = "mean",
#' normality.plot = TRUE,
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
    "sd"
  )
)

gghistostats <-
  function(data = NULL,
           x,
           xlab = NULL,
           title = NULL,
           subtitle = NULL,
           caption = NULL,
           type = "parametric",
           test.value = 0,
           k = 3,
           results.subtitle = TRUE,
           normality.plot = FALSE,
           centrality.plot = FALSE,
           centrality.para = "mean",
           centrality.colour = "blue",
           binwidth.adjust = FALSE,
           binwidth = NULL) {
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
      if (type == "parametric") {
        # model
        jmv_os <- jmv::ttestOneS(
          data = data,
          vars = "x",
          students = TRUE,
          bf = FALSE,
          bfPrior = 0.707,
          mann = FALSE,
          # Mann-Whitney U test
          testValue = test.value,
          hypothesis = "dt",
          # two-sided hypothesis-testing
          effectSize = TRUE
        )

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
            estimate = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_os$ttest)$stat, k),
            # df is integer value for Student's t-test
            df = as.data.frame(jmv_os$ttest)$df,
            pvalue = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_os$ttest)$p, k, p.value = TRUE),
            effsize = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_os$ttest)$es, k)
          )
        )
      } else if (type == "nonparametric") {
        # model
        jmv_os <- jmv::ttestOneS(
          data = data,
          vars = "x",
          students = FALSE,
          bf = FALSE,
          bfPrior = 0.707,
          mann = TRUE,
          # Mann-Whitney U test
          testValue = test.value,
          hypothesis = "dt",
          # two-sided hypothesis-testing
          effectSize = TRUE
        )

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
            estimate = as.data.frame(jmv_os$ttest)$stat,
            pvalue = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_os$ttest)$p, k, p.value = TRUE),
            effsize = ggstatsplot::specify_decimal_p(x = as.data.frame(jmv_os$ttest)$es, k)
          )
        )
      }
    } else {
      subtitle <- subtitle
    }
    # ========================================== plot ===================================================================

    # if the user wants to adjust the binwidth
    if (isTRUE(binwidth.adjust)) {
      plot <- ggplot2::ggplot(data = data,
                              mapping = aes(x = x)) +
        ggplot2::stat_bin(
          col = "black",
          alpha = 0.7,
          binwidth = binwidth,
          mapping = aes(y = ..density..,
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
          mapping = aes(y = ..density.., fill = ..count..)
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
    if (isTRUE(centrality.plot)) {
      if (centrality.para == "mean") {
        plot <- plot +
          ggplot2::geom_vline(
            xintercept = mean(data$x),
            linetype = "dashed",
            color = centrality.colour,
            size = 1.2
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
            size = 1.2
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
    if (isTRUE(normality.plot)) {
      plot <- plot +
        ggplot2::stat_function(
          fun = dnorm,
          color = "black",
          args = list(mean = mean(data$x),
                      sd = sd(data$x)),
          linetype = "dashed",
          size = 1.2
        )
    }
    # display homogeneity of variances test result as a message
    sw_norm <- stats::shapiro.test(data$x)
    base::message(cat(
      crayon::green("Note: "),
      crayon::blue("Shapiro-Wilk test of normality for",
                   crayon::yellow(lab.df[1]), # entered x argument
                   ": p-value = "),
      crayon::yellow(
        ggstatsplot::specify_decimal_p(x = sw_norm$p.value,
                                       k,
                                       p.value = TRUE)
      )
    ))
    # return the final plot
    return(plot)
  }
