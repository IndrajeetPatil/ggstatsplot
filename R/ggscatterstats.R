#'
#' @title Scatterplot with marginal distributions
#' @name ggscatterstats
#' @aliases ggscatterstats
#' @author Indrajeet Patil
#' @description Scatterplots from `ggplot2` combined with marginal
#'   histograms/boxplots/density plots with statistical details added as a
#'   subtitle.
#'
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param x A vector containing the explanatory variable.
#' @param y The response - a vector of length the number of rows of `x`.
#' @param xlab Label for `x` axis variable.
#' @param ylab Label for `y` axis variable.
#' @param line.color color for the regression line.
#' @param line.size Size for the regression line.
#' @param marginal Decides whether `ggExtra::ggMarginal()` plots will be
#'   displayed; the default is `TRUE`.
#' @param marginal.type Type of marginal distribution to be plotted on the axes
#'   (`"histogram"`, `"boxplot"`, `"density"`, `"violin"`).
#' @param marginal.size Integer describing the relative size of the marginal
#'   plots compared to the main plot. A size of `5` means that the main plot is
#'   5x wider and 5x taller than the marginal plots.
#' @param margins Character describing along which margins to show the plots.
#'   Any of the following arguments are accepted: `"both"`, `"x"`, `"y"`.
#' @param xfill color fill for x axis distribution (default: `"#009E73"`).
#' @param yfill color fill for y axis distribution (default: `"#D55E00"`).
#' @param type Type of association between paired samples required
#'   ("`"parametric"`: Pearson's product moment correlation coefficient" or
#'   "`"nonparametric"`: Spearman's rho" or "`"robust"`: Robust regression using
#'   an M estimator"). Corresponding abbreviations are also accepted: `"p"` (for
#'   parametric/pearson's), `"np"` (nonparametric/spearman), `"r"` (robust),
#'   resp.
#' @param results.subtitle Decides whether the results of statistical tests are
#'   to be displayed as subtitle.
#' @param centrality.para Decides *which* measure of central tendency (`"mean"`
#'   or `"median"`) is to be displayed as vertical (for `x`) and horizontal (for
#'   `y`) lines.
#' @param title The text for the plot title.
#' @param caption The text for the plot caption.
#' @param nboot Number of bootstrap samples for computing effect size (Default:
#'   `100`).
#' @param beta bending constant (Default: `0.1`). For more, see `?WRS2::pbcor`.
#' @param k Number of decimal places expected for results.
#' @param width.jitter Degree of jitter in `x` direction. Defaults to 40\% of
#'   the resolution of the data.
#' @param height.jitter Degree of jitter in `y` direction. Defaults to 40\% of
#'   the resolution of the data.
#' @param axes.range.restrict Logical decides whether to restrict the axes values
#'   ranges to min and max values of the `x` and `y` variables (Default: `FALSE`).
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
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom broom tidy
#' @importFrom ggExtra ggMarginal
#' @importFrom stats cor.test
#' @importFrom stats na.omit
#' @importFrom stats confint.default
#'
#' @seealso \code{\link{grouped_ggscatterstats}} \code{\link{ggcorrmat}} \code{\link{grouped_ggcorrmat}}
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/ggscatterstats.html}
#'
#' @examples
#'
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#'
#' # simple function call with the defaults
#' ggstatsplot::ggscatterstats(
#' data = datasets::mtcars,
#' x = wt,
#' y = mpg,
#' type = "np"
#' )
#'
#' @export
#'

# defining the function
ggscatterstats <-
  function(data,
           x,
           y,
           xlab = NULL,
           ylab = NULL,
           line.size = 1.5,
           line.color = "blue",
           marginal = TRUE,
           marginal.type = "histogram",
           marginal.size = 5,
           margins = c("both", "x", "y"),
           width.jitter = NULL,
           height.jitter = NULL,
           xfill = "#009E73",
           yfill = "#D55E00",
           centrality.para = NULL,
           type = "pearson",
           results.subtitle = NULL,
           title = NULL,
           caption = NULL,
           nboot = 100,
           beta = 0.1,
           k = 3,
           axes.range.restrict = FALSE,
           ggtheme = ggplot2::theme_bw(),
           messages = TRUE) {
    ################################################### dataframe ####################################################

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
    # if dataframe is provided
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(x),
        y = !!rlang::enquo(y)
      )

    ######################################## statistical labels ######################################################

    # if results.subtitle argument is not specified, default to showing the results
    if (is.null(results.subtitle)) {
      results.subtitle <- TRUE
    }
    # if results.subtitle argument is set to FALSE then subtitle should be set to NULL
    if (results.subtitle != TRUE) {
      stats_subtitle <- NULL
    }

    if (results.subtitle == TRUE) {
      # running the correlation test and preparing the subtitle text
      if (type == "pearson" || type == "p") {
        ################################################### Pearson's r ##################################################

        c <-
          stats::cor.test(
            formula = ~ x + y,
            data = data,
            method = "pearson",
            alternative = "two.sided",
            exact = FALSE,
            na.action = na.omit
          )

        # preparing the label
        stats_subtitle <-
          base::substitute(
            expr =
              paste(
                "Pearson's ",
                italic("r"),
                "(",
                df,
                ")",
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
              # degrees of freedom are always integer
              df = c$parameter[[1]],
              t = ggstatsplot::specify_decimal_p(x = c$statistic[[1]], k),
              estimate = ggstatsplot::specify_decimal_p(x = c$estimate[[1]], k),
              LL = ggstatsplot::specify_decimal_p(x = c$conf.int[1][[1]], k),
              UL = ggstatsplot::specify_decimal_p(x = c$conf.int[2][[1]], k),
              pvalue = ggstatsplot::specify_decimal_p(x = c$p.value[[1]], k, p.value = TRUE),
              n = nrow(x = data)
            )
          )
      } else if (type == "spearman" || type == "np") {
        ################################################### Spearnman's rho ##################################################
        # running the correlation test and preparing the subtitle text
        # note that stats::cor.test doesn't give degress of freedom; it's calculated as df = (no. of pairs - 2)
        c <-
          stats::cor.test(
            formula = ~ x + y,
            data = data,
            method = "spearman",
            alternative = "two.sided",
            exact = FALSE,
            na.action = na.omit
          )

        # getting confidence interval for rho using broom bootstrap
        c_ci <- cor_tets_ci(
          data = data,
          x = x,
          y = y,
          nboot = nboot
        )

        # preparing the label
        stats_subtitle <-
          base::substitute(
            expr =
              paste(
                "Spearman's ",
                italic(rho),
                "(",
                df,
                ")",
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
              df = (length(data$x) - 2),
              # degrees of freedom are always integer
              estimate = ggstatsplot::specify_decimal_p(x = c$estimate[[1]], k),
              LL = ggstatsplot::specify_decimal_p(x = c_ci$conf.low[[1]], k),
              UL = ggstatsplot::specify_decimal_p(x = c_ci$conf.high[[1]], k),
              pvalue = ggstatsplot::specify_decimal_p(x = c$p.value[[1]],
                                                      k,
                                                      p.value = TRUE),
              n = nrow(x = data)
            )
          )
        ################################################### robust ##################################################
      } else if (type == "robust" || type == "r") {
        # running robust correlation
        rob_res <- robcor_ci(
          data = data,
          x = x,
          y = y,
          beta = beta,
          nboot = nboot,
          conf.level = 0.95,
          conf.type = "norm"
        )

        # preparing the subtitle
        stats_subtitle <-
          base::substitute(
            expr =
              paste(
                "robust ",
                italic(r),
                " = ",
                estimate,
                ", 95% CI [",
                LL,
                ", ",
                UL,
                "], ",
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
              estimate = ggstatsplot::specify_decimal_p(x = rob_res$r[[1]], k),
              LL = ggstatsplot::specify_decimal_p(x = rob_res$conf.low[[1]], k),
              UL = ggstatsplot::specify_decimal_p(x = rob_res$conf.high[[1]], k),
              # degrees of freedom are always integer
              pvalue = ggstatsplot::specify_decimal_p(rob_res$`p-value`[[1]],
                                                      k,
                                                      p.value = TRUE),
              n = rob_res$n[[1]]
            )
          )

        # displaying message about what correlation was used
        if (isTRUE(messages)) {
          base::message(cat(
            crayon::green("Note:"),
            crayon::blue(
              "Percentage bend correlation with",
              crayon::yellow(nboot),
              "bootstrap samples was run."
            )
          ))
        }

      }
    }
    ################################################### plot ################################################################

    # preparing the scatterplotplot
    plot <-
      ggplot2::ggplot(data = data,
                      mapping = ggplot2::aes(x = x,
                                             y = y)) +
      ggplot2::geom_point(
        size = 3,
        alpha = 0.5,
        position = position_jitter(width = width.jitter,
                                   height = height.jitter),
        na.rm = TRUE
      ) +
      ggplot2::geom_smooth(
        method = "lm",
        se = TRUE,
        size = line.size,
        color = line.color,
        na.rm = TRUE
      ) +
      ggstatsplot::theme_mprl(ggtheme = ggtheme) +
      ggplot2::labs(
        x = xlab,
        y = ylab,
        title = title,
        subtitle = stats_subtitle,
        caption = caption
      )

    # forcing the plots to get cut off at min and max values of the variable
    if (isTRUE(axes.range.restrict)) {
      plot <- plot +
        ggplot2::coord_cartesian(xlim = c(min(data$x), max(data$x))) +
        ggplot2::coord_cartesian(ylim = c(min(data$y), max(data$y)))
    }
    ################################################ centrality.para ##################################################

    # by default, if the input is NULL, then no centrality.para lines will be plotted

    if (is.null(centrality.para)) {
      plot <- plot
    } else if (isTRUE(centrality.para) ||
               centrality.para == "mean") {
      plot <- plot +
        ggplot2::geom_vline(
          xintercept = mean(x = data$x, na.rm = TRUE),
          linetype = "dashed",
          color = xfill,
          size = 1.2,
          na.rm = TRUE
        ) +
        ggplot2::geom_hline(
          yintercept = mean(x = data$y, na.rm = TRUE),
          linetype = "dashed",
          color = yfill,
          size = 1.2,
          na.rm = TRUE
        )
    } else if (centrality.para == "median") {
      plot <- plot +
        ggplot2::geom_vline(
          xintercept = median(x = data$x, na.rm = TRUE),
          linetype = "dashed",
          color = xfill,
          size = 1.2,
          na.rm = TRUE
        ) +
        ggplot2::geom_hline(
          yintercept = median(x = data$y, na.rm = TRUE),
          linetype = "dashed",
          color = yfill,
          size = 1.2,
          na.rm = TRUE
        )
    }

    #################################################### ggMarginal ######################################################

    if (isTRUE(marginal)) {
      # creating the ggMarginal plot of a given marginal.type
      plot <-
        ggExtra::ggMarginal(
          p = plot,
          type = marginal.type,
          margins = margins,
          size = marginal.size,
          xparams = base::list(fill = xfill,
                               col = "black"),
          yparams = base::list(fill = yfill,
                               col = "black")
        )
    }

    ################################################### messages ##########################################################

    # display warning that this doesn't produce a ggplot2 object
    if (isTRUE(messages) &&
        isTRUE(marginal)) {
      base::message(cat(
        crayon::red("Warning:"),
        crayon::blue(
          "This function doesn't return a `ggplot2` object and is not further modifiable with `ggplot2` functions."
        )
      ))
    }

    # return the final plot
    return(plot)
  }
