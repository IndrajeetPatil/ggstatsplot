#'
#' @title scatterplot with ggMarginals
#' @name ggscatterstats
#' @aliases ggscatterstats
#' @author Indrajeet Patil
#' @description Scatterplots from `ggplot2`` combined with add marginal
#'   histograms/boxplots/density plots with statistical details added as a
#'   subtitle.
#'
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param x A vector containing the explanatory variable.
#' @param y The response - a vector of length the number of rows of `x`.
#' @param xlab Label for `x` axis variable.
#' @param ylab Label for `y` axis variable.
#' @param line.colour Colour for the regression line.
#' @param marginal Decides whether `ggExtra::ggMarginal()` plots will be
#'   displayed; the default is `TRUE`.
#' @param marginal.type Type of marginal distribution to be plotted on the axes
#'   ("histogram", "boxplot", "density", "violin").
#' @param xfill Colour fill for x axis distibution (default: `orange`).
#' @param yfill Colour fill for y axis distribution (default: `green`).
#' @param type Type of association between paired samples required ("parametric:
#'   Pearson's product moment correlation coefficient" or "nonparametric:
#'   Spearman's rho" or "robust: Robust regression using an M estimator").
#'   Corresponding abbreviations are also accepted: "p" (for
#'   parametric/pearson's), "np" (nonparametric/spearman), "r" (robust), resp.
#' @param results.subtitle Decides whether the results of statistical tests are
#'   to be displayed as subtitle.
#' @param intercept Decides whether "mean" or "median" or no intercept lines
#'   (`NULL`) are to be plotted.
#' @param title The text for the plot title.
#' @param caption The text for the plot caption.
#' @param maxit Maximum number of iterations for robust linear regression or
#'   bootstrap samples to compute Spearman's rho confidence intervals.
#' @param k Number of decimal places expected for results.
#' @param width.jitter Degree of jitter in `x` direction. Defaults to 40\% of
#'   the resolution of the data.
#' @param height.jitter Degree of jitter in `y` direction. Defaults to 40\% of
#'   the resolution of the data.
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#'
#' @import ggplot2
#' @import dplyr
#' @import rlang
#'
#' @importFrom MASS rlm
#' @importFrom sfsmisc f.robftest
#' @importFrom ggExtra ggMarginal
#' @importFrom stats cor.test
#' @importFrom stats na.omit
#' @importFrom stats confint.default
#' @importFrom RVAideMemoire spearman.ci
#'
#' @examples
#' ggstatsplot::ggscatterstats(data = iris, x = Petal.Length, y = Sepal.Length,
#' intercept = 'median', type = 'robust', marginal.type = 'density')
#'
#' @note If you want to use `marginal.type = "violin"`, you will have to use
#'   development version of `ggExtra`
#'
#' @export

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
    "UL"
  )
)

# defining the function
ggscatterstats <-
  function(data = NULL,
           x,
           y,
           xlab = NULL,
           ylab = NULL,
           line.colour = "blue",
           marginal = TRUE,
           marginal.type = "histogram",
           width.jitter = NULL,
           height.jitter = NULL,
           xfill = "orange",
           yfill = "green",
           intercept = NULL,
           type = "pearson",
           results.subtitle = NULL,
           title = NULL,
           caption = NULL,
           maxit = 1000,
           k = 3,
           messages = TRUE) {
    # if data is not available then don't display any messages
    if (is.null(data))
      messages <- FALSE
    ################################################### dataframe ####################################################
    # preparing a dataframe out of provided inputs
    if (!is.null(data)) {
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
      # if dataframe is provided
      data <-
        dplyr::select(
          .data = data,
          x = !!rlang::enquo(x),
          y = !!rlang::enquo(y)
        )
    } else {
      # if vectors are provided
      data <-
        base::cbind.data.frame(x = x,
                               y = y)
    }

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
      if (type == "pearson" | type == "p") {
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
                " = ",
                estimate,
                ", 95% CI [",
                LL,
                ", ",
                UL,
                "], ",
                italic("t"),
                "(",
                df,
                ")",
                " = ",
                t,
                ", ",
                italic("p"),
                " = ",
                pvalue
              ),
            env = base::list(
              # degrees of freedom are always integer
              df = c$parameter[[1]],
              t = ggstatsplot::specify_decimal_p(x = c$statistic[[1]], k),
              estimate = ggstatsplot::specify_decimal_p(x = c$estimate[[1]], k),
              LL = ggstatsplot::specify_decimal_p(x = c$conf.int[1][[1]], k),
              UL = ggstatsplot::specify_decimal_p(x = c$conf.int[2][[1]], k),
              pvalue = ggstatsplot::specify_decimal_p(x = c$p.value[[1]], k, p.value = TRUE)
            )
          )
      } else if (type == "spearman" | type == "np") {
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

        # getting confidence interval for rho
        c_ci <-
          RVAideMemoire::spearman.ci(
            var1 = data$x,
            var2 = data$y,
            # no. of interations
            nrep = maxit,
            conf.level = 0.95
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
                pvalue
              ),
            env = base::list(
              df = (length(data$x) - 2),
              # degrees of freedom are always integer
              estimate = ggstatsplot::specify_decimal_p(x = c$estimate, k),
              LL = ggstatsplot::specify_decimal_p(x = c_ci$conf.int[1][[1]], k),
              UL = ggstatsplot::specify_decimal_p(x = c_ci$conf.int[2][[1]], k),
              pvalue = ggstatsplot::specify_decimal_p(x = c$p.value, k, p.value = TRUE)
            )
          )
        ################################################### robust ##################################################
      } else if (type == "robust" | type == "r") {
        # running robust regression test and preparing the subtitle text
        MASS_res <-
          MASS::rlm(
            scale(y) ~ scale(x),
            data = data,
            maxit = maxit,
            # number of iterations
            na.action = na.omit
          )

        # getting confidence interval for rho
        c_ci <-
          stats::confint.default(object = MASS_res,
                                 parm = "scale(x)",
                                 level = 0.95)

        # preparing the label
        stats_subtitle <-
          base::substitute(
            expr =
              paste(
                "robust: ",
                italic(beta),
                " = ",
                estimate,
                ", 95% CI [",
                LL,
                ", ",
                UL,
                "], ",
                italic("t"),
                "(",
                df,
                ")",
                " = ",
                t,
                ", ",
                italic("p"),
                " = ",
                pvalue
              ),
            env = base::list(
              estimate = ggstatsplot::specify_decimal_p(x = summary(MASS_res)$coefficients[[2]], k),
              LL = ggstatsplot::specify_decimal_p(x = c_ci[1], k),
              UL = ggstatsplot::specify_decimal_p(x = c_ci[2], k),
              t = ggstatsplot::specify_decimal_p(x = summary(MASS_res)$coefficients[[6]], k),
              df = summary(MASS_res)$df[2],
              # degrees of freedom are always integer
              pvalue = ggstatsplot::specify_decimal_p(sfsmisc::f.robftest(MASS_res)$p.value,
                                                      k,
                                                      p.value = TRUE)
            )
          )
        # displaying the details of the test that was run
        if (isTRUE(messages)) {
          base::message(cat(
            crayon::green("Note:"),
            crayon::blue(
              "Standardized robust regression using an M estimator: no. of iterations =",
              crayon::yellow(maxit),
              "In case of non-convergence, increase maxit value."
            )
          ))
        }
      }
    }
    ################################################### plot ################################################################

    # preparing the scatterplotplot
    plot <-
      ggplot2::ggplot(data = data,
                      mapping = aes(x = x,
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
        size = 1.5,
        colour = line.colour,
        na.rm = TRUE
      ) +
      ggstatsplot::theme_mprl() +
      ggplot2::labs(
        x = xlab,
        y = ylab,
        title = title,
        subtitle = stats_subtitle,
        caption = caption
      ) +
      ggplot2::coord_cartesian(xlim = c(min(data$x), max(data$x))) +
      ggplot2::coord_cartesian(ylim = c(min(data$y), max(data$y)))

    ################################################ intercept ##################################################

    # by default, if the input is NULL, then no intercept lines will be plotted

    if (is.null(intercept)) {
      plot <- plot
    } else if (intercept == "mean") {
      plot <- plot +
        ggplot2::geom_vline(
          xintercept = mean(data$x),
          linetype = "dashed",
          colour = xfill,
          size = 1.2,
          na.rm = TRUE
        ) +
        ggplot2::geom_hline(
          yintercept = mean(data$y),
          linetype = "dashed",
          colour = yfill,
          size = 1.2,
          na.rm = TRUE
        )
    } else if (intercept == "median") {
      plot <- plot +
        ggplot2::geom_vline(
          xintercept = mean(data$x),
          linetype = "dashed",
          colour = xfill,
          size = 1.2,
          na.rm = TRUE
        ) +
        ggplot2::geom_hline(
          yintercept = mean(data$y),
          linetype = "dashed",
          colour = yfill,
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
          size = 5,
          xparams = base::list(fill = xfill,
                               col = "black"),
          yparams = base::list(fill = yfill,
                               col = "black")
        )

      ################################################### messages ##########################################################

      # display warning that this doesn't produce a ggplot2 object
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::red("Warning:"),
          crayon::blue(
            "This function doesn't return ggplot2 object and is not further modifiable with ggplot2 commands."
          )
        ))
      }
    }

    # return the final plot
    return(plot)
  }
