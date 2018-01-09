#'
#' @title scatterplot with ggMarginals
#' @name ggscatterstats
#' @author Indrajeet Patil
#'
#' @param data data frame from which variables specified are preferentially to be taken
#' @param x a vector containing the explanatory variable
#' @param y the response - a vector of length the number of rows of `x`
#' @param xlab label for x axis variable
#' @param ylab label for y axis variable
#' @param marginal decides whether `ggExtra::ggMarginal()` plots will be displayed; the default is `TRUE`
#' @param marginaltype type of marginal distribution to be plotted on the axes (NULL, "histogram", "boxplot", "density")
#' @param xfill color fill for x axis distibution
#' @param yfill color fill for y axis distribution
#' @param test statistical test to be run and displayed as subtitle ("pearson", "spearman", "robust")
#' @param intercept decides whether "mean" or "median" or no intercept lines (`NULL`) are to be plotted
#' @param title title for the plot
#' @param caption caption for the plot
#' @param k number of decimal places expected for results
#'
#' @export

library(ggplot2)

ggscatterstats <-
  function(data = NULL,
           x,
           y,
           xlab = NULL,
           ylab = NULL,
           marginal = NULL,
           marginaltype = NULL,
           xfill = NULL,
           yfill = NULL,
           intercept = NULL,
           test = NULL,
           title = NULL,
           caption = NULL,
           k = 3) {
    # if fill colors for x and y axes are not specified, use the defaults
    if (is.null(xfill))
      xfill <- "orange"
    if (is.null(yfill))
      yfill <- "green"

    # if test to be run is not satisfied, then use default, which is robust regression from MASS package
    if (is.null(test))
      test <- "pearson"

    if (test == "pearson") {
      # running the correlation test and preparing the subtitle text
      c <- stats::cor.test(x, y, method = "pearson", exact = FALSE)
      stat_label <-
        base::substitute(
          paste(
            "Pearson's ",
            italic("r"),
            "(",
            df,
            ")",
            " = ",
            estimate,
            ", ",
            italic("p"),
            " = ",
            pvalue
          ),
          list(
            df = c$parameter,
            # degrees of freedom are always integer
            estimate = ggstatsplot::specify_decimal(c$estimate, k),
            pvalue = ggstatsplot::specify_decimal_p(c$p.value, k)
          )
        )

    }   else if (test == "spearman") {
      # running the correlation test and preparing the subtitle text
      # note that stats::cor.test doesn't give degress of freedom; it's calculated as df = (no. of pairs - 2)
      c <- stats::cor.test(x, y, method = "spearman", exact = FALSE)
      stat_label <-
        base::substitute(
          paste(
            "Spearman's ",
            italic(rho),
            "(",
            df,
            ")",
            " = ",
            estimate,
            ", ",
            italic("p"),
            " = ",
            pvalue
          ),
          list(
            df = (length(x) - 2),
            # degrees of freedom are always integer
            estimate = ggstatsplot::specify_decimal(c$estimate, k),
            pvalue = ggstatsplot::specify_decimal_p(c$p.value, k)
          )
        )

    } else if (test == "robust") {
      # running robust regression test and preparing the subtitle text
      MASS_res <-
        MASS::rlm(
          scale(y) ~ scale(x),
          maxit = 1000,
          # number of iterations
          na.action = na.omit,
          data = data
        )
      stat_label <-
        base::substitute(
          paste(
            "robust regression: estimate = ",
            estimate,
            ", ",
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
          list(
            estimate = ggstatsplot::specify_decimal(summary(MASS_res)$coefficients[[2]], k),
            t = ggstatsplot::specify_decimal(summary(MASS_res)$coefficients[[6]], k),
            df = summary(MASS_res)$df[2],
            # degrees of freedom are always integer
            pvalue = ggstatsplot::specify_decimal_p((sfsmisc::f.robftest(MASS_res))$p.value),
            k
          )
        )
      base::warning(
        "For robust regression: no. of iterations = 1000; estimate is standardized",
        noBreaks. = TRUE,
        call. = TRUE
      )
    }

    # preparing the scatterplotplot
    plot <- ggplot2::ggplot(data = data, mapping = aes(x = x, y = y)) +
      geom_count(
        show.legend = FALSE,
        color = "black",
        size = 3,
        alpha = 0.5,
        position = position_jitterdodge(
          jitter.width = NULL,
          jitter.height = 0.2,
          dodge.width = 0.75
        )
      ) +
      geom_smooth(method = "lm",
                  se = TRUE,
                  size = 1.5) + # default is robust linear model
      ggstatsplot::theme_mprl() +
      labs(
        x = xlab,
        y = ylab,
        title = title,
        subtitle = stat_label,
        caption = caption
      )

    # by default, if the input is NULL, then no intercept lines will be plotted

    if (is.null(intercept)) {
      plot <- plot

    } else if (intercept == "mean") {
      plot <- plot +
        geom_vline(
          xintercept = mean(x),
          linetype = "dashed",
          color = xfill,
          size = 1.2
        ) +
        geom_hline(
          yintercept = mean(y),
          linetype = "dashed",
          color = yfill,
          size = 1.2
        )

    } else if (intercept == "median") {
      plot <- plot +
        geom_vline(
          xintercept = mean(x),
          linetype = "dashed",
          color = xfill,
          size = 1.2
        ) +
        geom_hline(
          yintercept = mean(y),
          linetype = "dashed",
          color = yfill,
          size = 1.2
        )

    }

    # if marginal should be plotted or not is not specified, it will be plotted by default
    if (is.null(marginal))
      marginal <- TRUE

    if (isTRUE(marginal)) {
      # if ggmarginal marginaltype has not been specified, go to this default
      if (is.null(marginaltype))
        marginaltype <- "histogram"

      # creating the ggMarginal plot of a given marginaltype
      plot <-
        ggExtra::ggMarginal(
          plot,
          type = marginaltype,
          xparams = list(fill = xfill, col = "black"),
          yparams = list(fill = yfill, col = "black")
        )
    }

    return(plot)

  }
