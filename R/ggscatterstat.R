#'
#' @title scatterplot with ggMarginals
#' @name ggscatterstat
#' @author Indrajeet Patil
#'
#' @param df data frame from which variables specified are preferentially to be taken
#' @param x a vector containing the explanatory variable
#' @param xlab label for x axis variable
#' @param ylab label for y axis variable
#' @param y the response - a vector of length the number of rows of `x`
#' @param marginal decides whether `ggExtra::ggMarginal()` plots will be displayed; the default is `TRUE`
#' @param marginaltype type of marginal distribution to be plotted on the axes (NULL, "histogram", "boxplot", "density")
#' @param xfill color fill for x axis distibution
#' @param yfill color fill for y axis distribution
#' @param test statistical test to be run and displayed as subtitle ("pearson", "spearman", "robust")
#' @param intercept decides whether "mean" or "median" or no intercept lines (`NULL`) are to be plotted
#' @param title title for the plot
#' @param caption caption for the plot
#' @export

ggscatterstat <-
  function(df,
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
           caption = NULL) {
    # if fill colors for x and y axes are not specified, use the defaults
    if (is.null(xfill))
      xfill <- "orange"
    if (is.null(yfill))
      yfill <- "green"

    # if test to be run is not satisfied, then use default, which is robust regression from MASS package
    if (is.null(test))
      test <- "robust"

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
            df = specify_decimal(c$parameter, 0),
            estimate = specify_decimal(c$estimate, 3),
            pvalue = specify_decimal_p(c$p.value, 3)
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
            df = specify_decimal((length(x) - 2), 0),
            estimate = specify_decimal(c$estimate, 3),
            pvalue = specify_decimal_p(c$p.value, 3)
          )
        )

    } else if (test == "robust") {
      # running robust regression test and preparing the subtitle text
      MASS_res <-
        MASS::rlm(
          scale(y) ~ scale(x),
          maxit = 1000,
          na.action = na.omit,
          data = df
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
            estimate = specify_decimal(summary(MASS_res)$coefficients[[2]], 3),
            t = specify_decimal(summary(MASS_res)$coefficients[[6]], 3),
            df = summary(MASS_res)$df[2],
            pvalue = specify_decimal_p((sfsmisc::f.robftest(MASS_res))$p.value),
            3
          )
        )
    }

    # preparing the scatterplotplot
    plot <- ggplot2::ggplot(df, aes(x = x, y = y)) +
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
      geom_smooth(method = "rlm",
                  se = TRUE,
                  size = 1.5) + # default is robust linear model
      theme_mprl() + # theme_mprl() is already defined below
      labs(
        x = xlab,
        y = ylab,
        title = title,
        subtitle = stat_label,
        caption = caption
      )

    # by default, if the input is NULL, then no intercept lines will be plotted

    if (intercept == "mean") {
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

    } else if (is.null(intercept)) {
      plot <- plot

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


#### other custom functions used inside this function


## default theme to use for ggplot figures

library(ggplot2)
theme_mprl <- function() {
  ggplot2::theme_grey() +
    ggplot2::theme(
      axis.title.x = element_text(size = 14, face = "bold"),
      strip.text.x = element_text(size = 14, face = "bold"),
      strip.text.y = element_text(size = 14, face = "bold"),
      strip.text = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      axis.line = element_line(),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 14, face = "bold"),
      legend.title.align = 0.5,
      legend.text.align = 0.5,
      legend.key.height = unit(1, "line"),
      legend.key.width = unit(1, "line"),
      plot.margin = unit(c(1, 1, 1, 1), "lines"),
      # requires library(grid))
      panel.border = element_rect(
        colour = "black",
        fill = NA,
        size = 1
      ),
      plot.title = element_text(
        color = "black",
        size = 16,
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        color = "black",
        size = 12,
        face = "bold",
        hjust = 0.5
      )
    )

}

## custom function for getting specified number of decimal places in results
# x is a numeric value, while k is the number of digits after decimal point (should be an integer)

specify_decimal <- function(x, k = NULL) {
  # if the number of decimal places hasn't been specified, use the default of 3
  if (is.null(k))
    k <- 3
  output <- trimws(format(round(x, k), nsmall = k))
  return(output)

}

### custom function for getting specified number of decimal places in results for p-value
# x is a numeric value, while k is the number of digits after decimal point (should be an integer)

specify_decimal_p <- function(x, k = NULL) {
  # if the number of decimal places hasn't been specified, use the default of 3
  if (is.null(k))
    k <- 3
  output <- trimws(format(round(x, k), nsmall = k))
  if (output < 0.001)
    output <- "< 0.001"
  return(output)

}
