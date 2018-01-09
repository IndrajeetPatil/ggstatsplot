#'
#' @title violin plots for group or condition comparisons
#' @name ggbetweenstats
#' @author Indrajeet Patil
#'
#' @param data data frame from which variables specified are preferentially to be taken
#' @param x the grouping variable
#' @param y the response - a vector of length the number of rows of `x`
#' @param xlab label for x axis variable
#' @param ylab label for y axis variable
#' @param test statistical test to be run and displayed as subtitle ("t-test", "anova")
#' @param type type of statistics expected ("parametric" or "robust")
#' @param title title for the plot
#' @param caption caption for the plot
#' @param k number of decimal places expected for results
#'
#' @export

library(ggplot2)

ggbetweenstats <- function(data = NULL,
                           x,
                           y,
                           test = NULL,
                           type = NULL,
                           xlab = NULL,
                           ylab = NULL,
                           caption = NULL,
                           title = NULL,
                           k = 3) {
  ## creating the plot

  plot <- ggplot2::ggplot(data = data, mapping = aes(x, y)) +
    geom_point(
      position = position_jitterdodge(
        jitter.width = NULL,
        jitter.height = 0.2,
        dodge.width = 0.75
      ),
      alpha = 0.5,
      size = 3,
      aes(color = factor(x))
    ) +
    geom_violin(width = 0.5,
                alpha = 0.2,
                fill = "white") +
    # geom_violin(aes(fill = factor(x))) + # uncomment if you also want to superimpose violin plot fill over the boxplot
    geom_boxplot(
      width = 0.3,
      alpha = 0.2,
      fill = "white",
      outlier.colour = "black",
      outlier.shape = 16,
      outlier.size = 3,
      outlier.alpha = 0.7,
      # aes(color = factor(x)),
      position = position_dodge(width = NULL)
    ) +
    # coord_flip() + # uncomment this line in case you want to flip the coordinate axes
    theme_mprl() + theme(legend.position = "none") +
    labs(
      x = xlab,
      y = ylab,
      title = title,
      caption = caption
    ) +
    scale_fill_brewer(palette = "Dark2") +
    scale_colour_brewer(palette = "Dark2")

  ## custom function to write results from group comparison test subtitle of a given plot

  # if test is not specified, then figure out which test to run based on the number of levels of the independent variables
  if (is.null(test)) {
    if (length(levels(as.factor(x))) < 3)
      test <- "t-test"
    else
      test <- "anova"
  }

  if (test == "anova") {
    # if type of test is not specified, then use the default, which is parametric test
    if (is.null(type))
      type <- "parametric"

    if (type == "parametric") {
      # aov_stat input represents the anova object summary derived from car library
      results_subtitle <- function(aov_stat, aov_effsize) {
        # extracting the elements of the statistical object
        base::substitute(
          paste(
            "ANOVA: ",
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
            italic("p"),
            italic(eta) ^ 2,
            " = ",
            effsize
          ),
          list(
            estimate = ggstatplot::specify_decimal(aov_stat$`F value`[2], k),
            df1 = aov_stat$`Df`[2],
            # degrees of freedom are always integer
            df2 = aov_stat$`Df`[3],
            # degrees of freedom are always integer
            pvalue = ggstatplot::specify_decimal_p(aov_stat$`Pr(>F)`[2], k),
            effsize = ggstatplot::specify_decimal(aov_effsize[[1]], k)
          )
        )
      }

      # setting up the anova model and getting its summary
      # Note before that setting white.adjust to TRUE will mean that anova will use a heteroscedasticity-corrected
      # coefficient covariance matrix, which is highly recommended. BUT doing so will create problems for
      # sjstats::eta_sq command, which doesn't know how to compute effect size in that case
      y_aov <- stats::aov(formula = y ~ x, data = data)
      y_aov_stat <- car::Anova(mod = y_aov, type = "III", white.adjust = FALSE)
      y_aov_effsize <- sjstats::eta_sq(model = y_aov, partial = TRUE)

      plot <-
        plot + labs(subtitle = results_subtitle(y_aov_stat, y_aov_effsize))

      return(plot)

    } else if (type == "robust") {
      # robust_aov_stat input represents the robust anova object summary derived from WRS2 library
      results_subtitle <- function(robust_aov_stat) {
        # extracting the elements of the statistical object
        base::substitute(
          paste(
            "robust ANOVA: ",
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
            effsize
          ),
          list(
            estimate = ggstatplot::specify_decimal(robust_aov_stat$test, k),
            df1 = robust_aov_stat$df1,
            # degrees of freedom are always integer
            df2 = ggstatplot::specify_decimal(robust_aov_stat$df2, k),
            pvalue = ggstatplot::specify_decimal_p(robust_aov_stat$p.value, k),
            effsize = ggstatplot::specify_decimal(robust_aov_stat$effsize, k)
          )
        )
      }

      # setting up the robust anova model
      robust_y_aov <- WRS2::t1way(formula = y ~ x, data = data, tr = 0.2, nboot = 100)

      plot <- plot + labs(subtitle = results_subtitle(robust_y_aov))

      return(plot)

    }

  }  else if (test == "t-test") {
    # if type of test is not specified, then use the default, which is parametric test
    if (is.null(type))
      type <- "parametric"

    if (type == "parametric") {
      # t_stat input represents the t-test object summary derived from stats library
      results_subtitle <- function(t_stat, t_effsize) {
        # extracting the elements of the statistical object
        base::substitute(
          paste(
            "t-test: ",
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
            effsize
          ),
          list(
            estimate = ggstatplot::specify_decimal(t_stat[[1]], k),
            df = ggstatplot::specify_decimal(t_stat[[2]], k),
            pvalue = ggstatplot::specify_decimal_p(t_stat[[3]], k),
            effsize = ggstatplot::specify_decimal(t_effsize[[3]], k)
          )
        )

      }

      # setting up the anova model and getting its summary and effect size
      y_t_stat <- stats::t.test(formula = y ~ x, data = data, na.action = na.omit)
      # Hedge's g is an unbiased estimate of the effect size
      y_t_effsize <-
        effsize::cohen.d(formula = y ~ x, data = data, hedges.correction = TRUE, na.rm = TRUE)

      plot <-
        plot + labs(subtitle = results_subtitle(y_t_stat, y_t_effsize))

      return(plot)

    } else if (type == "robust") {
      # t_robust_stat input represents the t-test object summary derived from WRS2 library
      results_subtitle <-
        function(t_robust_stat, t_robust_effsize) {
          # extracting the elements of the statistical object
          base::substitute(
            paste(
              "robust t-test: ",
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
              effsize
            ),
            list(
              estimate = ggstatplot::specify_decimal(t_robust_stat$test, k),
              df = ggstatplot::specify_decimal(t_robust_stat$df, k),
              pvalue = ggstatplot::specify_decimal_p(t_robust_stat$p.value, k),
              effsize = ggstatplot::specify_decimal(t_robust_effsize$effsize, k)
            )
          )

        }

      # setting up the robust anova model and getting its summary and effect size
      y_robust_t_stat <- WRS2::yuen(formula = y ~ x, data = data)
      y_robust_t_effsize <-
        WRS2::yuen.effect.ci(formula = y ~ x, data = data)

      plot <-
        plot + labs(subtitle = results_subtitle(y_robust_t_stat, y_robust_t_effsize))

      return(plot)


    }

  }

}
