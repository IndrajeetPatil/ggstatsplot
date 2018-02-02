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
#' @param test statistical test to be run and displayed as subtitle ("t-test" or "anova")
#' @param type type of statistics expected ("parametric" or "robust")
#' @param effsizetype type of effect size needed for parametric tests ("biased" (Cohen's d, partial eta-squared) or
#' "unbiased" (Hedge's g, omega-squared))
#' @param title title for the plot
#' @param caption caption for the plot
#' @param k number of decimal places expected for results
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal
#' @param nboot number of bootstrap samples
#' @param outlier.tagging whether outliers should be tagged
#' @param outlier.label label to put on the outliers that have been tagged; if data argument is missing
#' @param mean.plotting whether mean is to be highlighted and its value to be displayed
#' this will show y variable values for outliers
#'
#' @export

library(ggplot2)

ggbetweenstats <- function(data = NULL,
                           x,
                           y,
                           test = NULL,
                           type = NULL,
                           effsizetype = NULL,
                           xlab = NULL,
                           ylab = NULL,
                           caption = NULL,
                           title = NULL,
                           k = 3,
                           var.equal = FALSE,
                           nboot = 1000,
                           outlier.tagging = NULL,
                           outlier.label = NULL,
                           outlier.colour = "black",
                           mean.plotting = FALSE) {
  library(ggplot2)
  ####################################### creating a dataframe #######################################################

  # if dataframe is provided
  if (!is.null(data)) {
    # if outlier label is provided then include it in the dataframe
    if (!is.null(outlier.label)) {
      data <-
        dplyr::select(
          .data = data,
          x = !!rlang::enquo(x),
          y = !!rlang::enquo(y),
          outlier.label = !!rlang::enquo(outlier.label)
        )
    } else {
      # if outlier label is not provided then only include the two arguments provided
      data <-
        dplyr::select(
          .data = data,
          x = !!rlang::enquo(x),
          y = !!rlang::enquo(y)
        )
    }
  } else {
    if (!is.null(outlier.label)) {
      # if vectors are provided
      data <-
        base::cbind.data.frame(x = x,
                               y = y,
                               outlier.label = outlier.label)
    } else {
      data <-
        base::cbind.data.frame(x = x,
                               y = y)
    }
  }
  # x needs to be a factor for group or condition comparison
  # it is possible that sometimes the variable hasn't been converted to factor class and this will produce an error
  # if that's the case, convert it to factor
  # (this will be the case only when data has been set to NULL)
  if (!is.factor(data$x)) {
    data$x <- as.factor(data$x)
    base::warning("aesthetic `x` was not a factor; converting it to factor")
  }

  ################################################### plot ##############################################################
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
      outlier.colour = outlier.colour,
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
    if (length(levels(as.factor(data$x))) < 3)
      test <- "t-test"
    else
      test <- "anova"
  }

  if (test == "anova") {
    # if type of test is not specified, then use the default, which is parametric test
    if (is.null(type))
      type <- "parametric"

    ##################################### parametric ANOVA ############################################################

    if (type == "parametric") {
      # setting up the anova model and getting its summary
      # Note before that setting white.adjust to TRUE will mean that anova will use a heteroscedasticity-corrected
      # coefficient covariance matrix, which is highly recommended. BUT doing so will create problems for
      # sjstats::eta_sq command, which doesn't know how to compute effect size in that case
      y_aov <- stats::aov(formula = y ~ x, data = data)
      y_aov_stat <-
        car::Anova(mod = y_aov,
                   type = "III",
                   white.adjust = FALSE)

      # if type of effect size is not specified, use the unbiased estimate as the default
      if (is.null(effsizetype))
        effsizetype <- "unbiased"

      if (effsizetype == "unbiased") {
        # partial omega-squared is the biased estimate of effect size for parametric ANOVA
        y_aov_effsize <-
          sjstats::omega_sq(model = y_aov)
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
              italic(omega) ^ 2,
              " = ",
              effsize
            ),
            list(
              estimate = ggstatsplot::specify_decimal_p(aov_stat$`F value`[2], k),
              df1 = aov_stat$`Df`[2],
              # degrees of freedom are always integer
              df2 = aov_stat$`Df`[3],
              # degrees of freedom are always integer
              pvalue = ggstatsplot::specify_decimal_p(aov_stat$`Pr(>F)`[2], k, p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(aov_effsize[[1]], k)
            )
          )
        }

      } else if (effsizetype == "biased") {
        # partial eta-squared is the biased estimate of effect size for parametric ANOVA
        y_aov_effsize <-
          sjstats::eta_sq(model = y_aov, partial = TRUE)
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
              ", p",
              italic(eta) ^ 2,
              " = ",
              effsize
            ),
            list(
              estimate = ggstatsplot::specify_decimal_p(aov_stat$`F value`[2], k),
              df1 = aov_stat$`Df`[2],
              # degrees of freedom are always integer
              df2 = aov_stat$`Df`[3],
              # degrees of freedom are always integer
              pvalue = ggstatsplot::specify_decimal_p(aov_stat$`Pr(>F)`[2], k, p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(aov_effsize[[1]], k)
            )
          )
        }

      }
      plot <-
        plot + labs(subtitle = results_subtitle(y_aov_stat, y_aov_effsize))
      # display homogeneity of variances test result as a message
      bartlett <- stats::bartlett.test(formula = y ~ x, data = data)
      base::message(
        paste(
          "Note: Bartlett's test for homogeneity of variances: p-value = ",
          ggstatsplot::specify_decimal_p(bartlett$p.value, p.value = TRUE)
        )
      )
      #return(plot)

    } else if (type == "robust") {
      ######################################### robust ANOVA ############################################################

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
            estimate = ggstatsplot::specify_decimal_p(robust_aov_stat$test, k),
            df1 = robust_aov_stat$df1,
            # degrees of freedom are always integer
            df2 = ggstatsplot::specify_decimal_p(robust_aov_stat$df2, 0),
            pvalue = ggstatsplot::specify_decimal_p(robust_aov_stat$p.value, k, p.value = TRUE),
            effsize = ggstatsplot::specify_decimal_p(robust_aov_stat$effsize, k)
          )
        )
      }

      # setting up the Bootstrap version of the heteroscedastic one-way ANOVA for trimmed means
      robust_y_aov <-
        WRS2::t1waybt(
          formula = y ~ x,
          data = data,
          tr = 0.2,
          nboot = nboot
        )

      plot <- plot + labs(subtitle = results_subtitle(robust_y_aov))

      #return(plot)

    }

  }  else if (test == "t-test") {
    # if type of test is not specified, then use the default, which is parametric test
    if (is.null(type))
      type <- "parametric"

    ##################################### parametric t-test ############################################################

    if (type == "parametric") {
      # setting up the anova model and getting its summary and effect size
      y_t_stat <-
        stats::t.test(
          formula = y ~ x,
          data = data,
          var.equal = var.equal,
          na.action = na.omit
        )

      # if type of effect size is not specified, use the unbiased estimate as the default
      if (is.null(effsizetype))
        effsizetype <- "unbiased"

      if (effsizetype == "unbiased") {
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
              estimate = ggstatsplot::specify_decimal_p(t_stat[[1]], k),
              df = ggstatsplot::specify_decimal_p(t_stat[[2]], k),
              pvalue = ggstatsplot::specify_decimal_p(t_stat[[3]], k, p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(t_effsize[[3]], k)
            )
          )

        }
        # Hedge's g is an unbiased estimate of the effect size
        y_t_effsize <-
          effsize::cohen.d(
            formula = y ~ x,
            data = data,
            hedges.correction = TRUE,
            na.rm = TRUE
          )
      } else if (effsizetype == "biased") {
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
              italic("d"),
              " = ",
              effsize
            ),
            list(
              estimate = ggstatsplot::specify_decimal_p(t_stat[[1]], k),
              df = ggstatsplot::specify_decimal_p(t_stat[[2]], k),
              pvalue = ggstatsplot::specify_decimal_p(t_stat[[3]], k, p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(t_effsize[[3]], k)
            )
          )

        }
        # Cohen's d is a biased estimate of the effect size
        y_t_effsize <-
          effsize::cohen.d(
            formula = y ~ x,
            data = data,
            hedges.correction = FALSE,
            na.rm = TRUE
          )
      }

      plot <-
        plot + labs(subtitle = results_subtitle(y_t_stat, y_t_effsize))
      # display equality of variance result as a message
      vartest <- stats::var.test(x = as.numeric(data$x), y = data$y)
      base::message(
        paste(
          "Note: F test to compare two variances: p-value = ",
          ggstatsplot::specify_decimal_p(vartest$p.value, p.value = TRUE)
        )
      )

      #return(plot)

    } else if (type == "robust") {
      ######################################### robust t-test ############################################################

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
              estimate = ggstatsplot::specify_decimal_p(t_robust_stat$test, k),
              df = ggstatsplot::specify_decimal_p(t_robust_stat$df, k),
              pvalue = ggstatsplot::specify_decimal_p(t_robust_stat$p.value, k, p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(t_robust_effsize$effsize, k)
            )
          )

        }

      # setting up the independent samples t-tests on robust location measures including effect sizes
      y_robust_t_stat <-
        WRS2::yuenbt(formula = y ~ x,
                     data = data,
                     nboot = nboot)
      y_robust_t_effsize <-
        WRS2::yuen.effect.ci(formula = y ~ x,
                             data = data,
                             nboot = nboot)

      plot <-
        plot + labs(subtitle = results_subtitle(y_robust_t_stat, y_robust_t_effsize))

      #return(plot)


    }

  }

  ########################################### outlier tagging #########################################################
  # the default is not to tag the outliers
  if (is.null(outlier.tagging))
    outlier.tagging <- FALSE

  # if outlier.tagging is set to TRUE, first figure out what labels need to be attached to the outlier
  if (isTRUE(outlier.tagging)) {
    ## getting the data in dataframe format
    if (is.null(outlier.label)) {
      # if data is missing, then make a dataframe out of x and y vectors
      # if outlier label is not provided, outlier labels will just be values of the y vector
      data_df <-
        base::cbind.data.frame(x = data$x,
                               y = data$y,
                               outlier.label = data$y)
    } else {
      # if the outlier tag has been provided, just use the dataframe already created
      data_df <- data
    }
    ## finding the outliers in the dataframe
    # function to detect outliers
    check_outlier <- function(v, coef = 1.5) {
      quantiles <- stats::quantile(x = v, probs = c(0.25, 0.75))
      IQR <- quantiles[2] - quantiles[1]
      res <-
        ((v < (quantiles[1] - coef * IQR)) |
           (v > (quantiles[2] + coef * IQR)))
      return(res)

    }
    # finding and tagging the outliers
    library(dplyr)
    data_df <- data_df %>%
      dplyr::group_by(x) %>%
      dplyr::mutate(outlier = ifelse(check_outlier(y), outlier.label, NA))
    data_df$outlier[which(is.na(data_df$outlier))] <- as.numeric(NA)
    data_df <- base::as.data.frame(data_df)
    # applying the labels to tagged outliers with ggrepel
    plot <-
      plot + ggrepel::geom_label_repel(
        aes(label = data_df$outlier),
        fontface = 'bold',
        color = 'black',
        max.iter = 3e2,
        box.padding = 0.35,
        point.padding = 0.5,
        segment.color = 'grey50',
        force = 2
      )
  }

  ####################################################### mean plotting ################################################
  if (isTRUE(mean.plotting)) {
    # custom function to get the mean
    fun_mean <- function(x) {
      return(data.frame(
        y = as.numeric(as.character(
          ggstatsplot::specify_decimal_p(mean(x))
        )),
        label = as.numeric(as.character(
          ggstatsplot::specify_decimal_p(mean(x, na.rm = TRUE))
        ))
      ))
    }
    plot <- plot +
      stat_summary(
        fun.y = mean,
        geom = "point",
        colour = "darkred",
        size = 5
      ) +
      stat_summary(
        fun.data = fun_mean,
        geom = "text",
        vjust = -1.0,
        size = 5
      )
  }

  return(plot)

}
