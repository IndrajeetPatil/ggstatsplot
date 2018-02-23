#'
#' @title violin plots for group or condition comparisons
#' @name ggbetweenstats
#' @aliases ggbetweenstats
#' @description Violin plots for between-subjects designs with statistical details included in the plot as a subtitle
#' @author Indrajeet Patil
#'
#' @param data data frame from which variables specified are preferentially to be taken
#' @param x the grouping variable
#' @param y the response - a vector of length the number of rows of `x`
#' @param xlab label for x axis variable
#' @param ylab label for y axis variable
#' @param type type of statistics expected ("parametric" or "nonparametric" or "robust")
#' @param effsize.type type of effect size needed for *parametric* tests ("biased" (Cohen's d, partial eta-squared) or
#' "unbiased" (Hedge's g, omega-squared))
#' @param title title for the plot
#' @param caption caption for the plot
#' @param k number of decimal places expected for results
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal
#' @param nboot number of bootstrap samples
#' @param outlier.color default aesthetics for outliers
#' @param outlier.tagging whether outliers should be tagged
#' @param outlier.label label to put on the outliers that have been tagged; if data argument is missing
#' @param mean.plotting whether mean is to be highlighted and its value to be displayed
#' this will show y variable values for outliers
#' @param mean.color color for the data point corresponding to mean
#'
#' @import ggplot2
#' @import ggrepel
#' @import dplyr
#' @import rlang
#'
#' @importFrom WRS2 t1way
#' @importFrom WRS2 yuen
#' @importFrom WRS2 yuen.effect.ci
#' @importFrom effsize cohen.d
#' @importFrom sjstats omega_sq
#' @importFrom sjstats eta_sq
#' @importFrom stats na.omit
#' @importFrom stats t.test
#' @importFrom stats var.test
#' @importFrom stats bartlett.test
#' @importFrom stats kruskal.test
#' @importFrom stats aov
#' @importFrom stats quantile
#' @importFrom coin wilcox_test
#' @importFrom coin statistic
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom car Anova
#' @importFrom ggrepel geom_label_repel
#'
#' @examples
#' # the most basic and minimalistic way of entering arguments
#' library(datasets)
#' ggstatsplot::ggbetweenstats(data = iris, x = Species, y = Sepal.Length)
#' # or
#' ggstatsplot::ggbetweenstats(x = iris$Species, y = iris$Sepal.Length)
#'
#' # more detailed function call
#' ggstatsplot::ggbetweenstats(data = mtcars, x = cyl, y = mpg,
#' outlier.tagging = TRUE, outlier.label = disp, mean.plotting = TRUE)
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
    "phicoeff",
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
    "proptest_subtitle"
  )
)

# defining the function
ggbetweenstats <- function(data = NULL,
                           x,
                           y,
                           type = "parametric",
                           effsize.type = "unbiased",
                           xlab = NULL,
                           ylab = NULL,
                           caption = NULL,
                           title = NULL,
                           k = 3,
                           var.equal = FALSE,
                           nboot = 1000,
                           outlier.tagging = NULL,
                           outlier.label = NULL,
                           outlier.color = "black",
                           mean.plotting = FALSE,
                           mean.color = "darkred") {
  ####################################### creating a dataframe #################################################
  # if dataframe is provided
  if (!is.null(data)) {
    # preparing labels from given dataframe
    lab.df <- colnames(dplyr::select(.data = data,
                                     !!rlang::enquo(x),
                                     !!rlang::enquo(y)))
    # if xlab is not provided, use the variable x name
    if (is.null(xlab))
      xlab <- lab.df[1]
    # if ylab is not provided, use the variable y name
    if (is.null(ylab))
      ylab <- lab.df[2]
    # if outlier label is provided then include it in the dataframe
    if (base::missing(outlier.label)) {
      # if outlier label is not provided then only include the two arguments provided
      data <-
        dplyr::select(
          .data = data,
          x = !!rlang::enquo(x),
          y = !!rlang::enquo(y)
        )
    } else {
      # if outlier label is provided then include it to make a dataframe
      data <-
        dplyr::select(
          .data = data,
          x = !!rlang::enquo(x),
          y = !!rlang::enquo(y),
          outlier.label = !!rlang::quo_name(rlang::enquo(outlier.label))
        )

    }
  } else {
    if (!is.null(outlier.label)) {
      # if vectors are provided and outlier label vector is present
      data <-
        base::cbind.data.frame(x = x,
                               y = y,
                               outlier.label = outlier.label)
    } else {
      # if outlier label vector is absent
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
  # the default is not to tag the outliers
  if (is.null(outlier.tagging))
    outlier.tagging <- FALSE

  # create the basic plot
  plot <-
    ggplot2::ggplot(data = data, mapping = aes(x = x, y = y)) +
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
                fill = "white")

  # adding a boxplot
  if (isTRUE(outlier.tagging)) {
    plot <- plot +
      geom_boxplot(
        width = 0.3,
        alpha = 0.2,
        fill = "white",
        outlier.color = outlier.color,
        outlier.shape = 16,
        outlier.size = 3,
        outlier.alpha = 0.7,
        position = position_dodge(width = NULL)
      )
  } else {
    plot <- plot +
      geom_boxplot(width = 0.3,
                   alpha = 0.2,
                   fill = "white")
  }

  # specifying theme and labels for the plot
  plot <- plot + ggstatsplot::theme_mprl() +
    theme(legend.position = "none") +
    labs(
      x = xlab,
      y = ylab,
      title = title,
      caption = caption
    ) +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2")

  ################################################  preparing stats subtitles #########################################

  # figure out which test to run based on the number of levels of the independent variables
  if (length(levels(as.factor(data$x))) < 3) {
    test <- "t-test"
  } else {
    test <- "anova"
  }

  # running anova
  if (test == "anova") {
    ##################################### parametric ANOVA ############################################################

    # running parametric ANOVA
    if (type == "parametric") {
      # setting up the anova model and getting its summary
      # Note before that setting white.adjust to TRUE will mean that anova will use a heteroscedasticity-corrected
      # coefficient covariance matrix, which is highly recommended. BUT doing so will create problems for
      # sjstats::eta_sq command, which doesn't know how to compute effect size in that case
      # getting model summary
      aov_stat <-
        car::Anova(mod = stats::aov(formula = y ~ x,
                                    data = data),
                   type = "III",
                   white.adjust = FALSE)

      # preparing the subtitles with appropriate effect sizes
      if (effsize.type == "unbiased") {
        # partial omega-squared is the biased estimate of effect size for parametric ANOVA
        aov_effsize <-
          sjstats::omega_sq(model = stats::aov(formula = y ~ x,
                                               data = data))
        # aov_stat input represents the anova object summary derived from car library
        rsubtitle <- function(aov_stat, aov_effsize) {
          # extracting the elements of the statistical object
          base::substitute(
            expr =
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
            env = base::list(
              estimate = ggstatsplot::specify_decimal_p(x = aov_stat$`F value`[2], k),
              df1 = aov_stat$`Df`[2],
              # degrees of freedom are always integer
              df2 = aov_stat$`Df`[3],
              # degrees of freedom are always integer
              pvalue = ggstatsplot::specify_decimal_p(x = aov_stat$`Pr(>F)`[2], k, p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(x = aov_effsize[[1]], k)
            )
          )
        }

      } else if (effsize.type == "biased") {
        # partial eta-squared is the biased estimate of effect size for parametric ANOVA
        aov_effsize <-
          sjstats::eta_sq(model = stats::aov(formula = y ~ x,
                                             data = data),
                          partial = TRUE)
        # aov_stat input represents the anova object summary derived from car library
        rsubtitle <- function(aov_stat, aov_effsize) {
          # extracting the elements of the statistical object
          base::substitute(
            expr =
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
            env = base::list(
              estimate = ggstatsplot::specify_decimal_p(x = aov_stat$`F value`[2], k),
              df1 = aov_stat$`Df`[2],
              # degrees of freedom are always integer
              df2 = aov_stat$`Df`[3],
              # degrees of freedom are always integer
              pvalue = ggstatsplot::specify_decimal_p(x = aov_stat$`Pr(>F)`[2], k, p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(x = aov_effsize[[1]], k)
            )
          )
        }

      }
      # adding the subtitle to the plot
      plot <-
        plot +
        labs(subtitle = rsubtitle(aov_stat = aov_stat,
                                                aov_effsize = aov_effsize))


    } else if (type == "nonparametric") {
      ############################ Kruskal-Wallis (nonparametric ANOVA) #################################################
      # setting up the anova model and getting its summary
      kw_stat <- stats::kruskal.test(formula = y ~ x,
                                       data = data,
                                       na.action = na.omit)
      # aov_stat input represents the anova object summary derived from car library
      rsubtitle <- function(kw_stat) {
        # extracting the elements of the statistical object
        base::substitute(
          expr =
            paste(
              "Kruskal-Wallis: ",
              italic(chi) ^ 2,
              "(",
              df,
              ") = ",
              estimate,
              ", ",
              italic("p"),
              " = ",
              pvalue
            ),
          env = base::list(
            estimate = ggstatsplot::specify_decimal_p(x = kw_stat$statistic[[1]], k),
            df = kw_stat$parameter[[1]],
            # degrees of freedom are always integer
            pvalue = ggstatsplot::specify_decimal_p(x = kw_stat$p.value[[1]], k, p.value = TRUE)
          )
        )
      }

      # adding the subtitle to the plot
      plot <-
        plot +
        labs(subtitle = rsubtitle(kw_stat = kw_stat))

      # letting the user know that this test doesn't have agreed upon effect size
      base::message(paste(
        "Note: No effect size available for Kruskal-Wallis Rank Sum Test."
      ))
    } else if (type == "robust") {
      ######################################### robust ANOVA ############################################################

      # robust_aov_stat input represents the robust anova object summary derived from WRS2 library
      rsubtitle <- function(robust_aov_stat) {
        # extracting the elements of the statistical object
        base::substitute(
          expr =
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
          env = base::list(
            estimate = ggstatsplot::specify_decimal_p(x = robust_aov_stat$test, k),
            df1 = robust_aov_stat$df1,
            # degrees of freedom are always integer
            df2 = ggstatsplot::specify_decimal_p(x = robust_aov_stat$df2, k),
            pvalue = ggstatsplot::specify_decimal_p(x = robust_aov_stat$p.value, k, p.value = TRUE),
            effsize = ggstatsplot::specify_decimal_p(x = robust_aov_stat$effsize, k)
          )
        )
      }

      # setting up the Bootstrap version of the heteroscedastic one-way ANOVA for trimmed means
      robust_aov_stat <-
        WRS2::t1way(
          formula = y ~ x,
          data = data,
          tr = 0.2,
          nboot = nboot
        )

      # adding the label to the plot
      plot <-
        plot + labs(subtitle = rsubtitle(robust_aov_stat = robust_aov_stat))

    }

  }  else if (test == "t-test") {
    # if type of test is not specified, then use the default, which is parametric test
    if (is.null(type))
      type <- "parametric"

    ##################################### parametric t-test ############################################################

    if (type == "parametric")  {
      # setting up the anova model and getting its summary and effect size
      t_stat <-
        stats::t.test(
          formula = y ~ x,
          data = data,
          var.equal = var.equal,
          na.action = na.omit
        )

      if (effsize.type == "unbiased") {
        # t_stat input represents the t-test object summary derived from stats library
        rsubtitle <- function(t_stat, t_effsize) {
          # extracting the elements of the statistical object
          base::substitute(
            expr =
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
            env = base::list(
              estimate = ggstatsplot::specify_decimal_p(x = t_stat[[1]], k),
              df = ggstatsplot::specify_decimal_p(x = t_stat[[2]], k),
              pvalue = ggstatsplot::specify_decimal_p(x = t_stat[[3]], k, p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(x = abs(t_effsize[[3]]), k)
            )
          )

        }
        # Hedge's g is an unbiased estimate of the effect size
        t_effsize <-
          effsize::cohen.d(
            formula = y ~ x,
            data = data,
            hedges.correction = TRUE,
            na.rm = TRUE
          )
      } else if (effsize.type == "biased") {
        # t_stat input represents the t-test object summary derived from stats library
        rsubtitle <- function(t_stat, t_effsize) {
          # extracting the elements of the statistical object
          base::substitute(
            expr =
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
            env = base::list(
              estimate = ggstatsplot::specify_decimal_p(x = t_stat[[1]], k),
              df = ggstatsplot::specify_decimal_p(x = t_stat[[2]], k),
              pvalue = ggstatsplot::specify_decimal_p(x = t_stat[[3]], k, p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(x = abs(t_effsize[[3]]), k)
            )
          )

        }
        # Cohen's d is a biased estimate of the effect size
        t_effsize <-
          effsize::cohen.d(
            formula = y ~ x,
            data = data,
            hedges.correction = FALSE,
            na.rm = TRUE
          )
      }

      # adding subtitle to the plot
      plot <-
        plot +
        labs(subtitle = rsubtitle(t_stat = t_stat,
                                         t_effsize = t_effsize))

    }
    else if (type == "nonparametric") {
      ######################################### Mann-Whitney U test ######################################################
      # setting up the Mann-Whitney U-test and getting its summary
      mann_stat <- stats::wilcox.test(
        formula = y ~ x,
        data = data,
        paired = FALSE,
        alternative = "two.sided",
        na.action = na.omit,
        exact = FALSE,
        # asymptotic
        correct = TRUE,
        conf.int = TRUE,
        conf.level = 0.95
      )
      # computing Z score
      z_stat <- coin::wilcox_test(
        formula = y ~ x,
        data = data,
        distribution = "asymptotic",
        alternative = "two.sided",
        conf.int = TRUE
      )
      # mann_stat input represents the U-test summary derived from stats library, while Z is
      # from Exact Wilcoxon-Pratt Signed-Rank Test from coin library
      rsubtitle <- function(mann_stat, z_stat) {
        # extracting the elements of the statistical object
        base::substitute(
          expr =
            paste(
              "Mann-Whitney: ",
              italic(U),
              " = ",
              estimate,
              ", ",
              italic(Z),
              " = ",
              z_value,
              italic(" p"),
              " = ",
              pvalue,
              italic(", r"),
              " = ",
              r
            ),
          env = base::list(
            estimate = ggstatsplot::specify_decimal_p(x = mann_stat$statistic[[1]], k),
            z_value = ggstatsplot::specify_decimal_p(x = coin::statistic(z_stat)[[1]], k),
            pvalue = ggstatsplot::specify_decimal_p(x = mann_stat$p.value[[1]], k, p.value = TRUE),
            r = ggstatsplot::specify_decimal_p(x = (
              coin::statistic(z_stat)[[1]] / length(data$y)
            ), k) # effect size is r = z/sqrt(n)
          )
        )
      }
      # adding subtitle to the plot
      plot <-
        plot +
        labs(subtitle = rsubtitle(mann_stat = mann_stat,
                                         z_stat = z_stat))
    } else if (type == "robust") {
      ######################################### robust t-test ############################################################

      # t_robust_stat input represents the t-test object summary derived from WRS2 library
      rsubtitle <-
        function(t_robust_stat, t_robust_effsize) {
          # extracting the elements of the statistical object
          base::substitute(
            expr =
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
            env = base::list(
              estimate = ggstatsplot::specify_decimal_p(x = t_robust_stat$test, k),
              df = ggstatsplot::specify_decimal_p(x = t_robust_stat$df, k),
              pvalue = ggstatsplot::specify_decimal_p(x = t_robust_stat$p.value, k, p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(x = t_robust_effsize$effsize, k)
            )
          )

        }

      # setting up the independent samples t-tests on robust location measures (without bootstraps)
      t_robust_stat <-
        WRS2::yuen(formula = y ~ x,
                   data = data)
      # computing effect sizes
      t_robust_effsize <-
        WRS2::yuen.effect.ci(formula = y ~ x,
                             data = data)

      # adding the label to the plot
      plot <-
        plot +
        labs(
          subtitle = rsubtitle(t_robust_stat = t_robust_stat,
                                      t_robust_effsize = t_robust_effsize)
        )

    }

  }

  ########################################### outlier tagging #########################################################


  # if outlier.tagging is set to TRUE, first figure out what labels need to be attached to the outlier
  if (isTRUE(outlier.tagging)) {
    ## getting the data in dataframe format
    if (missing(outlier.label)) {
      # if outlier label is not provided, outlier labels will just be values of the y vector
      data_df <-
        base::cbind.data.frame(x = data$x,
                               y = data$y,
                               outlier.label = data$y)
    } else {
      # if the outlier tag has been provided, just use the dataframe already created
      data_df <-
        base::cbind.data.frame(
          x = data$x,
          y = data$y,
          outlier.label = data$outlier.label
        )
    }
    ## finding the outliers in the dataframe using Tukey's interquartile range rule

    # defining function to detect outliers
    check_outlier <- function(v, coef = 1.5) {
      # compute the quantiles
      quantiles <- stats::quantile(x = v,
                                   probs = c(0.25, 0.75))
      # compute the interquartile range
      IQR <- quantiles[2] - quantiles[1]
      # check for outlier and output a logical
      res <-
        ((v < (quantiles[1] - coef * IQR)) |
           (v > (quantiles[2] + coef * IQR)))
      # return the result
      return(res)
    }

    # finding and tagging the outliers
    data_df <- data_df %>%
      dplyr::group_by(.data = ., x) %>%
      dplyr::mutate(
        .data = .,
        outlier = base::ifelse(
          test = check_outlier(y),
          yes = outlier.label,
          no = NA
        )
      )

    # converting outlier column to a numeric value that can be attached to
    data_df$outlier[which(is.na(data_df$outlier))] <- NA
    # if outlier.label is in character format, convert it to factor
    if (is.character(data_df$outlier.label)) {
      data_df$outlier.label <- as.factor(data_df$outlier.label)
    }
    # if outlier labels are words or other types of characters, you want these characters to be diaplyed and not the values
    if (is.factor(data_df$outlier.label)) {
      data_df$outlier.label <- as.character(data_df$outlier.label)
      # applying the labels to tagged outliers with ggrepel
      plot <-
        plot +
        ggrepel::geom_label_repel(
          mapping = aes(label = data_df$outlier.label),
          fontface = 'bold',
          color = 'black',
          max.iter = 3e2,
          box.padding = 0.35,
          point.padding = 0.5,
          segment.color = 'grey50',
          force = 2
        )
    } else {
      # if the value for outliers are to be displated, no need to convert outlier labels to character vector
      # applying the labels to tagged outliers with ggrepel
      plot <-
        plot +
        ggrepel::geom_label_repel(
          mapping = aes(label = data_df$outlier),
          fontface = 'bold',
          color = 'black',
          max.iter = 3e2,
          box.padding = 0.35,
          point.padding = 0.5,
          segment.color = 'grey50',
          force = 2
        )
    }

  }

  ####################################################### mean plotting ################################################
  if (isTRUE(mean.plotting)) {
    # highlight the mean of each group
    plot <- plot +
      stat_summary(
        fun.y = mean,
        geom = "point",
        color = mean.color,
        size = 5
      )

    # use ggrepel to attach text label to each mean
    # create a dataframe with means
    mean_dat <- data %>%
      dplyr::group_by(.data = ., x) %>% # group by the independent variable
      dplyr::mutate_all(.tbl = .,
                        .funs = mean,
                        na.rm = TRUE) %>% # dependent variable mean for each level of grouping variable
      dplyr::distinct(.data = .) %>% # removed duplicated rows
      dplyr::mutate_if(
        .tbl = .,
        .predicate = is.numeric,
        .funs = ~ as.numeric(as.character(
          ggstatsplot::specify_decimal_p(x = ., k = k)
        )) # format the values for printing
      ) %>%
      dplyr::select(.data = ., -contains('outlier'))
    # in case outlier.label is present, remove it since it's of no utility here

    # attach the labels to the plot
    plot <- plot +
      ggrepel::geom_label_repel(
        data = mean_dat,
        mapping = aes(label = y),
        fontface = 'bold',
        color = 'black',
        #inherit.aes = FALSE, #would result in "error: geom_label_repel requires the following missing aesthetics: x, y"
        max.iter = 3e2,
        box.padding = 0.35,
        point.padding = 0.5,
        segment.color = 'grey50',
        force = 2
      )
  }

  # display homogeneity of variances test result as a message
  bartlett <- stats::bartlett.test(formula = y ~ x,
                                   data = data)
  base::message(
    paste(
      "Note: Bartlett's test for homogeneity of variances: p-value = ",
      ggstatsplot::specify_decimal_p(x = bartlett$p.value,
                                     k,
                                     p.value = TRUE)
    )
  )

  # return the final plot
  return(plot)

}
