#'
#' @title violin plots for group or condition comparisons
#' @name ggbetweenstats
#' @aliases ggbetweenstats
#' @description Violin plots for between-subjects designs with statistical
#'   details included in the plot as a subtitle.
#' @author Indrajeet Patil
#'
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param x The grouping variable.
#' @param y The response - a vector of length the number of rows of `x`.
#' @param xlab Label for `x` axis variable.
#' @param ylab Label for `y` axis variable.
#' @param type Type of statistic expected (`"parametric"` or `"nonparametric"`
#'   or `"robust"`).Corresponding abbreviations are also accepted: `"p"` (for
#'   parametric), `"np"` (nonparametric), `"r"` (robust), resp.
#' @param effsize.type Type of effect size needed for *parametric* tests
#'   (`"biased"` (Cohen's *d* for **t-test**; partial eta-squared for **anova**)
#'   or `"unbiased"` (Hedge's *g* for **t-test**; partial omega-squared for
#'   **anova**)).
#' @param title The text for the plot title.
#' @param caption The text for the plot caption.
#' @param k Number of decimal places expected for results.
#' @param var.equal A logical variable indicating whether to treat the two
#'   variances as being equal (Default: `FALSE`).
#' @param nboot Number of bootstrap samples for computing effect size (Default:
#'   `100`).
#' @param notch A logical. If `FALSE` (default), a standard box plot will be
#'   displayed. If `TRUE`, a notched box plot will be used. Notches are used to
#'   compare groups; if the notches of two boxes do not overlap, this suggests
#'   that the medians are significantly different.
#' @param notchwidth For a notched box plot, width of the notch relative to the
#'   body (default `0.5`).
#' @param linetype Character strings (`"blank"`, `"solid"`, `"dashed"`,
#'   `"dotted"`, `"dotdash"`, `"longdash"`, and `"twodash"`) specifiying the
#'   type of line to draw box plots (Default: `"solid"`). Alternatively, the
#'   numbers `0` to `6` can be used (`0` for "blank", `1` for "solid", etc.).
#' @param outlier.color Default aesthetics for outliers.
#' @param outlier.tagging Decides whether outliers should be tagged (Default:
#'   `FALSE`).
#' @param outlier.label Label to put on the outliers that have been tagged.
#' @param outlier.coef Coefficient for outlier detection using Tukey's method.
#'   With Tukey’s method, outliers are below (1st Quartile) or above (3rd
#'   Quartile) `outlier.coef` times the Inter-Quartile Range (IQR) (Default:
#'   `1.5`).
#' @param mean.plotting Decides whether mean is to be highlighted and its value
#'   to be displayed (Default: `TRUE`).
#' @param mean.color Color for the data point corresponding to mean.
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#'
#' @import ggplot2
#' @import ggrepel
#' @import dplyr
#' @import rlang
#'
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @importFrom WRS2 t1way
#' @importFrom WRS2 yuen
#' @importFrom WRS2 yuen.effect.ci
#' @importFrom effsize cohen.d
#' @importFrom sjstats eta_sq
#' @importFrom sjstats omega_sq
#' @importFrom stats aov
#' @importFrom stats na.omit
#' @importFrom stats t.test
#' @importFrom stats var.test
#' @importFrom stats bartlett.test
#' @importFrom stats kruskal.test
#' @importFrom stats aov
#' @importFrom stats quantile
#' @importFrom stats oneway.test
#' @importFrom nortest ad.test
#' @importFrom coin wilcox_test
#' @importFrom coin statistic
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom ggrepel geom_label_repel
#' @importFrom crayon green
#' @importFrom crayon blue
#' @importFrom crayon yellow
#' @importFrom crayon red
#' @importFrom jmv anova
#'
#' @examples
#'
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#'
#' # simple function call with the defaults
#' ggstatsplot::ggbetweenstats(
#' data = datasets::iris,
#' x = Species,
#' y = Sepal.Length
#' )
#'
#' # more detailed function call
#' ggstatsplot::ggbetweenstats(
#' data = datasets::ToothGrowth,
#' x = supp,
#' y = len,
#' xlab = "Supplement type",
#' ylab = "Tooth length",
#' outlier.tagging = TRUE)
#'
#' @export
#'

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
                           nboot = 100,
                           notch = FALSE,
                           notchwidth = 0.5,
                           linetype = "solid",
                           outlier.tagging = NULL,
                           outlier.label = NULL,
                           outlier.color = "black",
                           outlier.coef = 1.5,
                           mean.plotting = TRUE,
                           mean.color = "darkred",
                           messages = TRUE) {
  # if data is not available then don't display any messages
  if (is.null(data)) {
    messages <- FALSE
  }
  ####################################### creating a dataframe #################################################
  # if dataframe is provided
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
  # unused levels of the factor need to be dropped otherwise anova will be run instead of a t-test
  if (is.factor(data$x)) {
    # drop the unused levels of factor
    data %<>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "x",
        .funs = ~ base::droplevels(x = .)
      )
  } else if (!is.factor(data$x)) {
    # convert to factor
    data$x <- base::as.factor(x = data$x)
    # drop the unused levels of factor
    data %<>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "x",
        .funs = ~ base::droplevels(x = .)
      )
    # display message
    base::message(cat(
      crayon::red("Warning: "),
      crayon::blue("aesthetic `x` was not a factor; converting it to factor")
    ))
  }

  ################################################### plot ##############################################################
  # the default is not to tag the outliers
  if (is.null(outlier.tagging)) {
    outlier.tagging <- FALSE
  }

  # create the basic plot
  plot <-
    ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(
      position = ggplot2::position_jitterdodge(
        jitter.width = NULL,
        jitter.height = 0.2,
        dodge.width = 0.75
      ),
      alpha = 0.5,
      size = 3,
      na.rm = TRUE,
      ggplot2::aes(color = factor(x))
    ) +
    ggplot2::geom_violin(
      width = 0.5,
      alpha = 0.2,
      fill = "white",
      na.rm = TRUE
    )

  # adding a boxplot
  if (isTRUE(outlier.tagging)) {
    plot <- plot +
      ggplot2::geom_boxplot(
        notch = notch,
        notchwidth = notchwidth,
        linetype = linetype,
        width = 0.3,
        alpha = 0.2,
        fill = "white",
        outlier.color = outlier.color,
        outlier.shape = 16,
        outlier.size = 3,
        outlier.alpha = 0.7,
        position = ggplot2::position_dodge(width = NULL),
        na.rm = TRUE
      )
  } else {
    plot <- plot +
      ggplot2::geom_boxplot(
        notch = notch,
        notchwidth = notchwidth,
        linetype = linetype,
        width = 0.3,
        alpha = 0.2,
        fill = "white",
        na.rm = TRUE
      )
  }

  # specifying theme and labels for the final plot
  plot <- plot +
    ggstatsplot::theme_mprl() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      caption = caption
    ) +
    ggplot2::coord_cartesian(ylim = c(min(data$y), max(data$y))) +
    ggplot2::scale_y_continuous(limits = c(min(data$y), max(data$y))) +
    ggplot2::scale_fill_brewer(palette = "Dark2") +
    ggplot2::scale_color_brewer(palette = "Dark2")

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
    if (type == "parametric" || type == "p") {
      # Welch's ANOVA run by default
      aov_stat <-
        stats::oneway.test(
          formula = y ~ x,
          data = data,
          subset = NULL,
          na.action = na.omit,
          var.equal = var.equal
        )

      # preparing the subtitles with appropriate effect sizes
      if (effsize.type == "unbiased") {

        # partial omega-squared is the biased estimate of effect size for parametric ANOVA
        aov_effsize_ci <- sjstats::omega_sq(
          model = stats::lm(formula = y ~ x,
                            data = data,
                            na.action = na.omit),
          partial = TRUE,
          ci.lvl = 0.95,
          n = nboot
        )

        # aov_stat input represents the anova object summary derived from car library
        rsubtitle_omega <-
          function(aov_stat, aov_effsize_ci) {
            # extracting the elements of the statistical object
            base::substitute(
              expr =
                paste(
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
                  #italic("p"),
                  #italic(omega) ^ 2,
                  omega ^ 2,
                  " = ",
                  effsize,
                  ", 95% CI [",
                  LL,
                  ", ",
                  UL,
                  "]"
                ),
              env = base::list(
                estimate = ggstatsplot::specify_decimal_p(x = aov_stat$statistic[[1]], k),
                df1 = aov_stat$parameter[[1]],
                # numerator degrees of freedom are always integer
                df2 = ggstatsplot::specify_decimal_p(x = aov_stat$parameter[[2]], k),
                pvalue = ggstatsplot::specify_decimal_p(x = aov_stat$p.value[[1]], k, p.value = TRUE),
                effsize = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$partial.omegasq[[1]], k),
                LL = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$conf.low[[1]], k),
                UL = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$conf.high[[1]], k)
                # effsize = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$output$es[[1]], k),
                # LL = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$output$ci[[1]], k),
                # UL = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$output$ci[[2]], k)
              )
            )
          }

        # adding the subtitle to the plot
        plot <-
          plot +
          ggplot2::labs(subtitle = rsubtitle_omega(aov_stat = aov_stat,
                                                   aov_effsize_ci = aov_effsize_ci))
      } else if (effsize.type == "biased") {
        # partial eta-squared is the biased estimate of effect size for parametric ANOVA
        aov_effsize <-
          jmv::anova(
            data = data,
            dep = "y",
            factors = "x",
            effectSize = c("omega", "partEta")
          )

        # aov_stat2 object is *only* to compute partial eta-squared since there is no straightforward to get partial
        # eta-squared for Welch's ANOVA and its confidence interval
        aov_stat2 <- summary(stats::aov(formula = y ~ x,
                                        data = data))

        # getting confidence interval for partial eta-squared
         aov_effsize_ci <- sjstats::eta_sq(
          model = stats::lm(formula = y ~ x,
                            data = data,
                            na.action = na.omit),
          partial = TRUE,
          ci.lvl = 0.95,
          n = nboot
        )
        # aov_stat input represents the anova object summary derived from car library
        rsubtitle_peta <-
          function(aov_stat,
                   aov_effsize,
                   aov_effsize_ci) {
            # extracting the elements of the statistical object
            base::substitute(
              expr =
                paste(
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
                  #italic("p"),
                  #italic(eta) ^ 2,
                  eta ^ 2,
                  " = ",
                  effsize,
                  ", 95% CI [",
                  LL,
                  ", ",
                  UL,
                  "]"
                ),
              env = base::list(
                estimate = ggstatsplot::specify_decimal_p(x = aov_stat$statistic[[1]], k),
                df1 = aov_stat$parameter[[1]],
                # numerator degrees of freedom are always integer
                df2 = ggstatsplot::specify_decimal_p(x = aov_stat$parameter[[2]], k),
                pvalue = ggstatsplot::specify_decimal_p(x = aov_stat$p.value[[1]], k, p.value = TRUE),
                effsize = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$partial.etasq[[1]], k),
                LL = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$conf.low[[1]], k),
                UL = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$conf.high[[1]], k)
                # effsize = ggstatsplot::specify_decimal_p(x = as.data.frame(aov_effsize$main)$etaSqP[[1]], k),
                # LL = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$LL[[1]], k),
                # UL = ggstatsplot::specify_decimal_p(x = aov_effsize_ci$UL[[1]], k)
              )
            )
          }

        # adding the subtitle to the plot
        plot <-
          plot +
          ggplot2::labs(
            subtitle = rsubtitle_peta(
              aov_stat = aov_stat,
              aov_effsize = aov_effsize,
              aov_effsize_ci = aov_effsize_ci
            )
          )
      }

      # displaying the details of the test that was run
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::green("Reference: "),
          crayon::blue("Welch's ANOVA is used as a default."),
          crayon::yellow("(Delacre, Leys, Mora, & Lakens, PsyArXiv, 2018).")
        ))
      }
    } else if (type == "nonparametric" || type == "np") {
      ############################ Kruskal-Wallis (nonparametric ANOVA) #################################################
      # setting up the anova model and getting its summary
      kw_stat <- stats::kruskal.test(formula = y ~ x,
                                     data = data,
                                     na.action = na.omit)

      # aov_stat input represents the anova object summary derived from car library
      rsubtitle_kw <- function(kw_stat) {
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
            pvalue = ggstatsplot::specify_decimal_p(x = kw_stat$p.value[[1]],
                                                    k,
                                                    p.value = TRUE)
          )
        )
      }

      # adding the subtitle to the plot
      plot <-
        plot +
        ggplot2::labs(subtitle = rsubtitle_kw(kw_stat = kw_stat))

      # letting the user know that this test doesn't have agreed upon effect size
      base::message(cat(
        crayon::red("Note: "),
        crayon::blue("No effect size available for Kruskal-Wallis Rank Sum Test.")
      ))
    } else if (type == "robust" || type == "r") {
      ######################################### robust ANOVA ############################################################

      # robust_aov_stat input represents the robust anova object summary derived from WRS2 library
      rsubtitle_robaov <- function(robust_aov_stat) {
        # extracting the elements of the statistical object
        base::substitute(
          expr =
            paste(
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
            estimate = ggstatsplot::specify_decimal_p(x = robust_aov_stat$test[[1]], k),
            df1 = robust_aov_stat$df1[[1]],
            # degrees of freedom are always integer
            df2 = ggstatsplot::specify_decimal_p(x = robust_aov_stat$df2[[1]], k),
            pvalue = ggstatsplot::specify_decimal_p(x = robust_aov_stat$p.value[[1]],
                                                    k,
                                                    p.value = TRUE),
            effsize = ggstatsplot::specify_decimal_p(x = robust_aov_stat$effsize[[1]], k)
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
        plot +
        ggplot2::labs(subtitle = rsubtitle_robaov(robust_aov_stat = robust_aov_stat))
    }
  } else if (test == "t-test") {
    # if type of test is not specified, then use the default, which is parametric test
    if (is.null(type)) {
      type <- "parametric"
    }

    ##################################### parametric t-test ############################################################

    if (type == "parametric" || type == "p") {
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
        rsubtitle_g <- function(t_stat, t_effsize) {
          # extracting the elements of the statistical object
          base::substitute(
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
                italic("g"),
                " = ",
                effsize,
                ", 95% CI [",
                LL,
                ", ",
                UL,
                "]"
              ),
            env = base::list(
              estimate = ggstatsplot::specify_decimal_p(x = t_stat[[1]], k),
              df = ggstatsplot::specify_decimal_p(x = t_stat[[2]], k),
              pvalue = ggstatsplot::specify_decimal_p(x = t_stat[[3]], k, p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(x = t_effsize[[3]], k),
              LL = ggstatsplot::specify_decimal_p(x = t_effsize$conf.int[[1]], k),
              UL = ggstatsplot::specify_decimal_p(x = t_effsize$conf.int[[2]], k)
            )
          )
        }

        # Hedge's g is an unbiased estimate of the effect size
        t_effsize <-
          effsize::cohen.d(
            formula = y ~ x,
            data = data,
            hedges.correction = TRUE,
            # Hedge's g
            na.rm = TRUE
          )

        # adding subtitle to the plot
        plot <-
          plot +
          ggplot2::labs(subtitle = rsubtitle_g(t_stat = t_stat,
                                               t_effsize = t_effsize))
      } else if (effsize.type == "biased") {
        # t_stat input represents the t-test object summary derived from stats library
        rsubtitle_d <- function(t_stat, t_effsize) {
          # extracting the elements of the statistical object
          base::substitute(
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
                ", 95% CI [",
                LL,
                ", ",
                UL,
                "]"
              ),
            env = base::list(
              estimate = ggstatsplot::specify_decimal_p(x = t_stat[[1]], k),
              df = ggstatsplot::specify_decimal_p(x = t_stat[[2]], k),
              pvalue = ggstatsplot::specify_decimal_p(x = t_stat[[3]], k, p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(x = t_effsize[[3]], k),
              LL = ggstatsplot::specify_decimal_p(x = t_effsize$conf.int[[1]], k),
              UL = ggstatsplot::specify_decimal_p(x = t_effsize$conf.int[[2]], k)
            )
          )
        }

        # Cohen's d is a biased estimate of the effect size
        t_effsize <-
          effsize::cohen.d(
            formula = y ~ x,
            data = data,
            hedges.correction = FALSE,
            # Cohen's d
            na.rm = TRUE
          )

        # adding subtitle to the plot
        plot <-
          plot +
          ggplot2::labs(subtitle = rsubtitle_d(t_stat = t_stat,
                                               t_effsize = t_effsize))
      }

      # displaying the details of the test that was run
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::green("Reference: "),
          crayon::blue("Welch's t-test is used as a default."),
          crayon::yellow(
            "(Delacre, Lakens, & Leys, International Review of Social Psychology, 2017)."
          )
        ))
      }
    }
    else if (type == "nonparametric" || type == "np") {
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
      rsubtitle_mann <- function(mann_stat, z_stat) {
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
              ", ",
              italic(" p"),
              " = ",
              pvalue,
              ", ",
              italic("r"),
              " = ",
              r
            ),
          env = base::list(
            estimate = ggstatsplot::specify_decimal_p(x = mann_stat$statistic[[1]], k),
            z_value = ggstatsplot::specify_decimal_p(x = coin::statistic(z_stat)[[1]], k),
            pvalue = ggstatsplot::specify_decimal_p(x = mann_stat$p.value[[1]], k, p.value = TRUE),
            r = ggstatsplot::specify_decimal_p(x = (
              coin::statistic(z_stat)[[1]] / sqrt(length(data$y))
            ), k) # effect size is r = z/sqrt(n)
          )
        )
      }
      # adding subtitle to the plot
      plot <-
        plot +
        ggplot2::labs(subtitle = rsubtitle_mann(mann_stat = mann_stat,
                                                z_stat = z_stat))
    } else if (type == "robust" || type == "r") {
      ######################################### robust t-test ############################################################

      # t_robust_stat input represents the t-test object summary derived from WRS2 library
      rsubtitle_rob <-
        function(t_robust_stat, t_robust_effsize) {
          # extracting the elements of the statistical object
          base::substitute(
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
                italic(xi),
                " = ",
                effsize,
                ", 95% CI [",
                LL,
                ", ",
                UL,
                "]"
              ),
            env = base::list(
              estimate = ggstatsplot::specify_decimal_p(x = t_robust_stat$test[[1]], k),
              df = ggstatsplot::specify_decimal_p(x = t_robust_stat$df[[1]], k),
              pvalue = ggstatsplot::specify_decimal_p(x = t_robust_stat$p.value[[1]],
                                                      k,
                                                      p.value = TRUE),
              effsize = ggstatsplot::specify_decimal_p(x = t_robust_effsize$effsize[[1]], k),
              LL = ggstatsplot::specify_decimal_p(x = t_robust_effsize$CI[[1]][[1]], k),
              UL = ggstatsplot::specify_decimal_p(x = t_robust_effsize$CI[[2]][[1]], k)
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
        ggplot2::labs(subtitle = rsubtitle_rob(t_robust_stat = t_robust_stat,
                                               t_robust_effsize = t_robust_effsize))
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

    # finding and tagging the outliers
    data_df %<>%
      dplyr::group_by(.data = ., x) %>%
      dplyr::mutate(
        .data = .,
        outlier = base::ifelse(
          test = check_outlier(var = y,
                               coef = outlier.coef),
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

      # convert outlier.label to NAs when outlier is also NA
      data_df %<>%
        dplyr::mutate(
          .data = .,
          outlier = base::ifelse(
            test = !is.na(outlier),
            yes = outlier.label,
            no = NA
          )
        )

      # applying the labels to tagged outliers with ggrepel
      plot <-
        plot +
        ggrepel::geom_label_repel(
          mapping = ggplot2::aes(label = data_df$outlier),
          fontface = "bold",
          color = "black",
          max.iter = 3e2,
          box.padding = 0.35,
          point.padding = 0.5,
          segment.color = "grey50",
          force = 2,
          na.rm = TRUE
        )
    } else {
      # if the value for outliers are to be displated, no need to convert outlier labels to character vector
      # applying the labels to tagged outliers with ggrepel
      plot <-
        plot +
        ggrepel::geom_label_repel(
          mapping = ggplot2::aes(label = data_df$outlier),
          fontface = "bold",
          color = "black",
          max.iter = 3e2,
          box.padding = 0.35,
          point.padding = 0.5,
          segment.color = "grey50",
          force = 2,
          na.rm = TRUE
        )
    }
  }

  ####################################################### mean plotting ################################################
  if (isTRUE(mean.plotting)) {
    # highlight the mean of each group
    plot <- plot +
      ggplot2::stat_summary(
        fun.y = mean,
        geom = "point",
        color = mean.color,
        size = 5,
        na.rm = TRUE
      )

    # use ggrepel to attach text label to each mean
    # create a dataframe with means
    mean_dat <- data %>%
      # in case outlier.label is present, remove it since it's of no utility here
      dplyr::select(.data = ., -dplyr::contains("outlier")) %>%
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
      )

    # attach the labels to the plot
    plot <- plot +
      ggrepel::geom_label_repel(
        data = mean_dat,
        mapping = ggplot2::aes(label = y),
        fontface = "bold",
        color = "black",
        # inherit.aes = FALSE, #would result in "error: geom_label_repel requires the following missing aesthetics: x, y"
        max.iter = 3e2,
        box.padding = 0.35,
        point.padding = 0.5,
        segment.color = "grey50",
        force = 2,
        na.rm = TRUE
      )
  }

  ################################################### messages ############################################################

  if (isTRUE(messages)) {
    # display a note to the user about the validity of assumptions for the default linear model
    # display normality test result as a message
    # for AD test of normality, sample size must be greater than 7
    if (length(data$y) > 7) {
      ad_norm <- nortest::ad.test(x = data$y)
      base::message(cat(
        crayon::green("Note: "),
        crayon::blue(
          "Anderson-Darling Normality Test for",
          crayon::yellow(lab.df[2]),
          # entered y argument
          ": p-value = "
        ),
        crayon::yellow(
          ggstatsplot::specify_decimal_p(x = ad_norm$p.value[[1]],
                                         k,
                                         p.value = TRUE)
        )
      ))
    }
    # homogeneity of variance
    bartlett <- stats::bartlett.test(formula = y ~ x,
                                     data = data)
    # display homogeneity of variances test result as a message
    base::message(cat(
      crayon::green("Note: "),
      crayon::blue(
        "Bartlett's test for homogeneity of variances for factor",
        crayon::yellow(lab.df[1]),
        # entered x argument
        ": p-value = "
      ),
      crayon::yellow(
        ggstatsplot::specify_decimal_p(x = bartlett$p.value[[1]],
                                       k,
                                       p.value = TRUE)
      )
    ))
  }

  # adding colorbind friendly palette to the final plot
  #plot <- cbpalette_add(plot = plot)

  # return the final plot
  return(plot)
}
