#' @title Box/Violin plots for between-subjects comparisons
#' @name ggbetweenstats
#'
#' @description
#'
#' A combination of box and violin plots along with jittered data points for
#' between-subjects designs with statistical details included in the plot as a
#' subtitle.
#'
#' @section Summary of graphics:
#'
#' ```{r child="man/rmd-fragments/ggbetweenstats_graphics.Rmd"}
#' ```
#'
#' @param xlab Label for `x` axis variable. If `NULL` (default),
#'   variable name for `x` will be used.
#' @param ylab Labels for `y` axis variable. If `NULL` (default),
#'   variable name for `y` will be used.
#' @param p.adjust.method Adjustment method for *p*-values for multiple
#'   comparisons. Possible methods are: `"holm"` (default), `"hochberg"`,
#'   `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.
#' @param pairwise.display Decides *which* pairwise comparisons to display.
#'   Available options are:
#'   - `"significant"` (abbreviation accepted: `"s"`)
#'   - `"non-significant"` (abbreviation accepted: `"ns"`)
#'   - `"all"`
#'
#'   You can use this argument to make sure that your plot is not uber-cluttered
#'   when you have multiple groups being compared and scores of pairwise
#'   comparisons being displayed. If set to `"none"`, no pairwise comparisons
#'   will be displayed.
#' @param bf.message Logical that decides whether to display Bayes Factor in
#'   favor of the *null* hypothesis. This argument is relevant only **for
#'   parametric test** (Default: `TRUE`).
#' @param results.subtitle Decides whether the results of statistical tests are
#'   to be displayed as a subtitle (Default: `TRUE`). If set to `FALSE`, only
#'   the plot will be returned.
#' @param title The text for the plot title.
#' @param subtitle The text for the plot subtitle. Will work only if
#'   `results.subtitle = FALSE`.
#' @param caption The text for the plot caption. This argument is relevant only
#'   if `bf.message = FALSE`.
#' @param centrality.plotting Logical that decides whether centrality tendency
#'   measure is to be displayed as a point with a label (Default: `TRUE`).
#'   Function decides which central tendency measure to show depending on the
#'   `type` argument.
#'   - **mean** for parametric statistics
#'   - **median** for non-parametric statistics
#'   - **trimmed mean** for robust statistics
#'   - **MAP estimator** for Bayesian statistics
#'
#'   If you want default centrality parameter, you can specify this using
#'   `centrality.type` argument.
#' @param centrality.type Decides which centrality parameter is to be displayed.
#'   The default is to choose the same as `type` argument. You can specify this
#'   to be:
#'   - `"parameteric"` (for **mean**)
#'   - `"nonparametric"` (for **median**)
#'   - `robust` (for **trimmed mean**)
#'   - `bayes` (for **MAP estimator**)
#'
#'   Just as `type` argument, abbreviations are also accepted.
#' @param point.args A list of additional aesthetic arguments to be passed to
#'   the `ggplot2::geom_point()` displaying the raw data.
#' @param boxplot.args A list of additional aesthetic arguments passed on to
#'   `ggplot2::geom_boxplot()`.
#' @param violin.args A list of additional aesthetic arguments to be passed to
#'   the `ggplot2::geom_violin()`.
#' @param ggplot.component A `ggplot` component to be added to the plot prepared
#'   by `{ggstatsplot}`. This argument is primarily helpful for `grouped_`
#'   variants of all primary functions. Default is `NULL`. The argument should
#'   be entered as a `{ggplot2}` function or a list of `{ggplot2}` functions.
#' @param package,palette Name of the package from which the given palette is to
#'   be extracted. The available palettes and packages can be checked by running
#'   `View(paletteer::palettes_d_names)`.
#' @param ... Currently ignored.
#' @inheritParams theme_ggstatsplot
#' @param centrality.point.args,centrality.label.args A list of additional aesthetic
#'   arguments to be passed to `ggplot2::geom_point()` and
#'   `ggrepel::geom_label_repel` geoms, which are involved in mean plotting.
#' @param  ggsignif.args A list of additional aesthetic
#'   arguments to be passed to `ggsignif::geom_signif`.
#' @param ggtheme A `{ggplot2}` theme. Default value is
#'   `ggstatsplot::theme_ggstatsplot()`. Any of the `{ggplot2}` themes (e.g.,
#'   `theme_bw()`), or themes from extension packages are allowed (e.g.,
#'   `ggthemes::theme_fivethirtyeight()`, `hrbrthemes::theme_ipsum_ps()`, etc.).
#'   But note that sometimes these themes will remove some of the details that
#'   `{ggstatsplot}` plots typically contains. For example, if relevant,
#'   `ggbetweenstats()` shows details about multiple comparison test as a label
#'   on the secondary Y-axis. Some themes (e.g.
#'   `ggthemes::theme_fivethirtyeight()`) will remove the secondary Y-axis and
#'   thus the details as well.
#' @inheritParams statsExpressions::oneway_anova
#' @inheritParams statsExpressions::two_sample_test
#'
#' @inheritSection statsExpressions::centrality_description Centrality measures
#' @inheritSection statsExpressions::two_sample_test Two-sample tests
#' @inheritSection statsExpressions::oneway_anova One-way ANOVA
#' @inheritSection statsExpressions::pairwise_comparisons Pairwise comparison tests
#'
#' @seealso \code{\link{grouped_ggbetweenstats}}, \code{\link{ggwithinstats}},
#'  \code{\link{grouped_ggwithinstats}}
#'
#' @autoglobal
#'
#' @details For details, see:
#' <https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggbetweenstats.html>
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("PMCMRplus", quietly = TRUE)
#' # for reproducibility
#' set.seed(123)
#' library(PMCMRplus) # for pairwise comparisons
#'
#' # create a plot
#' p <- ggbetweenstats(mtcars, am, mpg)
#'
#' # looking at the plot
#' p
#'
#' # extracting details from statistical tests
#' extract_stats(p)
#'
#' # modifying defaults
#' ggbetweenstats(
#'   morley,
#'   x    = Expt,
#'   y    = Speed,
#'   type = "robust",
#'   xlab = "The experiment number",
#'   ylab = "Speed-of-light measurement"
#' )
#'
#' # you can remove a specific geom to reduce complexity of the plot
#' ggbetweenstats(
#'   mtcars, am, wt,
#'   # to remove violin plot
#'   violin.args = list(width = 0),
#'   # to remove boxplot
#'   boxplot.args = list(width = 0),
#'   # to remove points
#'   point.args = list(alpha = 0)
#' )
#' @export
ggbetweenstats <- function(data,
                           x,
                           y,
                           type = "parametric",
                           pairwise.display = "significant",
                           p.adjust.method = "holm",
                           effsize.type = "unbiased",
                           bf.prior = 0.707,
                           bf.message = TRUE,
                           results.subtitle = TRUE,
                           xlab = NULL,
                           ylab = NULL,
                           caption = NULL,
                           title = NULL,
                           subtitle = NULL,
                           k = 2L,
                           var.equal = FALSE,
                           conf.level = 0.95,
                           nboot = 100L,
                           tr = 0.2,
                           centrality.plotting = TRUE,
                           centrality.type = type,
                           centrality.point.args = list(size = 5, color = "darkred"),
                           centrality.label.args = list(
                             size = 3,
                             nudge_x = 0.4,
                             segment.linetype = 4,
                             min.segment.length = 0
                           ),
                           point.args = list(
                             position = ggplot2::position_jitterdodge(dodge.width = 0.60),
                             alpha = 0.4,
                             size = 3,
                             stroke = 0,
                             na.rm = TRUE
                           ),
                           boxplot.args = list(width = 0.3, alpha = 0.2, na.rm = TRUE),
                           violin.args = list(width = 0.5, alpha = 0.2, na.rm = TRUE),
                           ggsignif.args = list(textsize = 3, tip_length = 0.01, na.rm = TRUE),
                           ggtheme = ggstatsplot::theme_ggstatsplot(),
                           package = "RColorBrewer",
                           palette = "Dark2",
                           ggplot.component = NULL,
                           ...) {
  # data -----------------------------------


  type <- stats_type_switch(type)

  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(ensym(x), ensym(y))

  data %<>%
    select({{ x }}, {{ y }}) %>%
    tidyr::drop_na() %>%
    mutate({{ x }} := droplevels(as.factor({{ x }})))

  # statistical analysis ------------------------------------------

  # test to run; depends on the no. of levels of the independent variable
  test <- ifelse(nlevels(data %>% pull({{ x }})) < 3L, "t", "anova")

  if (results.subtitle) {
    .f.args <- list(
      data         = data,
      x            = as_string(x),
      y            = as_string(y),
      effsize.type = effsize.type,
      conf.level   = conf.level,
      var.equal    = var.equal,
      k            = k,
      tr           = tr,
      paired       = FALSE,
      bf.prior     = bf.prior,
      nboot        = nboot
    )

    .f <- .f_switch(test)
    subtitle_df <- .eval_f(.f, !!!.f.args, type = type)
    subtitle <- if (!is.null(subtitle_df)) subtitle_df$expression[[1L]]

    if (type == "parametric" && bf.message) {
      caption_df <- .eval_f(.f, !!!.f.args, type = "bayes")
      caption <- if (!is.null(caption_df)) caption_df$expression[[1L]]
    }
  }

  # plot -----------------------------------

  plot <- ggplot(data, mapping = aes({{ x }}, {{ y }})) +
    exec(geom_point, aes(color = {{ x }}), !!!point.args) +
    exec(geom_boxplot, !!!boxplot.args, outlier.shape = NA) +
    exec(geom_violin, !!!violin.args)

  # centrality tagging -------------------------------------

  if (isTRUE(centrality.plotting)) {
    plot <- suppressWarnings(.centrality_ggrepel(
      plot                  = plot,
      data                  = data,
      x                     = {{ x }},
      y                     = {{ y }},
      k                     = k,
      type                  = stats_type_switch(centrality.type),
      tr                    = tr,
      centrality.point.args = centrality.point.args,
      centrality.label.args = centrality.label.args
    ))
  }

  # ggsignif labels -------------------------------------

  seclabel <- NULL

  if (pairwise.display != "none" && test == "anova") {
    mpc_df <- pairwise_comparisons(
      data            = data,
      x               = {{ x }},
      y               = {{ y }},
      type            = type,
      tr              = tr,
      paired          = FALSE,
      var.equal       = var.equal,
      p.adjust.method = p.adjust.method,
      k               = k
    )

    # adding the layer for pairwise comparisons
    plot <- .ggsignif_adder(
      plot             = plot,
      mpc_df           = mpc_df,
      data             = data,
      x                = {{ x }},
      y                = {{ y }},
      pairwise.display = pairwise.display,
      ggsignif.args    = ggsignif.args
    )

    # secondary label axis to give pairwise comparisons test details
    seclabel <- .pairwise_seclabel(unique(mpc_df$test), ifelse(type == "bayes", "all", pairwise.display))
  }

  # annotations ------------------------

  .aesthetic_addon(
    plot             = plot,
    x                = data %>% pull({{ x }}),
    xlab             = xlab %||% as_name(x),
    ylab             = ylab %||% as_name(y),
    title            = title,
    subtitle         = subtitle,
    caption          = caption,
    seclabel         = seclabel,
    ggtheme          = ggtheme,
    package          = package,
    palette          = palette,
    ggplot.component = ggplot.component
  )
}


#' @title Violin plots for group or condition comparisons in between-subjects
#'   designs repeated across all levels of a grouping variable.
#' @name grouped_ggbetweenstats
#'
#' @description
#'
#' Helper function for `ggstatsplot::ggbetweenstats` to apply this function
#' across multiple levels of a given factor and combining the resulting plots
#' using `ggstatsplot::combine_plots`.
#'
#' @inheritParams ggbetweenstats
#' @inheritParams .grouped_list
#' @inheritParams combine_plots
#' @inheritDotParams ggbetweenstats -title
#'
#' @autoglobal
#'
#' @seealso \code{\link{ggbetweenstats}}, \code{\link{ggwithinstats}},
#'  \code{\link{grouped_ggwithinstats}}
#'
#' @inherit ggbetweenstats return references
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true") && requireNamespace("PMCMRplus", quietly = TRUE)
#' # for reproducibility
#' set.seed(123)
#' library(PMCMRplus) # for pairwise comparisons
#' library(dplyr, warn.conflicts = FALSE)
#' library(ggplot2)
#'
#' # the most basic function call
#' grouped_ggbetweenstats(
#'   data = filter(ggplot2::mpg, drv != "4"),
#'   x = year,
#'   y = hwy,
#'   grouping.var = drv
#' )
#'
#' # modifying individual plots using `ggplot.component` argument
#' grouped_ggbetweenstats(
#'   data = filter(
#'     movies_long,
#'     genre %in% c("Action", "Comedy"),
#'     mpaa %in% c("R", "PG")
#'   ),
#'   x = genre,
#'   y = rating,
#'   grouping.var = mpaa,
#'   ggplot.component = scale_y_continuous(
#'     breaks = seq(1, 9, 1),
#'     limits = (c(1, 9))
#'   )
#' )
#' @export
grouped_ggbetweenstats <- function(data,
                                   ...,
                                   grouping.var,
                                   plotgrid.args = list(),
                                   annotation.args = list()) {
  .grouped_list(data, {{ grouping.var }}) %>%
    purrr::pmap(.f = ggbetweenstats, ...) %>%
    combine_plots(plotgrid.args, annotation.args)
}
