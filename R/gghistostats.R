#' @title Histogram for distribution of a numeric variable
#' @name gghistostats
#'
#' @description
#'
#' Histogram with statistical details from one-sample test included in the plot
#' as a subtitle.
#'
#' @section Summary of graphics:
#'
#' ```{r child="man/rmd-fragments/gghistostats_graphics.Rmd"}
#' ```
#'
#' @param ... Currently ignored.
#' @param normal.curve A logical value that decides whether to super-impose a
#'   normal curve using `stats::dnorm(mean(x), sd(x))`. Default is `FALSE`.
#' @param normal.curve.args A list of additional aesthetic arguments to be
#'   passed to the normal curve.
#' @param binwidth The width of the histogram bins. Can be specified as a
#'   numeric value, or a function that calculates width from `x`. The default is
#'   to use the `max(x) - min(x) / sqrt(N)`. You should always check this value
#'   and explore multiple widths to find the best to illustrate the stories in
#'   your data.
#' @param bin.args A list of additional aesthetic arguments to be passed to the
#'   `stat_bin` used to display the bins. Do not specify `binwidth` argument in
#'   this list since it has already been specified using the dedicated argument.
#' @param centrality.line.args A list of additional aesthetic arguments to be
#'   passed to the `geom_line` used to display the lines corresponding to the
#'   centrality parameter.
#' @inheritParams statsExpressions::one_sample_test
#' @inheritParams ggbetweenstats
#'
#' @inheritSection statsExpressions::one_sample_test One-sample tests
#'
#' @seealso \code{\link{grouped_gghistostats}}, \code{\link{ggdotplotstats}},
#'  \code{\link{grouped_ggdotplotstats}}
#'
#' @details For details, see:
#' <https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/gghistostats.html>
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' # for reproducibility
#' set.seed(123)
#'
#' # creating a plot
#' p <- gghistostats(
#'   data            = ToothGrowth,
#'   x               = len,
#'   xlab            = "Tooth length",
#'   centrality.type = "np"
#' )
#'
#' # looking at the plot
#' p
#'
#' # extracting details from statistical tests
#' extract_stats(p)
#' @export
gghistostats <- function(data,
                         x,
                         binwidth = NULL,
                         xlab = NULL,
                         title = NULL,
                         subtitle = NULL,
                         caption = NULL,
                         type = "parametric",
                         test.value = 0,
                         bf.prior = 0.707,
                         bf.message = TRUE,
                         effsize.type = "g",
                         conf.level = 0.95,
                         tr = 0.2,
                         k = 2L,
                         ggtheme = ggstatsplot::theme_ggstatsplot(),
                         results.subtitle = TRUE,
                         bin.args = list(color = "black", fill = "grey50", alpha = 0.7),
                         centrality.plotting = TRUE,
                         centrality.type = type,
                         centrality.line.args = list(color = "blue", linewidth = 1, linetype = "dashed"),
                         normal.curve = FALSE,
                         normal.curve.args = list(linewidth = 2),
                         ggplot.component = NULL,
                         ...) {
  # data -----------------------------------

  x <- ensym(x)
  data <- tidyr::drop_na(select(data, {{ x }}))

  # extract a vector for convenience
  x_vec <- data %>% pull({{ x }})

  # statistical analysis ------------------------------------------

  if (results.subtitle) {
    # convert entered stats type to a standard notation
    type <- stats_type_switch(type)

    # relevant arguments for statistical tests
    .f.args <- list(
      data         = data,
      x            = {{ x }},
      test.value   = test.value,
      effsize.type = effsize.type,
      conf.level   = conf.level,
      k            = k,
      tr           = tr,
      bf.prior     = bf.prior
    )

    # preparing the subtitle with statistical results
    subtitle_df <- .eval_f(one_sample_test, !!!.f.args, type = type)
    subtitle <- if (!is.null(subtitle_df)) subtitle_df$expression[[1L]]

    # preparing the BF message
    if (type == "parametric" && bf.message) {
      caption_df <- .eval_f(one_sample_test, !!!.f.args, type = "bayes")
      caption <- if (!is.null(caption_df)) caption_df$expression[[1L]]
    }
  }

  # plot -----------------------------------

  plot <- ggplot(data, mapping = aes(x = {{ x }})) +
    exec(
      stat_bin,
      mapping  = aes(y = after_stat(count), fill = after_stat(count)),
      binwidth = binwidth %||% .binwidth(x_vec),
      !!!bin.args
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(
        trans  = ~ . / nrow(data),
        labels = function(x) paste0(x * 100, "%"),
        name   = "proportion"
      )
    ) +
    guides(fill = "none")

  # if normal curve overlay needs to be displayed
  if (normal.curve) {
    plot <- plot +
      exec(
        stat_function,
        fun  = function(x, mean, sd, n, bw) stats::dnorm(x, mean, sd) * n * bw,
        args = list(mean = mean(x_vec), sd = sd(x_vec), n = length(x_vec), bw = binwidth %||% .binwidth(x_vec)),
        !!!normal.curve.args
      )
  }

  # centrality plotting -------------------------------------

  if (isTRUE(centrality.plotting)) {
    plot <- .histo_labeller(
      plot,
      x                    = x_vec,
      type                 = stats_type_switch(centrality.type),
      tr                   = tr,
      k                    = k,
      centrality.line.args = centrality.line.args
    )
  }

  # annotations -------------------------------

  plot +
    labs(
      x        = xlab %||% as_name(x),
      y        = "count",
      title    = title,
      subtitle = subtitle,
      caption  = caption
    ) +
    ggtheme +
    ggplot.component
}


#' @noRd
.binwidth <- function(x) (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) / sqrt(length(x))


#' @title Grouped histograms for distribution of a numeric variable
#' @name grouped_gghistostats
#'
#' @description
#'
#' Helper function for `ggstatsplot::gghistostats` to apply this function
#' across multiple levels of a given factor and combining the resulting plots
#' using `ggstatsplot::combine_plots`.
#'
#' @inheritParams gghistostats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams gghistostats -title
#'
#' @seealso \code{\link{gghistostats}}, \code{\link{ggdotplotstats}},
#'  \code{\link{grouped_ggdotplotstats}}
#'
#' @inherit gghistostats return references
#' @inherit gghistostats return details
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' # for reproducibility
#' set.seed(123)
#'
#' # plot
#' grouped_gghistostats(
#'   data            = iris,
#'   x               = Sepal.Length,
#'   test.value      = 5,
#'   grouping.var    = Species,
#'   plotgrid.args   = list(nrow = 1),
#'   annotation.args = list(tag_levels = "i")
#' )
#' @export
grouped_gghistostats <- function(data,
                                 x,
                                 grouping.var,
                                 binwidth = NULL,
                                 plotgrid.args = list(),
                                 annotation.args = list(),
                                 ...) {
  purrr::pmap(
    .l = .grouped_list(data, {{ grouping.var }}),
    .f = ggstatsplot::gghistostats,
    x = {{ x }},
    binwidth = binwidth %||% .binwidth(data %>% pull({{ x }})),
    ...
  ) %>%
    combine_plots(plotgrid.args, annotation.args)
}
