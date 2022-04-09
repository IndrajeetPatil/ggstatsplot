#' @title Dot plot/chart for labeled numeric data.
#' @name ggdotplotstats
#'
#' @description
#'
#' A dot chart (as described by William S. Cleveland) with statistical details
#' from one-sample test details.
#'
#' @param ... Currently ignored.
#' @param y Label or grouping variable.
#' @param point.args A list of additional aesthetic arguments passed to
#'   `geom_point`.
#' @inheritParams gghistostats
#' @inheritParams ggcoefstats
#'
#' @details For details, see:
#' <https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggdotplotstats.html>
#'
#' @seealso \code{\link{grouped_gghistostats}}, \code{\link{gghistostats}},
#'  \code{\link{grouped_ggdotplotstats}}
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#'
#' # plot
#' ggdotplotstats(
#'   data = ggplot2::mpg,
#'   x = cty,
#'   y = manufacturer,
#'   title = "Fuel economy data",
#'   xlab = "city miles per gallon"
#' )
#' }
#' @export
ggdotplotstats <- function(data,
                           x,
                           y,
                           xlab = NULL,
                           ylab = NULL,
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
                           results.subtitle = TRUE,
                           point.args = list(
                             color = "black",
                             size = 3,
                             shape = 16
                           ),
                           centrality.plotting = TRUE,
                           centrality.type = type,
                           centrality.line.args = list(
                             color = "blue",
                             size = 1,
                             linetype = "dashed"
                           ),
                           ggplot.component = NULL,
                           ggtheme = ggstatsplot::theme_ggstatsplot(),
                           output = "plot",
                           ...) {

  # data -----------------------------------

  # ensure the variables work quoted or unquoted
  c(x, y) %<-% c(ensym(x), ensym(y))

  # creating a dataframe
  data %<>%
    select({{ x }}, {{ y }}) %>%
    tidyr::drop_na(.) %>%
    mutate({{ y }} := droplevels(as.factor({{ y }}))) %>%
    group_by({{ y }}) %>%
    summarise({{ x }} := mean({{ x }})) %>%
    ungroup(.) %>%
    # rank ordering the data
    arrange({{ x }}) %>%
    mutate(
      percent_rank = percent_rank({{ x }}),
      rank = row_number()
    )

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
      bf.prior     = bf.prior,
      top.text     = caption
    )

    # preparing the subtitle with statistical results
    subtitle_df <- eval_f(one_sample_test, !!!.f.args, type = type)
    subtitle <- if (!is.null(subtitle_df)) subtitle_df$expression[[1]]

    # preparing the BF message
    if (type == "parametric" && bf.message) {
      caption_df <- eval_f(one_sample_test, !!!.f.args, type = "bayes")
      caption <- if (!is.null(caption_df)) caption_df$expression[[1]]
    }
  }

  # return early if anything other than plot
  if (output != "plot") {
    return(switch(output,
      "caption" = caption,
      subtitle
    ))
  }

  # plot -----------------------------------

  plot <- ggplot(data, mapping = aes({{ x }}, y = rank)) +
    exec(geom_point, !!!point.args) +
    scale_y_continuous(
      name = ylab,
      labels = data %>% pull({{ y }}),
      breaks = data$rank,
      sec.axis = dup_axis(
        name   = "percentile",
        breaks = seq(1, nrow(data), (nrow(data) - 1) / 4),
        labels = 25 * 0:4
      )
    )

  # centrality plotting -------------------------------------

  if (isTRUE(centrality.plotting)) {
    plot <- histo_labeller(
      plot,
      x                    = data %>% pull({{ x }}),
      type                 = stats_type_switch(centrality.type),
      tr                   = tr,
      k                    = k,
      centrality.line.args = centrality.line.args
    )
  }

  # annotations -------------------------

  plot +
    labs(
      x        = xlab %||% as_name(x),
      y        = ylab %||% as_name(y),
      title    = title,
      subtitle = subtitle,
      caption  = caption
    ) +
    ggtheme +
    ggplot.component
}


#' @title Grouped histograms for distribution of a labeled numeric variable
#' @name grouped_ggdotplotstats
#'
#' @description
#'
#' Helper function for `ggstatsplot::ggdotplotstats` to apply this function
#' across multiple levels of a given factor and combining the resulting plots
#' using `ggstatsplot::combine_plots`.
#'
#' @inheritParams ggdotplotstats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggdotplotstats -title
#'
#' @seealso \code{\link{grouped_gghistostats}}, \code{\link{ggdotplotstats}},
#'  \code{\link{gghistostats}}
#'
#' @inherit ggdotplotstats return references
#' @inherit ggdotplotstats return details
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(ggstatsplot)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # removing factor level with very few no. of observations
#' df <- filter(ggplot2::mpg, cyl %in% c("4", "6", "8"))
#'
#' # plot
#' grouped_ggdotplotstats(
#'   data         = df,
#'   x            = cty,
#'   y            = manufacturer,
#'   grouping.var = cyl,
#'   test.value   = 15.5
#' )
#' }
#' @export
grouped_ggdotplotstats <- function(data,
                                   ...,
                                   grouping.var,
                                   output = "plot",
                                   plotgrid.args = list(),
                                   annotation.args = list()) {

  # dataframe
  data %<>% grouped_list(grouping.var = {{ grouping.var }})

  # creating a list of return objects
  p_ls <- purrr::pmap(
    .l = list(data = data, title = names(data), output = output),
    .f = ggstatsplot::ggdotplotstats,
    ...
  )

  # combining the list of plots into a single plot
  if (output == "plot") p_ls <- combine_plots(p_ls, plotgrid.args, annotation.args)

  p_ls
}
