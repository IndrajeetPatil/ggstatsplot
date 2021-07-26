#' @title Scatterplot with marginal distributions and statistical results
#' @name ggscatterstats
#'
#' @description
#'
#' Scatterplots from `ggplot2` combined with marginal densigram (density +
#' histogram) plots with statistical details.
#'
#' @param ... Currently ignored.
#' @param label.var Variable to use for points labels entered as a symbol (e.g.
#'   `var1`).
#' @param label.expression An expression evaluating to a logical vector that
#'   determines the subset of data points to label (e.g. `y < 4 & z < 20`).
#'   While using this argument with `purrr::pmap`, you will have to provide a
#'   quoted expression  (e.g. `quote(y < 4 & z < 20)`).
#' @param point.label.args A list of additional aesthetic arguments to be passed
#'   to `ggrepel::geom_label_repel` geom used to display the labels.
#' @param smooth.line.args A list of additional aesthetic arguments to be passed
#'   to `ggplot2::geom_smooth` geom used to display the regression line.
#' @param point.args A list of additional aesthetic arguments to be passed
#'   to `ggplot2::geom_point` geom used to display the raw data points.
#' @param marginal Decides whether marginal distributions will be plotted on
#'   axes using `ggside` functions. The default is `TRUE`. The package
#'   `ggside` must already be installed by the user.
#' @param point.width.jitter,point.height.jitter Degree of jitter in `x` and `y`
#'   direction, respectively. Defaults to `0` (0%) of the resolution of the
#'   data. Note that the jitter should not be specified in the `point.args`
#'   because this information will be passed to two different `geom`s: one
#'   displaying the **points** and the other displaying the ***labels** for
#'   these points.
#' @param xfill,yfill Character describing color fill for `x` and `y` axes
#'  marginal distributions (default: `"#009E73"` (for `x`) and `"#D55E00"` (for
#'  `y`)). Note that the defaults are colorblind-friendly.
#' @param xsidehistogram.args,ysidehistogram.args,xsidedensity.args,ysidedensity.args
#'   A list of arguments passed to respective `geom_`s from `ggside` package to
#'   change the marginal distribution histograms and density plots.
#' @inheritParams statsExpressions::corr_test
#' @inheritParams theme_ggstatsplot
#' @inheritParams ggbetweenstats
#' @inheritParams gghistostats
#'
#' @import ggplot2
#'
#' @importFrom dplyr filter pull
#' @importFrom stats lm
#' @importFrom rlang !! enquo quo_name ensym as_name enexpr exec !!!
#' @importFrom ggrepel geom_label_repel
#' @importFrom statsExpressions corr_test
#'
#' @seealso \code{\link{grouped_ggscatterstats}}, \code{\link{ggcorrmat}},
#' \code{\link{grouped_ggcorrmat}}
#'
#' @details For details, see:
#' <https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggscatterstats.html>
#'
#' @note
#' The plot uses `ggrepel::geom_label_repel` to attempt to keep labels
#' from over-lapping to the largest degree possible.  As a consequence plot
#' times will slow down massively (and the plot file will grow in size) if you
#' have a lot of labels that overlap.
#'
#' @examples
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # creating dataframe with rownames converted to a new column
#' mtcars_new <- as_tibble(mtcars, rownames = "car")
#'
#' # simple function call with the defaults
#' if (require("ggside")) {
#'   ggscatterstats(
#'     data = mtcars_new,
#'     x = wt,
#'     y = mpg,
#'     label.var = car,
#'     label.expression = wt < 4 & mpg < 20
#'   ) + # making further customization with `ggplot2` functions
#'     ggplot2::geom_rug(sides = "b")
#' }
#' @export

# defining the function
ggscatterstats <- function(data,
                           x,
                           y,
                           type = "parametric",
                           conf.level = 0.95,
                           bf.prior = 0.707,
                           bf.message = TRUE,
                           tr = 0.2,
                           k = 2L,
                           results.subtitle = TRUE,
                           label.var = NULL,
                           label.expression = NULL,
                           point.args = list(
                             size = 3,
                             alpha = 0.4,
                             stroke = 0,
                             na.rm = TRUE
                           ),
                           point.width.jitter = 0,
                           point.height.jitter = 0,
                           point.label.args = list(size = 3, max.overlaps = 1e6),
                           smooth.line.args = list(
                             size = 1.5,
                             color = "blue",
                             method = "lm",
                             formula = y ~ x,
                             na.rm = TRUE
                           ),
                           marginal = TRUE,
                           xfill = "#009E73",
                           yfill = "#D55E00",
                           xsidehistogram.args = list(
                             fill = xfill,
                             color = "black",
                             na.rm = TRUE
                           ),
                           ysidehistogram.args = list(
                             fill = yfill,
                             color = "black",
                             na.rm = TRUE
                           ),
                           xsidedensity.args = list(na.rm = TRUE),
                           ysidedensity.args = list(na.rm = TRUE),
                           xlab = NULL,
                           ylab = NULL,
                           title = NULL,
                           subtitle = NULL,
                           caption = NULL,
                           ggtheme = ggstatsplot::theme_ggstatsplot(),
                           ggplot.component = NULL,
                           output = "plot",
                           ...) {

  # data ---------------------------------------

  # ensure the arguments work quoted or unquoted
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # preparing the dataframe
  data %<>% dplyr::filter(!is.na({{ x }}), !is.na({{ y }}))

  # statistical analysis ------------------------------------------

  # adding a subtitle with statistical results
  if (isTRUE(results.subtitle)) {
    # convert entered stats type to a standard notation
    type <- statsExpressions::stats_type_switch(type)

    # relevant arguments for statistical tests
    .f.args <- list(
      data = data,
      x = {{ x }},
      y = {{ y }},
      conf.level = conf.level,
      k = k,
      tr = tr,
      bf.prior = bf.prior,
      top.text = caption
    )

    subtitle_df <- eval_f(corr_test, !!!.f.args, type = type)
    subtitle <- if (!is.null(subtitle_df)) subtitle_df$expression[[1]]

    # preparing the BF message for null hypothesis support
    if (type == "parametric" && isTRUE(bf.message)) {
      caption_df <- eval_f(corr_test, !!!.f.args, type = "bayes")
      caption <- if (!is.null(caption_df)) caption_df$expression[[1]]
    }
  }

  # quit early if only subtitle is needed
  if (output != "plot") {
    return(switch(output,
      "caption" = caption,
      subtitle
    ))
  }

  # plot ------------------------------------------

  # creating jittered positions
  pos <- ggplot2::position_jitter(width = point.width.jitter, height = point.height.jitter)

  # preparing the scatterplot
  plot <- ggplot2::ggplot(data, mapping = aes({{ x }}, {{ y }})) +
    exec(ggplot2::geom_point, position = pos, !!!point.args) +
    exec(ggplot2::geom_smooth, level = conf.level, !!!smooth.line.args)

  # point labels --------------------------------

  if (!rlang::quo_is_null(rlang::enquo(label.var))) {
    label.var <- rlang::ensym(label.var)

    # select data based on expression
    if (!rlang::quo_is_null(rlang::enquo(label.expression))) {
      label_data <- dplyr::filter(data, !!rlang::enexpr(label.expression))
    } else {
      label_data <- data
    }

    # display points labels using `geom_repel_label`
    plot <- plot +
      exec(
        .fn = ggrepel::geom_label_repel,
        data = label_data,
        mapping = aes(label = {{ label.var }}),
        min.segment.length = 0,
        position = pos,
        !!!point.label.args
      )
  }

  # annotations -------------------------------------

  plot <- plot +
    ggplot2::labs(
      x = xlab %||% rlang::as_name(x),
      y = ylab %||% rlang::as_name(y),
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    ggtheme +
    ggplot.component

  # marginal  ---------------------------------------------

  if (marginal) {
    # installed?
    insight::check_if_installed("ggside", minimum_version = "0.1.2")

    # adding marginal distributions
    plot <- plot +
      exec(ggside::geom_xsidehistogram, mapping = aes(y = after_stat(count)), !!!xsidehistogram.args) +
      exec(ggside::geom_ysidehistogram, mapping = aes(x = after_stat(count)), !!!ysidehistogram.args) +
      exec(ggside::geom_xsidedensity, mapping = aes(y = after_stat(count)), !!!xsidedensity.args) +
      exec(ggside::geom_ysidedensity, mapping = aes(x = after_stat(count)), !!!ysidedensity.args) +
      ggside::scale_ysidex_continuous() +
      ggside::scale_xsidey_continuous()
  }

  # return the final plot
  plot
}


#' @title Scatterplot with marginal distributions for all levels of a grouping
#'   variable
#' @name grouped_ggscatterstats
#'
#' @description
#'
#' Grouped scatterplots from `ggplot2` combined with marginal distribution plots
#' with statistical details added as a subtitle.
#'
#' @inheritParams ggscatterstats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggscatterstats -title
#'
#' @import ggplot2
#'
#' @importFrom dplyr select
#' @importFrom rlang as_name enexpr ensym
#' @importFrom purrr pmap
#'
#' @seealso \code{\link{ggscatterstats}}, \code{\link{ggcorrmat}},
#' \code{\link{grouped_ggcorrmat}}
#'
#' @inherit ggscatterstats return references
#' @inherit ggscatterstats return details
#'
#' @examples
#' # to ensure reproducibility
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # skipping marginal distributions so that the examples run fast
#'
#' # basic function call
#' grouped_ggscatterstats(
#'   data = dplyr::filter(movies_long, genre == "Comedy" | genre == "Drama"),
#'   x = length,
#'   y = rating,
#'   type = "robust",
#'   grouping.var = genre,
#'   ggplot.component = list(ggplot2::geom_rug(sides = "b"))
#' )
#'
#' # using labeling
#' # (also show how to modify basic plot from within function call)
#' grouped_ggscatterstats(
#'   data = dplyr::filter(ggplot2::mpg, cyl != 5),
#'   x = displ,
#'   y = hwy,
#'   grouping.var = cyl,
#'   type = "robust",
#'   label.var = manufacturer,
#'   label.expression = hwy > 25 & displ > 2.5,
#'   ggplot.component = ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis())
#' )
#'
#' # labeling without expression
#'
#' grouped_ggscatterstats(
#'   data = dplyr::filter(
#'     movies_long,
#'     rating == 7,
#'     genre %in% c("Drama", "Comedy")
#'   ),
#'   x = budget,
#'   y = length,
#'   grouping.var = genre,
#'   bf.message = FALSE,
#'   label.var = "title",
#'   annotation.args = list(tag_levels = "a")
#' )
#' @export

# defining the function
grouped_ggscatterstats <- function(data,
                                   grouping.var,
                                   output = "plot",
                                   plotgrid.args = list(),
                                   annotation.args = list(),
                                   ...) {
  # getting the dataframe ready
  data %<>% grouped_list({{ grouping.var }})

  # creating a list of plots using `pmap`
  p_ls <- purrr::pmap(
    .l = list(data = data, title = names(data), output = output),
    .f = ggstatsplot::ggscatterstats,
    ...
  )

  # combining the list of plots into a single plot
  if (output == "plot") p_ls <- combine_plots(p_ls, plotgrid.args, annotation.args)

  # return the object
  p_ls
}
