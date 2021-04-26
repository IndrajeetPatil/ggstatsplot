#' @title Scatterplot with marginal distributions and statistical results
#' @name ggscatterstats
#'
#' @description
#'
#' Scatterplots from `ggplot2` combined with marginal
#' histograms/boxplots/density plots with statistical details added as a
#' subtitle.
#'
#' @param ... Currently ignored.
#' @param label.var Variable to use for points labels. Can be entered either as
#'   a bare expression (e.g, `var1`) or as a string (e.g., `"var1"`).
#' @param label.expression An expression evaluating to a logical vector that
#'   determines the subset of data points to label. This argument can be entered
#'   either as a bare expression (e.g., `y < 4 & z < 20`) or as a string (e.g.,
#'   `"y < 4 & z < 20"`).
#' @param point.label.args A list of additional aesthetic arguments to be passed
#'   to `ggrepel::geom_label_repel` geom used to display the labels.
#' @param smooth.line.args A list of additional aesthetic arguments to be passed
#'   to `ggplot2::geom_smooth` geom used to display the regression line.
#' @param point.args A list of additional aesthetic arguments to be passed
#'   to `ggplot2::geom_point` geom used to display the raw data points.
#' @param marginal Decides whether marginal distributions will be plotted on
#'   axes using `ggExtra::ggMarginal()`. The default is `TRUE`. The package
#'   `ggExtra` must already be installed by the user.
#' @param point.width.jitter,point.height.jitter Degree of jitter in `x` and `y`
#'   direction, respectively. Defaults to `0` (0%) of the resolution of the
#'   data. Note that the jitter should not be specified in the `point.args`
#'   because this information will be passed to two different `geom`s: one
#'   displaying the **points** and the other displaying the ***labels** for
#'   these points.
#' @param marginal.type Type of marginal distribution to be plotted on the axes
#'   (`"histogram"`, `"boxplot"`, `"density"`, `"violin"`, `"densigram"`).
#' @param marginal.size Integer describing the relative size of the marginal
#'   plots compared to the main plot. A size of `5` means that the main plot is
#'   5x wider and 5x taller than the marginal plots.
#' @param xfill,yfill Character describing color fill for `x` and `y` axes
#'  marginal distributions (default: `"#009E73"` (for `x`) and `"#D55E00"` (for
#'  `y`)). Note that the defaults are colorblind-friendly.
#' @inheritParams statsExpressions::corr_test
#' @inheritParams theme_ggstatsplot
#' @inheritParams ggbetweenstats
#' @inheritParams ggExtra::ggMarginal
#' @inheritParams gghistostats
#'
#' @import ggplot2
#'
#' @importFrom dplyr filter pull
#' @importFrom stats lm
#' @importFrom rlang !! enquo quo_name parse_expr ensym as_name enexpr exec !!!
#' @importFrom ggrepel geom_label_repel
#' @importFrom statsExpressions corr_test
#'
#' @seealso \code{\link{grouped_ggscatterstats}}, \code{\link{ggcorrmat}},
#' \code{\link{grouped_ggcorrmat}}
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggscatterstats.html}
#'
#' @note
#' - If you set `marginal = TRUE`, the resulting plot can **not** be further
#' modified with `ggplot2` functions since it is no longer a `ggplot` object. In
#' case you want a `ggplot` object, set `marginal = FALSE`. Also have a look at
#' the `ggplot.component` argument.
#'
#' - The plot uses `ggrepel::geom_label_repel` to attempt to keep labels
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
#' if (require("ggExtra")) {
#'   ggscatterstats(
#'     data = mtcars_new,
#'     x = wt,
#'     y = mpg,
#'     label.var = car,
#'     label.expression = wt < 4 & mpg < 20,
#'     # making further customization with `ggplot2` functions
#'     ggplot.component = list(ggplot2::geom_rug(sides = "b"))
#'   )
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
                           point.args = list(size = 3, alpha = 0.4),
                           point.width.jitter = 0,
                           point.height.jitter = 0,
                           point.label.args = list(size = 3, max.overlaps = 1e6),
                           smooth.line.args = list(size = 1.5, color = "blue"),
                           marginal = TRUE,
                           marginal.type = "densigram",
                           marginal.size = 5,
                           xfill = "#009E73",
                           yfill = "#D55E00",
                           xlab = NULL,
                           ylab = NULL,
                           title = NULL,
                           subtitle = NULL,
                           caption = NULL,
                           ggtheme = ggplot2::theme_bw(),
                           ggstatsplot.layer = TRUE,
                           ggplot.component = NULL,
                           output = "plot",
                           ...) {

  # convert entered stats type to a standard notation
  type <- ipmisc::stats_type_switch(type)

  #---------------------- variable names --------------------------------

  # ensure the arguments work quoted or unquoted
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))

  # point labeling
  if (!rlang::quo_is_null(rlang::enquo(label.var))) {
    label.var <- rlang::ensym(label.var)
    point.labelling <- TRUE
  } else {
    point.labelling <- FALSE
  }

  #----------------------- dataframe ---------------------------------------

  # preparing the dataframe
  data %<>% dplyr::filter(!is.na({{ x }}), !is.na({{ y }}))

  #----------------------- creating results subtitle ------------------------

  # adding a subtitle with statistical results
  if (isTRUE(results.subtitle)) {
    # preparing the BF message for null hypothesis support
    if (type == "parametric" && isTRUE(bf.message)) {
      caption_df <-
        statsExpressions::corr_test(
          data = data,
          x = {{ x }},
          y = {{ y }},
          type = "bayes",
          bf.prior = bf.prior,
          top.text = caption,
          k = k
        )

      caption <- caption_df$expression[[1]]
    }

    # extracting the subtitle using the switch function
    subtitle_df <-
      statsExpressions::corr_test(
        data = data,
        x = {{ x }},
        y = {{ y }},
        tr = tr,
        type = type,
        conf.level = conf.level,
        k = k
      )

    subtitle <- subtitle_df$expression[[1]]
  }

  # quit early if only subtitle is needed
  if (output %in% c("subtitle", "caption")) {
    return(switch(output,
      "subtitle" = subtitle,
      "caption" = caption
    ))
  }

  #---------------------------- user expression -------------------------

  # check labeling variable has been entered
  if (isTRUE(point.labelling)) {
    # is expression provided?
    if (!rlang::quo_is_null(rlang::enquo(label.expression))) {
      expression.present <- TRUE
    } else {
      expression.present <- FALSE
    }

    # creating a new dataframe for showing labels
    if (isTRUE(expression.present)) {
      if (!rlang::quo_is_null(rlang::enquo(label.expression))) {
        label.expression <- rlang::enexpr(label.expression)
      }

      # testing for whether we received bare or quoted
      if (typeof(label.expression) == "language") {
        # unquoted case
        label_data <- dplyr::filter(data, !!label.expression)
      } else {
        # quoted case
        label_data <- dplyr::filter(data, !!rlang::parse_expr(label.expression))
      }
    } else {
      label_data <- data
    }
  }

  # --------------------------------- basic plot ---------------------------

  # creating jittered positions
  pos <- ggplot2::position_jitter(width = point.width.jitter, height = point.height.jitter)

  # preparing the scatterplot
  plot <-
    ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = {{ x }}, y = {{ y }})) +
    rlang::exec(
      .fn = ggplot2::geom_point,
      stroke = 0,
      position = pos,
      !!!point.args
    ) +
    rlang::exec(
      .fn = ggplot2::geom_smooth,
      method = "lm",
      formula = y ~ x,
      level = conf.level,
      !!!smooth.line.args
    )

  #-------------------- adding point labels --------------------------------

  # using geom_repel_label
  if (isTRUE(point.labelling)) {
    plot <- plot +
      rlang::exec(
        .fn = ggrepel::geom_label_repel,
        data = label_data,
        mapping = ggplot2::aes(label = {{ label.var }}),
        show.legend = FALSE,
        min.segment.length = 0,
        position = pos,
        !!!point.label.args
      )
  }

  #-------------------------- annotations -------------------------------------

  # annotations
  plot <- plot +
    ggplot2::labs(
      x = xlab %||% rlang::as_name(x),
      y = ylab %||% rlang::as_name(y),
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    theme_ggstatsplot(ggtheme, ggstatsplot.layer) +
    ggplot.component

  #------------------------- ggMarginal  ---------------------------------

  # creating the `ggMarginal` plot of a given `marginal.type`
  if (isTRUE(marginal)) {
    if (!requireNamespace("ggExtra")) stop("Package 'ggExtra' needs to be installed.")
    plot <-
      ggExtra::ggMarginal(
        p = plot,
        type = marginal.type,
        size = marginal.size,
        xparams = list(fill = xfill),
        yparams = list(fill = yfill)
      )
  }

  # return the final plot
  plot
}
