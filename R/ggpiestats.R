#' @title Pie charts with statistical tests
#' @name ggpiestats
#'
#' @description
#' Pie charts for categorical data with statistical details included in the plot
#' as a subtitle.
#'
#' @param x The variable to use as the **rows** in the contingency table. Please
#'   note that if there are empty factor levels in your variable, they will be
#'   dropped.
#' @param y The variable to use as the **columns** in the contingency table.
#'   Please note that if there are empty factor levels in your variable, they
#'   will be dropped. Default is `NULL`. If `NULL`, one-sample proportion test
#'   (a goodness of fit test) will be run for the `x` variable. Otherwise an
#'   appropriate association test will be run. This argument can not be `NULL`
#'   for `ggbarstats` function.
#' @param proportion.test Decides whether proportion test for `x` variable is to
#'   be carried out for each level of `y` (Default: `TRUE`). In `ggbarstats`,
#'   only *p*-values from this test will be displayed.
#' @param perc.k Numeric that decides number of decimal places for percentage
#'   labels (Default: `0`).
#' @param label Character decides what information needs to be displayed
#'   on the label in each pie slice. Possible options are `"percentage"`
#'   (default), `"counts"`, `"both"`.
#' @param label.args Additional aesthetic arguments that will be passed to
#'   `geom_label`.
#' @param label.repel Whether labels should be repelled using `ggrepel` package.
#'   This can be helpful in case the labels are overlapping.
#' @param legend.title Title text for the legend.
#' @inheritParams ggbetweenstats
#' @inheritParams statsExpressions::contingency_table
#' @inheritParams theme_ggstatsplot
#' @inheritParams gghistostats
#'
#' @seealso \code{\link{grouped_ggpiestats}}, \code{\link{ggbarstats}},
#'  \code{\link{grouped_ggbarstats}}
#'
#' @import ggplot2
#'
#' @importFrom dplyr select mutate vars pull across everything
#' @importFrom rlang !! enquo as_name ensym !!! exec
#' @importFrom ggrepel geom_label_repel
#' @importFrom tidyr uncount drop_na
#' @importFrom statsExpressions contingency_table
#'
#' @details For details, see:
#' <https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggpiestats.html>
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # one sample goodness of fit proportion test
#' ggpiestats(mtcars, vs)
#'
#' # association test (or contingency table analysis)
#' ggpiestats(mtcars, vs, cyl)
#' }
#' @export

# defining the function
ggpiestats <- function(data,
                       x,
                       y = NULL,
                       counts = NULL,
                       type = "parametric",
                       paired = FALSE,
                       results.subtitle = TRUE,
                       label = "percentage",
                       label.args = list(direction = "both"),
                       label.repel = FALSE,
                       k = 2L,
                       proportion.test = TRUE,
                       perc.k = 0,
                       bf.message = TRUE,
                       ratio = NULL,
                       conf.level = 0.95,
                       sampling.plan = "indepMulti",
                       fixed.margin = "rows",
                       prior.concentration = 1,
                       title = NULL,
                       subtitle = NULL,
                       caption = NULL,
                       legend.title = NULL,
                       ggtheme = ggstatsplot::theme_ggstatsplot(),
                       package = "RColorBrewer",
                       palette = "Dark2",
                       ggplot.component = NULL,
                       output = "plot",
                       ...) {
  # dataframe ------------------------------------------

  # convert entered stats type to a standard notation
  type <- statsExpressions::stats_type_switch(type)

  # ensure the variables work quoted or unquoted
  x <- rlang::ensym(x)
  y <- if (!rlang::quo_is_null(rlang::enquo(y))) rlang::ensym(y)

  # one-way or two-way table?
  test <- ifelse(!rlang::quo_is_null(rlang::enquo(y)), "two.way", "one.way")

  # creating a dataframe
  data %<>%
    dplyr::select({{ x }}, {{ y }}, .counts = {{ counts }}) %>%
    tidyr::drop_na(.)

  # untable the dataframe based on the count for each observation
  if (".counts" %in% names(data)) data %<>% tidyr::uncount(weights = .counts)

  # x and y need to be a factor; also drop the unused levels of the factors
  data %<>% dplyr::mutate(dplyr::across(.fns = ~ droplevels(as.factor(.x))))

  # x
  x_levels <- nlevels(data %>% dplyr::pull({{ x }}))

  # y
  if (test == "one.way") y_levels <- 0L
  if (test == "two.way") {
    y_levels <- nlevels(data %>% dplyr::pull({{ y }}))
    if (y_levels == 1L) bf.message <- FALSE # TODO: one-way table in `BayesFactor`
  }


  # faceting is happening only if both vars have more than one levels
  facet <- ifelse(y_levels > 1L, TRUE, FALSE)
  if ((x_levels == 1L && facet) || type == "bayes") proportion.test <- FALSE

  # statistical analysis ------------------------------------------

  # if subtitle with results is to be displayed
  if (results.subtitle) {
    # relevant arguments for statistical tests
    .f.args <- list(
      data = data,
      x = {{ x }},
      y = {{ y }},
      conf.level = conf.level,
      k = k,
      paired = paired,
      ratio = ratio,
      sampling.plan = sampling.plan,
      fixed.margin = fixed.margin,
      prior.concentration = prior.concentration,
      top.text = caption
    )

    subtitle_df <- eval_f(contingency_table, !!!.f.args, type = type)
    if (!is.null(subtitle_df)) subtitle <- subtitle_df$expression[[1]]

    # preparing Bayes Factor caption
    if (type != "bayes" && isTRUE(bf.message) && isFALSE(paired)) {
      caption_df <- eval_f(contingency_table, !!!.f.args, type = "bayes")
      if (!is.null(caption_df)) caption <- caption_df$expression[[1]]
    }
  }

  # return early if anything other than plot
  if (output != "plot") {
    return(switch(output,
      "caption" = caption,
      subtitle
    ))
  }

  # plot ------------------------------------------

  # dataframe with summary labels
  descriptive_df <- descriptive_df(data, {{ x }}, {{ y }}, label, perc.k)

  # dataframe containing all details needed for prop test
  if (test == "two.way") onesample_df <- onesample_df(data, {{ x }}, {{ y }}, k)

  # if no. of factor levels is greater than the default palette color count
  palette_message(package, palette, min_length = x_levels)

  # creating the basic plot
  p <- ggplot2::ggplot(descriptive_df, mapping = aes(x = "", y = perc)) +
    ggplot2::geom_col(
      mapping = aes(fill = {{ x }}),
      position = "fill",
      color = "black",
      width = 1
    )

  # whether labels need to be repelled
  if (label.repel) .fn <- ggrepel::geom_label_repel
  if (!label.repel) .fn <- ggplot2::geom_label

  # adding label with percentages and/or counts
  suppressWarnings(suppressMessages(p <- p +
    exec(
      .fn,
      mapping = aes(label = .label, group = {{ x }}),
      position = ggplot2::position_fill(vjust = 0.5),
      min.segment.length = 0,
      fill = "white",
      alpha = 1,
      !!!label.args
    )))

  # if facet_wrap *is* happening
  if (facet) p <- p + ggplot2::facet_wrap(facets = dplyr::vars({{ y }}))

  # polar coordinates plus formatting
  p <- p +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::scale_y_continuous(breaks = NULL) +
    paletteer::scale_fill_paletteer_d(paste0(package, "::", palette), name = "") +
    ggtheme +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(color = NA)))

  # sample size + proportion test ------------------------------------------

  # adding labels with proportion tests
  if (facet && proportion.test) {
    p <- p +
      exec(
        ggplot2::geom_text,
        data = onesample_df,
        mapping = aes(label = .label, x = 1.65, y = 0.5),
        position = ggplot2::position_fill(vjust = 1),
        size = 2.8,
        parse = TRUE
      )
  }

  # annotations ------------------------------------------

  p +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      subtitle = subtitle,
      title = title,
      caption = caption
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend.title %||% rlang::as_name(x))) +
    ggplot.component
}


#' @title Grouped pie charts with statistical tests
#' @name grouped_ggpiestats
#' @description Helper function for `ggstatsplot::ggpiestats` to apply this
#'   function across multiple levels of a given factor and combining the
#'   resulting plots using `ggstatsplot::combine_plots`.
#'
#' @inheritParams ggpiestats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggpiestats -title
#'
#' @importFrom purrr pmap
#'
#' @seealso \code{\link{ggbarstats}}, \code{\link{ggpiestats}},
#'  \code{\link{grouped_ggbarstats}}
#'
#' @inherit ggpiestats return references
#' @inherit ggpiestats return details
#' @inherit ggpiestats return return
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # grouped one-sample proportion test
#' grouped_ggpiestats(
#'   data = mtcars,
#'   grouping.var = am,
#'   x = cyl
#' )
#' }
#' @export

# defining the function
grouped_ggpiestats <- function(data,
                               ...,
                               grouping.var,
                               output = "plot",
                               plotgrid.args = list(),
                               annotation.args = list()) {

  # creating a dataframe
  data %<>% grouped_list(grouping.var = {{ grouping.var }})

  # creating a list of return objects
  p_ls <- purrr::pmap(
    .l = list(data = data, title = names(data), output = output),
    .f = ggstatsplot::ggpiestats,
    ...
  )

  # combining the list of plots into a single plot
  if (output == "plot") p_ls <- combine_plots(p_ls, plotgrid.args, annotation.args)

  # return the object
  p_ls
}
