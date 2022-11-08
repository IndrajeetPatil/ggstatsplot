#' @title Stacked bar charts with statistical tests
#' @name ggbarstats
#'
#' @description
#'
#' Bar charts for categorical data with statistical details included in the plot
#' as a subtitle.
#'
#' @inheritParams ggpiestats
#' @inheritParams ggbetweenstats
#'
#' @inheritSection statsExpressions::contingency_table Contingency table analyses
#'
#' @seealso \code{\link{grouped_ggbarstats}}, \code{\link{ggpiestats}},
#'  \code{\link{grouped_ggpiestats}}
#'
#' @inherit ggpiestats return details
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#'
#' # association test (or contingency table analysis)
#' ggbarstats(mtcars, x = vs, y = cyl)
#' }
#' @export
ggbarstats <- function(data,
                       x,
                       y,
                       counts = NULL,
                       type = "parametric",
                       paired = FALSE,
                       results.subtitle = TRUE,
                       label = "percentage",
                       label.args = list(alpha = 1, fill = "white"),
                       k = 2L,
                       proportion.test = results.subtitle,
                       perc.k = 0L,
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
                       xlab = NULL,
                       ylab = NULL,
                       ggtheme = ggstatsplot::theme_ggstatsplot(),
                       package = "RColorBrewer",
                       palette = "Dark2",
                       ggplot.component = NULL,
                       ...) {
  # data frame ------------------------------------------

  # convert entered stats type to a standard notation
  type <- stats_type_switch(type)

  # make sure both quoted and unquoted arguments are allowed
  c(x, y) %<-% c(ensym(x), ensym(y))

  # creating a data frame
  data %<>%
    select({{ x }}, {{ y }}, .counts = {{ counts }}) %>%
    tidyr::drop_na()

  # untable the data frame based on the count for each observation
  if (".counts" %in% names(data)) data %<>% tidyr::uncount(weights = .counts)

  # x and y need to be a factor; also drop the unused levels of the factors
  data %<>% mutate(across(.fns = ~ droplevels(as.factor(.x))))

  # TO DO: until one-way table is supported by `BayesFactor`
  if (nlevels(data %>% pull({{ y }})) == 1L) c(bf.message, proportion.test) %<-% c(FALSE, FALSE)
  if (type == "bayes") proportion.test <- FALSE

  # statistical analysis ------------------------------------------

  if (results.subtitle) {
    # relevant arguments for statistical tests
    .f.args <- list(
      data                = data,
      x                   = {{ x }},
      y                   = {{ y }},
      conf.level          = conf.level,
      k                   = k,
      paired              = paired,
      ratio               = ratio,
      sampling.plan       = sampling.plan,
      fixed.margin        = fixed.margin,
      prior.concentration = prior.concentration
    )

    subtitle_df <- .eval_f(contingency_table, !!!.f.args, type = type)
    if (!is.null(subtitle_df)) subtitle <- subtitle_df$expression[[1]]

    # preparing Bayes Factor caption
    if (type != "bayes" && bf.message && isFALSE(paired)) {
      caption_df <- .eval_f(contingency_table, !!!.f.args, type = "bayes")
      if (!is.null(caption_df)) caption <- caption_df$expression[[1]]
    }
  }

  # plot ------------------------------------------

  # data frame with summary labels
  descriptive_df <- descriptive_data(data, {{ x }}, {{ y }}, label, perc.k)

  # data frame containing all details needed for prop test
  onesample_df <- onesample_data(data, {{ x }}, {{ y }}, k)

  # if no. of factor levels is greater than the default palette color count
  .palette_message(package, palette, nlevels(data %>% pull({{ x }})))

  # plot
  plotBar <- ggplot(descriptive_df, aes({{ y }}, perc, fill = {{ x }})) +
    geom_bar(stat = "identity", position = "fill", color = "black") +
    scale_y_continuous(
      labels       = function(x) paste0(x * 100, "%"),
      breaks       = seq(from = 0, to = 1, by = 0.10),
      minor_breaks = seq(from = 0.05, to = 0.95, by = 0.10)
    ) +
    exec(
      geom_label,
      mapping  = aes(label = .label, group = {{ x }}),
      position = position_fill(vjust = 0.5),
      !!!label.args
    ) +
    ggtheme +
    theme(panel.grid.major.x = element_blank()) +
    guides(fill = guide_legend(title = legend.title %||% as_name(x))) +
    paletteer::scale_fill_paletteer_d(paste0(package, "::", palette), name = "")

  # sample size + proportion test ------------------------------------------

  # adding significance labels to bars for proportion tests
  if (isTRUE(proportion.test)) {
    plotBar <- plotBar +
      geom_text(
        data    = onesample_df,
        mapping = aes(x = {{ y }}, y = 1.05, label = .p.label, fill = NULL),
        size    = 2.8,
        parse   = TRUE
      )
  }

  # adding sample size info
  plotBar <- plotBar +
    geom_text(
      data    = onesample_df,
      mapping = aes(x = {{ y }}, y = -0.05, label = N, fill = NULL),
      size    = 4
    )

  # annotations ------------------------------------------

  plotBar +
    labs(
      x        = xlab %||% as_name(y),
      y        = ylab,
      subtitle = subtitle,
      title    = title,
      caption  = caption
    ) +
    ggplot.component
}

#' @title Grouped bar charts with statistical tests
#' @name grouped_ggbarstats
#'
#' @description
#'
#' Helper function for `ggstatsplot::ggbarstats` to apply this function across
#' multiple levels of a given factor and combining the resulting plots using
#' `ggstatsplot::combine_plots`.
#'
#' @inheritParams ggbarstats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggbarstats -title
#'
#' @seealso \code{\link{ggbarstats}}, \code{\link{ggpiestats}},
#'  \code{\link{grouped_ggpiestats}}
#'
#' @inherit ggbarstats return references
#' @inherit ggbarstats return details
#' @inherit ggbarstats return return
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#'
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # let's create a smaller data frame
#' diamonds_short <- ggplot2::diamonds %>%
#'   filter(cut %in% c("Very Good", "Ideal")) %>%
#'   filter(clarity %in% c("SI1", "SI2", "VS1", "VS2")) %>%
#'   sample_frac(size = 0.05)
#'
#' # plot
#' grouped_ggbarstats(
#'   data          = diamonds_short,
#'   x             = color,
#'   y             = clarity,
#'   grouping.var  = cut,
#'   plotgrid.args = list(nrow = 2)
#' )
#' }
#' @export
grouped_ggbarstats <- function(data,
                               ...,
                               grouping.var,
                               plotgrid.args = list(),
                               annotation.args = list()) {
  # creating a data frame
  data %<>% .grouped_list(grouping.var = {{ grouping.var }})

  # creating a list of return objects
  p_ls <- purrr::pmap(
    .l = list(data = data, title = names(data)),
    .f = ggstatsplot::ggbarstats,
    ...
  )

  combine_plots(p_ls, plotgrid.args, annotation.args)
}
