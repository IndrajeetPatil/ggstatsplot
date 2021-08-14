#' @title Box/Violin plots for within-subjects (or repeated measures) comparisons
#' @name ggwithinstats
#'
#' @description
#'
#' A combination of box and violin plots along with raw (unjittered) data points
#' for within-subjects designs with statistical details included in the plot as
#' a subtitle.
#'
#' @note
#'
#' To carry out Bayesian analysis for ANOVA designs, you will need to install
#' the development version of `BayesFactor` (`0.9.12-4.3`). You can download it
#' by running:
#' `remotes::install_github("richarddmorey/BayesFactor/pkg/BayesFactor")`.
#'
#' @inheritParams ggbetweenstats
#' @param point.path,centrality.path Logical that decides whether individual data
#'   points and means, respectively, should be connected using `geom_path`. Both
#'   default to `TRUE`. Note that `point.path` argument is relevant only when
#'   there are two groups (i.e., in case of a *t*-test). In case of large number
#'   of data points, it is advisable to set `point.path = FALSE` as these lines
#'   can overwhelm the plot.
#' @param centrality.path.args,point.path.args A list of additional aesthetic
#'   arguments passed on to `geom_path` connecting raw data points and mean
#'   points.
#' @param boxplot.args A list of additional aesthetic arguments passed on to
#'   `geom_boxplot`.
#' @inheritParams statsExpressions::oneway_anova
#'
#' @seealso \code{\link{grouped_ggbetweenstats}}, \code{\link{ggbetweenstats}},
#'  \code{\link{grouped_ggwithinstats}}
#'
#' @importFrom pairwiseComparisons pairwise_comparisons pairwise_caption
#' @importFrom dplyr select mutate row_number group_by ungroup anti_join
#' @importFrom ggplot2 ggplot aes geom_point geom_boxplot geom_violin geom_path
#'
#' @details For details, see:
#' <https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggwithinstats.html>
#'
#' @examples
#' \donttest{
#' # setup
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # two groups (*t*-test)
#' ggwithinstats(
#'   data = VR_dilemma,
#'   x = modality,
#'   y = score,
#'   xlab = "Presentation modality",
#'   ylab = "Proportion of utilitarian decisions"
#' )
#'
#' # more than two groups (anova)
#' library(WRS2)
#'
#' ggwithinstats(
#'   data = WineTasting,
#'   x = Wine,
#'   y = Taste,
#'   type = "r",
#'   outlier.tagging = TRUE,
#'   outlier.label = Taster
#' )
#' }
#' @export

# defining the function
ggwithinstats <- function(data,
                          x,
                          y,
                          type = "parametric",
                          pairwise.comparisons = TRUE,
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
                          conf.level = 0.95,
                          nboot = 100L,
                          tr = 0.2,
                          centrality.plotting = TRUE,
                          centrality.type = type,
                          centrality.point.args = list(
                            size = 5,
                            color = "darkred"
                          ),
                          centrality.label.args = list(
                            size = 3,
                            nudge_x = 0.4,
                            segment.linetype = 4
                          ),
                          centrality.path = TRUE,
                          centrality.path.args = list(
                            size = 1,
                            color = "red",
                            alpha = 0.5
                          ),
                          point.args = list(
                            size = 3,
                            alpha = 0.5
                          ),
                          point.path = TRUE,
                          point.path.args = list(
                            alpha = 0.5,
                            linetype = "dashed"
                          ),
                          outlier.tagging = FALSE,
                          outlier.label = NULL,
                          outlier.coef = 1.5,
                          outlier.label.args = list(size = 3),
                          boxplot.args = list(
                            width = 0.2,
                            alpha = 0.5
                          ),
                          violin.args = list(
                            width = 0.5,
                            alpha = 0.2
                          ),
                          ggsignif.args = list(
                            textsize = 3,
                            tip_length = 0.01
                          ),
                          ggtheme = ggstatsplot::theme_ggstatsplot(),
                          package = "RColorBrewer",
                          palette = "Dark2",
                          ggplot.component = NULL,
                          output = "plot",
                          ...) {

  # data -----------------------------------

  # ensure the variables work quoted or unquoted
  c(x, y) %<-% c(rlang::ensym(x), rlang::ensym(y))
  if (!quo_is_null(enquo(outlier.label))) rlang::ensym(outlier.label)

  # convert entered stats type to a standard notation
  type <- statsExpressions::stats_type_switch(type)

  # creating a dataframe
  data %<>%
    dplyr::select({{ x }}, {{ y }}, outlier.label = {{ outlier.label }}) %>%
    dplyr::mutate({{ x }} := droplevels(as.factor({{ x }}))) %>%
    dplyr::group_by({{ x }}) %>%
    dplyr::mutate(.rowid = dplyr::row_number()) %>%
    dplyr::ungroup(.) %>%
    dplyr::anti_join(x = ., y = dplyr::filter(., is.na({{ y }})), by = ".rowid")

  # if `outlier.label` column is not present, just use the values from `y` column
  if (!"outlier.label" %in% names(data)) data %<>% dplyr::mutate(outlier.label = {{ y }})

  # add a logical column indicating whether a point is or is not an outlier
  data %<>%
    outlier_df(
      x = {{ x }},
      y = {{ y }},
      outlier.coef = outlier.coef,
      outlier.label = outlier.label
    )

  # statistical analysis ------------------------------------------

  # test to run; depends on the no. of levels of the independent variable
  test <- ifelse(nlevels(data %>% dplyr::pull({{ x }})) < 3, "t", "anova")

  if (results.subtitle && insight::check_if_installed("afex")) {
    # relevant arguments for statistical tests
    .f.args <- list(
      data = data,
      x = rlang::as_string(x),
      y = rlang::as_string(y),
      effsize.type = effsize.type,
      conf.level = conf.level,
      k = k,
      tr = tr,
      paired = TRUE,
      bf.prior = bf.prior,
      nboot = nboot,
      top.text = caption
    )

    .f <- function_switch(test)
    subtitle_df <- eval_f(.f, !!!.f.args, type = type)
    subtitle <- if (!is.null(subtitle_df)) subtitle_df$expression[[1]]

    # preparing the Bayes factor message
    if (type == "parametric" && bf.message) {
      caption_df <- eval_f(.f, !!!.f.args, type = "bayes")
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

  # plot -------------------------------------------

  # plot
  plot <- ggplot(data, aes({{ x }}, {{ y }}, group = .rowid)) +
    exec(geom_point, aes(color = {{ x }}), !!!point.args) +
    exec(geom_boxplot, aes({{ x }}, {{ y }}), inherit.aes = FALSE, !!!boxplot.args) +
    exec(geom_violin, aes({{ x }}, {{ y }}), inherit.aes = FALSE, !!!violin.args)

  # add a connecting path only if there are only two groups
  if (test == "t" && point.path) plot <- plot + exec(geom_path, !!!point.path.args)

  # outlier labeling -----------------------------

  # If `outlier.label` is not provided, outlier labels will just be values of
  # the `y` vector. If the outlier tag has been provided, just use the dataframe
  # already created.

  if (isTRUE(outlier.tagging)) {
    # applying the labels to tagged outliers with `ggrepel`
    plot <- plot +
      exec(
        .fn = ggrepel::geom_label_repel,
        data = ~ dplyr::filter(.x, isanoutlier),
        mapping = aes(x = {{ x }}, y = {{ y }}, label = outlier.label),
        min.segment.length = 0,
        inherit.aes = FALSE,
        !!!outlier.label.args
      )
  }

  # centrality tagging -------------------------------------

  # add labels for mean values
  if (isTRUE(centrality.plotting)) {
    plot <- centrality_ggrepel(
      plot = plot,
      data = data,
      x = {{ x }},
      y = {{ y }},
      k = k,
      type = statsExpressions::stats_type_switch(centrality.type),
      tr = tr,
      centrality.path = centrality.path,
      centrality.path.args = centrality.path.args,
      centrality.point.args = centrality.point.args,
      centrality.label.args = centrality.label.args
    )
  }

  # ggsignif labels -------------------------------------

  if (isTRUE(pairwise.comparisons) && test == "anova") {
    # creating dataframe with pairwise comparison results
    mpc_df <- pairwiseComparisons::pairwise_comparisons(
      data = data,
      x = {{ x }},
      y = {{ y }},
      type = type,
      tr = tr,
      paired = TRUE,
      p.adjust.method = p.adjust.method,
      k = k
    )

    # adding the layer for pairwise comparisons
    plot <- ggsignif_adder(
      plot = plot,
      mpc_df = mpc_df,
      data = data,
      x = {{ x }},
      y = {{ y }},
      pairwise.display = pairwise.display,
      ggsignif.args = ggsignif.args
    )

    # preparing the caption for pairwise comparisons test
    caption <- pairwiseComparisons::pairwise_caption(
      caption,
      unique(mpc_df$test.details),
      ifelse(type == "bayes", "all", pairwise.display)
    )
  }

  # annotations -------------------------

  # specifying annotations and other aesthetic aspects for the plot
  aesthetic_addon(
    plot = plot,
    x = data %>% dplyr::pull({{ x }}),
    xlab = xlab %||% rlang::as_name(x),
    ylab = ylab %||% rlang::as_name(y),
    title = title,
    subtitle = subtitle,
    caption = caption,
    ggtheme = ggtheme,
    package = package,
    palette = palette,
    ggplot.component = ggplot.component
  )
}


#' @title Violin plots for group or condition comparisons in within-subjects
#'   designs repeated across all levels of a grouping variable.
#' @name grouped_ggwithinstats
#' @description A combined plot of comparison plot created for levels of a
#'   grouping variable.
#'
#' @inheritParams ggwithinstats
#' @inheritDotParams ggwithinstats -title
#' @inheritParams grouped_ggbetweenstats
#'
#' @import ggplot2
#'
#' @importFrom dplyr select
#' @importFrom rlang enquo quo_name ensym
#' @importFrom purrr pmap
#'
#' @seealso \code{\link{ggwithinstats}}, \code{\link{ggbetweenstats}},
#' \code{\link{grouped_ggbetweenstats}}
#'
#' @inherit ggwithinstats return references
#'
#' @examples
#' \donttest{
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # the most basic function call
#' grouped_ggwithinstats(
#'   data = VR_dilemma,
#'   x = modality,
#'   y = score,
#'   grouping.var = order,
#'   type = "np", # non-parametric test
#'   # additional modifications for **each** plot using `ggplot2` functions
#'   ggplot.component = ggplot2::scale_y_continuous(
#'     breaks = seq(0, 1, 0.1),
#'     limits = c(0, 1)
#'   )
#' )
#' }
#' @export

# defining the function
grouped_ggwithinstats <- function(data,
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
    .f = ggstatsplot::ggwithinstats,
    ...
  )

  # combining the list of plots into a single plot
  if (output == "plot") p_ls <- combine_plots(p_ls, plotgrid.args, annotation.args)

  # return the object
  p_ls
}
