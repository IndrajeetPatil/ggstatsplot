#'
#' @title Visualization of a correlalogram (or correlation matrix) for all
#'   levels of a grouping variable
#' @name grouped_ggcorrmat
#' @aliases grouped_ggcorrmat
#' @description Helper function for `ggstatsplot::ggcorrmat` to apply this
#'   function across multiple levels of a given factor and combining the
#'   resulting plots using `ggstatsplot::combine_plots`.
#' @author Indrajeet Patil
#'
#' @param grouping.var Grouping variable.
#' @param title.prefix Character specifying the prefix text for the fixed plot
#'   title (name of each factor level) (Default: `"Group"`).
#' @inheritParams ggcorrmat
#' @inheritDotParams combine_plots
#'
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr mutate_if
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%>%"
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom glue glue
#' @importFrom purrr map
#' @importFrom tidyr nest
#'
#' @seealso \code{\link{ggcorrmat}} \code{\link{ggscatterstats}}
#'   \code{\link{grouped_ggscatterstats}}
#'
#' @inherit ggcorrmat return references
#' @inherit ggcorrmat return details
#'
#'
#' @examples
#' 
#' # for getting correlations
#' ggstatsplot::grouped_ggcorrmat(
#'   data = datasets::iris,
#'   grouping.var = Species,
#'   cor.vars = Sepal.Length:Petal.Width,
#'   output = "plot",
#'   nrow = 3,
#'   ncol = 1
#' )
#' 
#' # for getting correlations
#' ggstatsplot::grouped_ggcorrmat(
#'   data = datasets::iris,
#'   grouping.var = Species,
#'   cor.vars = Sepal.Length:Petal.Width,
#'   output = "correlations"
#' )
#' 
#' # for getting confidence intervals
#' # if robust correlation is selected, confidence intervals will not be
#' # available
#' ggstatsplot::grouped_ggcorrmat(
#'   data = datasets::iris,
#'   grouping.var = Species,
#'   corr.method = "r",
#'   cor.vars = Sepal.Length:Petal.Width,
#'   output = "ci"
#' )
#' @export
#'

# defining the function
grouped_ggcorrmat <- function(data,
                              cor.vars,
                              cor.vars.names = NULL,
                              grouping.var,
                              title.prefix = "Group",
                              output = "plot",
                              type = "full",
                              method = "square",
                              corr.method = "pearson",
                              exact = FALSE,
                              continuity = TRUE,
                              beta = 0.1,
                              digits = 2,
                              sig.level = 0.05,
                              hc.order = FALSE,
                              hc.method = "complete",
                              lab = TRUE,
                              colors = c("#E69F00", "white", "#009E73"),
                              outline.color = "black",
                              ggtheme = ggplot2::theme_bw,
                              ggstatsplot.layer = TRUE,
                              subtitle = NULL,
                              caption = NULL,
                              caption.default = TRUE,
                              lab.col = "black",
                              lab.size = 5,
                              insig = "pch",
                              pch = 4,
                              pch.col = "black",
                              pch.cex = 11,
                              tl.cex = 12,
                              tl.col = "black",
                              tl.srt = 45,
                              axis.text.x.margin.t = 0,
                              axis.text.x.margin.r = 0,
                              axis.text.x.margin.b = 0,
                              axis.text.x.margin.l = 0,
                              messages = TRUE,
                              ...) {
  # ========================================= preparing dataframe ==================================================

  # getting the dataframe ready
  df <- dplyr::select(
    .data = data,
    !!rlang::enquo(grouping.var),
    !!rlang::enquo(cor.vars)
  ) %>%
    dplyr::mutate(
      .data = .,
      title.text = !!rlang::enquo(grouping.var)
    ) %>%
    stats::na.omit(object = .)

  # creating a nested dataframe
  df %<>%
    dplyr::mutate_if(
      .tbl = .,
      .predicate = purrr::is_bare_character,
      .funs = ~as.factor(.)
    ) %>%
    dplyr::mutate_if(
      .tbl = .,
      .predicate = is.factor,
      .funs = ~base::droplevels(.)
    ) %>%
    dplyr::arrange(.data = ., !!rlang::enquo(grouping.var)) %>%
    dplyr::group_by(.data = ., !!rlang::enquo(grouping.var)) %>%
    tidyr::nest(data = .)

  # ========================================= grouped plot ==================================================
  #
  if (output == "plot") {
    # creating a list of plots
    plotlist_purrr <- df %>%
      dplyr::mutate(
        .data = .,
        plots = data %>%
          purrr::set_names(!!rlang::enquo(grouping.var)) %>%
          purrr::map(
            .x = .,
            .f = ~ggstatsplot::ggcorrmat(
              title = glue::glue("{title.prefix}: {as.character(.$title.text)}"),
              data = .,
              cor.vars = !!rlang::enquo(cor.vars),
              cor.vars.names = cor.vars.names,
              output = output,
              type = type,
              method = method,
              corr.method = corr.method,
              exact = exact,
              continuity = continuity,
              beta = beta,
              digits = digits,
              sig.level = sig.level,
              hc.order = hc.order,
              hc.method = hc.method,
              lab = lab,
              colors = colors,
              outline.color = outline.color,
              ggtheme = ggtheme,
              ggstatsplot.layer = ggstatsplot.layer,
              subtitle = subtitle,
              caption = caption,
              caption.default = caption.default,
              lab.col = lab.col,
              lab.size = lab.size,
              insig = insig,
              pch = pch,
              pch.col = pch.col,
              pch.cex = pch.cex,
              tl.cex = tl.cex,
              tl.col = tl.col,
              tl.srt = tl.srt,
              axis.text.x.margin.t = axis.text.x.margin.t,
              axis.text.x.margin.r = axis.text.x.margin.r,
              axis.text.x.margin.b = axis.text.x.margin.b,
              axis.text.x.margin.l = axis.text.x.margin.l,
              messages = messages
            )
          )
      )

    # combining the list of plots into a single plot
    combined_plot <-
      ggstatsplot::combine_plots(
        plotlist = plotlist_purrr$plots,
        ...
      )

    # return the combined plot
    return(combined_plot)
  } else {
    # ========================================= grouped stats tables ==================================================
    #
    # creating a list of plots
    statsdf_purrr <- df %>%
      dplyr::mutate(
        .data = .,
        statsdf = data %>%
          purrr::set_names(!!rlang::enquo(grouping.var)) %>%
          purrr::map(
            .x = .,
            .f = ~ggstatsplot::ggcorrmat(
              data = .,
              cor.vars = !!rlang::enquo(cor.vars),
              cor.vars.names = cor.vars.names,
              output = output,
              type = type,
              method = method,
              corr.method = corr.method,
              exact = exact,
              continuity = continuity,
              beta = beta,
              digits = digits,
              sig.level = sig.level
            )
          )
      )

    # combining all
    final_statsdf <-
      dplyr::bind_rows(statsdf_purrr$statsdf, .id = title.prefix)

    # return the datafrmae
    return(final_statsdf)
  }
}
