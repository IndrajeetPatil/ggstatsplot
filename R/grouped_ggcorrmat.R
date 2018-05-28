#'
#' @title Visualization of a correlalogram (or correlation matrix) using
#'   'ggplot2'/'ggcorrplot' for all levels of a grouping variable
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
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param cor.vars List of variables for which the correlation matrix is to be
#'   computed and visualized.
#' @param cor.vars.names Optional list of names to be used for `cor.vars`. The
#'   names should be entered in the same order.
#' @param output Expected output from this function: `"plot"` (visualization
#'   matrix) or `"correlations"` (correlation matrix) or `"p-values"` (matrix of
#'   p-values).
#' @param type Character, `"full"` (default), `"upper"` or `"lower"`, display
#'   full matrix, lower triangular or upper triangular matrix.
#' @param method Character argument that decides the visualization method of
#'   correlation matrix to be used. Allowed values are `"square"` (default),
#'   `"circle"`
#' @param corr.method A character string indicating which correlation
#'   coefficient is to be computed (`"pearson"` (default) or `"kendall"` or
#'   `"spearman"`). `"robust"` can also be entered but only if `output` argument
#'   is set to either `"correlations"` or `"p-values"`. The robust correlation
#'   used is percentage bend correlation (see `?WRS2::pball`). Abbreviations
#'   will **not** work.
#' @param exact A logical indicating whether an exact *p*-value should be
#'   computed. Used for Kendall's *tau* and Spearman's *rho*. For more details,
#'   see `?stats::cor.test`.
#' @param continuity A logical. If `TRUE`, a continuity correction is used for
#'   Kendall's *tau* and Spearman's *rho* when not computed exactly (Default:
#'   `TRUE`).
#' @param beta A numeric bending constant for robust correlation coefficient
#'   (Default: `0.2`).
#' @param digits Decides the number of decimal digits to be added into the plot
#'   (Default: `2`).
#' @param sig.level Significance level (Default: `0.05`). If the p-value in
#'   p-mat (p-value matrix) is bigger than `sig.level`, then the corresponding
#'   correlation coefficient is regarded as insignificant.
#' @param hc.order Logical value. If `TRUE`, correlation matrix will be
#'   hc.ordered using `hclust` function (Default is `FALSE`).
#' @param hc.method The agglomeration method to be used in `hclust` (see
#'   `?hclust`).
#' @param lab Logical value. If `TRUE`, correlation coefficient values will be
#'   displayed in the plot.
#' @param colors A vector of 3 colors for low, mid, and high correlation values.
#' @param outline.color The outline color of square or circle. Default value is
#'   `"gray"`.
#' @param ggtheme A function, `ggplot2` theme name. Default value is
#'   `ggplot2::theme_gray`. Allowed values are the official `ggplot2` themes,
#'   including `theme_bw`, `theme_minimal`, `theme_classic`, `theme_void`, etc.
#' @param ggstatsplot.theme A logical. Decides whether default theme for
#'   `ggstatsplot`, which is `theme_mprl`, is to be overlaid on the entered
#'   theme (Default: `ggstatsplot.theme = TRUE`).
#' @param subtitle The text for the plot subtitle.
#' @param caption The text for the plot caption. If not specified (if it is
#'   `NULL`, i.e.), a default caption will be shown.
#' @param caption.default Logical decides whether the default caption should be
#'   shown.
#' @param lab.col Color to be used for the correlation coefficient labels
#'   (applicable only when `lab = TRUE`).
#' @param lab.size Size to be used for the correlation coefficient labels
#'   (applicable only when `lab = TRUE`).
#' @param axis.text.x.margin.t,axis.text.x.margin.r,axis.text.x.margin.b,axis.text.x.margin.l
#'   Margins between x-axis and the variable name texts (t: top, r: right, b:
#'   bottom, l:left), especially useful in case the names are slanted, i.e. when the tl.srt is
#'   between `45` and `75` (Defaults: `0`, `0`, `0`, `0`, resp.).
#' @param insig Character used to show specialized insignificant correlation
#'   coefficients (`"pch"` (default) or `"blank"`). If `"blank"`, the
#'   corresponding glyphs will be removed; if "pch" is used, characters (see
#'   `?pch` for details) will be added on the corresponding glyphs.
#' @param pch Decides the glyphs (read point shapes) to be used for insignificant correlation
#'   coefficients (only valid when `insig = "pch"`). Default value is `pch = 4`.
#' @param pch.col,pch.cex The color and the cex (size) of `pch` (only valid when
#'   `insig = "pch"`). Defaults are `pch.col = "#F0E442"` and `pch.cex = 10`.
#' @param tl.cex,tl.col,tl.srt The size, the color, and the string rotation of
#'   text label (variable names, i.e.).
#' @param legend.title.margin Logical indicating whether to adjust the margin between legend title and the
#'   colorbar (Default: `TRUE`).
#' @param t.margin,b.margin Margins in grid units. For more details, see
#'   `?grid::unit()`.
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
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
#' @seealso \code{\link{ggcorrmat}}
#'
#' @note If you are using R Notebook or Markdown and see a blank image being
#'   inserted when a chunk is executed, this behavior can be turned off by
#'   setting `legend.title.margin = FALSE`.
#'
#' @examples
#'
#' # for getting correlations
#' ggstatsplot::grouped_ggcorrmat(
#'  data = datasets::iris,
#'  grouping.var = Species,
#'  cor.vars = Sepal.Length:Petal.Width,
#'  output = "plot",
#'  nrow = 3,
#'  ncol = 1
#' )
#'
#' # for getting correlations
#' ggstatsplot::grouped_ggcorrmat(
#'  data = datasets::iris,
#'  grouping.var = Species,
#'  cor.vars = Sepal.Length:Petal.Width,
#'  output = "correlations"
#' )
#'
#' @export
#'

# defining the function
grouped_ggcorrmat <- function(grouping.var,
                              title.prefix = "Group",
                              data,
                              cor.vars,
                              cor.vars.names = NULL,
                              output = "plot",
                              type = "full",
                              method = "square",
                              corr.method = "pearson",
                              exact = FALSE,
                              continuity = TRUE,
                              beta = 0.2,
                              digits = 2,
                              sig.level = 0.05,
                              hc.order = FALSE,
                              hc.method = "complete",
                              lab = TRUE,
                              colors = c("#E69F00", "white", "#009E73"),
                              outline.color = "black",
                              ggtheme = ggplot2::theme_gray,
                              ggstatsplot.theme = TRUE,
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
                              legend.title.margin = TRUE,
                              t.margin = unit(0, "mm"),
                              b.margin = unit(3, "mm"),
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
    )

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
              ggstatsplot.theme = ggstatsplot.theme,
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
              legend.title.margin = legend.title.margin,
              t.margin = t.margin,
              b.margin = b.margin,
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
