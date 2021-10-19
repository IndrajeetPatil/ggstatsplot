#' @title Visualization of a correlation matrix
#' @name ggcorrmat
#'
#' @description
#'
#' Correlation matrix or a dataframe containing results from pairwise
#' correlation tests. The package internally uses `ggcorrplot::ggcorrplot` for
#' creating the visualization matrix, while the correlation analysis is carried
#' out using the `correlation::correlation` function.
#'
#' @param ... Currently ignored.
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param cor.vars List of variables for which the correlation matrix is to be
#'   computed and visualized. If `NULL` (default), all numeric variables from
#'   `data` will be used.
#' @param cor.vars.names Optional list of names to be used for `cor.vars`. The
#'   names should be entered in the same order.
#' @param partial Can be `TRUE` for partial correlations. For Bayesian partial
#'   correlations, "full" instead of pseudo-Bayesian partial correlations (i.e.,
#'   Bayesian correlation based on frequentist partialization) are returned.
#' @param output Character that decides expected output from this function. If
#'   `"plot"`, the visualization matrix will be returned. If `"dataframe"` (or
#'   literally anything other than `"plot"`), a dataframe containing all details
#'   from statistical analyses (e.g., correlation coefficients, statistic
#'   values, *p*-values, no. of observations, etc.) will be returned.
#' @param matrix.type Character, `"upper"` (default), `"lower"`, or `"full"`,
#'   display full matrix, lower triangular or upper triangular matrix.
#' @param sig.level Significance level (Default: `0.05`). If the *p*-value in
#'   *p*-value matrix is bigger than `sig.level`, then the corresponding
#'   correlation coefficient is regarded as insignificant and flagged as such in
#'   the plot. Relevant only when `output = "plot"`.
#' @param colors A vector of 3 colors for low, mid, and high correlation values.
#'   If set to `NULL`, manual specification of colors will be turned off and 3
#'   colors from the specified `palette` from `package` will be selected.
#' @param pch Decides the point shape to be used for insignificant correlation
#'   coefficients (only valid when `insig = "pch"`). Default: `pch = "cross"`.
#' @param ggcorrplot.args A list of additional (mostly aesthetic) arguments that
#'   will be passed to `ggcorrplot::ggcorrplot` function. The list should avoid
#'   any of the following arguments since they are already internally being
#'   used: `corr`, `method`, `p.mat`, `sig.level`, `ggtheme`, `colors`, `lab`,
#'   `pch`, `legend.title`, `digits`.
#' @inheritParams statsExpressions::corr_test
#' @inheritParams ggbetweenstats
#' @inheritParams theme_ggstatsplot
#' @inheritParams ggcorrplot::ggcorrplot
#' @inheritParams ggscatterstats
#'
#' @importFrom dplyr select matches
#' @importFrom purrr is_bare_numeric keep
#' @importFrom correlation correlation
#'
#' @seealso \code{\link{grouped_ggcorrmat}} \code{\link{ggscatterstats}}
#'   \code{\link{grouped_ggscatterstats}}
#'
#' @details For details, see:
#' <https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcorrmat.html>
#'
#' @examples
#' # for reproducibility
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # to get a plot (assumes that `ggcorrplot` is installed)
#' if (require("ggcorrplot")) ggcorrmat(iris)
#'
#' # to get a dataframe
#' ggcorrmat(
#'   data = ggplot2::msleep,
#'   cor.vars = sleep_total:bodywt,
#'   partial = TRUE,
#'   output = "dataframe"
#' )
#' @export

# defining the function
ggcorrmat <- function(data,
                      cor.vars = NULL,
                      cor.vars.names = NULL,
                      output = "plot",
                      matrix.type = "upper",
                      type = "parametric",
                      tr = 0.2,
                      partial = FALSE,
                      k = 2L,
                      sig.level = 0.05,
                      conf.level = 0.95,
                      bf.prior = 0.707,
                      p.adjust.method = "holm",
                      pch = "cross",
                      ggcorrplot.args = list(
                        method = "square",
                        outline.color = "black",
                        pch.cex = 14
                      ),
                      package = "RColorBrewer",
                      palette = "Dark2",
                      colors = c("#E69F00", "white", "#009E73"),
                      ggtheme = ggstatsplot::theme_ggstatsplot(),
                      ggplot.component = NULL,
                      title = NULL,
                      subtitle = NULL,
                      caption = NULL,
                      ...) {

  # dataframe -----------------------------------

  # creating a dataframe out of the entered variables
  if (missing(cor.vars)) {
    df <- purrr::keep(.x = data, .p = purrr::is_bare_numeric)
  } else {
    df <- select(data, {{ cor.vars }})
  }

  # statistical analysis ------------------------------------------

  # if any of the abbreviations have been entered, change them
  type <- statsExpressions::stats_type_switch(type)

  # creating a dataframe of results
  stats_df <- correlation::correlation(
    data = df,
    rename = cor.vars.names,
    method = ifelse(type == "nonparametric", "spearman", "pearson"),
    p_adjust = p.adjust.method,
    ci = conf.level,
    bayesian = ifelse(type == "bayes", TRUE, FALSE),
    bayesian_prior = bf.prior,
    tr = tr,
    partial = partial,
    partial_bayesian = ifelse(type == "bayes" && isTRUE(partial), TRUE, FALSE),
    winsorize = ifelse(type == "robust", tr, FALSE)
  )

  # type of correlation and if it is a partial correlation
  r.method.text <- gsub(" correlation", "", unique(stats_df$Method))
  r.type <- ifelse(isTRUE(partial), "correlation (partial):", "correlation:")

  # early stats return
  if (output != "plot") {
    return(as_tibble(parameters::standardize_names(stats_df, "broom")))
  }

  # plot -------------------------------------

  # in case of NAs, compute minimum and maximum sample sizes of pairs
  # also compute mode
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  # legend title with information about correlation type and sample
  if (isFALSE(any(is.na(df))) || isTRUE(partial)) {
    legend.title <- bquote(atop(
      atop(scriptstyle(bold("sample sizes:")), italic(n) ~ "=" ~ .(.prettyNum(stats_df$n_Obs[[1]]))),
      atop(scriptstyle(bold(.(r.type))), .(r.method.text))
    ))
  } else {
    # creating legend with sample size info
    legend.title <- bquote(atop(
      atop(
        atop(scriptstyle(bold("sample sizes:")), italic(n)[min] ~ "=" ~ .(.prettyNum(min(stats_df$n_Obs)))),
        atop(
          italic(n)[mode] ~ "=" ~ .(.prettyNum(getmode(stats_df$n_Obs))),
          italic(n)[max] ~ "=" ~ .(.prettyNum(max(stats_df$n_Obs)))
        )
      ),
      atop(scriptstyle(bold(.(r.type))), .(r.method.text))
    ))
  }

  # installed?
  insight::check_if_installed("ggcorrplot")

  # plotting the correlalogram
  plot <- exec(
    ggcorrplot::ggcorrplot,
    corr = as.matrix(select(stats_df, matches("^parameter|^r"))),
    p.mat = as.matrix(select(stats_df, matches("^parameter|^p"))),
    sig.level = ifelse(type == "bayes", Inf, sig.level),
    ggtheme = ggtheme,
    colors = colors %||% paletteer::paletteer_d(paste0(package, "::", palette), 3L),
    type = matrix.type,
    lab = TRUE,
    pch = pch,
    legend.title = legend.title,
    digits = k,
    !!!ggcorrplot.args
  )

  # annotations ------------------------------------------

  # preparing the `pch` caption
  if ((pch == "cross" || pch == 4) && type != "bayes") {
    caption <- substitute(
      atop(
        displaystyle(top.text),
        expr = paste(
          bold("X"), " = non-significant at ",
          italic("p"), " < ", sig.level, " (Adjustment: ", adj.text, ")"
        )
      ),
      env = list(
        sig.level = sig.level,
        adj.text = p_adjust_text(p.adjust.method),
        top.text = caption
      )
    )
  }

  # adding text details to the plot
  plot +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 15)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      xlab = NULL,
      ylab = NULL
    ) +
    ggplot.component
}


#' @title Visualization of a correlalogram (or correlation matrix) for all
#'   levels of a grouping variable
#' @name grouped_ggcorrmat
#'
#' @description
#'
#' Helper function for `ggstatsplot::ggcorrmat` to apply this function across
#' multiple levels of a given factor and combining the resulting plots using
#' `ggstatsplot::combine_plots`.
#'
#' @inheritParams ggcorrmat
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggcorrmat -title
#'
#' @seealso \code{\link{ggcorrmat}}, \code{\link{ggscatterstats}},
#'   \code{\link{grouped_ggscatterstats}}
#'
#' @inherit ggcorrmat return references
#' @inherit ggcorrmat return details
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#' library(ggstatsplot)
#'
#' # for plot
#' if (require("ggcorrplot")) {
#'   grouped_ggcorrmat(
#'     data = iris,
#'     grouping.var = Species,
#'     type = "robust",
#'     p.adjust.method = "holm",
#'     plotgrid.args = list(ncol = 1),
#'     annotation.args = list(tag_levels = "i")
#'   )
#' }
#'
#' # for dataframe
#' grouped_ggcorrmat(
#'   data = ggplot2::msleep,
#'   grouping.var = vore,
#'   type = "bayes",
#'   output = "dataframe"
#' )
#' }
#' @export

# defining the function
grouped_ggcorrmat <- function(data,
                              ...,
                              grouping.var,
                              output = "plot",
                              plotgrid.args = list(),
                              annotation.args = list()) {

  # dataframe
  data %<>%
    grouped_list({{ grouping.var }}) %>%
    purrr::map(.f = ~ select(.x, -{{ grouping.var }}))

  # creating a list of return objects
  p_ls <- purrr::pmap(
    .l = list(data = data, title = names(data), output = output),
    .f = ggstatsplot::ggcorrmat,
    ...
  )

  # combining the list of plots into a single plot
  if (output == "plot") {
    return(combine_plots(
      plotlist = p_ls,
      guides = "keep", # each legend is going to be different
      plotgrid.args = plotgrid.args,
      annotation.args = annotation.args
    ))
  } else {
    return(bind_rows(p_ls, .id = as_name(ensym(grouping.var))))
  }
}
