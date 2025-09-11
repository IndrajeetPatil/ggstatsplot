#' @title Visualization of a correlation matrix
#' @name ggcorrmat
#'
#' @description
#' Correlation matrix containing results from pairwise correlation tests.
#' If you want a data frame of (grouped) correlation matrix, use
#' [`correlation::correlation()`] instead. It can also do grouped analysis when
#' used with output from [`dplyr::group_by()`].
#'
#' @section Summary of graphics:
#'
#' ```{r child="man/rmd-fragments/ggcorrmat_graphics.Rmd"}
#' ```
#'
#' @param ... Currently ignored.
#' @param data A data frame from which variables specified are to be taken.
#' @param cor.vars List of variables for which the correlation matrix is to be
#'   computed and visualized. If `NULL` (default), all numeric variables from
#'   `data` will be used.
#' @param cor.vars.names Optional list of names to be used for `cor.vars`. The
#'   names should be entered in the same order.
#' @param partial Can be `TRUE` for partial correlations. For Bayesian partial
#'   correlations, "full" instead of pseudo-Bayesian partial correlations (i.e.,
#'   Bayesian correlation based on frequentist partialization) are returned.
#' @param matrix.type Character, `"upper"` (default), `"lower"`, or `"full"`,
#'   display full matrix, lower triangular or upper triangular matrix.
#' @param sig.level Significance level (Default: `0.05`). If the *p*-value in
#'   *p*-value matrix is bigger than `sig.level`, then the corresponding
#'   correlation coefficient is regarded as insignificant and flagged as such in
#'   the plot.
#' @param colors A vector of 3 colors for low, mid, and high correlation values.
#'   If set to `NULL`, manual specification of colors will be turned off and 3
#'   colors from the specified `palette` from `package` will be selected.
#' @param pch Decides the point shape to be used for insignificant correlation
#'   coefficients (only valid when `insig = "pch"`). Default: `pch = "cross"`.
#' @param ggcorrplot.args A list of additional (mostly aesthetic) arguments that
#'   will be passed to [`ggcorrplot::ggcorrplot()`] function. The list should
#'   avoid any of the following arguments since they are already internally
#'   being used: `corr`, `method`, `p.mat`, `sig.level`, `ggtheme`, `colors`,
#'   `lab`, `pch`, `legend.title`, `digits`.
#' @inheritParams statsExpressions::corr_test
#' @inheritParams ggbetweenstats
#' @inheritParams theme_ggstatsplot
#' @inheritParams ggcorrplot::ggcorrplot
#' @inheritParams ggscatterstats
#'
#' @inheritSection statsExpressions::corr_test Correlation analyses
#'
#' @autoglobal
#'
#' @seealso \code{\link{grouped_ggcorrmat}} \code{\link{ggscatterstats}}
#'   \code{\link{grouped_ggscatterstats}}
#'
#' @details For details, see:
#' <https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcorrmat.html>
#'
#' @examples
#' set.seed(123)
#' library(ggcorrplot)
#' ggcorrmat(iris)
#' @export
ggcorrmat <- function(
  data,
  cor.vars = NULL,
  cor.vars.names = NULL,
  matrix.type = "upper",
  type = "parametric",
  tr = 0.2,
  partial = FALSE,
  digits = 2L,
  sig.level = 0.05,
  conf.level = 0.95,
  bf.prior = 0.707,
  p.adjust.method = "holm",
  pch = "cross",
  ggcorrplot.args = list(method = "square", outline.color = "black", pch.cex = 14),
  package = "RColorBrewer",
  palette = "Dark2",
  colors = c("#E69F00", "white", "#009E73"),
  ggtheme = ggstatsplot::theme_ggstatsplot(),
  ggplot.component = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ...
) {
  type <- stats_type_switch(type)
  if (!missing(cor.vars)) data <- select(data, {{ cor.vars }})

  # statistical analysis ------------------------------------------

  mpc_df <- correlation::correlation(
    data             = data,
    rename           = cor.vars.names,
    method           = ifelse(type == "nonparametric", "spearman", "pearson"),
    p_adjust         = p.adjust.method,
    ci               = conf.level,
    bayesian         = type == "bayes",
    bayesian_prior   = bf.prior,
    tr               = tr,
    partial          = partial,
    partial_bayesian = type == "bayes" && partial,
    winsorize        = ifelse(type == "robust", tr, FALSE)
  )

  # type of correlation and if it is a partial correlation
  r.method.text <- gsub(" correlation", "", unique(mpc_df$Method), fixed = TRUE)
  r.type <- ifelse(partial, "correlation (partial):", "correlation:")

  # plot ------------------------------------------

  # legend title with information about correlation type and sample size
  if (!anyNA(data) || partial) {
    legend.title <- bquote(atop(
      atop(scriptstyle(bold("sample sizes:")), italic(n) ~ "=" ~ .(.prettyNum(mpc_df$n_Obs[[1L]]))),
      atop(scriptstyle(bold(.(r.type))), .(r.method.text))
    ))
  } else {
    legend.title <- bquote(atop(
      atop(
        atop(scriptstyle(bold("sample sizes:")), italic(n)[min] ~ "=" ~ .(.prettyNum(min(mpc_df$n_Obs)))),
        atop(
          italic(n)[mode] ~ "=" ~ .(.prettyNum(datawizard::distribution_mode(mpc_df$n_Obs))),
          italic(n)[max] ~ "=" ~ .(.prettyNum(max(mpc_df$n_Obs)))
        )
      ),
      atop(scriptstyle(bold(.(r.type))), .(r.method.text))
    ))
  }

  plot_corr <- .eval_f(
    ggcorrplot::ggcorrplot,
    corr         = as.matrix(select(mpc_df, matches("^parameter|^r"))),
    p.mat        = as.matrix(select(mpc_df, matches("^parameter|^p"))),
    sig.level    = ifelse(type == "bayes", Inf, sig.level),
    ggtheme      = ggtheme,
    colors       = colors %||% paletteer::paletteer_d(paste0(package, "::", palette), 3L),
    type         = matrix.type,
    lab          = TRUE,
    pch          = pch,
    legend.title = legend.title,
    digits       = digits,
    !!!ggcorrplot.args
  )

  # p-value adjustment message ------------------------------------------

  if ((pch == "cross" || pch == 4L) && type != "bayes") {
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

  # annotations ------------------------------------------

  plot_corr +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title     = element_text(size = 15)
    ) +
    labs(
      title    = title,
      subtitle = subtitle,
      caption  = caption,
      x        = NULL,
      y        = NULL
    ) +
    ggplot.component
}


#' @title Visualization of a correlalogram (or correlation matrix) for all
#'   levels of a grouping variable
#' @name grouped_ggcorrmat
#'
#' @description
#'
#' Helper function for `ggstatsplot::ggcorrmat()` to apply this function across
#' multiple levels of a given factor and combining the resulting plots using
#' `ggstatsplot::combine_plots()`.
#'
#' @inheritParams ggcorrmat
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggcorrmat -title
#'
#' @autoglobal
#'
#' @seealso \code{\link{ggcorrmat}}, \code{\link{ggscatterstats}},
#'   \code{\link{grouped_ggscatterstats}}
#'
#' @inherit ggcorrmat return references
#' @inherit ggcorrmat return details
#'
#' @examples
#' set.seed(123)
#'
#' grouped_ggcorrmat(
#'   data = iris,
#'   grouping.var = Species,
#'   type = "robust",
#'   p.adjust.method = "holm",
#'   plotgrid.args = list(ncol = 1L),
#'   annotation.args = list(tag_levels = "i")
#' )
#' @export
grouped_ggcorrmat <- function(
  data,
  ...,
  grouping.var,
  plotgrid.args = list(),
  annotation.args = list()
) {
  .grouped_list(data, {{ grouping.var }}) %>%
    purrr::pmap(.f = ggcorrmat, ...) %>%
    # `guides = "keep"` because legends can be different across grouping levels
    combine_plots(guides = "keep", plotgrid.args, annotation.args)
}
