#' @title Visualization of a correlation matrix
#' @name ggcorrmat
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{rlang:::lifecycle("maturing")}
#'
#' Correlation matrix plot or a dataframe containing results from pairwise
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
#' @inheritParams statsExpressions::expr_corr_test
#' @inheritParams ggbetweenstats
#' @inheritParams theme_ggstatsplot
#' @inheritParams ggcorrplot::ggcorrplot
#' @inheritParams ggscatterstats
#'
#' @import ggplot2
#'
#' @importFrom ggcorrplot ggcorrplot
#' @importFrom dplyr select matches
#' @importFrom purrr is_bare_numeric keep
#' @importFrom rlang !! enquo quo_name is_null
#' @importFrom pairwiseComparisons p_adjust_text
#' @importFrom statsExpressions correlation
#' @importFrom parameters standardize_names
#'
#' @seealso \code{\link{grouped_ggcorrmat}} \code{\link{ggscatterstats}}
#'   \code{\link{grouped_ggscatterstats}}
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcorrmat.html}
#'
#' @examples
#' \donttest{
#' # for reproducibility
#' set.seed(123)
#'
#' # if `cor.vars` not specified, all numeric variables used
#' ggstatsplot::ggcorrmat(iris)
#'
#' # to get the correlalogram
#' # note that the function will run even if the vector with variable names is
#' # not of same length as the number of variables
#' ggstatsplot::ggcorrmat(
#'   data = ggplot2::msleep,
#'   type = "robust",
#'   cor.vars = sleep_total:bodywt,
#'   cor.vars.names = c("total sleep", "REM sleep"),
#'   matrix.type = "lower"
#' )
#'
#' # to get the correlation analyses results in a dataframe
#' ggstatsplot::ggcorrmat(
#'   data = ggplot2::msleep,
#'   cor.vars = sleep_total:bodywt,
#'   partial = TRUE,
#'   output = "dataframe"
#' )
#' }
#' @export

# defining the function
ggcorrmat <- function(data,
                      cor.vars = NULL,
                      cor.vars.names = NULL,
                      output = "plot",
                      matrix.type = "upper",
                      type = "parametric",
                      beta = 0.1,
                      partial = FALSE,
                      k = 2L,
                      sig.level = 0.05,
                      conf.level = 0.95,
                      bf.prior = 0.707,
                      p.adjust.method = "holm",
                      pch = "cross",
                      ggcorrplot.args = list(method = "square", outline.color = "black"),
                      package = "RColorBrewer",
                      palette = "Dark2",
                      colors = c("#E69F00", "white", "#009E73"),
                      ggtheme = ggplot2::theme_bw(),
                      ggstatsplot.layer = TRUE,
                      ggplot.component = NULL,
                      title = NULL,
                      subtitle = NULL,
                      caption = NULL,
                      ...) {

  # ======================= dataframe ========================================

  # creating a dataframe out of the entered variables
  if (missing(cor.vars)) {
    df <- purrr::keep(.x = data, .p = purrr::is_bare_numeric)
  } else {
    df <- dplyr::select(.data = data, {{ cor.vars }})
  }

  # renaming the columns if so desired
  if (!is.null(cor.vars.names)) {
    # check if number of cor.vars is equal to the number of names entered
    if (length(df) != length(cor.vars.names)) {
      message("Warning: Mismatch between number of variables and names.")
    } else {
      colnames(df) <- cor.vars.names
    }
  }

  # ============================ checking r.method =======================

  # if any of the abbreviations have been entered, change them
  stats_type <- ipmisc::stats_type_switch(type)

  # see which method was used to specify type of correlation
  # create unique name for each method
  c(r.method, r.method.text) %<-%
    switch(
      EXPR = stats_type,
      "parametric" = c("pearson", "Pearson"),
      "nonparametric" = c("spearman", "Spearman"),
      "robust" = c("percentage", "robust (% bend)"),
      "bayes" = c("pearson", "Pearson (Bayesian)")
    )

  # is it a partial correlation?
  corr.nature <- ifelse(isTRUE(partial), "correlation (partial):", "correlation:")

  # ===================== statistics ========================================

  # creating a dataframe of results
  df_corr <-
    statsExpressions::correlation(
      data = df,
      method = r.method,
      p_adjust = p.adjust.method,
      ci = conf.level,
      bayesian = ifelse(stats_type == "bayes", TRUE, FALSE),
      bayesian_prior = bf.prior,
      bayesian_test = c("pd", "rope", "bf"),
      beta = beta,
      partial = partial,
      partial_bayesian = ifelse(stats_type == "bayes" && isTRUE(partial), TRUE, FALSE)
    )

  # early stats return
  if (output != "plot") {
    return(as_tibble(parameters::standardize_names(df_corr, "broom")))
  }

  # ========================== plot =========================================

  # create matrices for correlation coefficients and p-values
  corr.mat <- as.matrix(dplyr::select(df_corr, dplyr::matches("^parameter|^r")))
  p.mat <- as.matrix(dplyr::select(df_corr, dplyr::matches("^parameter|^p")))

  # creating the basic plot
  # if user has not specified colors, then use a color palette
  if (is.null(colors)) colors <- paletteer::paletteer_d(paste0(package, "::", palette), 3L)

  # in case of NAs, compute minimum and maximum sample sizes of pairs
  # also compute mode
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  # legend title with information about correlation type and sample
  if (isFALSE(any(is.na(df))) || isTRUE(partial)) {
    legend.title.text <-
      bquote(atop(
        atop(
          scriptstyle(bold("sample sizes:")), italic(n) ~ "=" ~ .(.prettyNum(df_corr$n_Obs[[1]]))
        ),
        atop(
          scriptstyle(bold(.(corr.nature))), .(r.method.text)
        )
      ))
  } else {
    # creating legend with sample size info
    legend.title.text <-
      bquote(atop(
        atop(
          atop(
            scriptstyle(bold("sample sizes:")), italic(n)[min] ~ "=" ~ .(.prettyNum(min(df_corr$n_Obs)))
          ),
          atop(
            italic(n)[mode] ~ "=" ~ .(.prettyNum(getmode(df_corr$n_Obs))),
            italic(n)[max] ~ "=" ~ .(.prettyNum(max(df_corr$n_Obs)))
          )
        ),
        atop(
          scriptstyle(bold(.(corr.nature))), .(r.method.text)
        )
      ))
  }

  # special treatment for Bayes
  if (stats_type == "bayes") sig.level <- Inf

  # plotting the correlalogram
  plot <-
    rlang::exec(
      .f = ggcorrplot::ggcorrplot,
      corr = corr.mat,
      p.mat = p.mat,
      sig.level = sig.level,
      ggtheme = ggtheme,
      colors = colors,
      type = matrix.type,
      lab = TRUE,
      pch = pch,
      legend.title = legend.title.text,
      digits = k,
      !!!ggcorrplot.args
    )

  # =========================== labels ==================================

  # preparing the `pch` caption
  if ((pch == "cross" || pch == 4) && stats_type != "bayes") {
    caption <-
      substitute(
        atop(
          displaystyle(top.text),
          expr = paste(
            bold("X"),
            " = non-significant at ",
            italic("p"),
            " < ",
            sig.level,
            " (Adjustment: ",
            adj_text,
            ")"
          )
        ),
        env = list(
          sig.level = sig.level,
          adj_text = pairwiseComparisons::p_adjust_text(p.adjust.method),
          top.text = caption
        )
      )
  }

  # adding text details to the plot
  plot <- plot +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      xlab = NULL,
      ylab = NULL
    )

  # adding `ggstatsplot` theme for correlation matrix
  if (isTRUE(ggstatsplot.layer)) plot <- plot + theme_corrmat()

  # if any additional modification needs to be made to the plot
  # this is primarily useful for grouped_ variant of this function
  plot + ggplot.component
}
