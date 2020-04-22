#' @title Visualization of a correlation matrix
#' @name ggcorrmat
#' @return Correlation matrix plot or a dataframe containing results from
#'   pairwise correlation tests. The package internally uses
#'   `ggcorrplot::ggcorrplot` for creating the visualization matrix, while the
#'   correlation analysis is carried out using the `correlation::correlation`
#'   function.
#'
#' @param ... Currently ignored.
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param cor.vars List of variables for which the correlation matrix is to be
#'   computed and visualized. If `NULL` (default), all numeric variables from
#'   `data` will be used.
#' @param cor.vars.names Optional list of names to be used for `cor.vars`. The
#'   names should be entered in the same order.
#' @param output Character that decides expected output from this function. If
#'   `"plot"`, the visualization matrix will be returned. If `"dataframe"` (or
#'   literally anything other than `"plot"`), a dataframe containing all details
#'   from statistical analyses (e.g., correlation coefficients, statistic
#'   values, *p*-values, no. of observations, etc.) will be returned.
#' @param matrix.type Character, `"full"` (default), `"upper"` or `"lower"`,
#'   display full matrix, lower triangular or upper triangular matrix.
#' @param sig.level Significance level (Default: `0.05`). If the *p*-value in
#'   *p*-value matrix is bigger than `sig.level`, then the corresponding
#'   correlation coefficient is regarded as insignificant and flagged as such in
#'   the plot. Relevant only when `output = "plot"`.
#' @param p.adjust.method What adjustment for multiple tests should be used?
#'   (`"holm"`, `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`,
#'   `"fdr"`, `"none"`). See `stats::p.adjust` for details about why to use
#'   `"holm"` rather than `"bonferroni"`). Default is `"none"`. If adjusted
#'   *p*-values are displayed in the visualization of correlation matrix, the
#'   **adjusted** *p*-values will be used for the **upper** triangle, while
#'   **unadjusted** *p*-values will be used for the **lower** triangle of the
#'   matrix.
#' @param colors A vector of 3 colors for low, mid, and high correlation values.
#'   If set to `NULL`, manual specification of colors will be turned off and 3
#'   colors from the specified `palette` from `package` will be selected.
#' @param caption The text for the plot caption. If `NULL`, a default caption
#'   will be shown.
#' @param pch Decides the glyphs (read point shapes) to be used for
#'   insignificant correlation coefficients (only valid when `insig = "pch"`).
#'   Default value is `pch = 4`.
#' @param ggcorrplot.args A list of additional (mostly aesthetic) arguments that
#'   will be passed to `ggcorrplot::ggcorrplot` function. The list should avoid
#'   any of the following arguments since they are already internally being used
#'   by `ggstatsplot`: `corr`, `method`, `p.mat`, `sig.level`, `ggtheme`,
#'   `colors`, `matrix.type`, `lab`, `pch`, `legend.title`, `digits`.
#' @inheritParams statsExpressions::expr_corr_test
#' @inheritParams theme_ggstatsplot
#' @inheritParams ggscatterstats
#' @inheritParams ggbetweenstats
#'
#' @import ggplot2
#'
#' @importFrom ggcorrplot ggcorrplot
#' @importFrom dplyr select
#' @importFrom purrr is_bare_numeric keep
#' @importFrom rlang !! enquo quo_name is_null
#' @importFrom ipmisc green blue yellow red
#' @importFrom pairwiseComparisons p_adjust_text
#' @importFrom correlation correlation
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
#'   output = "dataframe"
#' )
#' }
#' @export

# defining the function
ggcorrmat <- function(data,
                      cor.vars = NULL,
                      cor.vars.names = NULL,
                      output = "plot",
                      matrix.type = "full",
                      method = "square",
                      type = "parametric",
                      beta = 0.1,
                      k = 2,
                      sig.level = 0.05,
                      conf.level = 0.95,
                      bf.prior = 0.707,
                      p.adjust.method = "none",
                      pch = 4,
                      ggcorrplot.args = list(outline.color = "black"),
                      package = "RColorBrewer",
                      palette = "Dark2",
                      direction = 1,
                      colors = c("#E69F00", "white", "#009E73"),
                      ggtheme = ggplot2::theme_bw(),
                      ggstatsplot.layer = TRUE,
                      title = NULL,
                      subtitle = NULL,
                      caption = NULL,
                      messages = TRUE,
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
      # display a warning message if not
      message(cat(
        ipmisc::red("Warning: "),
        ipmisc::blue("No. of variable names doesn't equal no. of variables.\n"),
        sep = ""
      ))
    } else {
      # otherwise rename the columns with the new names
      colnames(df) <- cor.vars.names
    }
  }

  # ============================ checking corr.method =======================

  # see which method was used to specify type of correlation
  stats_type <- ipmisc::stats_type_switch(type)

  # if any of the abbreviations have been entered, change them
  corr.method <-
    switch(
      EXPR = stats_type,
      "parametric" = "pearson",
      "nonparametric" = "spearman",
      "robust" = "percentage",
      "bayes" = "pearson"
    )

  # create unique name for each method
  corr.method.text <-
    switch(
      EXPR = corr.method,
      "pearson" = "Pearson",
      "spearman" = "Spearman",
      "percentage" = "robust (% bend)",
      "bayes" = "Pearson"
    )

  # compute confidence intervals only when requested by the user
  bayesian <- ifelse(stats_type == "bayes", yes = TRUE, no = FALSE)

  # ===================== statistics ========================================

  # creating a dataframe of results
  df_correlation <-
    correlation::correlation(
      data = df,
      method = corr.method,
      p_adjust = p.adjust.method,
      ci = conf.level,
      bayesian = bayesian,
      bayesian_prior = bf.prior,
      bayesian_test = c("pd", "rope", "bf"),
      beta = beta
    )

  # early stats return
  if (output != "plot") {
    return(tibble::as_tibble(df_correlation) %>%
      dplyr::rename_all(., tolower) %>%
      dplyr::rename(., nobs = n_obs))
  }

  # ========================== plot =========================================

  # create matrices for correlation coefficients and p-values
  corr.mat <-
    df_correlation %>%
    dplyr::select(dplyr::matches("^parameter|^r")) %>%
    as.matrix()

  p.mat <-
    df_correlation %>%
    dplyr::select(dplyr::matches("^parameter|^p")) %>%
    as.matrix()

  # creating the basic plot
  # if user has not specified colors, then use a color palette
  if (is.null(colors)) {
    colors <-
      paletteer::paletteer_d(
        palette = paste0(package, "::", palette),
        n = 3,
        direction = direction,
        type = "discrete"
      )
  }

  # in case of NAs, compute minimum and maximum sample sizes of pairs
  # also compute mode
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  # legend title with information about correlation type and sample
  if (isFALSE(any(is.na(df)))) {
    legend.title.text <-
      bquote(atop(
        atop(
          scriptstyle(bold("sample size:")),
          italic(n) ~ "=" ~ .(nrow(df))
        ),
        atop(
          scriptstyle(bold("correlation:")),
          .(corr.method.text)
        )
      ))
  } else {
    # creating legend with sample size info
    legend.title.text <-
      bquote(atop(
        atop(
          atop(
            scriptstyle(bold("sample size:")),
            italic(n)[min] ~ "=" ~ .(min(df_correlation$n_Obs))
          ),
          atop(
            italic(n)[mode] ~ "=" ~ .(getmode(df_correlation$n_Obs)),
            italic(n)[max] ~ "=" ~ .(max(df_correlation$n_Obs))
          )
        ),
        atop(
          scriptstyle(bold("correlation:")),
          .(corr.method.text)
        )
      ))
  }

  # plotting the correlalogram
  plot <-
    rlang::exec(
      .f = ggcorrplot::ggcorrplot,
      corr = corr.mat,
      method = method,
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
  if (pch == 4) {
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

  # return the desired result
  return(plot)
}
