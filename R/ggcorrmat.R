#' @title Visualization of a correlalogram (or correlation matrix)
#' @name ggcorrmat
#' @aliases ggcorrmat
#' @author Indrajeet Patil
#' @return Correlation matrix plot or correlation coefficient matrix or matrix
#'   of *p*-values.
#'
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param cor.vars List of variables for which the correlation matrix is to be
#'   computed and visualized. If `NULL` (default), all numeric variables from
#'   `data` will be used.
#' @param cor.vars.names Optional list of names to be used for `cor.vars`. The
#'   names should be entered in the same order.
#' @param output,return Character that decides expected output from this
#'   function: `"plot"` (for visualization matrix) or `"correlations"` (or
#'   `"corr"` or `"r"`; for correlation matrix) or `"p-values"` (or `"p.values"`
#'   or `"p"`; for a matrix of *p*-values) or `"ci"` (for a tibble with
#'   confidence intervals for unique correlation pairs; not available for robust
#'   correlation) or `"n"` (or `"sample.size"` for a tibble with sample sizes
#'   for each correlation pair).
#' @param matrix.type Character, `"full"` (default), `"upper"` or `"lower"`,
#'   display full matrix, lower triangular or upper triangular matrix.
#' @param method Character argument that decides the visualization method of
#'   correlation matrix to be used. Allowed values are `"square"` (default),
#'   `"circle"`
#' @param corr.method,type A character string indicating which correlation
#'   coefficient is to be computed (`"pearson"` (default) or `"kendall"` or
#'   `"spearman"`). `"robust"` can also be entered but only if `output` argument
#'   is set to either `"correlations"` or `"p-values"`. The robust correlation
#'   used is percentage bend correlation (see `?WRS2::pball`). Abbreviations
#'   will also work: `"p"` (for parametric/Pearson's *r*), `"np"`
#'   (nonparametric/Spearman's *rho*), `"r"` (robust).
#' @param exact A logical indicating whether an exact *p*-value should be
#'   computed. Used for Spearman's *rho*. For more details, see
#'   `?stats::cor.test`.
#' @param continuity A logical. If `TRUE`, a continuity correction is used for
#'   Spearman's *rho* when not computed exactly (Default: `TRUE`).
#' @param beta A numeric bending constant for percentage bend robust correlation
#'   coefficient (Default: `0.1`).
#' @param digits,k Decides the number of decimal digits to be displayed
#'   (Default: `2`).
#' @param sig.level Significance level (Default: `0.05`). If the *p*-value in
#'   *p*-value matrix is bigger than `sig.level`, then the corresponding
#'   correlation coefficient is regarded as insignificant and flagged as such in
#'   the plot. This argument is relevant only when `output = "plot"`.
#' @param p.adjust.method What adjustment for multiple tests should be used?
#'   (`"holm"`, `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`,
#'   `"fdr"`, `"none"`). See `stats::p.adjust` for details about why to use
#'   `"holm"` rather than `"bonferroni"`). Default is `"none"`. If adjusted
#'   *p*-values are displayed in the visualization of correlation matrix, the
#'   **adjusted** *p*-values will be used for the **upper** triangle, while
#'   **unadjusted** *p*-values will be used for the **lower** triangle of the
#'   matrix.
#' @param hc.order Logical value. If `TRUE`, correlation matrix will be
#'   hc.ordered using `hclust` function (Default is `FALSE`).
#' @param hc.method The agglomeration method to be used in `hclust` (see
#'   `?hclust`).
#' @param lab Logical value. If `TRUE`, correlation coefficient values will be
#'   displayed in the plot.
#' @param colors A vector of 3 colors for low, mid, and high correlation values.
#'   If set to `NULL`, manual specification of colors will be turned off and 3
#'   colors from the specified `palette` from `package` will be selected.
#' @param outline.color The outline color of square or circle. Default value is
#'   `"gray"`.
#' @param title The text for the plot title.
#' @param subtitle The text for the plot subtitle.
#' @param caption The text for the plot caption. If `NULL`, a default caption
#'   will be shown.
#' @param caption.default Logical that decides whether the default caption
#'   should be shown (default: `TRUE`).
#' @param lab.col Color to be used for the correlation coefficient labels
#'   (applicable only when `lab = TRUE`).
#' @param lab.size Size to be used for the correlation coefficient labels
#'   (applicable only when `lab = TRUE`).
#' @param insig Character used to show specialized insignificant correlation
#'   coefficients (`"pch"` (default) or `"blank"`). If `"blank"`, the
#'   corresponding glyphs will be removed; if "pch" is used, characters (see
#'   `?pch` for details) will be added on the corresponding glyphs.
#' @param pch Decides the glyphs (read point shapes) to be used for
#'   insignificant correlation coefficients (only valid when `insig = "pch"`).
#'   Default value is `pch = 4`.
#' @param pch.col,pch.cex The color and the cex (size) of `pch` (only valid when
#'   `insig = "pch"`). Defaults are `pch.col = "#F0E442"` and `pch.cex = 10`.
#' @param tl.cex,tl.col,tl.srt The size, the color, and the string rotation of
#'   text label (variable names, i.e.).
#' @param messages Decides whether messages references, notes, and warnings are
#'   to be displayed (Default: `TRUE`).
#' @inheritParams theme_ggstatsplot
#' @inheritParams paletteer::paletteer_d
#' @inheritParams ggscatterstats
#'
#' @import ggplot2
#'
#' @importFrom ggcorrplot ggcorrplot
#' @importFrom dplyr select group_by summarize n arrange bind_cols
#' @importFrom dplyr mutate mutate_at mutate_if
#' @importFrom purrr is_bare_double is_bare_numeric %||%
#' @importFrom purrr flatten_dbl keep flatten_dfr compose
#' @importFrom stats cor na.omit median
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom rlang !! enquo quo_name is_null
#' @importFrom crayon green blue yellow red
#' @importFrom WRS2 pball
#' @importFrom psych corr.p corr.test
#'
#' @seealso \code{\link{grouped_ggcorrmat}} \code{\link{ggscatterstats}}
#'   \code{\link{grouped_ggscatterstats}}
#'
#' @references
#' \url{https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/ggcorrmat.html}
#'
#' @examples
#'
#' # for reproducibility
#' set.seed(123)
#'
#' # if `cor.vars` not specified, all numeric varibles used
#' ggstatsplot::ggcorrmat(data = iris)
#'
#' # to get the correlalogram
#' # note that the function will run even if the vector with variable names is
#' # not of same length as the number of variables
#' ggstatsplot::ggcorrmat(
#'   data = ggplot2::msleep,
#'   cor.vars = sleep_total:bodywt,
#'   cor.vars.names = c("total sleep", "REM sleep")
#' ) + # further modification using `ggplot2`
#'   ggplot2::scale_y_discrete(position = "right")
#'
#' # to get the correlation matrix
#' ggstatsplot::ggcorrmat(
#'   data = ggplot2::msleep,
#'   cor.vars = sleep_total:bodywt,
#'   output = "r"
#' )
#'
#' # setting output = "p-values" (or "p") will return the p-value matrix
#' ggstatsplot::ggcorrmat(
#'   data = ggplot2::msleep,
#'   cor.vars = sleep_total:bodywt,
#'   corr.method = "r",
#'   p.adjust.method = "bonferroni",
#'   output = "p"
#' )
#'
#' # setting `output = "ci"` will return the confidence intervals for unique
#' # correlation pairs
#' ggstatsplot::ggcorrmat(
#'   data = ggplot2::msleep,
#'   cor.vars = sleep_total:bodywt,
#'   p.adjust.method = "BH",
#'   output = "ci"
#' )
#'
#' # modifying elements of the correlation matrix by changing function defaults
#' ggstatsplot::ggcorrmat(
#'   data = datasets::iris,
#'   cor.vars = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
#'   sig.level = 0.01,
#'   ggtheme = ggplot2::theme_bw(),
#'   hc.order = TRUE,
#'   matrix.type = "lower",
#'   outline.col = "white",
#'   title = "Dataset: Iris"
#' )
#' @export

# defining the function
ggcorrmat <- function(data,
                      cor.vars = NULL,
                      cor.vars.names = NULL,
                      output = "plot",
                      matrix.type = "full",
                      method = "square",
                      corr.method = "pearson",
                      type = NULL,
                      exact = FALSE,
                      continuity = TRUE,
                      beta = 0.1,
                      digits = 2,
                      k = NULL,
                      sig.level = 0.05,
                      conf.level = 0.95,
                      p.adjust.method = "none",
                      hc.order = FALSE,
                      hc.method = "complete",
                      lab = TRUE,
                      package = "RColorBrewer",
                      palette = "Dark2",
                      direction = 1,
                      colors = c("#E69F00", "white", "#009E73"),
                      outline.color = "black",
                      ggtheme = ggplot2::theme_bw(),
                      ggstatsplot.layer = TRUE,
                      title = NULL,
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
                      messages = TRUE,
                      return = NULL) {

  # ======================= dataframe ========================================

  # creating a dataframe out of the entered variables
  if (missing(cor.vars)) {
    df <- purrr::keep(.x = data, .p = purrr::is_bare_numeric)
  } else {
    # creating a dataframe out of the entered variables
    df <- dplyr::select(.data = data, {{ cor.vars }})
  }

  # counting number of NAs present in the dataframe
  na_total <- df %>%
    purrr::map_df(.x = ., .f = ~ sum(is.na(.))) %>%
    purrr::flatten_dbl(.x = .) %>%
    sum(., na.rm = TRUE)

  # renaming the columns if so desired
  if (!is.null(cor.vars.names)) {
    # check if number of cor.vars is equal to the number of names entered
    if (length(df) != length(cor.vars.names)) {
      # display a warning message if not
      message(cat(
        crayon::red("Warning: "),
        crayon::blue("No. of variable names doesn't equal no. of variables.\n"),
        sep = ""
      ))
    } else {
      # otherwise rename the columns with the new names
      colnames(df) <- cor.vars.names
    }
  }

  # ============================ checking corr.method =======================

  # see which method was used to specify type of correlation
  corr.method <- type %||% corr.method
  digits <- k %||% digits
  output <- return %||% output

  # if any of the abbreviations have been entered, change them
  if (corr.method == "p") {
    corr.method <- "pearson"
  } else if (corr.method == "np") {
    corr.method <- "spearman"
  } else if (corr.method == "r") {
    corr.method <- "robust"
  }

  # create unique name for each method
  corr.method.text <-
    switch(corr.method,
      "pearson" = "Pearson",
      "spearman" = "Spearman",
      "robust" = "robust (% bend)",
      "kendall" = "Kendall"
    )

  # ===================== statistics ========================================
  #
  if (corr.method %in% c("pearson", "spearman", "kendall")) {
    # confidence interval computation can take some time and also produce
    # warnings, so compute them only when requested by the user
    if (output == "ci") {
      ci <- TRUE
    } else {
      ci <- FALSE
    }

    # computing correlations using `psych` package
    corr_df <-
      psych::corr.test(
        x = as.data.frame(df),
        y = NULL,
        use = "pairwise",
        method = corr.method,
        adjust = p.adjust.method,
        alpha = .05,
        ci = ci,
        minlength = 20
      )

    # computing correlations on all included variables
    corr.mat <- round(x = corr_df$r, digits = digits)

    # compute a correlation matrix of p-values
    p.mat <- corr_df$p
  } else if (corr.method == "robust") {

    # get matrix of samples sizes to be used later in `corr.p` function (`n`)
    corr_df <-
      psych::corr.test(
        x = as.data.frame(df),
        y = NULL,
        use = "pairwise",
        adjust = "none",
        alpha = 1 - conf.level,
        ci = FALSE,
        minlength = 20
      )

    # computing the percentage bend correlation matrix
    rob_cor <- WRS2::pball(x = df, beta = beta)

    # extracting the correlations and formatting them
    corr.mat <- round(x = rob_cor$pbcorm, digits = digits)

    # converting NAs to 1's
    rob_cor$p.values[is.na(rob_cor$p.values)] <- 0

    # adjusting for multiple comparisons (if needed)
    p.mat <-
      psych::corr.p(
        r = corr.mat,
        n = corr_df$n,
        adjust = p.adjust.method,
        alpha = 0.05,
        minlength = 20
      )$p
  }

  # ========================== plot =========================================

  # if user has not specified colors, then use a color palette
  if (is.null(colors)) {
    colors <- paletteer::paletteer_d(
      package = !!package,
      palette = !!palette,
      n = 3,
      direction = direction,
      type = "discrete"
    )
  }

  # creating the basic plot
  if (output == "plot") {
    if (corr.method %in% c("pearson", "spearman", "kendall", "robust")) {

      # legend title with information about correlation type and sample
      if (na_total == 0) {
        legend.title.text <-
          bquote(atop(
            atop(
              scriptstyle(bold("sample size:")),
              italic(n) ~ "=" ~ .(nrow(data))
            ),
            atop(
              scriptstyle(bold("correlation:")),
              .(corr.method.text)
            )
          ))
      } else {
        # in case of NAs, compute minimum and maximum sample sizes of pairs
        n_summary <- tibble::as_tibble(corr_df$n) %>%
          dplyr::select_if(.tbl = ., .predicate = purrr::is_bare_numeric) %>%
          purrr::flatten_dbl(.x = .) %>%
          {
            list(
              n_min = min(., na.rm = TRUE),
              n_median = stats::median(x = ., na.rm = TRUE),
              n_max = max(., na.rm = TRUE)
            )
          } %>%
          purrr::flatten_dfr(.x = .)

        # creating legend with sample size info
        legend.title.text <-
          bquote(atop(
            atop(
              atop(
                scriptstyle(bold("sample size:")),
                italic(n)[min] ~ "=" ~ .(n_summary$n_min[[1]])
              ),
              atop(
                italic(n)[median] ~ "=" ~ .(n_summary$n_median[[1]]),
                italic(n)[max] ~ "=" ~ .(n_summary$n_max[[1]])
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
        ggcorrplot::ggcorrplot(
          corr = corr.mat,
          method = method,
          p.mat = p.mat,
          sig.level = sig.level,
          type = matrix.type,
          hc.method = hc.method,
          hc.order = hc.order,
          lab = lab,
          outline.color = outline.color,
          ggtheme = ggtheme,
          colors = colors,
          legend.title = legend.title.text,
          lab_col = lab.col,
          lab_size = lab.size,
          insig = insig,
          pch = pch,
          pch.col = pch.col,
          pch.cex = pch.cex,
          tl.cex = tl.cex,
          tl.col = tl.col,
          tl.srt = tl.srt,
          digits = digits
        )

      # =========================== labels ==================================

      # if `caption` is not specified, use the generic version only if
      # `caption.default` is `TRUE`
      if (is.null(caption) && pch == 4 && isTRUE(caption.default)) {

        # p value adjustment method description
        p.adjust.method.text <-
          paste(
            "Adjustment (p-value): ",
            p.adjust.method.description(p.adjust.method = p.adjust.method),
            sep = ""
          )

        # preparing the caption
        caption <-
          substitute(
            atop(
              expr = paste(
                bold("X"),
                " = correlation non-significant at ",
                italic("p"),
                " < ",
                sig.level,
                sep = ""
              ),
              bottom.text
            ),
            env = list(
              sig.level = sig.level,
              bottom.text = p.adjust.method.text
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

      # adding ggstatsplot theme for correlation matrix
      if (isTRUE(ggstatsplot.layer)) {
        plot <- plot + theme_corrmat()
      }
    }
  }

  # ========================= confidence intervals ===========================

  if (output == "ci") {
    if (corr.method %in% c("pearson", "spearman", "kendall")) {
      # composing a function to convert dataframe to tibble
      tibble_helper <- purrr::compose(tibble::as_tibble, tibble::rownames_to_column)

      # merging data frame with CIs and adjusted CIs
      ci.mat <- list(corr_df$ci, corr_df$ci.adj) %>%
        purrr::map(.x = ., .f = tibble_helper, var = "pair") %>%
        dplyr::bind_cols(.) %>%
        dplyr::select(.data = ., pair, r, dplyr::everything(), -pair1)
    } else {
      return(message(cat(
        crayon::red("Warning: "),
        crayon::blue("Confidence intervals not supported for robust correlation.\n"),
        sep = ""
      )))
    }
  }

  # =============================== output ==================================

  # if p-values were adjusted, notify how they are going to be displayed
  if (p.adjust.method != "none" && isTRUE(messages) && corr.method != "robust") {
    ggcorrmat_matrix_message()
  }

  # return the desired result
  return(
    switch(output,
      "correlations" = tibble::as_tibble(x = corr.mat, rownames = "variable"),
      "corr" = tibble::as_tibble(x = corr.mat, rownames = "variable"),
      "r" = tibble::as_tibble(x = corr.mat, rownames = "variable"),
      "n" = tibble::as_tibble(x = corr_df$n, rownames = "variable"),
      "sample.size" = tibble::as_tibble(x = corr_df$n, rownames = "variable"),
      "ci" = tibble::as_tibble(ci.mat),
      "p-values" = tibble::as_tibble(x = p.mat, rownames = "variable"),
      "p.values" = tibble::as_tibble(x = p.mat, rownames = "variable"),
      "p" = tibble::as_tibble(x = p.mat, rownames = "variable"),
      "plot" = plot,
      plot
    )
  )
}
