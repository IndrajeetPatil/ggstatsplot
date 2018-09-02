#'
#' @title Visualization of a correlalogram (or correlation matrix)
#' @name ggcorrmat
#' @aliases ggcorrmat
#' @author Indrajeet Patil
#' @return Correlation matrix plot or correlation coefficient matrix or matrix
#'   of p-values.
#'
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
#'   will also work: `"p"` (for parametric/Pearson's *r*), `"np"`
#'   (nonparametric/Spearman's *rho*), `"r"` (robust).
#' @param exact A logical indicating whether an exact *p*-value should be
#'   computed. Used for Kendall's *tau* and Spearman's *rho*. For more details,
#'   see `?stats::cor.test`.
#' @param continuity A logical. If `TRUE`, a continuity correction is used for
#'   Kendall's *tau* and Spearman's *rho* when not computed exactly (Default:
#'   `TRUE`).
#' @param beta A numeric bending constant for robust correlation coefficient
#'   (Default: `0.1`).
#' @param digits Decides the number of decimal digits to be displayed (Default:
#'   `2`).
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
#' @param title The text for the plot title.
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
#'    Margins between x-axis and the variable name texts (t: top, r: right, b:
#'   bottom, l:left), especially useful in case the names are slanted, i.e. when
#'   the tl.srt is between `45` and `75` (Defaults: `0`, `0`, `0`, `0`, resp.).
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
#'
#' @import ggplot2
#'
#' @importFrom ggcorrplot cor_pmat
#' @importFrom ggcorrplot ggcorrplot
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
#' @importFrom purrr is_bare_double
#' @importFrom stats cor
#' @importFrom stats na.omit
#' @importFrom tibble as_data_frame
#' @importFrom tibble rownames_to_column
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom crayon green
#' @importFrom crayon blue
#' @importFrom crayon yellow
#' @importFrom crayon red
#' @importFrom WRS2 pball
#'
#' @seealso \code{\link{grouped_ggcorrmat}} \code{\link{ggscatterstats}}
#'   \code{\link{grouped_ggscatterstats}}
#'
#' @references
#' \url{https://cran.r-project.org/package=ggstatsplot/vignettes/ggcorrmat.html}
#'
#' @examples
#' 
#' # to get the correlalogram
#' ggstatsplot::ggcorrmat(
#'   data = datasets::iris,
#'   cor.vars = c(Sepal.Length:Petal.Width)
#' )
#' 
#' # to get the correlation matrix
#' ggstatsplot::ggcorrmat(
#'   data = datasets::iris,
#'   cor.vars = c(Sepal.Length:Petal.Width),
#'   output = "correlations"
#' )
#' 
#' # setting output = "p-values" will return the p-value matrix
#' 
#' # modifying few elements of the correlation matrix by changing function defaults
#' ggstatsplot::ggcorrmat(
#'   data = datasets::iris,
#'   cor.vars = c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width),
#'   sig.level = 0.01,
#'   ggtheme = ggplot2::theme_bw,
#'   hc.order = TRUE, type = "lower", outline.col = "white",
#'   title = "Dataset: Iris"
#' )
#' @export
#'

# defining the function
ggcorrmat <-
  function(data,
             cor.vars,
             cor.vars.names = NULL,
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
             axis.text.x.margin.l = 0,
             axis.text.x.margin.t = 0,
             axis.text.x.margin.r = 0,
             axis.text.x.margin.b = 0,
             messages = TRUE) {
    # ========================================== dataframe ==============================================================
    #
    # creating a dataframe out of the entered variables
    df <- data %>%
      dplyr::select(.data = ., !!rlang::enquo(cor.vars)) %>%
      stats::na.omit(.)

    # renaming the columns if so desired (must be equal to the number of number of cor.vars)
    if (!is.null(cor.vars.names)) {
      # check if number of cor.vars is equal to the number of names entered
      if (length(df) != length(cor.vars.names)) {
        # display error message if not
        base::message(cat(
          crayon::red("Warning: "),
          crayon::blue(
            "The number of variable names does not equal the number of variables."
          )
        ))
      } else {
        # otherwise rename the columns with the new names
        colnames(df) <- cor.vars.names
      }
    }

    # ========================================== checking corr.method ==============================================================

    # if any of the abbreviations have been entered, change them
    if (corr.method == "p") {
      corr.method <- "pearson"
    } else if (corr.method == "np") {
      corr.method <- "spearman"
    } else if (corr.method == "r") {
      corr.method <- "robust"
    }

    # ========================================== statistics =======================================================================
    #
    if (corr.method %in% c("pearson", "spearman", "kendall")) {
      # computing correlations on all included variables
      corr.mat <-
        base::round(
          x =
            stats::cor(
              x = base::as.data.frame(df),
              method = corr.method,
              use = "everything"
            ),
          digits = digits
        )

      # compute a correlation matrix of p-values
      p.mat <-
        ggcorrplot::cor_pmat(
          x = df,
          alternative = "two.sided",
          method = corr.method,
          na.action = na.omit,
          conf.level = 0.95,
          exact = exact,
          continuity = continuity
        )
    } else if (corr.method == "robust") {

      # computing the percentage bend correlation matrix
      rob_cor <- WRS2::pball(x = df, beta = beta)
      rob_cor$p.values[is.na(rob_cor$p.values)] <- 0
      corr.mat <- base::round(x = rob_cor$pbcorm, digits = digits)

      # creating a correlation matrix of p-values
      p.mat <- base::round(x = rob_cor$pbcorm, digits = digits)
    }

    # ========================================== plot ==============================================================
    if (output == "plot") {
      if (corr.method %in% c("pearson", "spearman", "kendall", "robust")) {
        # plotting the correlalogram
        plot <- ggcorrplot::ggcorrplot(
          corr = corr.mat,
          method = method,
          p.mat = p.mat,
          sig.level = sig.level,
          type = type,
          hc.method = hc.method,
          hc.order = hc.order,
          lab = lab,
          outline.color = outline.color,
          ggtheme = ggtheme,
          colors = colors,
          legend.title = paste("n = ", nrow(x = data), "\n", corr.method, sep = ""),
          lab_col = lab.col,
          lab_size = lab.size,
          insig = insig,
          pch = pch,
          pch.col = pch.col,
          pch.cex = pch.cex,
          tl.cex = tl.cex,
          tl.col = tl.col,
          tl.srt = tl.srt
        )

        # ========================================== labels ==============================================================
        #
        # if caption is not specified, use the generic version only if caption.default is TRUE
        if (is.null(caption) &&
          pch == 4 && isTRUE(caption.default)) {
          # adding text details to the plot
          plot <- plot +
            ggplot2::labs(
              title = title,
              subtitle = subtitle,
              caption = paste(
                "Note: X denotes correlation non-significant at p <",
                sig.level
              ),
              xlab = NULL,
              ylab = NULL
            )
        } else {
          # adding text details to the plot
          plot <- plot +
            ggplot2::labs(
              title = title,
              subtitle = subtitle,
              caption = caption,
              xlab = NULL,
              ylab = NULL
            )
        }

        # adding ggstatsplot theme for correlation matrix
        if (isTRUE(ggstatsplot.layer)) {
          plot <- plot +
            theme_corrmat()
        }

        # even if ggstatsplot theme is not overlaid, still make sure there is
        # enough distance between the axis and the label
        plot <- plot +
          ggplot2::theme(axis.text.x = ggplot2::element_text(
            margin = ggplot2::margin(
              t = axis.text.x.margin.t,
              r = axis.text.x.margin.r,
              b = axis.text.x.margin.b,
              l = axis.text.x.margin.l,
              unit = "pt"
            )
          ))
      }
    }
    # ========================================== output ==============================================================

    # return the desired result
    if (output == "correlations") {

      # correlation matrix
      corr.mat %<>%
        base::as.data.frame(x = .) %>%
        tibble::rownames_to_column(., var = "variable") %>%
        tibble::as_data_frame(x = .)

      # return the tibble
      return(corr.mat)
    } else if (output == "p-values") {

      # p-value matrix
      p.mat %<>%
        base::as.data.frame(x = .) %>%
        tibble::rownames_to_column(., var = "variable") %>%
        tibble::as_data_frame(x = .)

      # return the final tibble
      return(p.mat)
    } else if (output == "plot") {
      # correlalogram plot
      return(plot)
    }
  }
