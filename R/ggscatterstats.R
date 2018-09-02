#'
#' @title Scatterplot with marginal distributions
#' @name ggscatterstats
#' @aliases ggscatterstats
#' @author Indrajeet Patil
#' @description Scatterplots from `ggplot2` combined with marginal
#'   histograms/boxplots/density plots with statistical details added as a
#'   subtitle.
#'
#' @param data Dataframe from which variables specified are preferentially to be
#'   taken.
#' @param x A vector containing the explanatory variable.
#' @param y The response - a vector of length the number of rows of `x`.
#' @param label.var Variable to use for points labels.
#' @param label.expression An expression evaluating to a logical vector that
#'   determines the subset of data points to label.
#' @param xlab Label for `x` axis variable.
#' @param ylab Label for `y` axis variable.
#' @param line.color color for the regression line.
#' @param line.size Size for the regression line.
#' @param marginal Decides whether `ggExtra::ggMarginal()` plots will be
#'   displayed; the default is `TRUE`.
#' @param marginal.type Type of marginal distribution to be plotted on the axes
#'   (`"histogram"`, `"boxplot"`, `"density"`, `"violin"`, `"densigram"`).
#' @param marginal.size Integer describing the relative size of the marginal
#'   plots compared to the main plot. A size of `5` means that the main plot is
#'   5x wider and 5x taller than the marginal plots.
#' @param margins Character describing along which margins to show the plots.
#'   Any of the following arguments are accepted: `"both"`, `"x"`, `"y"`.
#' @param xfill,yfill Character describing color fill for `x` and `y` axes
#'   marginal distributions (default: `"#009E73"` (for `x`) and `"#D55E00"` (for
#'   `y`)).
#' @param xalpha,yalpha Numeric deciding transparency levels for the marginal
#'   distributions. Any numbers from `0` (transparent) to `1` (opaque). The
#'   default is `1` for both axes.
#' @param xsize,ysize Size for the marginal distribution boundaries (Default: `0.7`).
#' @param results.subtitle Decides whether the results of statistical tests are
#'   to be displayed as subtitle.
#' @param centrality.para Decides *which* measure of central tendency (`"mean"`
#'   or `"median"`) is to be displayed as vertical (for `x`) and horizontal (for
#'   `y`) lines.
#' @param title The text for the plot title.
#' @param subtitle The text for the plot subtitle. Will work only if
#'   `results.subtitle = FALSE`.
#' @param caption The text for the plot caption.
#' @param k Number of decimal places expected for results.
#' @param width.jitter Degree of jitter in `x` direction. Defaults to 40\% of
#'   the resolution of the data.
#' @param height.jitter Degree of jitter in `y` direction. Defaults to 40\% of
#'   the resolution of the data.
#' @param axes.range.restrict Logical decides whether to restrict the axes values
#'   ranges to min and max values of the `x` and `y` variables (Default: `FALSE`).
#' @inheritParams subtitle_ggscatterstats
#' @inheritParams ggplot2::geom_smooth
#' @inheritParams theme_ggstatsplot
#'
#' @import ggplot2
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
#' @importFrom broom tidy
#' @importFrom ggExtra ggMarginal
#' @importFrom stats cor.test
#' @importFrom stats na.omit
#' @importFrom stats confint.default
#' @importFrom ggrepel geom_label_repel
#'
#' @seealso \code{\link{grouped_ggscatterstats}} \code{\link{ggcorrmat}} \code{\link{grouped_ggcorrmat}}
#'
#' @references
#' \url{https://cran.r-project.org/package=ggstatsplot/vignettes/ggscatterstats.html}
#'
#' @note `marginal.type = "densigram"` will work only with the development
#'   version of `ggExtra` that you can download from `GitHub`:
#'   `devtools::install_github("daattali/ggExtra")`
#'
#' @examples
#' 
#' # to get reproducible results from bootstrapping
#' set.seed(123)
#' 
#' # creating dataframe
#' mtcars_new <- mtcars %>%
#'   tibble::rownames_to_column(., var = "car") %>%
#'   tibble::as_data_frame(x = .)
#' 
#' # simple function call with the defaults
#' ggstatsplot::ggscatterstats(
#'   data = mtcars_new,
#'   x = wt,
#'   y = mpg,
#'   type = "np",
#'   label.var = car,
#'   label.expression = wt < 4 & mpg < 20,
#'   axes.range.restrict = TRUE,
#'   centrality.para = "median"
#' )
#' @export
#'

# defining the function
ggscatterstats <-
  function(data,
             x,
             y,
             label.var = NULL,
             label.expression = NULL,
             xlab = NULL,
             ylab = NULL,
             method = "lm",
             method.args = list(),
             formula = y ~ x,
             line.size = 1.5,
             line.color = "blue",
             marginal = TRUE,
             marginal.type = "histogram",
             marginal.size = 5,
             margins = c("both", "x", "y"),
             width.jitter = NULL,
             height.jitter = NULL,
             xfill = "#009E73",
             yfill = "#D55E00",
             xalpha = 1,
             yalpha = 1,
             xsize = 0.7,
             ysize = 0.7,
             centrality.para = NULL,
             type = "pearson",
             results.subtitle = TRUE,
             title = NULL,
             subtitle = NULL,
             caption = NULL,
             nboot = 100,
             beta = 0.1,
             k = 3,
             axes.range.restrict = FALSE,
             ggtheme = ggplot2::theme_bw(),
             ggstatsplot.layer = TRUE,
             messages = TRUE) {


    #--------------------------------- variable names ----------------------------------------------------------

    # preparing a dataframe with variable names
    lab.df <- colnames(dplyr::select(
      .data = data,
      !!rlang::enquo(x),
      !!rlang::enquo(y)
    ))

    # if `xlab` is not provided, use the variable `x` name
    if (is.null(xlab)) {
      xlab <- lab.df[1]
    }

    # if `ylab` is not provided, use the variable `y` name
    if (is.null(ylab)) {
      ylab <- lab.df[2]
    }

    #--------------------------------- dataframe ----------------------------------------------------------

    # preparing the dataframe
    data <- dplyr::full_join(
      # dataframe where x and y are named "x" and "y"
      x = data %>%
        dplyr::select(
          .data = .,
          x = !!rlang::enquo(x),
          y = !!rlang::enquo(y)
        ) %>%
        tibble::rowid_to_column(., var = "rowid"),
      # dataframe where x and y retain their original names
      y = data %>%
        dplyr::select(
          .data = .,
          !!rlang::enquo(x),
          !!rlang::enquo(y),
          dplyr::everything()
        ) %>%
        tibble::rowid_to_column(., var = "rowid"),
      by = "rowid"
    ) %>%
      dplyr::select(.data = ., -rowid) %>%
      tibble::as_data_frame(x = .)

    #--------------------------------- user expression ----------------------------------------------------------

    # create a list of function call to check for label.expression
    param_list <- base::as.list(base::match.call())

    # check labeling variable has been entered
    if ("label.var" %in% names(param_list)) {
      point.labelling <- TRUE
    } else {
      point.labelling <- FALSE
    }

    # creating a new dataframe for showing labels
    label_data <-
      data %>%
      {
        if ("label.expression" %in% names(param_list)) {
          dplyr::filter(.data = ., !!rlang::enquo(label.expression))
        }
        else {
          (.)
        }
      }

    #--------------------------------- creating results subtitle ----------------------------------------------------------

    # adding a subtitle with statistical results
    if (results.subtitle == TRUE) {
      subtitle <- subtitle_ggscatterstats(
        data = data,
        x = x,
        y = y,
        nboot = nboot,
        beta = beta,
        type = type,
        conf.level = 0.95,
        conf.type = "norm",
        messages = messages,
        k = k
      )
    }

    #---------------------------------------------------- basic plot ----------------------------------------------------------

    # preparing the scatterplotplot
    plot <-
      ggplot2::ggplot(
        data = data,
        mapping = ggplot2::aes(
          x = x,
          y = y
        )
      ) +
      ggplot2::geom_point(
        size = 3,
        alpha = 0.5,
        position = ggplot2::position_jitter(
          width = width.jitter,
          height = height.jitter
        ),
        na.rm = TRUE
      ) +
      ggplot2::geom_smooth(
        method = method,
        method.args = method.args,
        formula = formula,
        se = TRUE,
        size = line.size,
        color = line.color,
        na.rm = TRUE,
        level = 0.95
      ) +
      ggstatsplot::theme_mprl(ggtheme = ggtheme, ggstatsplot.layer = ggstatsplot.layer) +
      ggplot2::labs(
        x = xlab,
        y = ylab,
        title = title,
        subtitle = subtitle,
        caption = caption
      )

    # forcing the plots to get cut off at min and max values of the variable
    if (isTRUE(axes.range.restrict)) {
      plot <- plot +
        ggplot2::coord_cartesian(xlim = c(min(data$x), max(data$x))) +
        ggplot2::coord_cartesian(ylim = c(min(data$y), max(data$y)))
    }

    #--------------------------------- adding centrality parameters ----------------------------------------------------------

    # by default, if the input is NULL, then no centrality.para lines will be plotted

    if (is.null(centrality.para)) {
      plot <- plot
    } else if (isTRUE(centrality.para) ||
      centrality.para == "mean") {
      plot <- plot +
        ggplot2::geom_vline(
          xintercept = mean(x = data$x, na.rm = TRUE),
          linetype = "dashed",
          color = xfill,
          size = 1.2,
          na.rm = TRUE
        ) +
        ggplot2::geom_hline(
          yintercept = mean(x = data$y, na.rm = TRUE),
          linetype = "dashed",
          color = yfill,
          size = 1.2,
          na.rm = TRUE
        )
    } else if (centrality.para == "median") {
      plot <- plot +
        ggplot2::geom_vline(
          xintercept = median(x = data$x, na.rm = TRUE),
          linetype = "dashed",
          color = xfill,
          size = 1.2,
          na.rm = TRUE
        ) +
        ggplot2::geom_hline(
          yintercept = median(x = data$y, na.rm = TRUE),
          linetype = "dashed",
          color = yfill,
          size = 1.2,
          na.rm = TRUE
        )
    }

    #--------------------------------- adding point labels ----------------------------------------------------------

    if (isTRUE(point.labelling)) {
      # using geom_repel_label
      plot <-
        plot +
        ggrepel::geom_label_repel(
          data = label_data,
          mapping = aes(
            label = !!rlang::enquo(label.var)
          ),
          fontface = "bold",
          color = "black",
          max.iter = 3e2,
          box.padding = 0.35,
          point.padding = 0.5,
          segment.color = "black",
          force = 2,
          seed = 123,
          na.rm = TRUE
        )
    }
    #-------------------------------------------------- ggMarginal  ----------------------------------------------------------

    # creating the ggMarginal plot of a given marginal.type
    if (isTRUE(marginal)) {
      plot <-
        ggExtra::ggMarginal(
          p = plot,
          type = marginal.type,
          margins = margins,
          size = marginal.size,
          xparams = base::list(
            fill = xfill,
            alpha = xalpha,
            size = xsize,
            col = "black"
          ),
          yparams = base::list(
            fill = yfill,
            alpha = yalpha,
            size = ysize,
            col = "black"
          )
        )
    }

    #-------------------------------------------------- messages  ----------------------------------------------------------
    #
    # display warning that this function doesn't produce a ggplot2 object
    if (isTRUE(marginal)) {
      base::message(cat(
        crayon::red("Warning:"),
        crayon::blue(
          "The plot is not a `ggplot` object and therefore can't be further modified with `ggplot2` functions."
        )
      ))
    }

    # return the final plot
    return(plot)
  }
