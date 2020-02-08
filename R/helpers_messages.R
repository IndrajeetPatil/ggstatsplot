#' @title Display normality test result as a message.
#' @name normality_message
#' @description A note to the user about the validity of assumptions for the
#'   default linear model.
#'
#' @param x A numeric vector.
#' @param lab A character describing label for the variable. If `NULL`, a
#'   generic `"x"` label will be used.
#' @param output What output is desired: `"message"` (default) or `"stats"` (or
#'   `"tidy"`) objects.
#' @param ... Additional arguments (ignored).
#' @inheritParams ggbetweenstats
#'
#' @importFrom stats shapiro.test
#' @importFrom crayon green blue yellow red
#'
#' @inherit stats::shapiro.test return value
#'
#' @family helper_messages
#'
#' @seealso \code{\link{ggbetweenstats}}
#'
#' @examples
#'
#' # message
#' normality_message(
#'   x = anscombe$x1,
#'   lab = "x1",
#'   k = 3
#' )
#'
#' # statistical test object
#' ggstatsplot::normality_message(
#'   x = anscombe$x2,
#'   output = "tidy"
#' )
#' @export

# function body
normality_message <- function(x,
                              lab = NULL,
                              k = 2,
                              output = "message",
                              ...) {

  # if label is not provided, use generic "x" variable
  if (is.null(lab)) lab <- "x"

  # works only if sample size is greater than 3 and less than 5000
  if (length(x) > 3 && length(x) < 5000) {
    # test object
    sw_norm <- stats::shapiro.test(x)
    p_value <- sw_norm$p.value[[1]]

    # what object to return?
    if (output == "message") {
      # exact message
      message(cat(
        crayon::green("Note: "),
        crayon::blue("Shapiro-Wilk Normality Test for "),
        crayon::yellow(lab),
        crayon::blue(": p-value = "),
        crayon::yellow(specify_decimal_p(x = p_value, k = k, p.value = TRUE)),
        "\n",
        sep = ""
      ))
    } else {
      return(broomExtra::tidy(sw_norm))
    }
  }
}

#' @title Display homogeneity of variance test as a message
#' @name bartlett_message
#' @description A note to the user about the validity of assumptions for the
#'   default linear model.
#'
#' @param lab A character describing label for the variable. If `NULL`, variable
#'   name will be used.
#' @inheritParams ggbetweenstats
#' @inheritParams normality_message
#'
#' @importFrom rlang enquo quo_name !! as_name ensym := new_formula
#' @importFrom stats bartlett.test
#' @importFrom crayon green blue yellow red
#'
#' @inherit stats::bartlett.test return value
#'
#' @seealso \code{\link{ggbetweenstats}}
#'
#' @family helper_messages
#'
#' @examples
#'
#' # getting message
#' ggstatsplot::bartlett_message(
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   lab = "Iris Species"
#' )
#'
#' # getting results from the test
#' ggstatsplot::bartlett_message(
#'   data = mtcars,
#'   x = am,
#'   y = wt,
#'   output = "tidy"
#' )
#' @export

# function body
bartlett_message <- function(data,
                             x,
                             y,
                             lab = NULL,
                             k = 2,
                             output = "message",
                             ...) {
  # make sure both quoted and unquoted arguments are supported
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)

  # if `lab` is not provided, use the variable `x` name
  if (is.null(lab)) lab <- rlang::as_name(x)

  # running the test
  bartlett <- stats::bartlett.test(
    formula = rlang::new_formula(y, x),
    data = data,
    na.action = na.omit
  )
  p_value <- bartlett$p.value[[1]]

  # preparing message
  if (output == "message") {
    # display homogeneity of variances test result as a message
    message(cat(
      crayon::green("Note: "),
      crayon::blue("Bartlett's test for homogeneity of variances for factor "),
      crayon::yellow(lab),
      crayon::blue(": p-value = "),
      crayon::yellow(specify_decimal_p(x = p_value, k = k, p.value = TRUE)),
      "\n",
      sep = ""
    ))
  } else {
    return(broomExtra::tidy(bartlett))
  }
}

#' @title Message if palette doesn't have enough number of colors.
#' @name palette_message
#' @description A note to the user about not using the default color palette
#'   when the number of factor levels is greater than 8, the maximum number of
#'   colors allowed by `"Dark2"` palette from the `RColorBrewer` package.
#' @param package Name of package from which the palette is desired as string
#' or symbol.
#' @param palette Name of palette as string or symbol.
#' @param min_length Minimum number of colors needed.
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter select
#' @importFrom crayon green blue yellow red
#' @importFrom rlang !! enquo
#'
#' @noRd

# function body
palette_message <- function(package, palette, min_length) {
  # computing the number of colors in a given palette
  palette_df <-
    tibble::as_tibble(paletteer::palettes_d_names) %>%
    dplyr::filter(.data = ., package == !!package, palette == !!palette) %>%
    dplyr::select(.data = ., length)

  # if insufficient number of colors are available in a given palette
  if (palette_df$length[[1]] < min_length) {
    # message to display
    message(cat(
      crayon::red("Warning: "),
      crayon::blue("No. of factor levels is greater than default palette color count.\n"),
      crayon::blue("Try using another color `palette` (and/or `package`).\n")
    ),
    sep = ""
    )
  }
}


#' @title Message to display when adjusted p-values are displayed in correlation
#'   matrix.
#' @name ggcorrmat_matrix_message
#' @noRd

# function body
ggcorrmat_matrix_message <- function() {
  message(
    cat(
      crayon::green("Note: "),
      crayon::blue("In the correlation matrix,\n"),
      crayon::blue("the upper triangle: p-values adjusted for multiple comparisons\n"),
      crayon::blue("the lower triangle: unadjusted p-values.\n"),
      sep = ""
    )
  )
}
