#' @title Display normality test result as a message.
#' @name normality_message
#' @aliases normality_message
#' @author Indrajeet Patil
#'
#' @param x A numeric vector.
#' @param lab A character describing label for the variable. If `NULL`, a
#'   generic `"x"` label will be used.
#' @param k Number of decimal places expected for results (Default: `2`).
#' @param output What output is desired: `"message"` (default) or `"stats"`
#'   objects.
#' @description A note to the user about the validity of assumptions for the
#'   default linear model.
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
#' normality_message(x = datasets::anscombe$x1)
#' 
#' # statistical test object
#' normality_message(
#'   x = datasets::anscombe$x2,
#'   output = "stats"
#' )
#' @export

# function body
normality_message <- function(x,
                              lab = NULL,
                              k = 2,
                              output = "message") {

  # if label is not provided, use generic "x" variable
  if (is.null(lab)) {
    lab <- "x"
  }

  # works only if sample size is greater than 3 and less than 5000
  if (length(x) > 3 && length(x) < 5000) {

    # test object
    sw_norm <- stats::shapiro.test(x = x)

    # what object to return?
    if (output == "message") {

      # exact message
      base::message(cat(
        crayon::green("Note: "),
        crayon::blue(
          "Shapiro-Wilk Normality Test for",
          crayon::yellow(lab),
          ": p-value = "
        ),
        crayon::yellow(
          ggstatsplot::specify_decimal_p(
            x = sw_norm$p.value[[1]],
            k = k,
            p.value = TRUE
          )
        ),
        "\n",
        sep = ""
      ))
    } else if (output == "stats") {

      # other return the tidy output
      return(broom::tidy(sw_norm))
    }
  }
}


#' @title Display homogeneity of variance test as a message
#' @name bartlett_message
#' @aliases bartlett_message
#' @author Indrajeet Patil
#'
#' @inheritParams ggbetweenstats
#' @param lab A character describing label for the variable. If `NULL`, variable
#'   name will be used.
#' @param output What output is desired: `"message"` (default) or `"stats"`
#'   objects.
#'
#' @description A note to the user about the validity of assumptions for the
#'   default linear model.
#'
#' @importFrom rlang enquo quo_name !! as_name ensym
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
#' bartlett_message(
#'   data = iris,
#'   x = Species,
#'   y = Sepal.Length,
#'   lab = "Iris Species"
#' )
#' 
#' # getting results from the test
#' bartlett_message(
#'   data = mtcars,
#'   x = am,
#'   y = wt,
#'   output = "stats"
#' )
#' @export

# function body
bartlett_message <- function(data,
                             x,
                             y,
                             lab = NULL,
                             k = 2,
                             output = "message") {

  #--------------------------- variable names ---------------------------------

  # if `lab` is not provided, use the variable `x` name
  if (is.null(lab)) {
    lab <- rlang::as_name(rlang::ensym(x))
  }

  #-------------------------- data -------------------------------------------

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    ) %>% # removing NAs and any dropped factor levels
    dplyr::filter(.data = ., !is.na(x), !is.na(y)) %>%
    dplyr::mutate(.data = ., x = droplevels(as.factor(x)))

  #------------------------------ bartlett's test ----------------------------

  # running the test
  bartlett <- stats::bartlett.test(
    formula = y ~ x,
    data = data,
    na.action = na.omit
  )

  # preparing message
  if (output == "message") {
    # display homogeneity of variances test result as a message
    base::message(cat(
      crayon::green("Note: "),
      crayon::blue(
        "Bartlett's test for homogeneity of variances for factor",
        crayon::yellow(lab),
        ": p-value = "
      ),
      crayon::yellow(
        ggstatsplot::specify_decimal_p(
          x = bartlett$p.value[[1]],
          k = k,
          p.value = TRUE
        )
      ),
      "\n",
      sep = ""
    ))
  } else if (output == "stats") {
    return(broom::tidy(bartlett))
  }
}

#' @title grouped_message
#' @description A note to the user about the class of the output object.
#' @author Indrajeet Patil
#'
#' @seealso \code{\link{grouped_ggbetweenstats}},
#'   \code{\link{grouped_gghistostats}}, \code{\link{grouped_ggscatterstats}},
#'   \code{\link{grouped_ggpiestats}}, \code{\link{grouped_ggcorrmat}}
#'
#' @family helper_messages
#'
#' @keywords internal

# function body
grouped_message <- function() {
  base::message(cat(
    crayon::red("Warning: "),
    crayon::blue("Individual plots in the combined `grouped_` plot\n"),
    crayon::blue("can't be further modified with `ggplot2` functions.\n"),
    sep = ""
  ))
}

#' @title Message if palette doesn't have enough number of colors.
#' @name palette_message
#' @author Indrajeet Patil
#' @description A note to the user about not using the default color palette
#'   when the number of factor levels is greater than 8, the maximum number of
#'   colors allowed by `"Dark2"` palette from the `RColorBrewer` package.
#'
#' @inheritParams paletteer::scale_fill_paletteer_d
#' @param min_length Minimum number of colors needed.
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter select
#' @importFrom crayon green blue yellow red
#' @importFrom rlang !! enquo
#'
#' @family helper_messages
#'
#' @examples
#' \dontrun{
#' ggstatsplot:::palette_message(
#'   package = "RColorBrewer",
#'   palette = "Dark2",
#'   min_length = 20
#' )
#' }
#' 
#' @keywords internal

# function body
palette_message <- function(package,
                            palette,
                            min_length) {
  # computing the number of colors in a given palette
  palette_df <-
    tibble::as_tibble(paletteer::palettes_d_names) %>%
    dplyr::filter(.data = ., package == !!package, palette == !!palette) %>%
    dplyr::select(.data = ., length)

  # if insufficient number of colors are available in a given palette
  if (palette_df$length[[1]] < min_length) {
    # message to display
    base::message(cat(
      crayon::red("Warning: "),
      crayon::blue(
        "No. of factor levels is greater than specified palette color count.\n"
      ),
      crayon::blue("Try using another color `palette` (and/or `package`).\n")
    ),
    sep = ""
    )
  }
}


#' @title Message to display when adjusted p-values are displayed in correlation
#'   matrix.
#' @name ggcorrmat_matrix_message
#' @author Indrajeet Patil
#'
#' @family helper_messages
#'
#' @keywords internal

# function body
ggcorrmat_matrix_message <- function() {
  base::message(
    cat(
      crayon::green("Note: "),
      crayon::blue("In the correlation matrix,\n"),
      crayon::blue(
        "the upper triangle: p-values adjusted for multiple comparisons\n"
      ),
      crayon::blue("the lower triangle: unadjusted p-values.\n"),
      sep = ""
    )
  )
}


#' @title Message to display when bootstrapped confidence intervals are shown
#'   for effect size measure.
#' @name effsize_ci_message
#' @author Indrajeet Patil
#'
#' @inheritParams t1way_ci
#' @family helper_messages
#' @keywords internal

# displaying message about bootstrap
effsize_ci_message <- function(nboot = 100, conf.level = 0.95) {
  base::message(cat(
    crayon::green("Note: "),
    crayon::blue(
      crayon::yellow(paste(conf.level * 100, "%", sep = "")),
      "CI for effect size estimate was computed with",
      crayon::yellow(nboot),
      "bootstrap samples.\n"
    ),
    sep = ""
  ))
}
