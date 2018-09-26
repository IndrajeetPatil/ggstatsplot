#' @title Display normality test result as a message.
#' @name normality_message
#' @aliases normality_message
#'
#' @param x A numeric vector.
#' @param lab A character describing label for the variable. If `NULL`, a
#'   generic `"x"` label will be used.
#' @param k Number of decimal places expected for results (Default: `3`).
#' @param output What output is desired: `"message"` (default) or `"stats"` objects.
#' @description A note to the user about the validity of assumptions for the
#'   default linear model.
#'
#' @importFrom stats shapiro.test
#' @importFrom crayon green
#' @importFrom crayon blue
#' @importFrom crayon yellow
#' @importFrom crayon red
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
                              k = 3,
                              output = "message") {

  # if label is not provided, use generic "x" variable
  if (is.null(lab)) {
    lab <- "x"
  }

  # for SW test of normality, sample size must be greater than 3 and less than 5000
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
          # entered y argument
          ": p-value ="
        ),
        crayon::yellow(
          ggstatsplot::specify_decimal_p(
            x = sw_norm$p.value[[1]],
            k = k,
            p.value = TRUE
          )
        ),
        sep = ""
      ))
    } else if (output == "stats") {

      # other return the stats object
      return(sw_norm)
    }
  }
}


#' @title Display homogeneity of variance test as a message
#' @name bartlett_message
#' @aliases bartlett_message
#'
#' @inheritParams ggbetweenstats
#' @param lab A character describing label for the variable. If `NULL`, variable
#'   name will be used.
#' @param output What output is desired: `"message"` (default) or `"stats"` objects.
#'
#' @description A note to the user about the validity of assumptions for the
#'   default linear model.
#'
#' @importFrom stats bartlett.test
#' @importFrom crayon green
#' @importFrom crayon blue
#' @importFrom crayon yellow
#' @importFrom crayon red
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
                             k = 3,
                             output = "message") {

  #------------------------------------- variable names --------------------------------------

  # preparing a dataframe with variable names
  lab.df <- colnames(x = dplyr::select(
    .data = data,
    !!rlang::enquo(x),
    !!rlang::enquo(y)
  ))

  # if `xlab` is not provided, use the variable `x` name
  if (is.null(lab)) {
    lab <- lab.df[1]
  }

  #---------------------------------------- data ----------------------------------------------------

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    )

  # convert the grouping variable to factor and drop unused levels
  data %<>%
    stats::na.omit(.) %>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "x",
      .funs = ~base::droplevels(x = base::as.factor(x = .))
    )

  #---------------------------------------- bartlett's test -------------------------------------------

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
        # entered x argument
        ": p-value ="
      ),
      crayon::yellow(
        ggstatsplot::specify_decimal_p(
          x = bartlett$p.value[[1]],
          k,
          p.value = TRUE
        )
      ),
      sep = ""
    ))
  } else if (output == "stats") {
    return(bartlett)
  }
}

#' @title grouped_message
#' @description A note to the user about the class of the output object.
#'
#' @seealso \code{\link{grouped_ggbetweenstats}}, \code{\link{grouped_gghistostats}},
#' \code{\link{grouped_ggscatterstats}}, \code{\link{grouped_ggpiestats}},
#' \code{\link{grouped_ggcorrmat}}
#'
#' @keywords internal

# function body
grouped_message <- function() {
  base::message(cat(
    crayon::red("Warning: "),
    crayon::blue(
      "The output from `grouped_` functions are not `ggplot` objects and therefore can't be further modified with `ggplot2` functions."
    ),
    sep = ""
  ))
}
