context("grouped_ggbarstats")

testthat::test_that(
  desc = "grouped_ggbarstats works",
  code = {
    testthat::skip_on_cran()

    # --------------------- without counts -----------------------------------

    # creating a smaller dataframe
    mpg_short <- dplyr::filter(.data = ggplot2::mpg, drv %in% c("4", "f"))

    ## expecting error message
    testthat::expect_error(ggstatsplot::grouped_ggbarstats(
      data = mpg_short,
      main = cyl,
      grouping.var = class,
      messages = FALSE
    ))

    testthat::expect_error(ggstatsplot::grouped_ggbarstats(
      data = mpg_short,
      main = cyl,
      messages = FALSE
    ))

    testthat::expect_output(
      ggstatsplot::grouped_ggbarstats(
        data = mpg_short,
        main = cyl,
        condition = class,
        grouping.var = class,
        messages = FALSE
      )
    )

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggbarstats(
        data = mpg_short,
        main = "cyl",
        condition = class,
        grouping.var = drv,
        x.axis.orientation = "horizontal",
        messages = FALSE
      )
    ),
    what = "gg"
    ))

    # when arguments are entered as character
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggbarstats(
        data = mpg_short,
        main = cyl,
        condition = "class",
        grouping.var = "drv",
        x.axis.orientation = "slant",
        messages = FALSE
      )
    ),
    what = "gg"
    ))

    # --------------------- with counts -----------------------------------

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggbarstats(
        data = as.data.frame(Titanic),
        grouping.var = Class,
        main = Sex,
        condition = Survived,
        counts = Freq,
        messages = FALSE
      )
    ),
    what = "gg"
    ))

    # when arguments are entered as character
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggbarstats(
        data = as.data.frame(Titanic),
        grouping.var = "Class",
        main = "Sex",
        condition = "Survived",
        counts = "Freq",
        messages = FALSE
      )
    ),
    what = "gg"
    ))
  }
)

# subtitle output --------------------------------------------------

testthat::test_that(
  desc = "subtitle output",
  code = {
    testthat::skip_on_cran()

    # should output a list of length 3
    set.seed(123)
    ls_results <-
      suppressWarnings(ggstatsplot::grouped_ggbarstats(
        data = dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1),
        main = relig,
        condition = marital,
        grouping.var = "race",
        output = "subtitle",
        k = 3,
        messages = FALSE
      ))

    # checking subtitle
    testthat::expect_equal(
      ls_results,
      list(
        Other = ggplot2::expr(paste(
          NULL,
          chi["Pearson"]^2,
          "(",
          "40",
          ") = ",
          "40.274",
          ", ",
          italic("p"),
          " = ",
          "0.458",
          ", ",
          widehat(italic("V"))["Cramer"],
          " = ",
          "0.009",
          ", CI"["95%"],
          " [",
          "-0.295",
          ", ",
          "-0.020",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          182L
        )),
        Black = ggplot2::expr(paste(
          NULL,
          chi["Pearson"]^
            2,
          "(",
          "32",
          ") = ",
          "25.113",
          ", ",
          italic("p"),
          " = ",
          "0.801",
          ", ",
          widehat(italic("V"))["Cramer"],
          " = ",
          "0.000",
          ", CI"["95%"],
          " [",
          "-0.168",
          ", ",
          "0.006",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          317L
        )),
        White = ggplot2::expr(paste(
          NULL,
          chi["Pearson"]^
            2,
          "(",
          "52",
          ") = ",
          "109.652",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          widehat(italic("V"))["Cramer"],
          " = ",
          "0.094",
          ", CI"["95%"],
          " [",
          "0.032",
          ", ",
          "0.100",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          1649L
        ))
      )
    )
  }
)
