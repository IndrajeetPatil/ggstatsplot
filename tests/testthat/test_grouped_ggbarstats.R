context("grouped_ggbarstats")

testthat::test_that(
  desc = "grouped_ggbarstats works",
  code = {
    testthat::skip_on_cran()

    # --------------------- without counts -----------------------------------

    # creating a smaller dataframe
    mpg_short <-
      ggplot2::mpg %>%
      dplyr::filter(
        .data = .,
        drv %in% c("4", "f"),
        class %in% c("suv", "midsize"),
        trans %in% c("auto(l4)", "auto(l5)")
      )

    ## expecting error message
    testthat::expect_error(ggstatsplot::grouped_ggbarstats(
      data = mpg_short,
      main = cyl,
      grouping.var = class,
      messages = TRUE
    ))

    testthat::expect_error(ggstatsplot::grouped_ggbarstats(
      data = mpg_short,
      main = cyl,
      messages = TRUE
    ))

    testthat::expect_output(
      ggstatsplot::grouped_ggbarstats(
        data = mpg_short,
        main = cyl,
        condition = class,
        grouping.var = class,
        messages = TRUE
      )
    )

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggbarstats(
        data = mpg_short,
        main = cyl,
        condition = class,
        grouping.var = drv,
        x.axis.orientation = "horizontal",
        messages = TRUE
      )
    ),
    what = "gg"
    ))

    # when arguments are entered as character
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggbarstats(
        data = mpg_short,
        main = "cyl",
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
