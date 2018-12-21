context("grouped_ggpiestats")

testthat::test_that(
  desc = "grouped_ggpiestats works",
  code = {
    testthat::skip_on_cran()

    #--------------------- only main variable -------------------------------

    ## without counts

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggpiestats(
        data = mtcars,
        grouping.var = am,
        main = cyl,
        simulate.p.value = TRUE,
        B = 3000,
        messages = FALSE
      )
    ),
    what = "gg"
    ))

    # when arguments are entered as character
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggpiestats(
        data = mtcars,
        grouping.var = "am",
        main = "cyl",
        messages = FALSE
      )
    ),
    what = "gg"
    ))

    ## with counts

    library(jmv)

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggpiestats(
        data = as.data.frame(HairEyeColor),
        main = Hair,
        counts = Freq,
        grouping.var = Sex,
        messages = FALSE
      )
    ),
    what = "gg"
    ))

    # when arguments are entered as character
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggpiestats(
        data = as.data.frame(HairEyeColor),
        main = "Hair",
        counts = "Freq",
        grouping.var = "Sex",
        messages = FALSE
      )
    ),
    what = "gg"
    ))

    #------------------ both main and condition variables ------------------

    ## without counts

    # creating a smaller dataframe
    mpg_short <-
      ggplot2::mpg %>%
      dplyr::filter(
        .data = .,
        drv %in% c("4", "f"),
        class %in% c("suv", "midsize"),
        trans %in% c("auto(l4)", "auto(l5)")
      )

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggpiestats(
        data = mpg_short,
        main = cyl,
        condition = class,
        grouping.var = drv,
        messages = TRUE
      )
    ),
    what = "gg"
    ))

    # when arguments are entered as character
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggpiestats(
        data = mpg_short,
        main = "cyl",
        condition = "class",
        grouping.var = "drv",
        messages = FALSE
      )
    ),
    what = "gg"
    ))

    ## with counts

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(suppressWarnings(
      ggstatsplot::grouped_ggpiestats(
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
      ggstatsplot::grouped_ggpiestats(
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
