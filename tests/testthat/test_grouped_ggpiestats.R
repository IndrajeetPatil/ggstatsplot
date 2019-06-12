context("grouped_ggpiestats")

testthat::test_that(
  desc = "grouped_ggpiestats works",
  code = {
    testthat::skip_on_cran()

    #--------------------- only main variable -------------------------------

    ## expecting error
    testthat::expect_error(
      ggstatsplot::grouped_ggpiestats(
        data = mtcars,
        main = cyl,
        simulate.p.value = TRUE,
        B = 3000,
        messages = FALSE
      )
    )

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

    ## expecting error message
    testthat::expect_output(
      ggstatsplot::grouped_ggpiestats(
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
        messages = TRUE
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

# subtitle return --------------------------------------------------

testthat::test_that(
  desc = "subtitle return",
  code = {
    testthat::skip_on_cran()

    # should return a list of length 3
    ls_results <- ggstatsplot::grouped_ggpiestats(
      data = dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1),
      main = relig,
      condition = marital,
      grouping.var = race,
      return = "caption",
      results.subtitle = FALSE,
      facet.proptest = FALSE,
      messages = FALSE
    )

    # tests
    testthat::expect_equal(length(ls_results), 3L)
    testthat::expect_null(ls_results[[1]], NULL)
    testthat::expect_null(ls_results[[2]], NULL)
    testthat::expect_null(ls_results[[3]], NULL)
  }
)
