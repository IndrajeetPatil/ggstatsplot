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
        drv %in% c("4", "f")
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

# subtitle return --------------------------------------------------

testthat::test_that(
  desc = "subtitle return",
  code = {
    testthat::skip_on_cran()

    # should return a list of length 3
    ls_results <- suppressWarnings(ggstatsplot::grouped_ggbarstats(
      data = dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1),
      main = relig,
      condition = marital,
      grouping.var = race,
      return = "subtitle",
      results.subtitle = FALSE,
      bar.proptest = FALSE,
      messages = FALSE
    ))

    # tests
    testthat::expect_equal(length(ls_results), 3L)
    testthat::expect_null(ls_results[[1]], NULL)
    testthat::expect_null(ls_results[[2]], NULL)
    testthat::expect_null(ls_results[[3]], NULL)
  }
)
