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
        main = "cyl",
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
        main = "cyl",
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
        x = cyl,
        y = "class",
        grouping.var = drv,
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
    set.seed(123)
    ls_results <- suppressWarnings(ggstatsplot::grouped_ggpiestats(
      data = dplyr::sample_frac(tbl = forcats::gss_cat, size = 0.1),
      main = relig,
      condition = marital,
      grouping.var = race,
      return = "caption",
      results.subtitle = FALSE,
      facet.proptest = FALSE,
      messages = FALSE
    ))

    # tests
    testthat::expect_equal(length(ls_results), 3L)
    testthat::expect_null(ls_results[[1]], NULL)
    testthat::expect_null(ls_results[[2]], NULL)
    testthat::expect_null(ls_results[[3]], NULL)
  }
)

# checking if results coincide with base version -----------------------------

testthat::test_that(
  desc = "checking if results coincide with base version",
  code = {
    testthat::skip_on_cran()

    # creating new datasets from the existing one
    mtcars2 <- dplyr::mutate(mtcars, grp = "1")
    mtcars3 <- dplyr::filter(mtcars2, cyl != "8")
    msleep2 <- dplyr::mutate(ggplot2::msleep, grp = "1")

    set.seed(123)
    p1 <-
      suppressWarnings(ggpiestats(
        data = mtcars,
        main = "am",
        condition = cyl,
        messages = FALSE,
        return = "subtitle"
      ))

    set.seed(123)
    p2 <-
      suppressWarnings(ggpiestats(
        data = mtcars3,
        x = am,
        y = cyl,
        messages = FALSE,
        return = "subtitle"
      ))

    set.seed(123)
    p3 <-
      suppressWarnings(grouped_ggpiestats(
        data = mtcars2,
        main = am,
        y = "cyl",
        grouping.var = grp,
        messages = FALSE,
        return = "subtitle"
      ))

    set.seed(123)
    p4 <-
      suppressWarnings(grouped_ggpiestats(
        data = mtcars3,
        x = "am",
        condition = cyl,
        grouping.var = "grp",
        messages = FALSE,
        return = "subtitle"
      ))

    set.seed(123)
    p5 <-
      suppressWarnings(ggpiestats(
        data = ggplot2::msleep,
        x = vore,
        messages = FALSE,
        return = "subtitle"
      ))

    set.seed(123)
    p6 <-
      suppressWarnings(grouped_ggpiestats(msleep2,
        vore,
        grouping.var = grp,
        messages = FALSE,
        return = "subtitle"
      ))

    # testing if grouped and base versions results are same
    testthat::expect_identical(p1, p3$`1`)
    testthat::expect_identical(p2, p4$`1`)
    testthat::expect_identical(p5, p6$`1`)
  }
)
