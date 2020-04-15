# grouped_ggdotplotstats works -----------------------------------------------

testthat::test_that(
  desc = "grouped_ggdotplotstats works",
  code = {
    testthat::skip_on_cran()

    # for reproducibility
    set.seed(123)

    # removing factor level with very few no. of observations
    df <- dplyr::filter(.data = ggplot2::mpg, cyl %in% c("4", "6", "8"))

    # plot
    # when arguments are entered as bare expressions
    set.seed(123)
    p1 <- ggstatsplot::grouped_ggdotplotstats(
      data = df,
      x = cty,
      y = manufacturer,
      xlab = "city miles per gallon",
      ylab = "car manufacturer",
      grouping.var = cyl,
      test.value = 15.5,
      effsize.noncentral = FALSE,
      title.prefix = "cylinder count",
      point.color = "red",
      point.size = 5,
      results.subtitle = FALSE,
      point.shape = 13,
      test.value.line = TRUE,
      ggtheme = ggplot2::theme_classic(),
      messages = FALSE,
      title.text = "Fuel economy data"
    )

    # when arguments are entered as characters
    p2 <- ggstatsplot::grouped_ggdotplotstats(
      data = df,
      x = "cty",
      y = manufacturer,
      grouping.var = "cyl",
      test.value = 15.5,
      results.subtitle = FALSE,
      effsize.type = "d",
      ggplot.component = ggplot2::scale_y_continuous(
        sec.axis = ggplot2::dup_axis(name = "percentile score"),
        breaks = seq(0, 12, 2)
      ),
      messages = FALSE
    )

    # testing output objects are ggplot
    testthat::expect_true(inherits(p1, what = "gg"))
    testthat::expect_true(inherits(p2, what = "gg"))
  }
)

# subtitle output --------------------------------------------------

testthat::test_that(
  desc = "subtitle output",
  code = {
    testthat::skip_on_cran()

    # removing factor level with very few no. of observations
    df <- dplyr::filter(.data = ggplot2::mpg, cyl %in% c("4"))

    # should output a list of length 3
    set.seed(123)
    ls_results <-
      ggstatsplot::grouped_ggdotplotstats(
        data = df,
        x = "cty",
        y = manufacturer,
        grouping.var = "cyl",
        test.value = 15.5,
        output = "subtitle",
        messages = FALSE
      )

    set.seed(123)
    basic_results <-
      ggstatsplot::ggdotplotstats(
        data = df,
        x = "cty",
        y = manufacturer,
        test.value = 15.5,
        output = "subtitle",
        messages = FALSE
      )

    # tests
    testthat::expect_equal(length(ls_results), 1L)
    testthat::expect_identical(ls_results$`4`, basic_results)
  }
)
