context("grouped_ggdotplotstats")

# grouped_ggdotplotstats works -----------------------------------------------

testthat::test_that(
  desc = "grouped_ggdotplotstats works",
  code = {


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
      point.shape = 13,
      test.value.line = TRUE,
      ggtheme = ggplot2::theme_classic(),
      messages = TRUE,
      title.text = "Fuel economy data"
    )

    # when arguments are entered as characters
    p2 <- ggstatsplot::grouped_ggdotplotstats(
      data = df,
      x = "cty",
      y = manufacturer,
      grouping.var = "cyl",
      test.value = 15.5,
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

# subtitle return --------------------------------------------------

testthat::test_that(
  desc = "subtitle return",
  code = {


    # removing factor level with very few no. of observations
    df <- dplyr::filter(.data = ggplot2::mpg, cyl %in% c("4", "6", "8"))

    # should return a list of length 3
    set.seed(123)
    ls_results <- ggstatsplot::grouped_ggdotplotstats(
      data = df,
      x = "cty",
      y = manufacturer,
      grouping.var = "cyl",
      test.value = 15.5,
      return = "subtitle",
      messages = FALSE
    )

    # tests
    testthat::expect_equal(length(ls_results), 3L)
    testthat::expect_identical(
      ls_results$`4`,
      ggplot2::expr(
        paste(
          NULL,
          italic("t"),
          "(",
          "8",
          ") = ",
          "7.82",
          ", ",
          italic("p"),
          " = ",
          "< 0.001",
          ", ",
          italic("g"),
          " = ",
          "2.32",
          ", CI"["95%"],
          " [",
          "1.25",
          ", ",
          "4.25",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          9L
        )
      )
    )
    testthat::expect_identical(
      ls_results$`6`,
      ggplot2::expr(
        paste(
          NULL,
          italic("t"),
          "(",
          "10",
          ") = ",
          "1.99",
          ", ",
          italic("p"),
          " = ",
          "0.075",
          ", ",
          italic("g"),
          " = ",
          "0.55",
          ", CI"["95%"],
          " [",
          "-0.06",
          ", ",
          "1.29",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          11L
        )
      )
    )
    testthat::expect_identical(
      ls_results$`8`,
      ggplot2::expr(
        paste(
          NULL,
          italic("t"),
          "(",
          "10",
          ") = ",
          "-5.01",
          ", ",
          italic("p"),
          " = ",
          "0.001",
          ", ",
          italic("g"),
          " = ",
          "-1.38",
          ", CI"["95%"],
          " [",
          "-2.49",
          ", ",
          "-0.64",
          "]",
          ", ",
          italic("n")["obs"],
          " = ",
          11L
        )
      )
    )
  }
)

# checking if results coincide with base version -----------------------------

testthat::test_that(
  desc = "checking if results coincide with base version",
  code = {


    # creating new datasets from the existing one
    msleep2 <- dplyr::mutate(ggplot2::msleep, grp = "1")

    set.seed(123)
    p1 <-
      ggdotplotstats(ggplot2::msleep,
        brainwt,
        "vore",
        messages = FALSE,
        return = "subtitle"
      )

    set.seed(123)
    p2 <-
      grouped_ggdotplotstats(
        msleep2,
        "brainwt",
        vore,
        grouping.var = grp,
        messages = FALSE,
        return = "subtitle"
      )

    # testing if grouped and base versions results are same
    testthat::expect_identical(p1, p2$`1`)
  }
)
