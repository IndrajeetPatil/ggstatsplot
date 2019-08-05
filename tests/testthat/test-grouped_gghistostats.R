context("grouped_gghistostats")

# grouped_gghistostats works ---------------------------------------------

testthat::test_that(
  desc = "grouped_gghistostats works",
  code = {
    testthat::skip_on_cran()

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_gghistostats(
        data = ggplot2::msleep,
        x = brainwt,
        grouping.var = vore,
        type = "p",
        normal.curve = TRUE,
        bar.measure = "mix",
        bf.message = TRUE,
        messages = TRUE
      ),
      what = "gg"
    ))

    # when arguments are entered as character
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_gghistostats(
        data = ggplot2::msleep,
        x = "brainwt",
        grouping.var = "vore",
        type = "r",
        normal.curve = TRUE,
        effsize.type = "d",
        effsize.noncentral = FALSE,
        bar.measure = "proportion",
        ggplot.component = ggplot2::scale_x_continuous(
          sec.axis = ggplot2::dup_axis(name = ggplot2::element_blank())
        ),
        messages = FALSE
      ),
      what = "gg"
    ))

    # when the data argument is missing, expect error
    # this is supposed to be the case only for `gghistostats` and not its
    # `grouped_` variant
    set.seed(123)
    testthat::expect_error(
      ggstatsplot::grouped_gghistostats(
        x = ggplot2::msleep$brainwt,
        grouping.var = "vore",
        type = "np",
        normal.curve = TRUE,
        bar.measure = "density",
        messages = TRUE
      )
    )
  }
)

# subtitle return --------------------------------------------------

testthat::test_that(
  desc = "subtitle return",
  code = {
    testthat::skip_on_cran()

    # should return a list of length 3
    ls_results <- ggstatsplot::grouped_gghistostats(
      data = ggplot2::msleep,
      x = brainwt,
      grouping.var = vore,
      return = "subtitle",
      results.subtitle = NULL,
      messages = FALSE
    )

    # tests
    testthat::expect_equal(length(ls_results), 4L)
    testthat::expect_null(ls_results[[1]], NULL)
    testthat::expect_null(ls_results[[2]], NULL)
    testthat::expect_null(ls_results[[3]], NULL)
    testthat::expect_null(ls_results[[4]], NULL)
  }
)
