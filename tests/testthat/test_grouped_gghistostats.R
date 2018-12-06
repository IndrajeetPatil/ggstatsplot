context("grouped_gghistostats")

testthat::test_that(
  desc = "grouped_gghistostats works",
  code = {

    # when arguments are entered as bare expressions
    set.seed(123)
    testthat::expect_true(inherits(
      ggstatsplot::grouped_gghistostats(
        data = ggplot2::msleep,
        x = brainwt,
        grouping.var = vore,
        type = "p",
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
        messages = TRUE
      )
    )
  }
)
