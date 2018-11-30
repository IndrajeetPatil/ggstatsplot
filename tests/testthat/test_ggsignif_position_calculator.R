# context -------------------------------------------------------------------
context(desc = "ggsignif_position_calculator")

# y coordinates for ggsignif -------------------------------------------------

testthat::test_that(
  desc = "y coordinates for ggsignif are accurate",
  code = {
    testthat::expect_error(ggstatsplot:::ggsignif_position_calculator(NA, NA))

    # creating needed data
    set.seed(123)
    library(ggplot2)

    # check if unused levels of x are not getting used
    msleep2 <- dplyr::filter(.data = msleep, vore != "omni")

    # without NAs
    testthat::expect_equal(
      ggstatsplot:::ggsignif_position_calculator(x = mtcars$cyl, y = mtcars$mpg),
      c(34.7475, 36.5100, 38.2725),
      tolerance = 0.002
    )

    # with NAs
    testthat::expect_equal(
      ggstatsplot:::ggsignif_position_calculator(x = msleep$vore, y = msleep$brainwt),
      c(
        5.854800,
        6.172126,
        6.489451,
        6.806777,
        7.124102,
        7.441428,
        7.758753,
        8.076079,
        8.393404,
        8.710730
      ),
      tolerance = 0.002
    )

    # with NAs and dropped levels
    testthat::expect_equal(
      ggstatsplot:::ggsignif_position_calculator(x = msleep2$vore, y = msleep2$brainwt),
      c(5.854800, 6.283181, 6.711562),
      tolerance = 0.002
    )
  }
)
