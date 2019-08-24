# context -------------------------------------------------------------------

context("ggsignif_adder")

# ggsignif_adder works ----------------------------------------------------

testthat::test_that(
  desc = "ggsignif_adder works",
  code = {
    set.seed(123)
    library(ggplot2)

    # data
    df <- data.frame(x = iris$Species, y = iris$Sepal.Length)

    # plot
    p <- ggplot(df, aes(x, y)) + geom_boxplot()

    # dataframe with pairwise comparison test results
    df_pair <- pairwiseComparisons::pairwise_comparisons(df, x, y, messages = FALSE)

    # adding plot with
    p_new <- ggstatsplot:::ggsignif_adder(
      plot = p,
      x = x,
      y = y,
      df_pairwise = df_pair,
      data = df
    )

    # build the plot
    pb <- ggplot2::ggplot_build(p_new)

    # tests
    testthat::expect_equal(length(pb$data), 2L)
    testthat::expect_identical(
      as.character(unique(pb$data[[2]]$group)),
      c(
        "setosa-versicolor-1",
        "setosa-virginica-2",
        "versicolor-virginica-3"
      )
    )
  }
)

# context -------------------------------------------------------------------

context(desc = "ggsignif_position_calculator")

# y coordinates for ggsignif -------------------------------------------------

testthat::test_that(
  desc = "y coordinates for ggsignif are accurate",
  code = {


    # shouldn't work with NAs
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
