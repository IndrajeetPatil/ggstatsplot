context("theme_ggstatsplot")

# `theme_ggstatsplot()` works --------------------------------------------------

testthat::test_that(
  desc = "`theme_ggstatsplot()` works",
  code = {
    testthat::skip_on_cran()

    set.seed(123)
    library(ggplot2)

    # plot
    p <- ggplot(msleep, aes(vore, brainwt)) + geom_point(na.rm = TRUE)

    # changing the basic plot
    p1 <- p +
      ggstatsplot::theme_ggstatsplot(ggstatsplot.layer = FALSE)
    p2 <- p +
      ggstatsplot::theme_ggstatsplot(ggplot2::theme_dark())
    p3 <- p +
      ggstatsplot::theme_ggstatsplot(ggplot2::theme_minimal())

    # check if outputs are ggplot objects
    testthat::expect_identical(class(p1), c("gg", "ggplot"))
    testthat::expect_identical(class(p2), c("gg", "ggplot"))
    testthat::expect_identical(class(p3), c("gg", "ggplot"))

    # creating dataframes with theme information
    df1 <- tibble::enframe(ggstatsplot::theme_ggstatsplot())
    df2 <- tibble::enframe(ggstatsplot::theme_ggstatsplot(theme_bw()), TRUE)
    df3 <- tibble::enframe(ggstatsplot::theme_ggstatsplot(
      ggplot2::theme_grey(),
      ggstatsplot.layer = FALSE
    ))

    # checking legend text
    testthat::expect_equal(df1$value[[47]]$size, 13L)
    testthat::expect_equal(df2$value[[47]]$size, 13L)
    testthat::expect_equal(as.numeric(df3$value[[27]]$size), 0.8, tolerance = 0.1)
  }
)

# `theme_pie()` works --------------------------------------------------

testthat::test_that(
  desc = "`theme_pie()` works",
  code = {
    set.seed(123)
    library(ggplot2)

    df1 <- tibble::enframe(ggstatsplot::theme_pie())
    df2 <- tibble::enframe(ggstatsplot::theme_pie(theme_classic()), FALSE)
    df3 <- tibble::enframe(ggstatsplot::theme_pie(
      ggplot2::theme_bw(),
      ggstatsplot.layer = FALSE
    ))

    # checking legend text
    testthat::expect_equal(df1$value[[27]]$size, 10L)
    testthat::expect_equal(df2$value[[27]]$size, 10L)
    testthat::expect_equal(as.numeric(df3$value[[27]]$size), 0.8, tolerance = 0.1)
    testthat::expect_identical(as.character(df3$value[[35]][1]), "0cm")
    testthat::expect_identical(as.character(df2$value[[35]][1]), "5pt")
  }
)
