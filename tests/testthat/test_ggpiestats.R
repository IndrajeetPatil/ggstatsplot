# context ------------------------------------------------------------
context(desc = "ggpiestats")

# checking labels and data from plot -------------------------------------

testthat::test_that(
  desc = "checking labels and data from plot with only `main` variable",
  code = {
    # creating the plot
    set.seed(123)
    p <- ggstatsplot::ggpiestats(
      data = ggplot2::msleep,
      main = vore,
      bf.message = TRUE,
      title = "mammalian sleep",
      legend.title = "vorarephilia",
      caption = "From ggplot2 package",
      perc.k = 2,
      messages = FALSE
    )

    # checking data used to create a plot
    dat <- p$data

    # subtitle used
    set.seed(123)
    p_subtitle <-
      ggstatsplot::subtitle_onesample_proptest(
        data = ggplot2::msleep,
        main = vore
      )

    # checking dimensions of data
    data_dims <- dim(dat)

    # testing everything is okay with data
    testthat::expect_equal(data_dims[1], 4L)
    testthat::expect_equal(data_dims[2], 4L)
    testthat::expect_equal(dat$perc[1], 26.30, tolerance = 1e-3)
    testthat::expect_equal(dat$perc[2], 6.58, tolerance = 1e-3)
    testthat::expect_equal(dat$perc[3], 42.10, tolerance = 1e-3)
    testthat::expect_equal(dat$perc[4], 25.00, tolerance = 1e-3)

    # checking plot labels
    testthat::expect_identical(p$labels$subtitle, p_subtitle)
    testthat::expect_identical(p$labels$title, "mammalian sleep")
    testthat::expect_identical(p$labels$caption, "From ggplot2 package")
    testthat::expect_null(p$labels$x, NULL)
    testthat::expect_null(p$labels$y, NULL)
  }
)
