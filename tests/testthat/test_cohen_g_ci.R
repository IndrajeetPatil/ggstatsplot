context("cohen_G_ci")

# cohen_G_ci works --------------------------------

testthat::test_that(
  desc = "cohen_G_ci",
  code = {
    testthat::skip_on_cran()
    library(rcompanion)

    set.seed(123)
    df1 <- ggstatsplot:::cohenG_ci(AndersonRainGarden,
      nboot = 25,
      conf.level = 0.95,
      conf.type = "norm"
    )

    set.seed(123)
    df2 <- ggstatsplot:::cohenG_ci(AndersonRainGarden,
      nboot = 25,
      conf.level = 0.90,
      conf.type = "perc"
    )

    set.seed(123)
    df3 <- suppressWarnings(ggstatsplot:::cohenG_ci(AndersonRainGarden,
      nboot = 50,
      conf.level = 0.99,
      conf.type = "bca"
    ))

    set.seed(123)
    df4 <- ggstatsplot:::cohenG_ci(AndersonRainGarden,
      nboot = 25,
      conf.level = 0.50,
      conf.type = "basic"
    )

    # tests
    testthat::expect_equal(df1$r, 0.42000, tolerance = 0.001)
    testthat::expect_equal(df1$lower.ci, 0.3306817, tolerance = 0.001)
    testthat::expect_equal(df1$upper.ci, 0.5065983, tolerance = 0.001)

    testthat::expect_equal(df2$r, 0.42000, tolerance = 0.001)
    testthat::expect_equal(df2$lower.ci, 0.343617, tolerance = 0.001)
    testthat::expect_equal(df2$upper.ci, 0.500000, tolerance = 0.001)

    testthat::expect_equal(df3$r, 0.42000, tolerance = 0.001)
    testthat::expect_equal(df3$lower.ci, 0.250000, tolerance = 0.001)
    testthat::expect_equal(df3$upper.ci, 0.500000, tolerance = 0.001)

    testthat::expect_equal(df4$r, 0.42000, tolerance = 0.001)
    testthat::expect_equal(df4$lower.ci, 0.3795511, tolerance = 0.001)
    testthat::expect_equal(df4$upper.ci, 0.4479387, tolerance = 0.001)
  }
)
