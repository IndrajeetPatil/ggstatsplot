context("Cramer's V ci")

testthat::test_that(
  desc = "`cramer_v_ci()` works",
  code = {
    testthat::skip_on_cran()

    testthat::test_that(
      desc = "error when not a table",
      code = {
        testthat::expect_error(
          ggstatsplot:::cramer_v_ci(Titanic_full$Class)
        )
      }
    )


    set.seed(123)
    library(ggstatsplot)
    res1 <- ggstatsplot:::cramer_v_ci(table(Titanic_full$Class))

    # tests
    testthat::expect_equal(res1[[1]], 0.35905, tolerance = 0.001)
    testthat::expect_equal(res1[[2]], 0.33767, tolerance = 0.001)
    testthat::expect_equal(res1[[3]], 0.37948, tolerance = 0.001)

    set.seed(123)
    res2 <- ggstatsplot:::cramer_v_ci(table(Titanic_full$Sex),
      conf.level = .999,
      method = "ncchisqadj",
      p = c(.4, .6)
    )

    # tests
    testthat::expect_equal(res2[[1]], 0.38061, tolerance = 0.001)
    testthat::expect_equal(res2[[2]], 0.31112, tolerance = 0.001)
    testthat::expect_equal(res2[[3]], 0.45123, tolerance = 0.001)

    # Too long for CRAN
    #    set.seed(123)
    #    res3 <- ggstatsplot:::cramer_v_ci(table(Titanic_full$Sex),
    #      conf.level = .99,
    #      method = "ncchisq",
    #      p = c(.2135, .7865)
    #    )
    #
    #    # tests
    #    testthat:::expect_equal(res3[[1]], 0.008689, tolerance = 0.001)
    #    testthat:::expect_equal(res3[[2]], 0.000000, tolerance = 0.001)
    #    testthat:::expect_equal(res3[[3]], 0.063036, tolerance = 0.001)
  }
)
