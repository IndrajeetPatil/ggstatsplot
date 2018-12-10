context("test_kw_eta_h_ci")

testthat::test_that(
  desc = "Confidence interval for effect size for Kruskal-Wallis test ",
  code = {
    testthat::skip_on_cran()

    # function output
    set.seed(123)
    using_function1 <-
      suppressWarnings(ggstatsplot:::kw_eta_h_ci(
        data = movies_long,
        genre,
        budget,
        nboot = 50,
        conf.level = 0.999,
        conf.type = "perc"
      ))


    # testing 4 conditions
    testthat::expect_equal(using_function1$eta_sq_H, 0.126, tolerance = 0.0001)
    testthat::expect_equal(using_function1$conf.low, 0.0978, tolerance = 0.0001)
    testthat::expect_equal(using_function1$conf.high, 0.178, tolerance = 0.0003)
    testthat::expect_equal(using_function1$nboot, 50)
  }
)
