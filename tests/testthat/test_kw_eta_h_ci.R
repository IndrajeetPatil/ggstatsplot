context("test_kw_eta_h_ci")

testthat::test_that(
  desc = "Confidence interval for effect size for Kruskal-Wallis test ",
  code = {

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
    testthat::expect_equal(using_function1$eta_sq_H, 0.0765, tolerance = 0.0001)
    testthat::expect_equal(using_function1$conf.low, 0.0365, tolerance = 0.0001)
    testthat::expect_equal(using_function1$conf.high, 0.110, tolerance = 0.0001)
    testthat::expect_equal(using_function1$nboot, 50)
  }
)
