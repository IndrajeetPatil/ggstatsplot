context("lm_effsize_ci")

# lm_effsize_ci works (eta, partial = FALSE) --------------------------------

testthat::test_that(
  desc = "lm_effsize_ci works (eta, partial = FALSE)",
  code = {
    testthat::skip_on_cran()

    # creating lm object-1
    set.seed(123)
    lmobject1 <- stats::lm(
      formula = mpg ~ hp * wt,
      data = mtcars
    )

    # induce an NA in the dataset
    mtcars2 <- datasets::mtcars
    mtcars2[1, 1] <- NA
    mtcars2[2, 4] <- NA
    mtcars2[3, 6] <- NA

    # creating lm object-2
    set.seed(123)
    lmobject2 <- stats::aov(
      formula = mpg ~ hp * wt,
      data = mtcars2
    )

    # creating lm object-3
    set.seed(123)
    lmobject3 <-
      stats::aov(
        formula = mpg ~ wt + qsec + Error(disp / am),
        data = mtcars
      )

    # model-1
    set.seed(123)
    df1 <- ggstatsplot::lm_effsize_ci(
      object = lmobject1,
      partial = FALSE,
      conf.level = 0.95,
      nboot = 100
    )

    # model-2
    set.seed(123)
    df2 <- ggstatsplot::lm_effsize_ci(
      object = lmobject2,
      partial = FALSE,
      conf.level = 0.95,
      nboot = 100
    )

    # model-3
    set.seed(123)
    df3 <- ggstatsplot::lm_effsize_ci(
      object = lmobject3,
      partial = FALSE,
      conf.level = 0.95,
      nboot = 100
    )

    # model-1
    testthat::expect_equal(df1$etasq,
      c(0.60243734, 0.22434811, 0.05797826),
      tolerance = 0.001
    )
    testthat::expect_equal(df1$conf.low,
      c(0.467069780, 0.061202567, -0.005654606),
      tolerance = 0.001
    )
    testthat::expect_equal(df1$conf.high,
      c(0.7632418, 0.3793348, 0.1165496),
      tolerance = 0.001
    )
    testthat::expect_equal(df1$p.value,
      c(1.227536e-12, 4.856431e-08, 8.108307e-04),
      tolerance = 0.001
    )

    # model-2
    testthat::expect_equal(df2$etasq,
      c(0.60509996, 0.23602005, 0.05575475),
      tolerance = 0.001
    )
    testthat::expect_equal(df2$conf.low,
      c(0.47388762, 0.08958920, -0.01051093),
      tolerance = 0.001
    )
    testthat::expect_equal(df2$conf.high,
      c(0.7542232, 0.3781342, 0.1314104),
      tolerance = 0.001
    )
    testthat::expect_equal(df2$p.value,
      c(5.888676e-12, 6.423958e-08, 1.131499e-03),
      tolerance = 0.001
    )

    # model-3
    testthat::expect_equal(c(df3$etasq[[1]], df3$etasq[[2]]),
      c(0.07741842, 0.03034323),
      tolerance = 0.001
    )
    testthat::expect_equal(df3$conf.low,
      c(-0.008902341, -0.011105971),
      tolerance = 0.001
    )
    testthat::expect_equal(df3$conf.high,
      c(0.18450114, 0.07611324),
      tolerance = 0.001
    )
    testthat::expect_equal(df3$p.value,
      c(0.001762058, 0.038749571),
      tolerance = 0.001
    )

    # check if it's a tibble
    testthat::expect_identical(class(df1)[[1]], "tbl_df")
  }
)


# lm_effsize_ci works (eta, partial = TRUE) --------------------------------

testthat::test_that(
  desc = "lm_effsize_ci works (eta, partial = TRUE)",
  code = {
    testthat::skip_on_cran()

    # creating lm object-1
    set.seed(123)
    lmobject1 <- stats::lm(
      formula = mpg ~ hp + wt,
      data = mtcars
    )

    # induce an NA in the dataset
    mtcars2 <- datasets::mtcars
    mtcars2[1, 1] <- NA
    mtcars2[2, 4] <- NA
    mtcars2[3, 6] <- NA

    # creating lm object-2
    set.seed(123)
    lmobject2 <- stats::aov(
      formula = mpg ~ hp + wt,
      data = mtcars2
    )

    # creating lm object-3
    set.seed(123)
    lmobject3 <-
      stats::aov(
        formula = mpg ~ wt + Error(disp / am),
        data = mtcars
      )

    # model-1
    set.seed(123)
    df1 <- ggstatsplot::lm_effsize_ci(
      object = lmobject1,
      partial = TRUE,
      conf.level = 0.95,
      nboot = 30
    )

    # model-2
    set.seed(123)
    df2 <- ggstatsplot::lm_effsize_ci(
      object = lmobject2,
      partial = TRUE,
      conf.level = 0.95,
      nboot = 30
    )

    # model-3
    set.seed(123)
    df3 <- ggstatsplot::lm_effsize_ci(
      object = lmobject3,
      partial = TRUE,
      conf.level = 0.95,
      nboot = 30
    )

    # model-1
    testthat::expect_equal(df1$partial.etasq,
      c(0.7766852, 0.5643088),
      tolerance = 0.001
    )
    testthat::expect_equal(df1$conf.low,
      c(0.5969630, 0.2930543),
      tolerance = 0.001
    )
    testthat::expect_equal(df1$conf.high,
      c(0.8497958, 0.7050021),
      tolerance = 0.001
    )

    # model-2
    testthat::expect_equal(df2$partial.etasq,
      c(0.7920364, 0.5976704),
      tolerance = 0.001
    )
    testthat::expect_equal(df2$conf.low,
      c(0.6078739, 0.3161779),
      tolerance = 0.001
    )
    testthat::expect_equal(df2$conf.high,
      c(0.8623490, 0.7327352),
      tolerance = 0.001
    )

    # model-3
    testthat::expect_equal(df3$partial.etasq[[1]], 0.2752055, tolerance = 0.001)
    testthat::expect_equal(df3$conf.low, 0.03870961, tolerance = 0.001)
    testthat::expect_equal(df3$conf.high, 0.4903451, tolerance = 0.001)

    # check if it's a tibble
    testthat::expect_identical(class(df1)[[1]], "tbl_df")
  }
)

# lm_effsize_ci works (omega, partial = FALSE) --------------------------------

testthat::test_that(
  desc = "lm_effsize_ci works (omega, partial = FALSE)",
  code = {
    testthat::skip_on_cran()

    # creating lm object-1
    set.seed(123)
    lmobject1 <- stats::lm(
      formula = mpg ~ hp * cyl,
      data = mtcars
    )

    # induce an NA in the dataset
    mtcars2 <- datasets::mtcars
    mtcars2[1, 1] <- NA
    mtcars2[2, 4] <- NA
    mtcars2[3, 6] <- NA

    # creating lm object-2
    set.seed(123)
    lmobject2 <- stats::aov(
      formula = mpg ~ hp * cyl,
      data = mtcars2
    )

    # creating lm object-3
    set.seed(123)
    lmobject3 <-
      stats::aov(
        formula = mpg ~ wt + qsec + Error(disp / cyl),
        data = mtcars
      )

    # model-1
    set.seed(123)
    df1 <- ggstatsplot::lm_effsize_ci(
      object = lmobject1,
      effsize = "omega",
      partial = FALSE,
      conf.level = 0.90,
      nboot = 100
    )

    # model-2
    set.seed(123)
    df2 <- ggstatsplot::lm_effsize_ci(
      object = lmobject2,
      effsize = "omega",
      partial = FALSE,
      conf.level = 0.90,
      nboot = 100
    )

    # model-3
    set.seed(123)
    df3 <- ggstatsplot::lm_effsize_ci(
      object = lmobject3,
      effsize = "omega",
      partial = FALSE,
      conf.level = 0.90,
      nboot = 100
    )

    # model-1
    testthat::expect_equal(df1$omegasq,
      c(0.58995145, 0.12940189, 0.03130857),
      tolerance = 0.001
    )
    testthat::expect_equal(df1$conf.low,
      c(0.54423922, 0.13977454, 0.00628617),
      tolerance = 0.001
    )
    testthat::expect_equal(df1$conf.high,
      c(0.7976812, 0.5333574, 0.3263694),
      tolerance = 0.001
    )

    # model-2
    testthat::expect_equal(df2$omegasq,
      c(0.59329884, 0.12418814, 0.03173151),
      tolerance = 0.001
    )
    testthat::expect_equal(df2$conf.low,
      c(0.536910626, 0.125214261, 0.004576956),
      tolerance = 0.001
    )
    testthat::expect_equal(df2$conf.high,
      c(0.8002324, 0.5308140, 0.3350955),
      tolerance = 0.001
    )

    # model-3
    testthat::expect_equal(c(df3$omegasq[[1]], df3$omegasq[[2]]),
      c(0.05480726, 0.04170147),
      tolerance = 0.001
    )
    testthat::expect_equal(df3$conf.low,
      c(0.05130400, 0.02967707),
      tolerance = 0.001
    )
    testthat::expect_equal(df3$conf.high,
      c(0.4275268, 0.3881490),
      tolerance = 0.001
    )

    # check if it's a tibble
    testthat::expect_identical(class(df1)[[1]], "tbl_df")
  }
)


# lm_effsize_ci works (omega, partial = TRUE) --------------------------------

testthat::test_that(
  desc = "lm_effsize_ci works (omega, partial = TRUE)",
  code = {
    testthat::skip_on_cran()

    # creating lm object-1
    set.seed(123)
    lmobject1 <- stats::lm(
      formula = mpg ~ hp * wt,
      data = mtcars
    )

    # induce an NA in the dataset
    mtcars2 <- datasets::mtcars
    mtcars2[1, 1] <- NA
    mtcars2[2, 4] <- NA
    mtcars2[3, 6] <- NA

    # creating lm object-2
    set.seed(123)
    lmobject2 <- stats::aov(
      formula = mpg ~ hp * wt,
      data = mtcars2
    )

    # creating lm object-3
    set.seed(123)
    lmobject3 <-
      stats::aov(
        formula = mpg ~ wt + qsec + Error(disp / am),
        data = mtcars
      )

    # model-1
    set.seed(123)
    df1 <- ggstatsplot::lm_effsize_ci(
      object = lmobject1,
      effsize = "omega",
      partial = TRUE,
      conf.level = 0.95,
      nboot = 20
    )

    # model-2
    set.seed(123)
    df2 <- ggstatsplot::lm_effsize_ci(
      object = lmobject2,
      effsize = "omega",
      partial = TRUE,
      conf.level = 0.95,
      nboot = 20
    )

    # model-3
    set.seed(123)
    df3 <- ggstatsplot::lm_effsize_ci(
      object = lmobject3,
      effsize = "omega",
      partial = TRUE,
      conf.level = 0.95,
      nboot = 20
    )

    # model-1
    testthat::expect_equal(df1$partial.omegasq,
      c(0.8195960, 0.6257830, 0.2902689),
      tolerance = 0.001
    )
    testthat::expect_equal(df1$conf.low,
      c(0.71560787, 0.34674153, -0.02239435),
      tolerance = 0.001
    )
    testthat::expect_equal(df1$conf.high,
      c(0.9358406, 0.8688754, 0.5913419),
      tolerance = 0.001
    )

    # model-2
    testthat::expect_equal(df2$partial.omegasq,
      c(0.8339922, 0.6596917, 0.3014787),
      tolerance = 0.001
    )
    testthat::expect_equal(df2$conf.low,
      c(0.78771654, 0.36172052, 0.07688201),
      tolerance = 0.001
    )
    testthat::expect_equal(df2$conf.high,
      c(0.9195726, 0.9426915, 0.5887994),
      tolerance = 0.001
    )

    # model-3
    testthat::expect_equal(c(df3$partial.omegasq[[1]], df3$partial.omegasq[[2]]),
      c(0.2565812, 0.1041597),
      tolerance = 0.001
    )
    testthat::expect_equal(df3$conf.low,
      c(-0.02471128, -0.11370840),
      tolerance = 0.001
    )
    testthat::expect_equal(df3$conf.high,
      c(0.6057236, 0.4098428),
      tolerance = 0.001
    )

    # check if it's a tibble
    testthat::expect_identical(class(df1)[[1]], "tbl_df")
  }
)
