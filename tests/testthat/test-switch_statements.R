context("switch statements")

# switch for p adjustment ------------------------------------------------

testthat::test_that(
  desc = "switch for p adjustment works",
  code = {
    testthat::skip_on_cran()

    testthat::expect_error(p_adjust_text(NULL))
    testthat::expect_identical(p_adjust_text("none"), "None")
    testthat::expect_identical(
      p_adjust_text("fdr"),
      p_adjust_text("BH")
    )
    testthat::expect_identical(p_adjust_text("xyz"), "Holm")
  }
)

# switch for effct size type works ------------------------------------------

testthat::test_that(
  desc = "switch for effct size type works",
  code = {
    testthat::skip_on_cran()

    testthat::expect_identical(effsize_type_switch(NULL), "unbiased")
    testthat::expect_identical(effsize_type_switch("none"), "unbiased")
    testthat::expect_identical(effsize_type_switch("d"), "biased")
    testthat::expect_identical(effsize_type_switch("g"), "unbiased")
    testthat::expect_identical(effsize_type_switch("eta"), "biased")
    testthat::expect_identical(effsize_type_switch("p_eta"), "biased")
    testthat::expect_identical(effsize_type_switch("partial_eta"), "biased")
    testthat::expect_identical(effsize_type_switch("partial.eta"), "biased")
    testthat::expect_identical(effsize_type_switch("omega"), "unbiased")
    testthat::expect_identical(effsize_type_switch("p_omega"), "unbiased")
    testthat::expect_identical(effsize_type_switch("partial_omega"), "unbiased")
    testthat::expect_identical(effsize_type_switch("partial.omega"), "unbiased")
    testthat::expect_identical(effsize_type_switch("xyz"), "unbiased")
  }
)


# switch for stats type works ------------------------------------------

testthat::test_that(
  desc = "switch for stats type works",
  code = {
    testthat::skip_on_cran()

    testthat::expect_identical(stats_type_switch(NULL), "parametric")
    testthat::expect_identical(stats_type_switch("p"), "parametric")
    testthat::expect_identical(stats_type_switch("pearson"), "parametric")
    testthat::expect_identical(stats_type_switch("non-parametric"), "nonparametric")
    testthat::expect_identical(stats_type_switch("np"), "nonparametric")
    testthat::expect_identical(stats_type_switch("r"), "robust")
    testthat::expect_identical(stats_type_switch("bf"), "bayes")
    testthat::expect_identical(stats_type_switch("xxx"), "parametric")
  }
)
