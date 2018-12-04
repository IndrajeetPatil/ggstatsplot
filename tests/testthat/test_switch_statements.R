context("switch statements")

# switch for p adjustment ------------------------------------------------

testthat::test_that(
  desc = "switch for p adjustment works",
  code = {
    testthat::expect_error(p.adjust.method.description(NULL))
    testthat::expect_identical(p.adjust.method.description("none"), "None")
    testthat::expect_identical(
      p.adjust.method.description("fdr"),
      p.adjust.method.description("BH")
    )
    testthat::expect_identical(p.adjust.method.description("xyz"), "Holm")
  }
)


# switch for effct size type works ------------------------------------------

testthat::test_that(
  desc = "switch for effct size type works",
  code = {
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
