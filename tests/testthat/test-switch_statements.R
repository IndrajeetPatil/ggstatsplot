context("switch statements")

# switch for p adjustment ------------------------------------------------

testthat::test_that(
  desc = "switch for p adjustment works",
  code = {
    testthat::expect_error(p_adjust_text(NULL))
    testthat::expect_identical(p_adjust_text("none"), "None")
    testthat::expect_identical(
      p_adjust_text("fdr"),
      p_adjust_text("BH")
    )
    testthat::expect_identical(p_adjust_text("hochberg"), "Hochberg")
    testthat::expect_identical(p_adjust_text("hommel"), "Hommel")
    testthat::expect_identical(p_adjust_text("BY"), "Benjamini & Yekutieli")
    testthat::expect_identical(p_adjust_text("xyz"), "Holm")
  }
)


# switch for effect size type works ------------------------------------------

testthat::test_that(
  desc = "switch for effct size type works",
  code = {
    testthat::expect_identical(effsize_type_switch("none"), "unbiased")
    testthat::expect_identical(effsize_type_switch("d"), "biased")
    testthat::expect_identical(effsize_type_switch("biased"), "biased")
    testthat::expect_identical(effsize_type_switch("unbiased"), "unbiased")
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
