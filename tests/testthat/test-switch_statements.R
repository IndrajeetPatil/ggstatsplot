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
    testthat::expect_identical(p_adjust_text("bonferroni"), "Bonferroni")
    testthat::expect_identical(p_adjust_text("holm"), "Holm")
    testthat::expect_identical(p_adjust_text("hommel"), "Hommel")
    testthat::expect_identical(p_adjust_text("BY"), "Benjamini & Yekutieli")
    testthat::expect_identical(p_adjust_text("xyz"), "Holm")
  }
)
