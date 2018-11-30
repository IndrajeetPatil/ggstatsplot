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
  }
)
