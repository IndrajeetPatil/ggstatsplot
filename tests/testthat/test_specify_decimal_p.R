context("Specify decimals")

test_that("specify_decimal_p works", {

  # for reproducibility
  set.seed(123)

  # creating objects to test
  string1 <- specify_decimal_p(x = .00001234)
  string2 <- specify_decimal_p(x = .00001234, p.value = TRUE)
  string3 <- specify_decimal_p(x = .00001234, p.value = TRUE, k = 8)

  # testing three conditions
  set.seed(123)

  # tests
  testthat::expect_match(object = string1, regexp = "0.000")
  testthat::expect_match(object = string2, regexp = "< 0.001")
  testthat::expect_match(object = string3, regexp = "1.234e-05")
  testthat::expect_error(object = specify_decimal_p("123"))
})
