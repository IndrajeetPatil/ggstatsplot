context("Specify decimals")

string1 <- specify_decimal_p(.00001234)
string2 <- specify_decimal_p(.00001234, p.value = TRUE)
string3 <- specify_decimal_p(.00001234, p.value = TRUE, k = 8)

test_that("specify_decimal_p works", {
#  set.seed(123)

  # testing three conditions
  testthat::expect_match(string1, "0.000")
  testthat::expect_match(string2, "< 0.001")
  testthat::expect_match(string3, "1.234e-05")
})
