if (require("vdiffr")) {
  test_that("plots are rendered correctly", {
    skip_on_cran()

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "gghistostats works",
      gghistostats(mtcars, wt, test.value = 3)
    )
  })
}
