# context ------------------------------------------------------------
context(desc = "ggbetweenstats")

# visual tests ------------------------------------------------------------

testthat::test_that(
  desc = "ggbetweenstats works",
  code = {
    # plot to compare to
    ggbetweenstats_anova <- ggstatsplot::ggbetweenstats(
      data = datasets::iris,
      x = Species,
      y = Sepal.Length,
      messages = FALSE
    )
    # comparison using vdiffr package
    vdiffr::expect_doppelganger(
      title = "ggbetweenstats_anova",
      fig = ggbetweenstats_anova
    )
  }
)
