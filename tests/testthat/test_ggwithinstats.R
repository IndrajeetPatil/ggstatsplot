context(desc = "ggwithinstats")

# basic plotting works ----------------------------------------------------

testthat::test_that(
  desc = "basic plotting works",
  code = {
    set.seed(123)

    p1 <- ggstatsplot:::ggwithinstats(
      data = ggstatsplot::iris_long,
      x = condition,
      y = value,
      type = "bf",
      outlier.tagging = TRUE,
      pairwise.comparisons = TRUE,
      messages = TRUE
    )

    p2 <- ggstatsplot:::ggwithinstats(
      data = ggstatsplot::iris_long,
      x = condition,
      y = value,
      outlier.tagging = TRUE,
      pairwise.comparisons = TRUE,
      pairwise.display = "ns",
      pairwise.annotation = "p",
      bf.message = TRUE,
      messages = TRUE
    )

    p3 <- ggstatsplot:::ggwithinstats(
      data = ggstatsplot::iris_long,
      x = attribute,
      y = value,
      pairwise.comparisons = TRUE,
      pairwise.display = "s",
      pairwise.annotation = "asterisk",
      messages = FALSE,
      bf.message = TRUE
    )

    p4 <- ggstatsplot:::ggwithinstats(
      data = ggstatsplot::iris_long,
      x = attribute,
      y = value,
      results.subtitle = FALSE,
      pairwise.comparisons = TRUE,
      axes.range.restrict = TRUE,
      messages = FALSE
    )

    p5 <- ggstatsplot:::ggwithinstats(
      data = ggstatsplot::iris_long,
      x = attribute,
      y = value,
      outlier.tagging = TRUE,
      outlier.label = condition,
      messages = FALSE
    )

    testthat::expect_is(p1, "ggplot")
    testthat::expect_is(p2, "ggplot")
    testthat::expect_is(p3, "ggplot")
    testthat::expect_is(p4, "ggplot")
  }
)
