context(desc = "ggwithinstats")

# basic plotting works ----------------------------------------------------

testthat::test_that(
  desc = "basic plotting works",
  code = {
    set.seed(123)

    p0 <- ggstatsplot:::ggwithinstats(
      data = ggstatsplot::iris_long,
      x = condition,
      y = value,
      type = "p",
      outlier.tagging = TRUE,
      pairwise.comparisons = TRUE,
      messages = TRUE
    )

    p1 <- ggstatsplot:::ggwithinstats(
      data = ggstatsplot::iris_long,
      x = condition,
      y = value,
      type = "p",
      pairwise.display = "ns",
      outlier.tagging = TRUE,
      pairwise.comparisons = TRUE,
      messages = FALSE
    )

    p2 <- ggstatsplot:::ggwithinstats(
      data = ggstatsplot::iris_long,
      x = condition,
      y = value,
      type = "bf",
      outlier.tagging = TRUE,
      pairwise.comparisons = TRUE,
      messages = FALSE
    )

    p3 <- ggstatsplot:::ggwithinstats(
      data = ggstatsplot::iris_long,
      x = condition,
      y = value,
      outlier.tagging = TRUE,
      pairwise.comparisons = TRUE,
      pairwise.annotation = "p",
      bf.message = TRUE,
      messages = FALSE
    )

    p4 <- ggstatsplot:::ggwithinstats(
      data = ggstatsplot::iris_long,
      x = attribute,
      y = value,
      pairwise.comparisons = TRUE,
      pairwise.display = "ns",
      pairwise.annotation = "asterisk",
      messages = FALSE,
      bf.message = TRUE
    )

    p5 <- ggstatsplot:::ggwithinstats(
      data = ggstatsplot::iris_long,
      x = attribute,
      y = value,
      results.subtitle = FALSE,
      pairwise.comparisons = TRUE,
      axes.range.restrict = TRUE,
      messages = FALSE
    )

    p6 <- ggstatsplot:::ggwithinstats(
      data = ggstatsplot::iris_long,
      x = attribute,
      y = value,
      axes.range.restrict = TRUE,
      outlier.tagging = TRUE,
      outlier.label = condition,
      messages = FALSE
    )

    testthat::expect_is(p0, "ggplot")
    testthat::expect_is(p1, "ggplot")
    testthat::expect_is(p2, "ggplot")
    testthat::expect_is(p3, "ggplot")
    testthat::expect_is(p4, "ggplot")
    testthat::expect_is(p5, "ggplot")
    testthat::expect_is(p6, "ggplot")
  }
)
