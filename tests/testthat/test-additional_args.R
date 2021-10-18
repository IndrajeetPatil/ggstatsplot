# within-subjects design - NAs ---------------------------------------

test_that(
  desc = "`pairwise_comparisons()` - test additional arguments",
  code = {
    options(tibble.width = Inf)

    # student's t test
    set.seed(123)
    df1 <- pairwise_comparisons(
      data = bugs_long,
      x = condition,
      y = desire,
      paired = TRUE,
      p.adjust.method = "none",
      alternative = "less"
    )

    set.seed(123)
    df2 <- pairwise_comparisons(
      data = bugs_long,
      x = condition,
      y = desire,
      paired = TRUE,
      p.adjust.method = "none",
      alternative = "greater"
    )

    set.seed(123)
    df3 <- pairwise_comparisons(
      data = mtcars,
      x = cyl,
      y = wt,
      var.equal = TRUE,
      p.adjust.method = "none",
      alternative = "less"
    )

    set.seed(123)
    df4 <- pairwise_comparisons(
      data = mtcars,
      x = cyl,
      y = wt,
      var.equal = TRUE,
      p.adjust.method = "none",
      alternative = "greater"
    )

    set.seed(123)
    expect_snapshot(list(df1, df2, df3, df4))
  }
)
