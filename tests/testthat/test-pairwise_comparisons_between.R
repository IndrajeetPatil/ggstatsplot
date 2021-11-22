# between-subjects design --------------------------------------------------

test_that(
  desc = "`pairwise_comparisons()` works for between-subjects design",
  code = {
    set.seed(123)
    skip_if_not_installed("PMCMRplus")

    options(tibble.width = Inf)

    # student's t
    set.seed(123)
    df1 <- pairwise_comparisons(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      type = "p",
      var.equal = TRUE,
      paired = FALSE,
      p.adjust.method = "bonferroni"
    )

    # games-howell
    df_ggplot2::msleep <- ggplot2::msleep

    # adding empty factor level (shouldn't change results)
    df_ggplot2::msleep %<>% dplyr::mutate(vore = as.factor(vore))

    df_ggplot2::msleep$vore <- factor(df_ggplot2::msleep$vore, levels = c(levels(df_ggplot2::msleep$vore), "Random"))

    set.seed(123)
    df2 <- pairwise_comparisons(
      data = df_ggplot2::msleep,
      x = vore,
      y = brainwt,
      type = "p",
      var.equal = FALSE,
      paired = FALSE,
      p.adjust.method = "bonferroni"
    )

    # Dunn test
    set.seed(123)
    df3 <- pairwise_comparisons(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      type = "np",
      paired = FALSE,
      p.adjust.method = "none"
    )

    # robust t test
    set.seed(123)
    df4 <- pairwise_comparisons(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      type = "r",
      paired = FALSE,
      p.adjust.method = "fdr"
    )

    # checking the edge case where factor level names contain `-`
    set.seed(123)
    df5 <- pairwise_comparisons(
      data = movies_long,
      x = mpaa,
      y = rating,
      var.equal = TRUE
    )

    # bayes test
    set.seed(123)
    df6 <- pairwise_comparisons(
      data = df_ggplot2::msleep,
      x = vore,
      y = brainwt,
      type = "bf",
      k = 3
    )

    expect_snapshot(list(df1, df2, df3, df4, df5))


    expect_equal(df6$log_e_bf10,
      c(
        -0.616556955077368,
        -0.331816123738985,
        -0.850766925918558,
        -0.615915090483787,
        -0.559562332764069,
        -0.6062922675725
      ),
      tolerance = 0.01
    )
  }
)

# dropped levels --------------------------------------------------

test_that(
  desc = "dropped levels are not included",
  code = {
    set.seed(123)
    skip_if_not_installed("PMCMRplus")

    # drop levels
    ggplot2::msleep2 <- dplyr::filter(.data = ggplot2::msleep, vore %in% c("carni", "omni"))

    # check those levels are not included
    set.seed(123)
    df1 <- pairwise_comparisons(
      data = ggplot2::msleep2,
      x = vore,
      y = brainwt,
      p.adjust.method = "none"
    )

    set.seed(123)
    df2 <- pairwise_comparisons(
      data = ggplot2::msleep,
      x = vore,
      y = brainwt,
      p.adjust.method = "none"
    ) %>%
      dplyr::filter(group2 == "omni", group1 == "carni")

    # tests
    expect_equal(df1$statistic, df2$statistic, tolerance = 0.01)
    expect_identical(df2$label, "list(~italic(p)[uncorrected]==0.865)")
  }
)

# data without NAs --------------------------------------------------

test_that(
  desc = "data without NAs",
  code = {
    skip_if_not_installed("PMCMRplus")
    set.seed(123)
    df <- pairwise_comparisons(
      data = iris,
      x = Species,
      y = Sepal.Length,
      type = "p",
      p.adjust.method = "fdr",
      var.equal = TRUE
    )

    expect_equal(
      df$label,
      c(
        "list(~italic(p)[FDR-corrected]==1.32e-15)",
        "list(~italic(p)[FDR-corrected]==6.64e-32)",
        "list(~italic(p)[FDR-corrected]==2.77e-09)"
      )
    )
  }
)
