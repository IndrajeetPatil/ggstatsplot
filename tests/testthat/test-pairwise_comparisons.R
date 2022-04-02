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
    df_msleep <- ggplot2::msleep

    # adding empty factor level (shouldn't change results)
    df_msleep %<>% dplyr::mutate(vore = as.factor(vore))

    df_msleep$vore <- factor(df_msleep$vore, levels = c(levels(df_msleep$vore), "Random"))

    set.seed(123)
    df2 <- pairwise_comparisons(
      data = df_msleep,
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
      data = df_msleep,
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
    msleep2 <- dplyr::filter(.data = ggplot2::msleep, vore %in% c("carni", "omni"))

    # check those levels are not included
    set.seed(123)
    df1 <- pairwise_comparisons(
      data = msleep2,
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


    expect_equal(df1$statistic, df2$statistic, tolerance = 0.01)
    expect_snapshot(df2$label)
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

    expect_snapshot(df$label)
  }
)


# within-subjects design - NAs --------------------------------------------

test_that(
  desc = "`pairwise_comparisons()` works for within-subjects design - NAs",
  code = {
    options(tibble.width = Inf)
    skip_if_not_installed("PMCMRplus")

    # student's t test
    set.seed(123)
    df1 <- pairwise_comparisons(
      data = bugs_long,
      x = condition,
      y = desire,
      type = "p",
      k = 3,
      paired = TRUE,
      p.adjust.method = "bonferroni"
    )

    # Durbin-Conover test
    set.seed(123)
    df2 <- pairwise_comparisons(
      data = bugs_long,
      x = condition,
      y = desire,
      type = "np",
      k = 3,
      paired = TRUE,
      p.adjust.method = "BY"
    )

    # robust t test
    set.seed(123)
    df3 <- pairwise_comparisons(
      data = bugs_long,
      x = condition,
      y = desire,
      type = "r",
      k = 3,
      paired = TRUE,
      p.adjust.method = "hommel"
    )

    # bf
    set.seed(123)
    df4 <- pairwise_comparisons(
      data = bugs_long,
      x = condition,
      y = desire,
      type = "bf",
      k = 4,
      paired = TRUE
    )

    set.seed(123)
    expect_snapshot(list(df1, df2, df3))

    expect_equal(df4$log_e_bf10,
      c(
        3.72728778555223,
        -0.539360770276211,
        23.2071488954099,
        -0.3589384624894,
        2.89663700378694,
        15.3854222237555
      ),
      tolerance = 0.01
    )
  }
)



# within-subjects design - no NAs -----------------------------------------

test_that(
  desc = "`pairwise_comparisons()` works for within-subjects design - without NAs",
  code = {
    options(tibble.width = Inf)
    skip_if_not_installed("PMCMRplus")

    # student's t test
    set.seed(123)
    df1 <- pairwise_comparisons(
      data = WRS2::WineTasting,
      x = Wine,
      y = Taste,
      type = "p",
      k = 3,
      paired = TRUE,
      p.adjust.method = "none"
    )

    # Durbin-Conover test
    set.seed(123)
    df2 <- pairwise_comparisons(
      data = WRS2::WineTasting,
      x = Wine,
      y = Taste,
      type = "np",
      k = 3,
      paired = TRUE,
      p.adjust.method = "none"
    )

    # robust t test
    set.seed(123)
    df3 <- pairwise_comparisons(
      data = WRS2::WineTasting,
      x = Wine,
      y = Taste,
      type = "r",
      k = 3,
      paired = TRUE,
      p.adjust.method = "none"
    )

    # bf
    set.seed(123)
    df4 <- pairwise_comparisons(
      data = WRS2::WineTasting,
      x = Wine,
      y = Taste,
      type = "bf",
      k = 4,
      paired = TRUE
    )

    expect_snapshot(list(df1, df2, df3))

    expect_equal(df4$log_e_bf10,
      c(-1.44618964442711, 1.31224804728311, 3.92141234993467),
      tolerance = 0.01
    )
  }
)

# works with subject id ------------------------------------------------------

test_that(
  desc = "works with subject id",
  code = {
    set.seed(123)
    skip_if_not_installed("PMCMRplus")

    # with subject id
    df1 <- purrr::pmap_dfr(
      .f = pairwise_comparisons,
      .l = list(
        data = list(WRS2::WineTasting),
        x = list("Wine"),
        y = list("Taste"),
        type = list("p", "np", "r", "bf"),
        k = 3,
        subject.id = list("Taster"),
        paired = TRUE
      )
    )

    # without subject id but sorted by it
    set.seed(123)
    df2 <- purrr::pmap_dfr(
      .f = pairwise_comparisons,
      .l = list(
        data = list(dplyr::arrange(WRS2::WineTasting, Taster)),
        x = list("Wine"),
        y = list("Taste"),
        type = list("p", "np", "r", "bf"),
        k = 3,
        paired = TRUE
      )
    )

    # columns should be same no matter the test
    expect_equal(dplyr::select(df1, -expression), dplyr::select(df2, -expression))
    expect_equal(dplyr::select(df1, expression), dplyr::select(df2, expression), ignore_attr = TRUE)
  }
)

# additional arguments are passed ---------------------------------------

test_that(
  desc = "additional arguments are passed to underlying methods",
  code = {
    options(tibble.width = Inf)
    skip_if_not_installed("PMCMRplus")

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
