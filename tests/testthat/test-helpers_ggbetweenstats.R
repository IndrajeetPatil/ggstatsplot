
# mean_ggrepel works ----------------------------------------------

testthat::test_that(
  desc = "mean_ggrepel works",
  code = {
    testthat::skip_on_cran()

    set.seed(123)
    library(ggplot2)

    # make a plot
    p <- ggplot(data = msleep, aes(x = vore, y = brainwt)) +
      geom_boxplot(na.rm = TRUE)

    p_dat <- ggstatsplot:::mean_labeller(
      data = msleep,
      x = vore,
      y = brainwt,
      mean.ci = TRUE
    )

    # add means
    p_mean <- ggstatsplot:::mean_ggrepel(
      plot = p,
      x = vore,
      y = brainwt,
      mean.data = p_dat,
      mean.color = "blue"
    )

    p_new <- ggstatsplot:::mean_ggrepel(
      plot = p,
      x = vore,
      y = brainwt,
      mean.data = p_dat,
      mean.color = "blue",
      inherit.aes = FALSE
    )

    # build plot for tests
    pb_mean <- ggplot2::ggplot_build(p_mean)

    # check data
    testthat::expect_equal(dim(pb_mean$data[[1]]), c(5L, 25L))
    testthat::expect_equal(dim(pb_mean$data[[2]]), c(5L, 12L))
    testthat::expect_equal(dim(pb_mean$data[[3]]), c(4L, 15L))
    testthat::expect_equal(
      pb_mean$data[[2]]$y,
      c(0.07925556, 0.62159750, 0.02155000, 0.14573118, 0.00762600),
      tolerance = 0.001
    )
    testthat::expect_identical(
      as.character(unique(pb_mean$data[[2]]$colour)),
      "blue"
    )

    testthat::expect_is(p_new, "ggplot")
  }
)


# mean labelling works -------------------------------------------------------

testthat::test_that(
  desc = "mean_labeller works",
  code = {
    testthat::skip_on_cran()

    # ----------------------- data without NAs ------------------------------

    # creating a smaller dataframe
    set.seed(123)
    diamonds_short <-
      dplyr::sample_frac(tbl = ggplot2::diamonds, size = 0.05) %>%
      dplyr::filter(.data = ., cut != "Ideal")

    # ggstatsplot output
    set.seed(123)
    mean_dat <-
      ggstatsplot:::mean_labeller(
        data = diamonds_short,
        x = cut,
        y = price,
        mean.ci = TRUE,
        k = 4
      )

    # check that dropped factor level is not retained
    testthat::expect_equal(
      object = length(levels(mean_dat$cut)) + 1,
      expected = length(levels(diamonds_short$cut))
    )

    # check mean label for first factor level
    testthat::expect_identical(
      mean_dat$label,
      c(
        "list(~italic(mu)==3819.5769,CI[95*'%'](3140.8008,4498.3531))",
        "list(~italic(mu)==4053.4397,CI[95*'%'](3613.1020,4493.7773))",
        "list(~italic(mu)==3928.2492,CI[95*'%'](3607.0316,4249.4667))",
        "list(~italic(mu)==4602.0879,CI[95*'%'](4274.7306,4929.4452))"
      )
    )

    # check sample size label for first factor level
    testthat::expect_identical(
      mean_dat$n_label,
      c(
        "Fair\n(n = 78)",
        "Good\n(n = 257)",
        "Very Good\n(n = 602)",
        "Premium\n(n = 671)"
      )
    )

    # ------------------------- data with NAs ------------------------------

    # ggstatsplot output
    set.seed(123)
    mean_dat2 <-
      ggstatsplot:::mean_labeller(
        data = ggplot2::msleep,
        x = vore,
        y = brainwt,
        mean.ci = TRUE,
        k = 3
      )

    # when factor level contains NAs
    testthat::expect_equal(
      object = length(levels(mean_dat2$vore)),
      expected = length(levels(as.factor(
        ggplot2::msleep$vore
      )))
    )
  }
)

# outlier_df ----------------------------------------------------

context(desc = "outlier_df")

# outlier_df works ----------------------------------------------------

testthat::test_that(
  desc = "outlier_df works as expected",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # dataframe with outlier column (data without NA)
    df1 <- ggstatsplot:::outlier_df(
      data = morley,
      x = "Expt",
      y = Speed,
      outlier.label = "Run",
      outlier.coef = 2
    ) %>%
      dplyr::arrange(outlier)

    testthat::expect_equal(dim(df1), c(100L, 5L))
    testthat::expect_equal(dim(tidyr::drop_na(df1)), c(4L, 5L))

    # dataframe with outlier column (data with NA)
    df2 <- ggstatsplot:::outlier_df(
      data = ggplot2::msleep,
      x = vore,
      y = "brainwt",
      outlier.label = genus,
      outlier.coef = 3
    ) %>%
      dplyr::arrange(outlier)

    testthat::expect_equal(dim(df2), c(83L, 13L))
    testthat::expect_equal(dim(dplyr::filter(df2, !is.na(outlier))), c(4L, 13L))
  }
)

# sort_xy works as expected ---------------------------------------------------

testthat::test_that(
  desc = "sort_xy works as expected",
  code = {
    testthat::skip_on_cran()
    library(ggplot2)

    # without NAs
    set.seed(123)
    df1 <- ggstatsplot:::sort_xy(iris_long, condition, value, sort = "none")
    df2 <- ggstatsplot:::sort_xy(iris_long, condition, value, sort = "descending")
    df3 <- ggstatsplot:::sort_xy(iris_long, condition, value, sort = "ascending")

    testthat::expect_equal(
      levels(df1$condition),
      c("Sepal.Length", "Petal.Length", "Sepal.Width", "Petal.Width")
    )
    testthat::expect_equal(levels(df1$condition), levels(df2$condition))
    testthat::expect_equal(levels(df3$condition), rev(levels(df1$condition)))
    testthat::expect_equal(names(iris_long), names(df1))

    # with NAs
    set.seed(123)
    df4 <- ggstatsplot:::sort_xy(msleep, vore, brainwt, sort = "none")
    df5 <- ggstatsplot:::sort_xy(msleep, vore, brainwt, sort = "descending")
    df6 <- ggstatsplot:::sort_xy(msleep, vore, brainwt, sort = "ascending")

    testthat::expect_equal(
      levels(df4$vore),
      c("herbi", "omni", "carni", "insecti")
    )
    testthat::expect_equal(levels(df4$vore), levels(df5$vore))
    testthat::expect_equal(levels(df5$vore), rev(levels(df6$vore)))
    testthat::expect_equal(names(msleep), names(df5))
  }
)
