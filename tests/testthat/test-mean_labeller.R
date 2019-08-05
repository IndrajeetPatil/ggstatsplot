context(desc = "mean_labeller")

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
        k = 3
      )

    # check that dropped factor level is not retained
    testthat::expect_equal(
      object = length(levels(mean_dat$cut)) + 1,
      expected = length(levels(diamonds_short$cut))
    )

    # check mean label for first factor level
    testthat::expect_identical(
      object = mean_dat$label[[1]],
      expected = "3819.580, 95% CI [3140.804, 4498.356]"
    )

    # check mean label for first factor level
    testthat::expect_identical(
      object = mean_dat$label[[4]],
      expected = "4602.090, 95% CI [4274.733, 4929.447]"
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
    mean_dat2 <- ggstatsplot:::mean_labeller(
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
