context("anova")

test_that("anova works", {
  # the expected result
  r <-
    broom::tidy(
      stats::oneway.test(
        formula = sleep_total ~ vore,
        na.action = na.omit,
        subset = NULL,
        data = ggplot2::msleep
      )
    )

  # output from ggstatsplot helper subtitle
  subtitle <-
    ggstatsplot::subtitle_ggbetween_anova_parametric(
      data = ggplot2::msleep,
      x = vore,
      y = sleep_total,
      messages = FALSE
    )

  # extracting only the numbers and creating a tibble
  subtitle_vec <-
    stringr::str_extract(string = as.character(subtitle), pattern = "\\-*\\d+\\.*\\d*") %>%
    tibble::as.tibble() %>%
    stats::na.omit()

  # converting to numeric
  subtitle_vec$value <- as.numeric(as.character(subtitle_vec$value))

  # testing values

  # numerator degress of freedom
  testthat::expect_equal(expected = r$`num df`[[1]], object = subtitle_vec$value[[1]])

  # denominator degress of freedom
  testthat::expect_equal(
    expected = r$`denom df`[[1]],
    object = subtitle_vec$value[[2]],
    tolerance = 1e-3
  )

  # F-value
  testthat::expect_equal(
    expected = r$statistic[[1]],
    object = subtitle_vec$value[[3]],
    tolerance = 1e-3
  )

  # p-value
  testthat::expect_equal(
    expected = r$p.value[[1]],
    object = subtitle_vec$value[[4]],
    tolerance = 1e-3
  )
})
