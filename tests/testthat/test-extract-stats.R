test_that("checking if extract_stats works", {
  skip_if_not_installed("rstantools")
  skip_if_not_installed("withr")
  withr::local_options(tibble.width = Inf)

  set.seed(123)
  expect_snapshot(extract_details(ggbetweenstats(mtcars, am, mpg)))

  set.seed(123)
  expect_snapshot(extract_details(ggscatterstats(
    mtcars,
    wt,
    mpg,
    marginal = FALSE,
    type = "r"
  )))

  # https://github.com/kassambara/ggcorrplot/issues/57
  withr::with_options(
    list(warn = -1L),
    code = {
      set.seed(123)
      expect_snapshot(extract_details(ggcorrmat(iris)))
    }
  )

  set.seed(123)
  expect_snapshot(extract_details(ggbetweenstats(mtcars, cyl, mpg)))

  set.seed(123)
  expect_snapshot(extract_details(ggpiestats(mtcars, cyl)))

  set.seed(123)
  expect_snapshot(extract_details(ggbarstats(mtcars, cyl, am)))

  set.seed(123)
  expect_snapshot(extract_details(ggcoefstats(lm(wt ~ mpg, mtcars))))
})

test_that("checking if extract_stats works for grouped plots", {
  set.seed(123)
  expect_snapshot({
    p <- grouped_ggpiestats(mtcars, x = cyl, grouping.var = am)
    extracted_data <- extract_stats(p)
    summary(extracted_data)
    extract_subtitle(p)
    extract_caption(p)
  })

  # extract functions return one element per group for grouped plots
  set.seed(123)
  histograms <- grouped_gghistostats(iris, Sepal.Length, grouping.var = Species)
  subtitle_result <- extract_subtitle(histograms)
  caption_result <- extract_caption(histograms)

  expect_type(subtitle_result, "list")
  expect_type(caption_result, "list")
  expect_length(subtitle_result, 3L) # 3 species groups
  expect_length(caption_result, 3L)
})

test_that("checking if extract_stats produces NULL on unsupported objects", {
  expect_length(purrr::compact(extract_stats(iris)), 0L)
})

test_that("extract_stats retains field names, order, and data frames", {
  subtitle <- tibble::tibble(statistic = 1)
  caption <- tibble::tibble(bf10 = 2)
  pairwise <- tibble::tibble(group1 = "a", group2 = "b")
  descriptive <- tibble::tibble(counts = 3L)
  one_sample <- tibble::tibble(p.value = 0.5)
  tidy <- tibble::tibble(Coefficient = 4)
  glance <- tibble::tibble(R2 = 0.6)
  p <- list(
    plot_env = rlang::env(
      subtitle_df = subtitle,
      caption_df = caption,
      mpc_df = pairwise,
      descriptive_df = descriptive,
      onesample_df = one_sample,
      tidy_df = tidy,
      glance_df = glance
    )
  )

  expect_identical(
    extract_stats(p),
    structure(
      list(
        subtitle_data = subtitle,
        caption_data = caption,
        pairwise_comparisons_data = pairwise,
        descriptive_data = descriptive,
        one_sample_data = one_sample,
        tidy_data = tidy,
        glance_data = glance
      ),
      class = c("ggstatsplot_stats", "list")
    )
  )
})

test_that("extract_stats falls back to stats_output and retains missing fields", {
  direct <- tibble::tibble(statistic = 1)
  fallback <- tibble::tibble(statistic = 2)
  p <- list(
    plot_env = rlang::env(
      subtitle_df = direct,
      caption_df = NULL,
      stats_output = list(subtitle_df = fallback, caption_df = fallback)
    )
  )

  result <- extract_stats(p)
  expect_identical(result$subtitle_data, direct)
  expect_identical(result$caption_data, fallback)
  expect_identical(
    unclass(result)[3:7],
    list(
      pairwise_comparisons_data = NULL,
      descriptive_data = NULL,
      one_sample_data = NULL,
      tidy_data = NULL,
      glance_data = NULL
    )
  )

  rm("subtitle_df", envir = p$plot_env)
  expect_identical(extract_stats(p)$subtitle_data, fallback)
})
