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
    p8 <- grouped_ggpiestats(mtcars, x = cyl, grouping.var = am)
    extracted_data <- extract_stats(p8)
    summary(extracted_data)
    extract_subtitle(p8)
    extract_caption(p8)
  })

  # extract functions return one element per group for grouped plots
  set.seed(123)
  p9 <- grouped_gghistostats(iris, Sepal.Length, grouping.var = Species)
  subtitle_result <- extract_subtitle(p9)
  caption_result <- extract_caption(p9)

  expect_type(subtitle_result, "list")
  expect_type(caption_result, "list")
  expect_length(subtitle_result, 3L) # 3 species groups
  expect_length(caption_result, 3L)
})

test_that("checking if extract_stats produces NULL on unsupported objects", {
  expect_length(purrr::compact(extract_stats(iris)), 0L)
})
