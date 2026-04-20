# errors ------------------------------------------

test_that("ggcoefstats doesn't work if no estimate column found", {
  expect_snapshot_error(ggcoefstats(iris))
})

# default plots for each statistic ------------------------------------------

test_that("default plots are rendered correctly for each type of statistic", {
  skip_if_not_installed("withr")

  set.seed(123)
  expect_doppelganger(
    title = "t-statistic",
    fig = ggcoefstats(stats::lm(formula = wt ~ am * cyl, data = mtcars))
  )

  set.seed(123)
  expect_doppelganger(
    title = "F-statistic",
    fig = ggcoefstats(aov(yield ~ N * K, npk))
  )

  set.seed(123)
  expect_doppelganger(
    title = "F-statistic with omega",
    fig = ggcoefstats(stats::aov(wt ~ mpg * am, mtcars), effectsize.type = "omega")
  )

  df <- as.data.frame(Titanic)

  mod_glm <- stats::glm(
    formula = Survived ~ Sex + Age,
    data = df,
    weights = df$Freq,
    family = stats::binomial(link = "logit")
  )

  set.seed(123)
  expect_doppelganger(
    title = "z-statistic",
    fig = ggcoefstats(mod_glm, conf.level = 0.90)
  )

  skip_if_not_installed("survival")
  withr::local_package("survival")

  set.seed(123)
  expect_doppelganger(
    title = "chi2-statistic",
    fig = suppressWarnings(ggcoefstats(
      survival::coxph(Surv(time, status) ~ age + sex + frailty(inst), lung)
    ))
  )
})

df_meta <- tibble(
  estimate = c(0.111, 0.245, 0.8, 1.1, 0.03),
  std.error = c(0.05, 0.111, 0.001, 0.2, 0.01)
)

test_that("meta-analysis works", {
  skip_on_cran()
  skip_if_not_installed("metafor")

  set.seed(123)
  p_meta <- ggcoefstats(
    df_meta,
    meta.analytic.effect = TRUE,
    bf.message = TRUE
  )

  # don't run graphical snapshot tests because values are slightly different
  # locally on CI
  expect_s3_class(p_meta, "ggplot")

  set.seed(123)
  expect_doppelganger(
    title = "meta-analysis works",
    fig = ggcoefstats(
      df_meta,
      meta.analytic.effect = FALSE,
      bf.message = FALSE,
      results.subtitle = FALSE
    )
  )
})

# plot modifications--------------------------------------------------

test_that(
  "plot modifications work as expected",
  {
    set.seed(123)
    mod1 <- stats::lm(data = mtcars, formula = wt ~ mpg * am)

    set.seed(123)
    expect_doppelganger(
      title = "plot modifications",
      fig = suppressWarnings(ggcoefstats(
        x = mod1,
        conf.level = 0.99,
        exclude.intercept = TRUE,
        only.significant = TRUE,
        palette = "ggsci::category20c_d3",
        digits = 3L
      ))
    )

    set.seed(123)
    mod2 <- stats::aov(
      data = ggplot2::msleep,
      formula = sleep_rem ~ vore * brainwt,
      na.action = na.omit
    )

    set.seed(123)
    expect_doppelganger(
      title = "sorting works",
      fig = ggcoefstats(
        x = mod2,
        exclude.intercept = FALSE,
        sort = "ascending",
        effectsize.type = "omega",
        title = "mammalian sleep",
        subtitle = "Source: `{ggplot2}` package",
        palette = "wesanderson::BottleRocket2",
        digits = 3L
      )
    )
  }
)

# edge cases -------------------------------------

test_that(
  "works when CIs unavailable",
  {
    set.seed(123)
    df_base <- tidy_model_parameters(stats::lm(wt ~ am * cyl, mtcars))

    set.seed(123)
    expect_doppelganger(
      title = "CIs missing",
      fig = ggcoefstats(dplyr::select(df_base, -dplyr::matches("conf")), statistic = "t")
    )

    expect_snapshot_error(
      ggcoefstats(
        dplyr::bind_rows(df_base, df_base),
        statistic = "t"
      )
    )
  }
)

test_that("term ordering is preserved in the plotted top-to-bottom order", {
  df <- tibble::tibble(
    term = c("first", "second", "third"),
    estimate = c(0.3, -0.4, 1.1)
  )

  expect_identical(
    levels(.preprocess_tidy_data(df, sort = "none")$term),
    rev(df$term)
  )

  expect_identical(
    levels(.preprocess_tidy_data(df, sort = "ascending")$term),
    rev(df$term[c(2L, 1L, 3L)])
  )

  expect_identical(
    as.character(.preprocess_tidy_data(tibble::tibble(estimate = c(0.3, -0.4)), sort = "none")$term),
    c("term_1", "term_2")
  )
})

test_that("mixed-model stats labels drop rows with empty expressions", {
  skip_if_not_installed("lme4")

  set.seed(123)
  expect_doppelganger(
    title = "mixed-model fixed-effects labels",
    fig = suppressWarnings(ggcoefstats(
      lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy),
      effects = "fixed",
      stats.label.args = list(
        size = 3.0,
        direction = "y",
        min.segment.length = 0,
        na.rm = TRUE,
        seed = 123
      )
    ))
  )
})

test_that("stats label colors stay aligned after filtering labels", {
  df_tidy <- tidy_model_parameters(stats::lm(wt ~ am * cyl, mtcars))
  df_tidy$p.value[2L] <- 0.42

  plot <- ggcoefstats(
    df_tidy,
    statistic = "t",
    only.significant = TRUE,
    stats.label.color = c("firebrick", "grey50", "forestgreen", "navy")
  )

  expect_identical(as.character(plot$layers[[4L]]$data$term), c("(Intercept)", "cyl"))
  expect_identical(plot$layers[[4L]]$aes_params$colour, c("firebrick", "forestgreen"))
})

test_that("tidy data without statistic inputs disables stats labels", {
  plot <- ggcoefstats(tibble::tibble(term = c("a", "b"), estimate = c(0.5, -0.2)), stats.labels = TRUE)

  expect_length(plot$layers, 2L)
  expect_true(all(vapply(plot$layers, function(x) !inherits(x$geom, "GeomLabelRepel"), logical(1L))))
})

test_that("stats label helpers cover filtering and color branches", {
  df_labels <- tibble::tibble(
    term = c("a", "b", "c"),
    p.value = c(0.01, 0.03, 0.02),
    expression = list("alpha", character(0), "gamma")
  )

  expect_identical(.prepare_stats_label_data(df_labels, only.significant = TRUE)$term, c("a", "c"))

  expect_identical(.prepare_stats_label_data(df_labels, only.significant = FALSE)$term, c("a", "c"))

  expect_identical(
    .prepare_stats_label_data(dplyr::select(df_labels, -p.value), only.significant = TRUE)$term,
    c("a", "c")
  )

  label_data <- tibble::tibble(term = "a", expression = list("alpha"))
  expected_color <- unname(as.character(paletteer::paletteer_d("ggthemes::gdoc", 1L)))

  expect_identical(
    unname(as.character(.prepare_stats_label_colors(df_labels, label_data, NULL, "ggthemes::gdoc"))),
    expected_color
  )

  expect_identical(
    .prepare_stats_label_colors(
      df_labels,
      label_data,
      c("firebrick", "grey50", "forestgreen"),
      "ggthemes::gdoc"
    ),
    "firebrick"
  )

  expect_identical(
    .prepare_stats_label_colors(df_labels, label_data, "firebrick", "ggthemes::gdoc"),
    "firebrick"
  )

  # 30 total terms but only 1 labeled — no error even with small palette
  expect_no_error(
    .prepare_stats_label_colors(
      tibble::tibble(term = letters[1:30], expression = rep(list("alpha"), 30L)),
      label_data, # label_data has only term "a"
      NULL,
      "ggthemes::gdoc"
    )
  )

  # error only when labeled terms exceed palette size
  expect_error(
    .prepare_stats_label_colors(
      tibble::tibble(term = letters[1:30], expression = rep(list("alpha"), 30L)),
      tibble::tibble(term = letters[1:30], expression = rep(list("alpha"), 30L)),
      NULL,
      "ggthemes::gdoc" # 24 colors, not enough for 30 labeled terms
    )
  )
})

# meta subtitle and caption -------------------------------------

test_that(
  "meta analysis subtitle and caption",
  {
    skip_on_cran()
    skip_if_not_installed("metafor")
    skip_if_not_installed("metaBMA")
    skip_if_not_installed("metaplus")

    set.seed(123)
    subtitle_expr <- suppressWarnings(meta_analysis(df_meta, type = "p"))

    set.seed(123)
    caption_expr <- suppressWarnings(meta_analysis(df_meta, type = "bayes"))

    set.seed(123)
    ggcoef_subtitle <- extract_subtitle(suppressWarnings(ggcoefstats(
      df_meta,
      meta.analytic.effect = TRUE,
      bf.message = FALSE,
      meta.type = "p"
    )))

    set.seed(123)
    ggcoef_caption <- extract_caption(suppressWarnings(ggcoefstats(
      df_meta,
      meta.analytic.effect = TRUE,
      bf.message = TRUE,
      meta.type = "p"
    )))

    expect_identical(subtitle_expr$expression[[1L]], ggcoef_subtitle)
    expect_identical(caption_expr$expression[[1L]], ggcoef_caption)
  }
)
