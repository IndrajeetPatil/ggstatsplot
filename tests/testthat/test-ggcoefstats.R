testthat::skip_on_cran()

df_meta <- structure(
  list(
    estimate = c(0.111, 0.245, 0.8, 1.1, 0.03),
    std.error = c(0.05, 0.111, 0.001, 0.2, 0.01)
  ),
  row.names = c(NA, -5L),
  class = c("tbl_df", "tbl", "data.frame")
)

# errors ------------------------------------------

test_that("ggcoefstats doesn't work if no estimate column found", {
  expect_snapshot_error(ggcoefstats(iris))
})

# default plots for each statistic ------------------------------------------

test_that("default plots are rendered correctly for each type of statistic", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  skip_if_not_installed("survival")

  library(survival)

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "t-statistic",
    fig = ggcoefstats(stats::lm(formula = wt ~ am * cyl, data = mtcars))
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "F-statistic",
    fig = ggcoefstats(aov(yield ~ N * K, npk))
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "F-statistic with partial effect size",
    fig = ggcoefstats(stats::aov(wt ~ mpg * am, mtcars), effsize = "eta", partial = TRUE)
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "chi2-statistic",
    fig = suppressWarnings(ggcoefstats(
      survival::coxph(Surv(time, status) ~ age + sex + frailty(inst), lung)
    ))
  )

  df <- as.data.frame(Titanic)

  mod_glm <- stats::glm(
    formula = Survived ~ Sex + Age,
    data = df,
    weights = df$Freq,
    family = stats::binomial(link = "logit")
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "z-statistic",
    fig = ggcoefstats(mod_glm, conf.level = 0.90)
  )
})

test_that("meta-analysis works", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  skip_if_not_installed("survival")
  skip_if_not_installed("metaBMA")
  skip_if_not_installed("metafor")

  expect_s3_class(
    suppressWarnings(ggcoefstats(
      df_meta,
      meta.analytic.effect = TRUE,
      bf.message = TRUE
    )),
    "ggplot"
  )

  # TODO: generate from windows
  # set.seed(123)
  # vdiffr::expect_doppelganger(
  #   title = "meta-analysis works",
  #   fig = ggcoefstats(
  #     df_meta,
  #     meta.analytic.effect = TRUE,
  #     bf.message = TRUE
  #   )
  # )
})

# plot modifications--------------------------------------------------

test_that(
  desc = "plot modifications work as expected",
  code = {
    skip_if_not_installed("vdiffr")
    skip_if(getRversion() < "4.1")


    set.seed(123)
    mod1 <- stats::lm(data = mtcars, formula = wt ~ mpg * am)

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "plot modifications",
      fig = suppressWarnings(ggcoefstats(
        x = mod1,
        conf.level = 0.99,
        exclude.intercept = TRUE,
        only.significant = TRUE,
        package = "ggsci",
        palette = "category20c_d3",
        k = 3
      ))
    )

    set.seed(123)
    mod2 <- stats::aov(
      data = ggplot2::msleep,
      formula = sleep_rem ~ vore * brainwt,
      na.action = na.omit
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "sorting works",
      fig = ggcoefstats(
        x = mod2,
        exclude.intercept = FALSE,
        sort = "ascending",
        effsize = "omega",
        title = "mammalian sleep",
        subtitle = "Source: `{ggplot2}` package",
        package = "wesanderson",
        palette = "BottleRocket2",
        k = 3
      )
    )
  }
)

# data frame outputs ----------------------------------------------

test_that(
  desc = "data frame outputs as expected",
  code = {
    options(tibble.width = Inf)

    set.seed(123)
    mod <- stats::lm(wt ~ mpg, mtcars)

    set.seed(123)
    tidy_df <- ggcoefstats(mod, output = "tidy")
    glance_df <- ggcoefstats(x = mod, output = "glance")

    expect_snapshot(list(tidy_df, glance_df))
  }
)

# edge cases -------------------------------------

test_that(
  desc = "edge cases",
  code = {
    skip_if_not_installed("vdiffr")
    skip_if(getRversion() < "4.1")


    set.seed(123)
    df_base <- tidy_model_parameters(stats::lm(wt ~ am * cyl, mtcars))

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "works when CIs are missing",
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

# meta subtitle and caption -------------------------------------

test_that(
  desc = "meta analysis subtitle and caption",
  code = {
    set.seed(123)
    subtitle_expr <- suppressWarnings(statsExpressions::meta_analysis(
      data = df_meta,
      type = "p"
    ))

    set.seed(123)
    caption_expr <- suppressWarnings(statsExpressions::meta_analysis(
      data = df_meta,
      type = "bayes"
    ))

    set.seed(123)
    ggcoef_subtitle <- suppressWarnings(ggcoefstats(
      df_meta,
      meta.analytic.effect = TRUE,
      bf.message = FALSE,
      meta.type = "p",
      output = "subtitle"
    ))

    set.seed(123)
    ggcoef_caption <- suppressWarnings(ggcoefstats(
      df_meta,
      meta.analytic.effect = TRUE,
      bf.message = TRUE,
      meta.type = "p",
      output = "caption"
    ))

    expect_equal(subtitle_expr$expression[[1]], ggcoef_subtitle)
    expect_equal(caption_expr$expression[[1]], ggcoef_caption)
  }
)
