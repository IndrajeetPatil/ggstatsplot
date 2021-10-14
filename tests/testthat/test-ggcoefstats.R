# z-statistic --------------------------------------------------

test_that(
  desc = "ggcoefstats with glm with z",
  code = {
    options(tibble.width = Inf)
    set.seed(123)

    # having a look at the Titanic dataset
    df <- as.data.frame(Titanic)

    # model
    mod <-
      stats::glm(
        formula = Survived ~ Sex + Age,
        data = df,
        weights = df$Freq,
        family = stats::binomial(link = "logit")
      )

    # plot
    set.seed(123)
    p <-
      ggcoefstats(
        x = mod,
        conf.level = 0.90,
        exclude.intercept = FALSE
      )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data)
  }
)

# chi^2-statistic --------------------------------------------------

test_that(
  desc = "ggcoefstats with chi-squared statistic model",
  code = {
    skip_if_not_installed("survival")
    options(tibble.width = Inf)

    # setup
    set.seed(123)
    library(survival)

    # model
    mod_coxph <- survival::coxph(
      formula = Surv(time, status) ~ age + sex + frailty(inst),
      data = lung
    )

    # plot
    p <- ggcoefstats(
      x = mod_coxph,
      package = "ggsci",
      palette = "category20c_d3"
    )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data)
  }
)

# t-statistic --------------------------------------------------

test_that(
  desc = "ggcoefstats with lm model",
  code = {
    options(tibble.width = Inf)
    set.seed(123)

    # model
    mod <- stats::lm(data = mtcars, formula = wt ~ mpg * am)

    # plot
    p <-
      ggcoefstats(
        x = mod,
        conf.level = 0.99,
        exclude.intercept = TRUE,
        only.significant = TRUE,
        k = 3
      )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # checking panel parameters
    expect_equal(
      pb$layout$panel_params[[1]]$y$breaks,
      structure(c("mpg", "am", "mpg:am"), pos = 1:3)
    )

    # check data
    set.seed(123)
    expect_snapshot(pb$data)
  }
)


# f-statistic and partial eta- and omega-squared -----------------------------

test_that(
  desc = "ggcoefstats with partial variants of effect size for f-statistic",
  code = {
    options(tibble.width = Inf)

    ## partial eta-squared

    set.seed(123)

    # model
    mod <- stats::aov(data = mtcars, formula = wt ~ mpg * am)

    # plot
    p <- ggcoefstats(
      x = mod,
      exclude.intercept = FALSE,
      effsize = "eta",
      partial = TRUE,
      k = 2,
      ylab = "effect"
    )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # tidy dataframe from the function
    tidy_df1 <- p$plot_env$tidy_df

    expect_snapshot(list(tidy_df1, p$labels))

    ## partial omega-squared

    set.seed(123)

    # model
    mod <-
      stats::aov(
        data = ggplot2::msleep,
        formula = sleep_rem ~ vore * brainwt,
        na.action = na.omit
      )

    # plot
    p <-
      ggcoefstats(
        x = mod,
        exclude.intercept = FALSE,
        sort = "ascending",
        effsize = "omega",
        title = "mammalian sleep",
        subtitle = "Source: `{ggplot2}` package",
        caption = substitute(paste(italic("Note"), ": From `tidyverse`")),
        package = "wesanderson",
        palette = "BottleRocket2",
        k = 3
      )

    # built plot
    pb <- ggplot2::ggplot_build(p)

    # tidy dataframe from the function
    tidy_df2 <- p$plot_env$tidy_df

    # tests
    expect_equal(pb$data[[2]]$x, tidy_df2$estimate, tolerance = 0.001)
    expect_equal(pb$data[[2]]$xmin, tidy_df2$conf.low, tolerance = 0.001)
    expect_equal(pb$data[[2]]$xmax, tidy_df2$conf.high, tolerance = 0.001)
    expect_equal(
      pb$data[[2]]$y,
      structure(c(3L, 1L, 2L), class = c("mapped_discrete", "numeric"))
    )

    expect_snapshot(list(tidy_df2, p$labels))
  }
)

# check tidy output ----------------------------------------------

test_that(
  desc = "check tidy output",
  code = {
    options(tibble.width = Inf)

    library(ggstatsplot)

    set.seed(123)
    m <- aov(yield ~ N * P * K + Error(block), npk)
    m2 <- aov(yield ~ N * P * K, npk)

    # computed dataframes
    set.seed(123)
    tidy_df1 <- ggcoefstats(m, output = "tidy")

    set.seed(123)
    tidy_df2 <- ggcoefstats(m2, output = "tidy")

    # checking entire objects
    expect_snapshot(list(tidy_df1, tidy_df2))
  }
)

# check if glance works ----------------------------------------------

test_that(
  desc = "check if glance works",
  code = {
    options(tibble.width = Inf)

    # lm
    set.seed(123)
    mod1 <- stats::lm(data = iris, formula = Sepal.Length ~ Species)
    glance_df1 <- ggcoefstats(x = mod1, output = "glance")

    # checking if they are present
    expect_true(all(c("AIC", "BIC") %in% names(glance_df1)))
  }
)

# CIs missing and palette change message -------------------------------------

test_that(
  desc = "CIs missing and palette change message",
  code = {
    options(tibble.width = Inf)

    df <-
      structure(list(
        term = c(
          "(Intercept)", "CuCu035", "CuCu175",
          "Time", "I(Time^2)", "I(Time^3)", "CuCu035:Time", "CuCu175:Time",
          "CuCu035:I(Time^2)", "CuCu175:I(Time^2)", "CuCu035:I(Time^3)",
          "CuCu175:I(Time^3)"
        ), estimate = c(
          21.8578677803274, 0.526733674732889,
          0.0427835422515209, 2.8851574385696, 0.614042414524674, -0.0262947719084166,
          -0.40542426165259, 0.85694067753032, 0.0182862906307392, -0.0960713284650128,
          0.000546698056168512, 0.00269961148409459
        ), std.error = c(
          0.693144489488905,
          0.941360576277464, 1.01964740267859, 0.360257238194542, 0.0701643241562136,
          0.00382178339260795, 0.637125037623185, 0.615624385440062, 0.11543261090741,
          0.110799697486582, 0.00598959833358501, 0.00565019761578672
        ),
        statistic = c(
          994.415853036382, 0.313090691524612, 0.00176057059261262,
          64.1377326818143, 76.5885855307124, 47.337648230833, 0.404920835633417,
          1.93762571038971, 0.0250954043758227, 0.751814059246073,
          0.00833104846354157, 0.228283887066837
        ), p.value = c(
          0, 0.57578977738106,
          0.966531259919043, 1.11022302462516e-15, 0, 5.97533134083506e-12,
          0.524558812161593, 0.16392656280827, 0.874129565558699, 0.38590249597292,
          0.927274418017278, 0.632799230168403
        )
      ), class = c(
        "tbl_df",
        "tbl", "data.frame"
      ), row.names = c(NA, -12L))


    p <- ggcoefstats(df, statistic = "z")

    pb <- ggplot2::ggplot_build(p)

    expect_equal(length(pb$data), 3L)
  }
)

# meta subtitle -------------------------------------

test_that(
  desc = "meta subtitle",
  code = {
    options(tibble.width = Inf)

    # dataframe
    df_eg <-
      structure(
        list(
          estimate = c(0.111, 0.245, 0.8, 1.1, 0.03),
          std.error = c(0.05, 0.111, 0.001, 0.2, 0.01)
        ),
        row.names = c(NA, -5L),
        class = c("tbl_df", "tbl", "data.frame")
      )

    # subtitle output
    set.seed(123)
    using_function1 <-
      statsExpressions::meta_analysis(
        data = df_eg,
        k = 4,
        type = "p"
      )$expression[[1]]

    # ggstatsplot output
    set.seed(123)
    ggcoef_label <-
      ggcoefstats(df_eg,
        k = 4,
        meta.analytic.effect = TRUE,
        bf.message = FALSE,
        meta.type = "p",
        output = "subtitle"
      )

    expect_identical(using_function1, ggcoef_label)
  }
)

# duplicated terms -------------------------------------

test_that(
  desc = "duplicated terms",
  code = {
    options(tibble.width = Inf)

    df <-
      structure(
        list(
          term = c(
            "(Intercept)", "x", "(Intercept)", "x",
            "(Intercept)", "x"
          ),
          estimate = c(
            29.3220715172958,
            1.1244506550584,
            29.9547605920406,
            1.1822574944936,
            30.6283792821576,
            1.25165747424685
          ),
          std.error = c(
            0.117485050182681,
            0.255357284283965,
            0.113389002110287,
            0.16192823674221,
            0.0997134241212493,
            0.184844896331203
          ),
          ci.width = c(
            95,
            95, 95, 95, 95, 95
          ),
          conf.low = c(
            29.0912440592861,
            0.622740244041031,
            29.7319807993548,
            0.864110775323021,
            30.4324684306615,
            0.888485500622392
          ),
          conf.high = c(
            29.5528989753056,
            1.62616106607576,
            30.1775403847265,
            1.50040421366417,
            30.8242901336538,
            1.61482944787131
          ),
          statistic = c(
            249.581299677722,
            4.40344068590569,
            264.176948685952,
            7.30112004106952,
            307.164050899648,
            6.77139320094696
          ),
          df = c(498L, 498L, 498L, 498L, 498L, 498L),
          p.value = c(
            9.8364342079353e-78,
            5.77239788522003e-05,
            6.0849598304989e-79,
            2.26914690385169e-09,
            3.78325168389556e-82,
            1.49961152818978e-08
          ),
          component = c(
            "tau (0.25)",
            "tau (0.25)",
            "tau (0.50)",
            "tau (0.50)",
            "tau (0.75)",
            "tau (0.75)"
          )
        ),
        row.names = c(
          NA,
          6L
        ),
        pretty_names = c(`(Intercept)` = "(Intercept)", x = "x"),
        ci = 0.95,
        verbose = TRUE,
        exponentiate = FALSE,
        ordinal_model = FALSE,
        model_class = "lqm",
        bootstrap = FALSE,
        iterations = 1000,
        model_formula = "y ~ x",
        coefficient_name = "Coefficient",
        zi_coefficient_name = "Log-Odds",
        digits = 2,
        ci_digits = 2,
        p_digits = 3,
        class = "data.frame",
        object_name = "fit.lqm"
      )

    p <- ggcoefstats(df, statistic = "z")

    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data)
  }
)


test_that("plots are rendered correctly - ggcoefstats", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("survival")
  skip_on_os("linux")
  skip_if(getRversion() < "4.1")

  # vdiffr tests --------------------------------

  ## t-statistic --------------------------------
  # already tested in vdiffr test main file

  ## F-statistic --------------------------------

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "F-statistic - vdiffr",
    fig = ggcoefstats(aov(yield ~ N * P * K + Error(block), npk))
  )

  ## chi2-statistic --------------------------------

  # setup
  set.seed(123)
  library(survival)

  # model
  mod_coxph <- survival::coxph(
    formula = Surv(time, status) ~ age + sex + frailty(inst),
    data = lung
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "chi2-statistic - vdiffr",
    fig = ggcoefstats(mod_coxph)
  )

  ## z-statistic --------------------------------

  # having a look at the Titanic dataset
  df <- as.data.frame(Titanic)

  # model
  mod_glm <- stats::glm(
    formula = Survived ~ Sex + Age,
    data = df,
    weights = df$Freq,
    family = stats::binomial(link = "logit")
  )

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "z-statistic - vdiffr",
    fig = ggcoefstats(mod_glm, conf.level = 0.90)
  )
})
