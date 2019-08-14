# effsize works for Cohen's d and Hedge's g (between - without NA) ------------

context("effsize_t_parametric")

testthat::test_that(
  desc = "effsize works for Cohen's d and Hedge's g (between - without NA)",
  code = {
    testthat::skip_on_cran()

    # checking for errors
    testthat::expect_error(ggstatsplot:::effsize_t_parametric(formula = wt ~ cyl))

    testthat::expect_error(ggstatsplot:::effsize_t_parametric(
      formula = wt ~ .,
      data = mtcars
    ))

    testthat::expect_error(ggstatsplot:::effsize_t_parametric(
      formula = wt ~ am + cyl,
      data = mtcars
    ))

    testthat::expect_error(ggstatsplot:::effsize_t_parametric(
      formula = ~ am + cyl,
      data = mtcars
    ))

    # creating a new dataframe with a variable that has dropped factor level
    mtcars_short <- dplyr::filter(.data = mtcars, cyl != "4")

    # shouldn't work
    testthat::expect_error(ggstatsplot:::effsize_t_parametric(
      formula = wt ~ cyl,
      data = mtcars
    ))

    # g and central
    set.seed(123)
    tobject1 <-
      t.test(
        formula = wt ~ am,
        data = mtcars,
        var.equal = TRUE,
        conf.level = .95
      )
    tobject2 <-
      t.test(
        formula = wt ~ cyl,
        data = mtcars_short,
        var.equal = TRUE,
        conf.level = .99
      )

    df1 <- ggstatsplot:::effsize_t_parametric(
      formula = wt ~ am,
      data = mtcars,
      paired = FALSE,
      hedges.correction = TRUE,
      conf.level = 0.95,
      noncentral = FALSE,
      tobject = tobject1
    )

    # g and non-central
    set.seed(123)
    df2 <- ggstatsplot:::effsize_t_parametric(
      formula = wt ~ am,
      data = mtcars,
      paired = FALSE,
      hedges.correction = TRUE,
      conf.level = 0.95,
      noncentral = TRUE,
      tobject = tobject1
    )

    # d and central
    set.seed(123)
    df3 <- ggstatsplot:::effsize_t_parametric(
      formula = wt ~ cyl,
      data = mtcars_short,
      paired = FALSE,
      hedges.correction = FALSE,
      conf.level = 0.99,
      noncentral = FALSE,
      tobject = tobject2
    )

    # d and non-central
    set.seed(123)
    df4 <- ggstatsplot:::effsize_t_parametric(
      formula = wt ~ cyl,
      data = mtcars_short,
      paired = FALSE,
      hedges.correction = FALSE,
      conf.level = 0.99,
      noncentral = TRUE,
      tobject = tobject2
    )

    # checking attributes of dataframe
    testthat::expect_identical(
      c(class(df1), class(df2), class(df3), class(df4)),
      rep(c("tbl_df", "tbl", "data.frame"), 4)
    )
    testthat::expect_equal(
      c(dim(df1), dim(df2), dim(df3), dim(df4)),
      rep(c(1L, 9L), 4)
    )

    # checking estimates and CIs
    testthat::expect_equal(df1$estimate, 1.886124, tolerance = 0.001)
    testthat::expect_equal(df1$conf.low, 1.000492, tolerance = 0.001)
    testthat::expect_equal(df1$conf.high, 2.771755, tolerance = 0.001)

    testthat::expect_equal(df2$estimate, df1$estimate, tolerance = 0.001)
    testthat::expect_equal(df2$conf.low, 1.030023, tolerance = 0.001)
    testthat::expect_equal(df2$conf.high, 2.732913, tolerance = 0.001)
    testthat::expect_equal(df3$estimate, -1.487072, tolerance = 0.001)
    testthat::expect_equal(df3$conf.low, -2.965202, tolerance = 0.001)
    testthat::expect_equal(df3$conf.high, -0.008940757, tolerance = 0.001)
    testthat::expect_equal(df4$estimate, df3$estimate, tolerance = 0.001)
    testthat::expect_equal(df4$conf.low, -2.641733, tolerance = 0.001)
    testthat::expect_equal(df4$conf.high, -0.01115432, tolerance = 0.001)

    # checking details
    testthat::expect_identical(
      c(df1$method, df2$method, df3$method, df4$method),
      c("Hedges's g", "Hedges's g", "Cohen's d", "Cohen's d")
    )
    testthat::expect_identical(
      c(
        df1$alternative,
        df2$alternative,
        df3$alternative,
        df4$alternative
      ),
      c(rep("two.sided", 4L))
    )
    testthat::expect_true(df2$noncentral)
    testthat::expect_false(df1$noncentral)
    testthat::expect_true(df4$noncentral)
    testthat::expect_false(df3$noncentral)
    testthat::expect_identical(
      c(
        df1$noncentral,
        df2$noncentral,
        df3$noncentral,
        df4$noncentral
      ),
      c(FALSE, TRUE, FALSE, TRUE)
    )
    testthat::expect_identical(
      c(df1$var.equal, df2$var.equal, df3$var.equal, df4$var.equal),
      c(rep(FALSE, 4L))
    )
    testthat::expect_identical(
      c(df1$paired, df2$paired, df3$paired, df4$paired),
      c(rep(FALSE, 4L))
    )
    testthat::expect_identical(
      c(df1$paired, df2$paired, df3$paired, df4$paired),
      c(rep(FALSE, 4L))
    )
  }
)

# effsize works for Cohen's d and Hedge's g (between - with NA) ------------

testthat::test_that(
  desc = "effsize works for Cohen's d and Hedge's g (between - with NA)",
  code = {
    testthat::skip_on_cran()

    library(ggplot2)

    # creating a new dataframe with a variable that has dropped factor level
    msleep_short <- dplyr::filter(
      .data = ggplot2::msleep,
      vore %in% c("herbi", "carni")
    )

    # shouldn't work
    testthat::expect_error(ggstatsplot:::effsize_t_parametric(
      formula = brainwt ~ vore,
      data = msleep
    ))

    tobject1 <-
      t.test(
        formula = brainwt ~ vore,
        data = msleep_short,
        var.equal = TRUE,
        conf.level = .95
      )
    tobject2 <-
      t.test(
        formula = sleep_rem ~ vore,
        data = msleep_short,
        var.equal = TRUE,
        conf.level = .90
      )


    # g and central
    set.seed(123)
    df1 <- ggstatsplot:::effsize_t_parametric(
      formula = brainwt ~ vore,
      data = msleep_short,
      paired = FALSE,
      hedges.correction = TRUE,
      conf.level = 0.95,
      noncentral = FALSE,
      tobject = tobject1
    )

    # g and non-central
    set.seed(123)
    df2 <- ggstatsplot:::effsize_t_parametric(
      formula = brainwt ~ vore,
      data = msleep_short,
      paired = FALSE,
      hedges.correction = TRUE,
      conf.level = 0.95,
      noncentral = TRUE,
      tobject = tobject1
    )

    # d and central
    set.seed(123)
    df3 <- ggstatsplot:::effsize_t_parametric(
      formula = sleep_rem ~ vore,
      data = msleep_short,
      paired = FALSE,
      hedges.correction = FALSE,
      conf.level = 0.90,
      noncentral = FALSE,
      tobject = tobject2
    )

    # d and non-central
    set.seed(123)
    df4 <- ggstatsplot:::effsize_t_parametric(
      formula = sleep_rem ~ vore,
      data = msleep_short,
      paired = FALSE,
      hedges.correction = FALSE,
      conf.level = 0.90,
      noncentral = TRUE,
      tobject = tobject2
    )

    # checking attributes of dataframe
    testthat::expect_identical(
      c(class(df1), class(df2), class(df3), class(df4)),
      rep(c("tbl_df", "tbl", "data.frame"), 4)
    )
    testthat::expect_equal(
      c(dim(df1), dim(df2), dim(df3), dim(df4)),
      rep(c(1L, 9L), 4)
    )

    # checking estimates and CIs
    testthat::expect_equal(df1$estimate, -0.4732142, tolerance = 0.001)
    testthat::expect_equal(df1$conf.low, -1.307171, tolerance = 0.001)
    testthat::expect_equal(df1$conf.high, 0.3607431, tolerance = 0.001)

    testthat::expect_equal(df2$estimate, df1$estimate, tolerance = 0.001)
    testthat::expect_equal(df2$conf.low, -1.201409, tolerance = 0.001)
    testthat::expect_equal(df2$conf.high, 0.3870483, tolerance = 0.001)

    testthat::expect_equal(df3$estimate, 0.6284079, tolerance = 0.001)
    testthat::expect_equal(df3$conf.low, -0.02208496, tolerance = 0.001)
    testthat::expect_equal(df3$conf.high, 1.278901, tolerance = 0.001)

    testthat::expect_equal(df4$estimate, df3$estimate, tolerance = 0.001)
    testthat::expect_equal(df4$conf.low, 0.09053439, tolerance = 0.001)
    testthat::expect_equal(df4$conf.high, 1.364623, tolerance = 0.001)

    # checking details
    testthat::expect_identical(
      c(df1$method, df2$method, df3$method, df4$method),
      c("Hedges's g", "Hedges's g", "Cohen's d", "Cohen's d")
    )
    testthat::expect_identical(
      c(
        df1$alternative,
        df2$alternative,
        df3$alternative,
        df4$alternative
      ),
      c(rep("two.sided", 4L))
    )
    testthat::expect_true(df2$noncentral)
    testthat::expect_false(df1$noncentral)
    testthat::expect_true(df4$noncentral)
    testthat::expect_false(df3$noncentral)
    testthat::expect_identical(
      c(
        df1$noncentral,
        df2$noncentral,
        df3$noncentral,
        df4$noncentral
      ),
      c(FALSE, TRUE, FALSE, TRUE)
    )
    testthat::expect_identical(
      c(df1$var.equal, df2$var.equal, df3$var.equal, df4$var.equal),
      c(rep(FALSE, 4L))
    )
    testthat::expect_identical(
      c(df1$paired, df2$paired, df3$paired, df4$paired),
      c(rep(FALSE, 4L))
    )
  }
)

# effsize works for Cohen's d and Hedge's g (within - without NA) ------------

testthat::test_that(
  desc = "effsize works for Cohen's d and Hedge's g (within - without NA)",
  code = {
    testthat::skip_on_cran()

    # creating a new dataframe with a variable that has dropped factor level
    iris_short <- dplyr::filter(
      .data = ggstatsplot::iris_long,
      condition %in% c("Sepal.Length", "Sepal.Width")
    )

    # shouldn't work
    testthat::expect_error(
      ggstatsplot:::effsize_t_parametric(
        formula = value ~ condition,
        data = iris_long,
        paired = TRUE
      )
    )

    tobject1 <-
      t.test(
        formula = value ~ condition,
        data = iris_short,
        var.equal = TRUE,
        conf.level = .95,
        paired = TRUE
      )
    tobject2 <-
      t.test(
        formula = value ~ condition,
        data = iris_short,
        var.equal = TRUE,
        conf.level = .99,
        paired = TRUE
      )
    tobject3 <-
      t.test(
        formula = value ~ condition,
        data = iris_short,
        var.equal = TRUE,
        conf.level = .50,
        paired = TRUE
      )

    # g and central
    set.seed(123)
    df1 <- ggstatsplot:::effsize_t_parametric(
      formula = value ~ condition,
      data = iris_short,
      paired = TRUE,
      hedges.correction = TRUE,
      conf.level = 0.95,
      noncentral = FALSE,
      tobject = tobject1
    )

    # g and non-central
    set.seed(123)
    df2 <- suppressWarnings(ggstatsplot:::effsize_t_parametric(
      formula = value ~ condition,
      data = iris_short,
      paired = TRUE,
      hedges.correction = TRUE,
      conf.level = 0.95,
      noncentral = TRUE,
      tobject = tobject1
    ))

    # d and central
    set.seed(123)
    df3 <- ggstatsplot:::effsize_t_parametric(
      formula = value ~ condition,
      data = iris_short,
      paired = TRUE,
      hedges.correction = FALSE,
      conf.level = 0.99,
      noncentral = FALSE,
      tobject = tobject2
    )

    # d and non-central
    set.seed(123)
    df4 <- suppressWarnings(ggstatsplot:::effsize_t_parametric(
      formula = value ~ condition,
      data = iris_short,
      paired = TRUE,
      hedges.correction = FALSE,
      conf.level = 0.50,
      noncentral = TRUE,
      tobject = tobject3
    ))

    # checking attributes of dataframe
    testthat::expect_identical(
      c(class(df1), class(df2), class(df3), class(df4)),
      rep(c("tbl_df", "tbl", "data.frame"), 4)
    )
    testthat::expect_equal(
      c(dim(df1), dim(df2), dim(df3), dim(df4)),
      rep(c(1L, 9L), 4)
    )

    # checking estimates and CIs
    testthat::expect_equal(df1$estimate, 2.828219, tolerance = 0.001)
    testthat::expect_equal(df1$conf.low, 2.101352, tolerance = 0.001)
    testthat::expect_equal(df1$conf.high, 3.555086, tolerance = 0.001)
    testthat::expect_equal(df2$estimate, df1$estimate, tolerance = 0.001)
    testthat::expect_equal(df3$estimate, 2.842649, tolerance = 0.001)
    testthat::expect_equal(df3$conf.low, 1.882856, tolerance = 0.001)
    testthat::expect_equal(df3$conf.high, 3.802441, tolerance = 0.001)
    testthat::expect_equal(df4$estimate, df3$estimate, tolerance = 0.001)
    testthat::expect_equal(df2$conf.low, 2.489882, tolerance = 0.001)
    testthat::expect_equal(df2$conf.high, 3.208837, tolerance = 0.001)
    testthat::expect_equal(df4$conf.low, 2.722362, tolerance = 0.001)
    testthat::expect_equal(df4$conf.high, 2.971019, tolerance = 0.001)


    # checking details
    testthat::expect_equal(df4$conf.level, 0.50, tolerance = 0.01)
    testthat::expect_equal(df2$conf.level, 0.95, tolerance = 0.01)
    testthat::expect_identical(
      c(df1$method, df2$method, df3$method, df4$method),
      c("Hedges's g", "Hedges's g", "Cohen's d", "Cohen's d")
    )
    testthat::expect_identical(
      c(
        df1$alternative,
        df2$alternative,
        df3$alternative,
        df4$alternative
      ),
      c(rep("two.sided", 4L))
    )
    testthat::expect_true(df2$noncentral)
    testthat::expect_false(df1$noncentral)
    testthat::expect_true(df4$noncentral)
    testthat::expect_false(df3$noncentral)
    testthat::expect_identical(
      c(
        df1$noncentral,
        df2$noncentral,
        df3$noncentral,
        df4$noncentral
      ),
      c(FALSE, TRUE, FALSE, TRUE)
    )
    testthat::expect_identical(
      c(df1$var.equal, df2$var.equal, df3$var.equal, df4$var.equal),
      c(rep(FALSE, 4L))
    )
    testthat::expect_identical(
      c(df1$paired, df2$paired, df3$paired, df4$paired),
      c(rep(TRUE, 4L))
    )
  }
)

# effsize works for Cohen's d and Hedge's g (within - with NA) ------------

testthat::test_that(
  desc = "effsize works for Cohen's d and Hedge's g (within - with NA)",
  code = {
    testthat::skip_on_cran()

    library(jmv)
    data(bugs)

    # creating a new dataframe with a variable that has dropped factor level
    bugs_short_unequal <- bugs %>%
      tibble::as_tibble(x = .) %>%
      tidyr::gather(
        data = .,
        key = "condition",
        value = "value",
        LDLF:LDHF,
        na.rm = TRUE
      )

    bugs_short <- bugs_short_unequal %>%
      dplyr::filter(.data = ., Subject != 2L, Subject != 80)

    tobject1 <-
      t.test(
        formula = value ~ condition,
        data = bugs_short,
        var.equal = TRUE,
        conf.level = .95,
        paired = TRUE
      )
    tobject2 <-
      t.test(
        formula = value ~ condition,
        data = bugs_short,
        var.equal = TRUE,
        conf.level = .95,
        paired = TRUE
      )
    tobject3 <-
      t.test(
        formula = value ~ condition,
        data = bugs_short,
        var.equal = TRUE,
        conf.level = .90,
        paired = TRUE
      )
    tobject4 <-
      t.test(
        formula = value ~ condition,
        data = bugs_short,
        var.equal = TRUE,
        conf.level = .90,
        paired = TRUE
      )
    tobject5 <-
      t.test(
        x = bugs$LDLF,
        y = bugs$LDHF,
        var.equal = TRUE,
        conf.level = .90,
        paired = TRUE
      )

    testthat::expect_error(
      ggstatsplot:::effsize_t_parametric(
        formula = value ~ condition,
        data = bugs_short_unequal,
        paired = TRUE,
        hedges.correction = TRUE,
        conf.level = 0.95,
        noncentral = FALSE
      )
    )

    # g and central
    set.seed(123)
    df1 <- ggstatsplot:::effsize_t_parametric(
      formula = value ~ condition,
      data = bugs_short,
      paired = TRUE,
      hedges.correction = TRUE,
      conf.level = 0.95,
      noncentral = FALSE,
      tobject = tobject1
    )

    # g and non-central
    set.seed(123)
    df2 <- ggstatsplot:::effsize_t_parametric(
      formula = value ~ condition,
      data = bugs_short,
      paired = TRUE,
      hedges.correction = TRUE,
      conf.level = 0.95,
      noncentral = TRUE,
      tobject = tobject2
    )

    # d and central
    set.seed(123)
    df3 <- ggstatsplot:::effsize_t_parametric(
      formula = value ~ condition,
      data = bugs_short,
      paired = TRUE,
      hedges.correction = FALSE,
      conf.level = 0.90,
      noncentral = FALSE,
      tobject = tobject3
    )

    # d and non-central
    set.seed(123)
    df4 <- ggstatsplot:::effsize_t_parametric(
      formula = value ~ condition,
      data = bugs_short,
      paired = TRUE,
      hedges.correction = FALSE,
      conf.level = 0.90,
      noncentral = TRUE,
      tobject = tobject4
    )

    # not tidy data
    set.seed(123)
    df5 <- ggstatsplot:::effsize_t_parametric(
      formula = LDLF ~ LDHF,
      data = bugs,
      paired = TRUE,
      hedges.correction = FALSE,
      conf.level = 0.90,
      noncentral = TRUE,
      tobject = tobject5
    )

    # checking attributes of dataframe
    testthat::expect_identical(
      c(class(df1), class(df2), class(df3), class(df4)),
      rep(c("tbl_df", "tbl", "data.frame"), 4)
    )
    testthat::expect_equal(
      c(dim(df1), dim(df2), dim(df3), dim(df4)),
      rep(c(1L, 9L), 4)
    )

    # checking estimates and CIs
    testthat::expect_equal(df1$estimate, 0.6910747, tolerance = 0.001)
    testthat::expect_equal(df1$conf.low, 0.457525, tolerance = 0.001)
    testthat::expect_equal(df1$conf.high, 0.9246243, tolerance = 0.001)

    testthat::expect_equal(df2$estimate, df1$estimate, tolerance = 0.001)
    testthat::expect_equal(df3$estimate, 0.6969645, tolerance = 0.001)
    testthat::expect_equal(df3$conf.low, 0.5015875, tolerance = 0.001)
    testthat::expect_equal(df3$conf.high, 0.8923415, tolerance = 0.001)
    testthat::expect_equal(df4$estimate, df3$estimate, tolerance = 0.001)
    testthat::expect_equal(df5$estimate, -0.6969645, tolerance = 0.001)


    # checking details
    testthat::expect_equal(df4$conf.level, 0.90, tolerance = 0.01)
    testthat::expect_equal(df1$conf.level, 0.95, tolerance = 0.01)
    testthat::expect_identical(
      c(df1$method, df2$method, df3$method, df4$method),
      c("Hedges's g", "Hedges's g", "Cohen's d", "Cohen's d")
    )
    testthat::expect_identical(
      c(
        df1$alternative,
        df2$alternative,
        df3$alternative,
        df4$alternative
      ),
      c(rep("two.sided", 4L))
    )
    testthat::expect_true(df2$noncentral)
    testthat::expect_false(df1$noncentral)
    testthat::expect_true(df4$noncentral)
    testthat::expect_false(df3$noncentral)
    testthat::expect_identical(
      c(
        df1$noncentral,
        df2$noncentral,
        df3$noncentral,
        df4$noncentral
      ),
      c(FALSE, TRUE, FALSE, TRUE)
    )
    testthat::expect_identical(
      c(df1$var.equal, df2$var.equal, df3$var.equal, df4$var.equal),
      c(rep(FALSE, 4L))
    )
    testthat::expect_identical(
      c(df1$paired, df2$paired, df3$paired, df4$paired),
      c(rep(TRUE, 4L))
    )
  }
)


# effsize works for one sample test ------------

testthat::test_that(
  desc = "effsize works for one sample test",
  code = {
    testthat::skip_on_cran()

    tobject1 <-
      t.test(
        x = dplyr::starwars$height,
        var.equal = TRUE,
        conf.level = .99,
        mu = 175
      )
    tobject2 <-
      t.test(
        x = dplyr::starwars$height,
        var.equal = TRUE,
        conf.level = .90,
        mu = 175
      )
    tobject3 <-
      t.test(
        x = dplyr::starwars$height,
        var.equal = TRUE,
        conf.level = .99,
        mu = 100
      )
    tobject4 <-
      t.test(
        x = dplyr::starwars$height,
        var.equal = TRUE,
        conf.level = .90,
        mu = 100
      )


    # d and central
    set.seed(123)
    df1 <- ggstatsplot:::effsize_t_parametric(
      formula = ~height,
      data = dplyr::starwars,
      mu = 175,
      hedges.correction = FALSE,
      conf.level = 0.99,
      noncentral = FALSE,
      tobject = tobject1
    )

    # d and noncentral
    set.seed(123)
    df2 <- ggstatsplot:::effsize_t_parametric(
      formula = ~height,
      data = dplyr::starwars,
      mu = 175,
      hedges.correction = FALSE,
      conf.level = 0.90,
      noncentral = TRUE,
      tobject = tobject2
    )

    # g and central
    set.seed(123)
    df3 <- ggstatsplot:::effsize_t_parametric(
      formula = ~height,
      data = dplyr::starwars,
      hedges.correction = TRUE,
      mu = 100,
      conf.level = 0.99,
      noncentral = FALSE,
      tobject = tobject3
    )

    # g and noncentral
    set.seed(123)
    df4 <- suppressWarnings(
      ggstatsplot:::effsize_t_parametric(
        formula = ~height,
        data = dplyr::starwars,
        mu = 100,
        hedges.correction = TRUE,
        conf.level = 0.90,
        noncentral = TRUE,
        tobject = tobject4
      )
    )

    # checking estimates and CIs
    testthat::expect_equal(df1$estimate, -0.01846326, tolerance = 0.001)
    testthat::expect_equal(df1$conf.low, -0.605, tolerance = 0.001)
    testthat::expect_equal(df1$conf.high, 0.568, tolerance = 0.001)

    testthat::expect_equal(df2$estimate, df1$estimate, tolerance = 0.001)
    testthat::expect_equal(df2$conf.low, -0.2024353, tolerance = 0.001)
    testthat::expect_equal(df2$conf.high, 0.1653947, tolerance = 0.001)

    testthat::expect_equal(df3$estimate, 2.118175, tolerance = 0.001)
    testthat::expect_equal(df3$conf.low, 1.383058, tolerance = 0.001)
    testthat::expect_equal(df3$conf.high, 2.853292, tolerance = 0.001)

    testthat::expect_equal(df4$estimate, df3$estimate, tolerance = 0.001)
    testthat::expect_equal(df4$conf.low, 1.813373, tolerance = 0.001)
    testthat::expect_equal(df4$conf.high, 2.482309, tolerance = 0.001)

    testthat::expect_equal(df4$conf.level, 0.90, tolerance = 0.01)
    testthat::expect_equal(df3$conf.level, 0.99, tolerance = 0.01)
  }
)


# t1way_ci works ------------------------------------------------------------

context("t1way_ci")

testthat::test_that(
  desc = "t1way_ci works",
  code = {
    testthat::skip_on_cran()

    # normal
    set.seed(123)
    df1 <-
      ggstatsplot:::t1way_ci(
        data = dplyr::filter(.data = ggplot2::msleep, vore != "insecti"),
        x = vore,
        y = brainwt,
        nboot = 25,
        conf.level = 0.99,
        tr = 0.05,
        conf.type = c("norm")
      )

    # percentile
    set.seed(123)
    df2 <-
      suppressWarnings(ggstatsplot:::t1way_ci(
        data = dplyr::filter(.data = ggplot2::msleep, vore != "insecti"),
        x = vore,
        y = brainwt,
        tr = 0.1,
        nboot = 50,
        conf.level = 0.99,
        conf.type = "perc"
      ))

    # bca
    set.seed(123)
    df3 <-
      suppressWarnings(ggstatsplot:::t1way_ci(
        data = dplyr::filter(.data = ggplot2::msleep, vore != "insecti"),
        x = vore,
        y = brainwt,
        nboot = 50,
        conf.level = 0.99,
        tr = 0.05,
        conf.type = c("bca")
      ))

    # test normal CI
    testthat::expect_equal(df1$xi, 0.7639015, tolerance = 0.00002)
    testthat::expect_equal(df1$conf.low, 0.06259349, tolerance = 0.00002)
    testthat::expect_equal(df1$conf.high, 1.838343, tolerance = 0.00002)
    testthat::expect_equal(df1$F.value, 0.6146867, tolerance = 0.00002)
    testthat::expect_equal(df1$p.value, 0.5487093, tolerance = 0.00002)

    # test percentile CI
    testthat::expect_equal(df2$xi, 1.452066, tolerance = 0.00002)
    testthat::expect_equal(df2$conf.low, 0.1435678, tolerance = 0.00002)
    testthat::expect_equal(df2$conf.high, 2.357306, tolerance = 0.00002)
    testthat::expect_equal(df2$F.value, 0.260884, tolerance = 0.00002)
    testthat::expect_equal(df2$p.value, 0.772501, tolerance = 0.00002)

    # test bca
    testthat::expect_equal(df3$xi, 0.5664255, tolerance = 0.00002)
    testthat::expect_equal(df3$conf.low, 0.2904682, tolerance = 0.00002)
    testthat::expect_equal(df3$conf.high, 1.724382, tolerance = 0.00002)
  }
)

# test_yuend_ci works ---------------------------------------------------------

context("test_yuend_ci")

testthat::test_that(
  desc = "Yuen's test on trimmed means for dependent samples works",
  code = {
    testthat::skip_on_cran()

    # made up data
    mydata <-
      structure(list(
        time = structure(
          c(
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            1L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L,
            2L
          ),
          .Label = c("test1", "test2"),
          class = "factor"
        ),
        grade = c(
          42.9,
          51.8,
          71.7,
          51.6,
          63.5,
          58,
          59.8,
          50.8,
          62.5,
          61.9,
          50.4,
          52.6,
          63,
          58.3,
          53.3,
          58.7,
          50.1,
          64.2,
          57.4,
          57.1,
          44.6,
          54,
          72.3,
          53.4,
          63.8,
          59.3,
          60.8,
          51.6,
          64.3,
          63.2,
          51.8,
          52.2,
          63,
          60.5,
          57.1,
          60.1,
          51.7,
          65.6,
          58.3,
          60.1
        )
      ),
      row.names = c(NA, -40L),
      class = "data.frame"
      )

    # ggstatsplot output
    set.seed(123)
    df1 <-
      ggstatsplot:::yuend_ci(
        data = mydata,
        x = time,
        y = grade
      )

    # creating a dataframe with NAs
    mydata1 <- purrr::map_df(
      .x = mydata,
      .f = ~ .[sample(
        x = c(TRUE, NA),
        prob = c(0.8, 0.2),
        size = length(.),
        replace = TRUE
      )]
    )

    # creating a dataframe
    set.seed(123)
    df2 <-
      ggstatsplot:::yuend_ci(
        data = mydata1,
        x = time,
        y = grade,
        conf.level = 0.90,
        conf.type = "basic"
      )

    # percentile CI
    set.seed(123)
    df3 <-
      ggstatsplot:::yuend_ci(
        data = mydata1,
        x = time,
        y = grade,
        conf.level = 0.50,
        conf.type = "perc"
      )

    # bca CI
    set.seed(123)
    df4 <-
      suppressWarnings(ggstatsplot:::yuend_ci(
        data = mydata1,
        x = time,
        y = grade,
        conf.level = 0.85,
        conf.type = "bca"
      ))

    # testing (dataframe without NAs)
    testthat::expect_equal(df1$t.value, -5.268876, tolerance = .001)
    testthat::expect_equal(df1$xi, 0.166875, tolerance = 0.0001)
    testthat::expect_equal(df1$conf.low, 0.07917537, tolerance = 0.0001)
    testthat::expect_equal(df1$conf.high, 0.2513106, tolerance = 0.001)
    testthat::expect_equal(df1$df, 15L)
    testthat::expect_equal(df1$p.value, 0.0000945, tolerance = 0.000001)

    # testing (dataframe with NAs)
    testthat::expect_equal(df2$t.value, -0.3976015, tolerance = .001)
    testthat::expect_equal(df2$xi, 0.1607089, tolerance = 0.0001)
    testthat::expect_equal(df2$conf.low, -0.408429, tolerance = 0.0001)
    testthat::expect_equal(df2$conf.high, 0.3024555, tolerance = 0.001)
    testthat::expect_equal(df2$df, 7L)
    testthat::expect_equal(df2$p.value, 0.7027698, tolerance = 0.000001)

    # testing (dataframe with NAs + percentile CI)
    testthat::expect_equal(df3$xi, 0.1607089, tolerance = 0.0001)
    testthat::expect_equal(df3$conf.low, 0.1566501, tolerance = 0.0001)
    testthat::expect_equal(df3$conf.high, 0.4350129, tolerance = 0.0001)

    # testing (dataframe with NAs + bca CI)
    testthat::expect_equal(df4$xi, 0.1607089, tolerance = 0.0001)
    testthat::expect_equal(df4$conf.low, 0.00291321, tolerance = 0.0001)
    testthat::expect_equal(df4$conf.high, 0.3079085, tolerance = 0.0001)
  }
)

# robcor_ci works ---------------------------------------------------------

context("robcor_ci")

testthat::test_that(
  desc = "robcor_ci works",
  code = {
    testthat::skip_on_cran()

    # using mtcars dataset
    set.seed(123)
    df1 <- ggstatsplot:::robcor_ci(
      data = datasets::mtcars,
      x = hp,
      y = mpg,
      beta = .01,
      nboot = 125,
      conf.level = .99,
      conf.type = c("norm")
    )

    # induce an NA
    mtcars2 <- datasets::mtcars
    mtcars2[1, 1] <- NA
    set.seed(123)

    # this also makes sure that the quoted arguments work
    df2 <-
      suppressWarnings(ggstatsplot:::robcor_ci(
        data = mtcars2,
        x = "hp",
        y = "mpg",
        beta = .01,
        nboot = 125,
        conf.level = .99,
        conf.type = c("basic")
      ))

    # percentile CI
    set.seed(123)
    df3 <- suppressWarnings(ggstatsplot:::robcor_ci(
      data = ggplot2::msleep,
      x = brainwt,
      y = sleep_rem,
      beta = 0.1,
      nboot = 55,
      conf.level = 0.99,
      conf.type = "perc"
    ))

    # bca CI
    set.seed(123)
    df4 <- suppressWarnings(ggstatsplot:::robcor_ci(
      data = ggplot2::msleep,
      x = brainwt,
      y = sleep_rem,
      beta = 0.2,
      nboot = 100,
      conf.level = 0.90,
      conf.type = "bca"
    ))

    # data without NAs
    testthat::expect_equal(df1$estimate, -0.8042457, tolerance = 0.00001)
    testthat::expect_equal(df1$conf.low, -0.9367074, tolerance = 0.00001)
    testthat::expect_equal(df1$conf.high, -0.6795268, tolerance = 0.00001)
    testthat::expect_equal(df1$p.value, 2.933186e-08, tolerance = 0.00001)
    testthat::expect_equal(df1$statistic, -7.412179, tolerance = 0.00001)
    testthat::expect_identical(class(df1)[[1]], "tbl_df")

    # data with NAs
    testthat::expect_equal(df2$estimate, -0.8052814, tolerance = 0.00001)
    testthat::expect_equal(df2$conf.low, -0.9576768, tolerance = 0.00001)
    testthat::expect_equal(df2$conf.high, -0.717556, tolerance = 0.00001)
    testthat::expect_equal(df2$p.value, 4.677899e-08, tolerance = 0.00001)
    testthat::expect_equal(df2$statistic, -7.314263, tolerance = 0.00001)

    # percentile CI
    testthat::expect_equal(df3$estimate, -0.3956043, tolerance = 0.00001)
    testthat::expect_equal(df3$conf.low, -0.5488374, tolerance = 0.00001)
    testthat::expect_equal(df3$conf.high, -0.1557196, tolerance = 0.00001)
    testthat::expect_equal(df3$p.value, 0.005384018, tolerance = 0.00001)
    testthat::expect_equal(df3$statistic, -2.921448, tolerance = 0.00001)
    testthat::expect_equal(df3$conf, 0.99, tolerance = 0.001)
    testthat::expect_equal(df3$beta, 0.1, tolerance = 0.01)
    testthat::expect_equal(df3$nboot, 55L)
    testthat::expect_equal(df3$n, 48L)

    # bca CI
    testthat::expect_equal(df4$estimate, -0.4085762, tolerance = 0.00001)
    testthat::expect_equal(df4$conf.low, -0.5629717, tolerance = 0.00001)
    testthat::expect_equal(df4$conf.high, -0.1625011, tolerance = 0.00001)
  }
)
