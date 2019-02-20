context("effsize_t_parametric")

# effsize works for Cohen's d and Hedge's g (between - without NA) ------------

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

    testthat::expect_error(ggstatsplot:::effsize_t_parametric(
      formula = wt ~ am,
      data = mtcars,
      paired = TRUE
    ))

    testthat::expect_error(ggstatsplot:::effsize_t_parametric(
      formula = wt ~ cyl,
      data = mtcars,
      paired = TRUE
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
    testthat::expect_equal(df1$estimate, -2.828219, tolerance = 0.001)
    testthat::expect_equal(df1$conf.low, -3.555086, tolerance = 0.001)
    testthat::expect_equal(df1$conf.high, -2.101352, tolerance = 0.001)
    testthat::expect_equal(df2$estimate, df1$estimate, tolerance = 0.001)
    testthat::expect_equal(df3$estimate, -2.842649, tolerance = 0.001)
    testthat::expect_equal(df3$conf.low, -3.802441, tolerance = 0.001)
    testthat::expect_equal(df3$conf.high, -1.882856, tolerance = 0.001)
    testthat::expect_equal(df4$estimate, df3$estimate, tolerance = 0.001)
    testthat::expect_equal(df2$conf.low, -3.208837, tolerance = 0.001)
    testthat::expect_equal(df2$conf.high, -2.489882, tolerance = 0.001)
    testthat::expect_equal(df4$conf.low, -2.971019, tolerance = 0.001)
    testthat::expect_equal(df4$conf.high, -2.722362, tolerance = 0.001)


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
    testthat::expect_equal(df1$estimate, -0.6910747, tolerance = 0.001)
    testthat::expect_equal(df1$conf.low, -0.9246243, tolerance = 0.001)
    testthat::expect_equal(df1$conf.high, -0.457525, tolerance = 0.001)

    testthat::expect_equal(df2$estimate, df1$estimate, tolerance = 0.001)
    testthat::expect_equal(df3$estimate, -0.6969645, tolerance = 0.001)
    testthat::expect_equal(df3$conf.low, -0.8923415, tolerance = 0.001)
    testthat::expect_equal(df3$conf.high, -0.5015875, tolerance = 0.001)
    testthat::expect_equal(df4$estimate, df3$estimate, tolerance = 0.001)
    testthat::expect_equal(df5$estimate, 0.6969645, tolerance = 0.001)


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
