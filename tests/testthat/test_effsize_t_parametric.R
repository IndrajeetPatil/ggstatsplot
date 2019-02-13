context("effsize_t_parametric")

# effsize works for Cohen's d and Hedge's g (between - without NA) ------------

testthat::test_that(
  desc = "effsize works for Cohen's d and Hedge's g (between - without NA)",
  code = {
    testthat::skip_on_cran()

    # creating a new dataframe with a variable that has dropped factor level
    mtcars_short <- dplyr::filter(.data = mtcars, cyl != "4")

    # shouldn't work
    testthat::expect_error(ggstatsplot::effsize_t_parametric(
      formula = wt ~ cyl,
      data = mtcars
    ))

    # g and central
    set.seed(123)
    df1 <- ggstatsplot::effsize_t_parametric(
      formula = wt ~ am,
      data = mtcars,
      paired = FALSE,
      hedges.correction = TRUE,
      conf.level = 0.95,
      noncentral = FALSE
    )

    # g and non-central
    set.seed(123)
    df2 <- ggstatsplot::effsize_t_parametric(
      formula = wt ~ am,
      data = mtcars,
      paired = FALSE,
      hedges.correction = TRUE,
      conf.level = 0.95,
      noncentral = TRUE
    )

    # d and central
    set.seed(123)
    df3 <- ggstatsplot::effsize_t_parametric(
      formula = wt ~ cyl,
      data = mtcars_short,
      paired = FALSE,
      hedges.correction = FALSE,
      conf.level = 0.99,
      noncentral = FALSE
    )

    # d and non-central
    set.seed(123)
    df4 <- ggstatsplot::effsize_t_parametric(
      formula = wt ~ cyl,
      data = mtcars_short,
      paired = FALSE,
      hedges.correction = FALSE,
      conf.level = 0.99,
      noncentral = TRUE
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
    testthat::expect_equal(df1$estimate, 1.844698, tolerance = 0.001)
    testthat::expect_equal(df1$conf.low, 1.012779, tolerance = 0.001)
    testthat::expect_equal(df1$conf.high, 2.772033, tolerance = 0.001)

    testthat::expect_equal(df2$estimate, df1$estimate, tolerance = 0.001)
    testthat::expect_equal(df2$conf.low, 1.030023, tolerance = 0.001)
    testthat::expect_equal(df2$conf.high, 2.732913, tolerance = 0.001)

    testthat::expect_equal(df3$estimate, -1.337884, tolerance = 0.001)
    testthat::expect_equal(df3$conf.low, -2.787967, tolerance = 0.001)
    testthat::expect_equal(df3$conf.high, 0.1121982, tolerance = 0.001)

    testthat::expect_equal(df4$estimate, df3$estimate, tolerance = 0.001)
    testthat::expect_equal(df4$conf.low, -2.641733, tolerance = 0.001)
    testthat::expect_equal(df4$conf.high, -0.01115432, tolerance = 0.001)

    # checking details
    testthat::expect_identical(
      c(df1$method, df2$method, df3$method, df4$method),
      c("Hedges's g", "Hedges's g", "Cohen's d", "Cohen's d")
    )
    testthat::expect_identical(
      c(df1$alternative, df2$alternative, df3$alternative, df4$alternative),
      c(rep("two.sided", 4L))
    )
    testthat::expect_true(df2$noncentral)
    testthat::expect_false(df1$noncentral)
    testthat::expect_true(df4$noncentral)
    testthat::expect_false(df3$noncentral)
    testthat::expect_identical(
      c(df1$noncentral, df2$noncentral, df3$noncentral, df4$noncentral),
      c(FALSE, TRUE, FALSE, TRUE)
    )
    testthat::expect_identical(
      c(df1$var.equal, df2$var.equal, df3$var.equal, df4$var.equal),
      c(rep(TRUE, 4L))
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
    testthat::expect_error(ggstatsplot::effsize_t_parametric(
      formula = brainwt ~ vore,
      data = msleep
    ))

    # g and central
    set.seed(123)
    df1 <- ggstatsplot::effsize_t_parametric(
      formula = brainwt ~ vore,
      data = msleep_short,
      paired = FALSE,
      hedges.correction = TRUE,
      conf.level = 0.95,
      noncentral = FALSE
    )

    # g and non-central
    set.seed(123)
    df2 <- ggstatsplot::effsize_t_parametric(
      formula = brainwt ~ vore,
      data = msleep_short,
      paired = FALSE,
      hedges.correction = TRUE,
      conf.level = 0.95,
      noncentral = TRUE
    )

    # d and central
    set.seed(123)
    df3 <- ggstatsplot::effsize_t_parametric(
      formula = sleep_rem ~ vore,
      data = msleep_short,
      paired = FALSE,
      hedges.correction = FALSE,
      conf.level = 0.90,
      noncentral = FALSE
    )

    # d and non-central
    set.seed(123)
    df4 <- ggstatsplot::effsize_t_parametric(
      formula = sleep_rem ~ vore,
      data = msleep_short,
      paired = FALSE,
      hedges.correction = FALSE,
      conf.level = 0.90,
      noncentral = TRUE
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
    testthat::expect_equal(df1$estimate, -0.3993768, tolerance = 0.001)
    testthat::expect_equal(df1$conf.low, -1.241881, tolerance = 0.001)
    testthat::expect_equal(df1$conf.high, 0.4200867, tolerance = 0.001)

    testthat::expect_equal(df2$estimate, df1$estimate, tolerance = 0.001)
    testthat::expect_equal(df2$conf.low, -1.201409, tolerance = 0.001)
    testthat::expect_equal(df2$conf.high, 0.3870483, tolerance = 0.001)

    testthat::expect_equal(df3$estimate, 0.7330991, tolerance = 0.001)
    testthat::expect_equal(df3$conf.low, 0.07799961, tolerance = 0.001)
    testthat::expect_equal(df3$conf.high, 1.388199, tolerance = 0.001)

    testthat::expect_equal(df4$estimate, df3$estimate, tolerance = 0.001)
    testthat::expect_equal(df4$conf.low, 0.09053439, tolerance = 0.001)
    testthat::expect_equal(df4$conf.high, 1.364623, tolerance = 0.001)

    # checking details
    testthat::expect_identical(
      c(df1$method, df2$method, df3$method, df4$method),
      c("Hedges's g", "Hedges's g", "Cohen's d", "Cohen's d")
    )
    testthat::expect_identical(
      c(df1$alternative, df2$alternative, df3$alternative, df4$alternative),
      c(rep("two.sided", 4L))
    )
    testthat::expect_true(df2$noncentral)
    testthat::expect_false(df1$noncentral)
    testthat::expect_true(df4$noncentral)
    testthat::expect_false(df3$noncentral)
    testthat::expect_identical(
      c(df1$noncentral, df2$noncentral, df3$noncentral, df4$noncentral),
      c(FALSE, TRUE, FALSE, TRUE)
    )
    testthat::expect_identical(
      c(df1$var.equal, df2$var.equal, df3$var.equal, df4$var.equal),
      c(rep(TRUE, 4L))
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
    testthat::expect_error(ggstatsplot::effsize_t_parametric(
      formula = value ~ condition,
      data = iris_long,
      paired = TRUE
    ))

    # g and central
    set.seed(123)
    df1 <- ggstatsplot::effsize_t_parametric(
      formula = value ~ condition,
      data = iris_short,
      paired = TRUE,
      hedges.correction = TRUE,
      conf.level = 0.95,
      noncentral = FALSE
    )

    # g and non-central
    set.seed(123)
    df2 <- ggstatsplot::effsize_t_parametric(
      formula = value ~ condition,
      data = iris_short,
      paired = TRUE,
      hedges.correction = TRUE,
      conf.level = 0.95,
      noncentral = TRUE
    )

    # d and central
    set.seed(123)
    df3 <- ggstatsplot::effsize_t_parametric(
      formula = value ~ condition,
      data = iris_short,
      paired = TRUE,
      hedges.correction = FALSE,
      conf.level = 0.99,
      noncentral = FALSE
    )

    # d and non-central
    set.seed(123)
    df4 <- ggstatsplot::effsize_t_parametric(
      formula = value ~ condition,
      data = iris_short,
      paired = TRUE,
      hedges.correction = FALSE,
      conf.level = 0.50,
      noncentral = TRUE
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
    testthat::expect_equal(df1$estimate, -2.828219, tolerance = 0.001)
    testthat::expect_equal(df1$conf.low, -3.569516, tolerance = 0.001)
    testthat::expect_equal(df1$conf.high, -2.115781, tolerance = 0.001)

    testthat::expect_equal(df2$estimate, df1$estimate, tolerance = 0.001)
    testthat::expect_equal(df2$conf.low, -3.208837, tolerance = 0.001)
    testthat::expect_equal(df2$conf.high, -2.489882, tolerance = 0.001)

    testthat::expect_equal(df3$estimate, -2.842649, tolerance = 0.001)
    testthat::expect_equal(df3$conf.low, -3.802441, tolerance = 0.001)
    testthat::expect_equal(df3$conf.high, -1.882856, tolerance = 0.001)

    testthat::expect_equal(df4$estimate, df3$estimate, tolerance = 0.001)
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
      c(df1$alternative, df2$alternative, df3$alternative, df4$alternative),
      c(rep("two.sided", 4L))
    )
    testthat::expect_true(df2$noncentral)
    testthat::expect_false(df1$noncentral)
    testthat::expect_true(df4$noncentral)
    testthat::expect_false(df3$noncentral)
    testthat::expect_identical(
      c(df1$noncentral, df2$noncentral, df3$noncentral, df4$noncentral),
      c(FALSE, TRUE, FALSE, TRUE)
    )
    testthat::expect_identical(
      c(df1$var.equal, df2$var.equal, df3$var.equal, df4$var.equal),
      c(rep(TRUE, 4L))
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

    testthat::expect_error(ggstatsplot::effsize_t_parametric(
      formula = value ~ condition,
      data = bugs_short_unequal,
      paired = TRUE,
      hedges.correction = TRUE,
      conf.level = 0.95,
      noncentral = FALSE
    ))

    # g and central
    set.seed(123)
    df1 <- ggstatsplot::effsize_t_parametric(
      formula = value ~ condition,
      data = bugs_short,
      paired = TRUE,
      hedges.correction = TRUE,
      conf.level = 0.95,
      noncentral = FALSE
    )

    # g and non-central
    set.seed(123)
    df2 <- ggstatsplot::effsize_t_parametric(
      formula = value ~ condition,
      data = bugs_short,
      paired = TRUE,
      hedges.correction = TRUE,
      conf.level = 0.95,
      noncentral = TRUE
    )

    # d and central
    set.seed(123)
    df3 <- ggstatsplot::effsize_t_parametric(
      formula = value ~ condition,
      data = bugs_short,
      paired = TRUE,
      hedges.correction = FALSE,
      conf.level = 0.90,
      noncentral = FALSE
    )

    # d and non-central
    set.seed(123)
    df4 <- ggstatsplot::effsize_t_parametric(
      formula = value ~ condition,
      data = bugs_short,
      paired = TRUE,
      hedges.correction = FALSE,
      conf.level = 0.90,
      noncentral = TRUE
    )

    # not tidy data
    set.seed(123)
    df5 <- ggstatsplot::effsize_t_parametric(
      formula = LDLF ~ LDHF,
      data = bugs,
      paired = TRUE,
      hedges.correction = FALSE,
      conf.level = 0.90,
      noncentral = TRUE
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
    testthat::expect_equal(df1$conf.low, -0.9305142, tolerance = 0.001)
    testthat::expect_equal(df1$conf.high, -0.4634148, tolerance = 0.001)

    testthat::expect_equal(df2$estimate, df1$estimate, tolerance = 0.001)
    testthat::expect_equal(df2$conf.low, -0.9297575, tolerance = 0.001)
    testthat::expect_equal(df2$conf.high, -0.4687366, tolerance = 0.001)

    testthat::expect_equal(df3$estimate, -0.6969645, tolerance = 0.001)
    testthat::expect_equal(df3$conf.low, -0.8923415, tolerance = 0.001)
    testthat::expect_equal(df3$conf.high, -0.5015875, tolerance = 0.001)

    testthat::expect_equal(df4$estimate, df3$estimate, tolerance = 0.001)
    testthat::expect_equal(df4$conf.low, -0.8925503, tolerance = 0.001)
    testthat::expect_equal(df4$conf.high, -0.5056502, tolerance = 0.001)

    testthat::expect_equal(df5$estimate, 0.6969645, tolerance = 0.001)
    testthat::expect_equal(df5$conf.low, 0.5056489, tolerance = 0.001)
    testthat::expect_equal(df5$conf.high, 0.8925545, tolerance = 0.001)

    # checking details
    testthat::expect_equal(df4$conf.level, 0.90, tolerance = 0.01)
    testthat::expect_equal(df1$conf.level, 0.95, tolerance = 0.01)
    testthat::expect_identical(
      c(df1$method, df2$method, df3$method, df4$method),
      c("Hedges's g", "Hedges's g", "Cohen's d", "Cohen's d")
    )
    testthat::expect_identical(
      c(df1$alternative, df2$alternative, df3$alternative, df4$alternative),
      c(rep("two.sided", 4L))
    )
    testthat::expect_true(df2$noncentral)
    testthat::expect_false(df1$noncentral)
    testthat::expect_true(df4$noncentral)
    testthat::expect_false(df3$noncentral)
    testthat::expect_identical(
      c(df1$noncentral, df2$noncentral, df3$noncentral, df4$noncentral),
      c(FALSE, TRUE, FALSE, TRUE)
    )
    testthat::expect_identical(
      c(df1$var.equal, df2$var.equal, df3$var.equal, df4$var.equal),
      c(rep(TRUE, 4L))
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

    # d and central
    set.seed(123)
    df1 <- ggstatsplot::effsize_t_parametric(
      formula = ~ height,
      data = dplyr::starwars,
      mu = 175,
      hedges.correction = FALSE,
      conf.level = 0.99,
      noncentral = FALSE
    )

    # d and noncentral
    set.seed(123)
    df2 <- ggstatsplot::effsize_t_parametric(
      formula = ~ height,
      data = dplyr::starwars,
      mu = 175,
      hedges.correction = FALSE,
      conf.level = 0.90,
      noncentral = TRUE
    )

    # g and central
    set.seed(123)
    df3 <- ggstatsplot::effsize_t_parametric(
      formula = ~ height,
      data = dplyr::starwars,
      hedges.correction = TRUE,
      mu = 100,
      conf.level = 0.99,
      noncentral = FALSE
    )

    # g and noncentral
    set.seed(123)
    df4 <- suppressWarnings(ggstatsplot::effsize_t_parametric(
      formula = ~ height,
      data = dplyr::starwars,
      mu = 100,
      hedges.correction = TRUE,
      conf.level = 0.90,
      noncentral = TRUE
    ))

    # checking estimates and CIs
    testthat::expect_equal(df1$estimate, -0.01846326, tolerance = 0.001)
    testthat::expect_equal(df1$conf.low, -15.57789, tolerance = 0.001)
    testthat::expect_equal(df1$conf.high, 15.54096, tolerance = 0.001)

    testthat::expect_equal(df2$estimate, df1$estimate, tolerance = 0.001)
    testthat::expect_equal(df2$conf.low, -0.2024353, tolerance = 0.001)
    testthat::expect_equal(df2$conf.high, 0.1653947, tolerance = 0.001)

    testthat::expect_equal(df3$estimate, 2.118175, tolerance = 0.001)
    testthat::expect_equal(df3$conf.low, -13.42088, tolerance = 0.001)
    testthat::expect_equal(df3$conf.high, 17.69797, tolerance = 0.001)

    testthat::expect_equal(df4$estimate, df3$estimate, tolerance = 0.001)
    testthat::expect_equal(df4$conf.low, 1.813373, tolerance = 0.001)
    testthat::expect_equal(df4$conf.high, 2.482309, tolerance = 0.001)

    testthat::expect_equal(df4$conf.level, 0.90, tolerance = 0.01)
    testthat::expect_equal(df3$conf.level, 0.99, tolerance = 0.01)
  }
)
