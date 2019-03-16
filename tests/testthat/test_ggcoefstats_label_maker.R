context("ggcoefstats_label_maker")

# glmRob works -------------------------------------------------------

testthat::test_that(
  desc = "glmRob works",
  code = {
    testthat::skip_on_cran()

    library(robust)

    # data
    data(breslow.dat)

    # new data
    set.seed(123)
    x1 <- stats::rnorm(50)
    y1 <- stats::rpois(n = 50, lambda = exp(1 + x1))
    df_quasi <- data.frame(x = x1, y = y1) %>%
      tibble::as_tibble(x = .)

    # model
    set.seed(123)
    mod <-
      robust::glmRob(
        formula = sumY ~ Base4 * Trt,
        family = poisson(),
        data = breslow.dat,
        method = "cubif"
      )

    # create a dataframe with labels column
    df <- ggstatsplot:::ggcoefstats_label_maker(
      x = mod,
      tidy_df = broom::tidy(mod),
      glance_df = broom::glance(mod)
    )

    # checking the label
    testthat::expect_equal(
      df$label,
      c(
        "list(~italic(beta)==2.01, ~italic(z)==12.19, ~italic(p)<= 0.001)",
        "list(~italic(beta)==0.19, ~italic(z)==5.59, ~italic(p)<= 0.001)",
        "list(~italic(beta)==-0.22, ~italic(z)==-1.01, ~italic(p)==0.312)",
        "list(~italic(beta)==-0.03, ~italic(z)==-0.82, ~italic(p)==0.411)"
      )
    )

    # certain families are still not supported in robust package
    testthat::expect_error(ggstatsplot:::ggcoefstats_label_maker(robust::glmRob(
      formula = y ~ x,
      family = quasi(variance = "mu", link = "log"),
      data = df_quasi
    )))
  }
)

# glmmTMB works -------------------------------------------------------

testthat::test_that(
  desc = "glmmTMB works",
  code = {
    testthat::skip_on_cran()

    library(glmmTMB)

    # model
    set.seed(123)
    m1 <- glmmTMB::glmmTMB(
      formula = count ~ mined + (1 | site),
      ziformula = ~mined,
      family = poisson,
      data = Salamanders
    )

    # tidy dataframe
    df <-
      ggstatsplot:::ggcoefstats_label_maker(
        x = m1,
        tidy_df = broom.mixed::tidy(m1),
        glance_df = broom.mixed::glance(m1)
      ) %>%
      dplyr::filter(.data = ., !is.na(std.error))

    # checking the labels
    testthat::expect_equal(
      df$label,
      c(
        "list(~italic(beta)==0.09, ~italic(z)==0.38, ~italic(p)==0.706)",
        "list(~italic(beta)==1.14, ~italic(z)==4.64, ~italic(p)<= 0.001)",
        "list(~italic(beta)==1.14, ~italic(z)==4.85, ~italic(p)<= 0.001)",
        "list(~italic(beta)==-1.74, ~italic(z)==-6.63, ~italic(p)<= 0.001)"
      )
    )
  }
)

# glmerMod works -------------------------------------------------------

testthat::test_that(
  desc = "glmerMod works",
  code = {
    testthat::skip_on_cran()

    library(lme4)

    # data
    anorexia <- structure(list(Treat = structure(c(
      2L, 2L, 2L, 2L, 2L, 2L, 2L,
      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L,
      2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
      3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
      3L
    ), .Label = c("CBT", "Cont", "FT"), class = "factor"), Prewt = c(
      80.7,
      89.4, 91.8, 74, 78.1, 88.3, 87.3, 75.1, 80.6, 78.4, 77.6, 88.7,
      81.3, 78.1, 70.5, 77.3, 85.2, 86, 84.1, 79.7, 85.5, 84.4, 79.6,
      77.5, 72.3, 89, 80.5, 84.9, 81.5, 82.6, 79.9, 88.7, 94.9, 76.3,
      81, 80.5, 85, 89.2, 81.3, 76.5, 70, 80.4, 83.3, 83, 87.7, 84.2,
      86.4, 76.5, 80.2, 87.8, 83.3, 79.7, 84.5, 80.8, 87.4, 83.8, 83.3,
      86, 82.5, 86.7, 79.6, 76.9, 94.2, 73.4, 80.5, 81.6, 82.1, 77.6,
      83.5, 89.9, 86, 87.3
    ), Postwt = c(
      80.2, 80.1, 86.4, 86.3, 76.1,
      78.1, 75.1, 86.7, 73.5, 84.6, 77.4, 79.5, 89.6, 81.4, 81.8, 77.3,
      84.2, 75.4, 79.5, 73, 88.3, 84.7, 81.4, 81.2, 88.2, 78.8, 82.2,
      85.6, 81.4, 81.9, 76.4, 103.6, 98.4, 93.4, 73.4, 82.1, 96.7,
      95.3, 82.4, 72.5, 90.9, 71.3, 85.4, 81.6, 89.1, 83.9, 82.7, 75.7,
      82.6, 100.4, 85.2, 83.6, 84.6, 96.2, 86.7, 95.2, 94.3, 91.5,
      91.9, 100.3, 76.7, 76.8, 101.6, 94.9, 75.2, 77.8, 95.5, 90.7,
      92.5, 93.8, 91.7, 98
    )), class = "data.frame", row.names = c(
      NA,
      72L
    ))

    # model
    set.seed(123)
    mod <-
      lme4::glmer(
        formula = Postwt ~ Prewt + (1 | Treat),
        family = stats::Gamma(),
        control = lme4::glmerControl(
          "Nelder_Mead",
          check.conv.grad = .makeCC(
            action = "message",
            tol = 0.01,
            relTol = NULL
          ),
          check.conv.singular = .makeCC(action = "message", tol = 0.01),
          check.conv.hess = .makeCC(action = "message", tol = 0.01)
        ),
        data = anorexia
      )

    # dataframe with labels
    df <- ggstatsplot:::ggcoefstats_label_maker(
      x = mod,
      tidy_df = broom.mixed::tidy(x = mod, effects = "fixed"),
      glance_df = broom.mixed::glance(mod)
    )

    # checking the labels
    testthat::expect_equal(
      df$label,
      c(
        "list(~italic(beta)==0.02, ~italic(t)(68)==41.12, ~italic(p)<= 0.001)",
        "list(~italic(beta)==0.00, ~italic(t)(68)==-7.27, ~italic(p)<= 0.001)"
      )
    )
  }
)
