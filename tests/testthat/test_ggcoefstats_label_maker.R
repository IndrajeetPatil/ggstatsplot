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
