context("ggcoefstats_label_maker")

# glmRob works -------------------------------------------------------

testthat::test_that(
  desc = "glmRob works",
  code = {
    library(robust)

    # data
    data(breslow.dat)
    x1 <- stats::rnorm(50)
    y1 <- stats::rpois(n = 50, lambda = exp(1 + x1))
    df_quasi <- data.frame(x = x1, y = y1) %>%
      tibble::as_data_frame(x = .)

    # model
    set.seed(123)
    mod <-
      robust::glmRob(
        formula = sumY ~ Age10 + Base4 * Trt,
        family = poisson(),
        data = breslow.dat,
        method = "cubif"
      )


    # create a dataframe with labels column
    df <- ggstatsplot:::ggcoefstats_label_maker(
      x = mod,
      tidy_df = broom::tidy(mod),
      broom::glance(mod)
    )

    # checking the label
    testthat::expect_equal(
      df$label,
      c(
        "list(~italic(beta)==9560.74, ~italic(z)==1.167672e+12, ~italic(p)<= 0.001)",
        "list(~italic(beta)==-3867.83, ~italic(z)==-1.923114e+19, ~italic(p)<= 0.001)",
        "list(~italic(beta)==416.31, ~italic(z)==8.012557e+17, ~italic(p)<= 0.001)",
        "list(~italic(beta)==14153.05, ~italic(z)==2.111098e+20, ~italic(p)<= 0.001)",
        "list(~italic(beta)==-1978.37, ~italic(z)==-3.807709e+18, ~italic(p)<= 0.001)"
      )
    )

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

    library(glmmTMB)

    # model
    set.seed(123)
    m1 <- glmmTMB::glmmTMB(
      formula = count ~ mined + (1 | site),
      ziformula =  ~ mined,
      family = poisson,
      data = Salamanders
    )

    # tidy dataframe
    df <-
      ggstatsplot:::ggcoefstats_label_maker(
        x = m1,
        tidy_df = broom.mixed::tidy(m1),
        glance_df = broom.mixed::glance(m1)
      )

    # checking the labels
    testthat::expect_equal(
      df$label,
      c(
        "list(~italic(beta)==0.09, ~italic(z)==0.38, ~italic(p)==0.706)",
        "list(~italic(beta)==1.14, ~italic(z)==4.64, ~italic(p)<= 0.001)",
        "list(~italic(beta)==1.14, ~italic(z)==4.85, ~italic(p)<= 0.001)",
        "list(~italic(beta)==-1.74, ~italic(z)==-6.63, ~italic(p)<= 0.001)",
        "list(~italic(beta)==0.28, ~italic(z)==NA, ~italic(p)==NA)"
      )
    )

  })
