context("ggcoefstats")

# t-statistic --------------------------------------------------

testthat::test_that(
  desc = "ggcoefstats with lm model",
  code = {
    set.seed(123)

    # model
    mod <- stats::lm(data = mtcars, formula = wt ~ mpg * am)

    # plot
    p <-
      ggstatsplot::ggcoefstats(
        x = mod,
        conf.level = 0.99,
        exclude.intercept = FALSE,
        k = 3
      )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # tidy dataframe from the function
    tidy_df <- p$plot_env$tidy_df

    # dataframe from `broom` package
    broom_df <- broom::tidy(
      x = mod,
      conf.int = TRUE,
      conf.level = 0.99
    )

    # testing statistical details
    testthat::expect_equal(tidy_df$estimate, broom_df$estimate, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$std.error, broom_df$std.error, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$conf.low, broom_df$conf.low, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$conf.high, broom_df$conf.high, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$p.value, broom_df$p.value, tolerance = 1e-3)
    testthat::expect_identical(tidy_df$significance, c("***", "***", "*", "ns"))
    testthat::expect_identical(
      tidy_df$p.value.formatted,
      c("< 0.001", "< 0.001", "0.014", "0.064")
    )

    # checking panel parameters
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.range,
      c(-4.292139, 8.302817),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      c("-4", "0", "4", "8")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      c(0.4, 4.6),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.labels,
      c("(Intercept)", "mpg", "am", "mpg:am")
    )

    # checking layered data
    testthat::expect_equal(dim(pb$data[[1]]), c(1L, 7L))
    testthat::expect_equal(dim(pb$data[[2]]), c(4L, 13L))
    testthat::expect_equal(dim(pb$data[[3]]), c(4L, 10L))
    testthat::expect_equal(dim(pb$data[[4]]), c(4L, 15L))

    # checking ggrepel label layer
    testthat::expect_identical(
      pb$data[[4]]$label,
      c(
        "list(~italic(beta)==6.438, ~italic(t)(28)==13.765, ~italic(p)<= 0.001)",
        "list(~italic(beta)==-0.156, ~italic(t)(28)==-5.840, ~italic(p)<= 0.001)",
        "list(~italic(beta)==-1.809, ~italic(t)(28)==-2.615, ~italic(p)==0.014)",
        "list(~italic(beta)==0.065, ~italic(t)(28)==1.932, ~italic(p)==0.064)"
      )
    )
    testthat::expect_identical(
      pb$data[[4]]$colour,
      c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")
    )
  }
)


# z-statistic --------------------------------------------------

testthat::test_that(
  desc = "ggcoefstats with glmer model",
  code = {
    library(lme4)

    # model
    set.seed(123)
    mod <-
      lme4::glmer(
        formula = cbind(incidence, size - incidence) ~ period + (1 | herd),
        data = cbpp,
        family = binomial
      )

    # plot
    set.seed(123)
    p <-
      ggstatsplot::ggcoefstats(
        x = mod,
        conf.level = 0.90,
        exclude.intercept = FALSE
      )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # tidy dataframe from the function
    tidy_df <- p$plot_env$tidy_df
    pb_df <- pb$data[[4]]

    # dataframe from `broom` package
    set.seed(123)
    broom_df <- broom.mixed::tidy(
      x = mod,
      conf.int = TRUE,
      conf.level = 0.90,
      effects = "fixed"
    )

    testthat::expect_equal(tidy_df$estimate, broom_df$estimate, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$std.error, broom_df$std.error, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$conf.low, broom_df$conf.low, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$conf.high, broom_df$conf.high, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$p.value, broom_df$p.value, tolerance = 1e-3)

    testthat::expect_identical(tidy_df$significance, c("***", "**", "***", "***"))
    testthat::expect_identical(
      tidy_df$statistic,
      trimws(as.character(format(broom_df$statistic, digits = 3)))
    )
    testthat::expect_identical(tidy_df$label, pb_df$label)

    # checking panel parameters
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.range,
      c(-2.3876522, 0.1136977),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      c("-2.0", "-1.5", "-1.0", "-0.5", "0.0")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      c(0.4, 4.6),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.labels,
      c("(Intercept)", "period2", "period3", "period4")
    )
  }
)

# f-statistic and partial eta- and omega-squared -----------------------------

testthat::test_that(
  desc = "ggcoefstats with partial variants of effect size for f-statistic",
  code = {

    ## partial eta-squared

    set.seed(123)

    # model
    mod <- stats::aov(data = mtcars, formula = wt ~ mpg * am)

    # plot
    p <-
      ggstatsplot::ggcoefstats(
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
    tidy_df <- p$plot_env$tidy_df

    testthat::expect_equal(tidy_df$estimate,
      c(0.8093822, 0.2068347, 0.1176152),
      tolerance = 1e-3
    )
    testthat::expect_equal(tidy_df$df1[1], 1L)
    testthat::expect_equal(tidy_df$df2[1], 28L)
    testthat::expect_equal(tidy_df$conf.low,
      c(0.64723930, 0.01103056, 0.00000000),
      tolerance = 1e-3
    )
    testthat::expect_equal(tidy_df$conf.high,
      c(0.8724070, 0.4306446, 0.3409322),
      tolerance = 1e-3
    )
    testthat::expect_equal(tidy_df$p.value,
      c(1.378306e-11, 1.156944e-02, 6.355055e-02),
      tolerance = 1e-5
    )

    testthat::expect_identical(p$labels$x, "partial eta-squared")
    testthat::expect_identical(p$labels$y, "effect")
    testthat::expect_identical(
      p$labels$caption,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          "AIC = ",
          "43",
          ", BIC = ",
          "50",
          ", log-likelihood = ",
          "-17"
        )
      ))
    )
    testthat::expect_null(p$labels$title, NULL)
    testthat::expect_null(p$labels$subtitle, NULL)

    testthat::expect_identical(tidy_df$significance, c("***", "*", "ns"))
    testthat::expect_identical(
      tidy_df$p.value.formatted,
      c("< 0.001", "0.012", "0.064")
    )
    testthat::expect_identical(
      tidy_df$p.value.formatted2,
      c("<= 0.001", "==0.012", "==0.064")
    )
    testthat::expect_identical(
      tidy_df$label,
      c(
        "list(~italic(F)(1*\",\"*28)==118.89, ~italic(p)<= 0.001, ~italic(eta)[p]^2==0.81)",
        "list(~italic(F)(1*\",\"*28)==7.30, ~italic(p)==0.012, ~italic(eta)[p]^2==0.21)",
        "list(~italic(F)(1*\",\"*28)==3.73, ~italic(p)==0.064, ~italic(eta)[p]^2==0.12)"
      )
    )

    # checking panel parameters
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.range,
      c(-0.04362035, 0.91602734),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      c("0.00", "0.25", "0.50", "0.75")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      c(0.4, 3.6),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.labels,
      c("mpg", "am", "mpg:am")
    )

    ## partial omega-squared

    set.seed(123)

    # model
    mod <- stats::aov(
      data = ggplot2::msleep,
      formula = sleep_rem ~ vore * brainwt,
      na.action = na.omit
    )

    # plot
    p <-
      ggstatsplot::ggcoefstats(
        x = mod,
        exclude.intercept = FALSE,
        sort = "ascending",
        effsize = "omega",
        partial = TRUE,
        title = "mammalian sleep",
        subtitle = "Source: `ggplot2` package",
        caption = substitute(paste(italic("Note"), ": From `tidyverse`")),
        package = "wesanderson",
        palette = "BottleRocket2",
        caption.summary = FALSE,
        k = 3
      )

    # built plot
    pb <- ggplot2::ggplot_build(p)

    # tidy dataframe from the function
    tidy_df <- p$plot_env$tidy_df

    testthat::expect_identical(p$labels$x, "partial omega-squared")
    testthat::expect_identical(p$labels$y, "term")
    testthat::expect_identical(
      p$labels$caption,
      ggplot2::expr(paste(italic("Note"), ": From `tidyverse`"))
    )
    testthat::expect_identical(p$labels$title, "mammalian sleep")
    testthat::expect_identical(p$labels$subtitle, "Source: `ggplot2` package")

    testthat::expect_equal(pb$data[[2]]$x, tidy_df$estimate, tolerance = 0.001)
    testthat::expect_equal(pb$data[[2]]$xmin, tidy_df$conf.low, tolerance = 0.001)
    testthat::expect_equal(pb$data[[2]]$xmax, tidy_df$conf.high, tolerance = 0.001)
    testthat::expect_equal(pb$data[[2]]$y, c(3L, 1L, 2L))

    testthat::expect_identical(tidy_df$label, pb$data[[4]]$label)

    testthat::expect_equal(tidy_df$estimate,
      c(0.30828881, 0.02348073, 0.17365008),
      tolerance = 1e-3
    )
    testthat::expect_equal(tidy_df$df1[1], 3L)
    testthat::expect_equal(tidy_df$df2[1], 35L)
    testthat::expect_equal(tidy_df$p.value,
      c(0.000584, 0.163, 0.0148),
      tolerance = 1e-3
    )

    testthat::expect_identical(
      tidy_df$label,
      c(
        "list(~italic(F)(3*\",\"*35)==7.388, ~italic(p)==0.001, ~italic(omega)[p]^2==0.308)",
        "list(~italic(F)(1*\",\"*35)==2.034, ~italic(p)==0.163, ~italic(omega)[p]^2==0.023)",
        "list(~italic(F)(3*\",\"*35)==4.012, ~italic(p)==0.015, ~italic(omega)[p]^2==0.174)"
      )
    )
    testthat::expect_identical(tidy_df$significance, c("***", "ns", "*"))
    testthat::expect_identical(
      tidy_df$p.value.formatted,
      c("0.001", "0.163", "0.015")
    )
    testthat::expect_identical(
      tidy_df$p.value.formatted2,
      c("==0.001", "==0.163", "==0.015")
    )

    # checking panel parameters
    testthat::expect_equal(pb$layout$panel_params[[1]]$x.range,
      c(-0.1754901, 0.7496432),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$x.labels,
      c("0.00", "0.25", "0.50")
    )
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      c(0.4, 3.6),
      tolerance = 0.001
    )
    testthat::expect_identical(
      pb$layout$panel_params[[1]]$y.labels,
      c("brainwt", "vore:brainwt", "vore")
    )
    testthat::expect_identical(
      pb$data[[4]]$colour,
      c("#FAD510", "#CB2314", "#273046")
    )
  }
)

# f-statistic and eta- and omega-squared -------------------------------------

testthat::test_that(
  desc = "ggcoefstats with non-partial variants of effect size for f-statistic",
  code = {

    # model
    set.seed(123)
    mod <- stats::aov(
      data = ggplot2::msleep,
      formula = sleep_rem ~ brainwt * vore
    )

    # plot
    p1 <-
      ggstatsplot::ggcoefstats(
        x = mod,
        exclude.intercept = FALSE,
        k = 2,
        ylab = "effect",
        effsize = "eta",
        partial = FALSE
      )

    p2 <-
      ggstatsplot::ggcoefstats(
        x = mod,
        exclude.intercept = FALSE,
        k = 2,
        ylab = "effect",
        effsize = "omega",
        partial = FALSE
      )

    # plot build
    pb1 <- ggplot2::ggplot_build(p1)
    pb2 <- ggplot2::ggplot_build(p2)

    # checking label
    testthat::expect_identical(
      pb1$data[[4]]$label,
      c(
        "list(~italic(F)(1*\",\"*35)==3.72, ~italic(p)==0.062, ~italic(eta)^2==0.05)",
        "list(~italic(F)(3*\",\"*35)==6.83, ~italic(p)==0.001, ~italic(eta)^2==0.29)",
        "list(~italic(F)(3*\",\"*35)==4.01, ~italic(p)==0.015, ~italic(eta)^2==0.17)"
      )
    )

    testthat::expect_identical(
      pb2$data[[4]]$label,
      c(
        "list(~italic(F)(1*\",\"*35)==3.72, ~italic(p)==0.062, ~italic(omega)^2==0.04)",
        "list(~italic(F)(3*\",\"*35)==6.83, ~italic(p)==0.001, ~italic(omega)^2==0.24)",
        "list(~italic(F)(3*\",\"*35)==4.01, ~italic(p)==0.015, ~italic(omega)^2==0.13)"
      )
    )
  }
)

# check merMod output ----------------------------------------------

testthat::test_that(
  desc = "check merMod output",
  code = {
    testthat::skip_on_cran()

    # setup
    set.seed(123)
    library(lme4)

    # model
    mod1 <-
      lme4::glmer(
        cbind(incidence, size - incidence) ~ period + (1 | herd),
        data = cbpp,
        family = binomial()
      )

    set.seed(123)
    d <- data.frame(
      y = rpois(1000, lambda = 3),
      x = runif(1000),
      f = factor(sample(
        1:10,
        size = 1000, replace = TRUE
      ))
    )
    mod2 <- lme4::glmer(y ~ x + (1 | f), data = d, family = poisson)
    mod3 <-
      lme4::lmer(
        formula = weight ~ Time * Diet + (1 + Time | Chick),
        data = ChickWeight,
        REML = FALSE,
        control = lme4::lmerControl(
          optimizer = "bobyqa",
          check.conv.grad = .makeCC("message", tol = 2e-2, relTol = NULL)
        )
      )

    # broom output
    broom_df1 <- broom.mixed::tidy(
      x = mod1,
      conf.int = TRUE,
      conf.level = 0.99,
      effects = "fixed"
    )

    broom_df2 <- broom.mixed::tidy(
      x = mod2,
      conf.int = TRUE,
      conf.level = 0.50,
      effects = "fixed"
    )

    # ggstatsplot output
    tidy_df1 <- ggstatsplot::ggcoefstats(
      x = mod1,
      conf.int = TRUE,
      conf.level = 0.99,
      output = "tidy",
      exclude.intercept = FALSE
    )

    tidy_df2 <- ggstatsplot::ggcoefstats(
      x = mod2,
      conf.int = TRUE,
      conf.level = 0.50,
      output = "tidy",
      exclude.intercept = FALSE
    )

    tidy_df3 <- ggstatsplot::ggcoefstats(
      x = mod3,
      exclude.intercept = TRUE,
      exponentiate = TRUE,
      output = "tidy"
    )

    # testing glmer
    testthat::expect_equal(broom_df1$conf.low, tidy_df1$conf.low, tolerance = 0.001)
    testthat::expect_equal(broom_df2$conf.low, tidy_df2$conf.low, tolerance = 0.001)
    testthat::expect_equal(broom_df1$conf.high, tidy_df1$conf.high, tolerance = 0.001)
    testthat::expect_equal(broom_df2$conf.high, tidy_df2$conf.high, tolerance = 0.001)
    testthat::expect_equal(broom_df1$estimate, tidy_df1$estimate, tolerance = 0.001)
    testthat::expect_equal(broom_df2$estimate, tidy_df2$estimate, tolerance = 0.001)
    testthat::expect_equal(broom_df1$std.error, tidy_df1$std.error, tolerance = 0.001)
    testthat::expect_equal(broom_df2$std.error, tidy_df2$std.error, tolerance = 0.001)
    testthat::expect_equal(broom_df1$p.value, tidy_df1$p.value, tolerance = 0.001)
    testthat::expect_equal(broom_df2$p.value, tidy_df2$p.value, tolerance = 0.001)

    # testing lmer
    testthat::expect_identical(
      tidy_df3$label,
      c(
        "list(~italic(beta)==533.71, ~italic(t)(566)==8.60, ~italic(p)<= 0.001)",
        "list(~italic(beta)==0.01, ~italic(t)(566)==-1.04, ~italic(p)==0.321)",
        "list(~italic(beta)==0.00, ~italic(t)(566)==-3.20, ~italic(p)==0.003)",
        "list(~italic(beta)==0.17, ~italic(t)(566)==-0.36, ~italic(p)==0.729)",
        "list(~italic(beta)==10.27, ~italic(t)(566)==1.86, ~italic(p)==0.080)",
        "list(~italic(beta)==171.23, ~italic(t)(566)==4.11, ~italic(p)<= 0.001)",
        "list(~italic(beta)==25.86, ~italic(t)(566)==2.60, ~italic(p)==0.016)"
      )
    )
  }
)

# check glm output ----------------------------------------------

testthat::test_that(
  desc = "check glm output",
  code = {

    # set up
    set.seed(123)

    # dataframe-1
    counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
    outcome <- gl(3, 1, 9)
    treatment <- gl(3, 3)
    d.AD <- data.frame(treatment, outcome, counts)

    # dataframe-2
    x1 <- stats::rnorm(50)
    y1 <- stats::rpois(n = 50, lambda = exp(1 + x1))
    df <- data.frame(x = x1, y = y1) %>%
      tibble::as_tibble(x = .)

    # models
    mod1 <- stats::glm(counts ~ outcome + treatment, family = poisson())
    mod2 <- stats::glm(
      formula = y ~ x,
      family = quasi(variance = "mu", link = "log"),
      data = df
    )

    # broom outputs
    broom_df1 <- broom::tidy(mod1, conf.int = 0.90)
    broom_df2 <- broom::tidy(mod2, conf.int = 0.99)

    # ggcoefstats outputs
    tidy_df1 <- ggstatsplot::ggcoefstats(
      x = mod1,
      exclude.intercept = FALSE,
      conf.int = 0.90,
      output = "tidy"
    )
    tidy_df2 <- ggstatsplot::ggcoefstats(
      x = mod2,
      exclude.intercept = FALSE,
      conf.int = 0.99,
      output = "tidy"
    )

    # testing
    testthat::expect_equal(broom_df1$conf.low, tidy_df1$conf.low, tolerance = 0.001)
    testthat::expect_equal(broom_df2$conf.low, tidy_df2$conf.low, tolerance = 0.001)
    testthat::expect_equal(broom_df1$conf.high, tidy_df1$conf.high, tolerance = 0.001)
    testthat::expect_equal(broom_df2$conf.high, tidy_df2$conf.high, tolerance = 0.001)
    testthat::expect_equal(broom_df1$estimate, tidy_df1$estimate, tolerance = 0.001)
    testthat::expect_equal(broom_df2$estimate, tidy_df2$estimate, tolerance = 0.001)
    testthat::expect_equal(broom_df1$std.error, tidy_df1$std.error, tolerance = 0.001)
    testthat::expect_equal(broom_df2$std.error, tidy_df2$std.error, tolerance = 0.001)
    testthat::expect_equal(broom_df1$p.value, tidy_df1$p.value, tolerance = 0.001)
    testthat::expect_equal(broom_df2$p.value, tidy_df2$p.value, tolerance = 0.001)
  }
)

# check lmRob output ----------------------------------------------

testthat::test_that(
  desc = "check lmRob output",
  code = {

    # set up
    set.seed(123)
    library(robust)
    data(stack.dat)

    # model
    stack.rob <- robust::lmRob(formula = Loss ~ ., data = stack.dat)

    # broom outputs
    broom_df <- broom::tidy(stack.rob)

    # ggcoefstats outputs
    tidy_df <- ggstatsplot::ggcoefstats(
      x = stack.rob,
      exclude.intercept = FALSE,
      conf.int = 0.90,
      output = "tidy"
    )

    # testing
    testthat::expect_identical(broom_df$term, as.character(tidy_df$term))
    testthat::expect_equal(broom_df$estimate, tidy_df$estimate, tolerance = 0.001)
    testthat::expect_equal(broom_df$std.error, tidy_df$std.error, tolerance = 0.001)
    testthat::expect_equal(broom_df$p.value, tidy_df$p.value, tolerance = 0.001)
  }
)

# check quantreg output ----------------------------------------------

testthat::test_that(
  desc = "check quantreg output",
  code = {
    testthat::skip_on_cran()

    # set up
    set.seed(123)
    library(quantreg)
    data(stackloss)

    # model
    mod <-
      quantreg::rq(
        formula = stack.loss ~ stack.x,
        data = stackloss,
        method = "br"
      )

    # broom outputs
    broom_df <-
      broom::tidy(
        x = mod,
        se.type = "iid",
        conf.int = TRUE,
        conf.level = 0.90
      )

    # ggcoefstats outputs
    tidy_df <- ggstatsplot::ggcoefstats(
      x = mod,
      se.type = "iid",
      exclude.intercept = FALSE,
      conf.level = 0.90,
      output = "tidy"
    )

    # testing
    testthat::expect_identical(broom_df$term, as.character(tidy_df$term))
    testthat::expect_equal(broom_df$estimate, tidy_df$estimate, tolerance = 0.001)
    testthat::expect_equal(broom_df$std.error, tidy_df$std.error, tolerance = 0.001)
    testthat::expect_equal(broom_df$p.value, tidy_df$p.value, tolerance = 0.001)
    testthat::expect_equal(broom_df$conf.low, tidy_df$conf.low, tolerance = 0.001)
    testthat::expect_equal(broom_df$conf.high, tidy_df$conf.high, tolerance = 0.001)
  }
)

# check gmm output ----------------------------------------------

testthat::test_that(
  desc = "check gmm output",
  code = {
    testthat::skip_on_cran()

    # setup
    set.seed(123)
    library(gmm)

    # examples come from the "gmm" package
    ## CAPM test with GMM
    data(Finance)
    r <- Finance[1:300, 1:10]
    rm <- Finance[1:300, "rm"]
    rf <- Finance[1:300, "rf"]

    z <- as.matrix(r - rf)
    t <- nrow(z)
    zm <- rm - rf
    h <- matrix(zm, t, 1)
    res <- gmm::gmm(z ~ zm, x = h)

    # plot
    df <- ggstatsplot::ggcoefstats(
      x = res,
      output = "tidy"
    )

    # tests
    testthat::expect_equal(dim(df), c(10L, 12L))
    testthat::expect_identical(as.character(df$term[[1]]), "WMK_zm")
  }
)

# check aareg output ----------------------------------------------

testthat::test_that(
  desc = "check aareg output",
  code = {

    # model
    library(survival)
    set.seed(123)
    afit <- survival::aareg(
      formula = Surv(time, status) ~ age + sex + ph.ecog,
      data = lung,
      dfbeta = TRUE
    )

    # broom outputs
    broom_df <- broom::tidy(afit)

    # ggcoefstats outputs
    tidy_df <- ggstatsplot::ggcoefstats(
      x = afit,
      output = "tidy",
      exclude.intercept = FALSE
    )

    # testing
    testthat::expect_identical(broom_df$term, as.character(tidy_df$term))
    testthat::expect_equal(broom_df$estimate, tidy_df$estimate, tolerance = 0.001)
    testthat::expect_equal(broom_df$std.error, tidy_df$std.error, tolerance = 0.001)
    testthat::expect_equal(broom_df$p.value, tidy_df$p.value, tolerance = 0.001)
    testthat::expect_equal(broom_df$statistic, tidy_df$coefficient, tolerance = 0.001)
    testthat::expect_identical(
      as.character(round(broom_df$statistic.z, 2)),
      tidy_df$statistic
    )
  }
)

# check clm and polr models (minimal) ----------------------------------------

testthat::test_that(
  desc = "check clm and polr models (minimal)",
  code = {
    testthat::skip_on_cran()

    # clm model
    set.seed(123)
    library(ordinal)
    mod.clm <- ordinal::clm(
      formula = as.factor(rating) ~ belief * outcome,
      data = ggstatsplot::intent_morality
    )

    # dataframes
    df.clm1 <-
      ggstatsplot::ggcoefstats(
        x = mod.clm,
        coefficient.type = "both",
        output = "tidy"
      )
    df.clm2 <-
      ggstatsplot::ggcoefstats(
        x = mod.clm,
        coefficient.type = "alpha",
        output = "tidy"
      )
    df.clm3 <-
      ggstatsplot::ggcoefstats(
        x = mod.clm,
        coefficient.type = "beta",
        output = "tidy"
      )
    df.clm4 <-
      ggstatsplot::ggcoefstats(
        x = mod.clm,
        coefficient.type = NULL,
        output = "tidy"
      )

    # tests
    testthat::expect_equal(dim(df.clm1), c(9L, 12L))
    testthat::expect_equal(dim(df.clm2), c(6L, 12L))
    testthat::expect_equal(dim(df.clm3), c(3L, 12L))
    testthat::expect_equal(dim(df.clm4), c(3L, 12L))
    testthat::expect_identical(
      unique(df.clm1$coefficient_type),
      c("alpha", "beta")
    )
    testthat::expect_identical(unique(df.clm2$coefficient_type), "alpha")
    testthat::expect_identical(unique(df.clm3$coefficient_type), "beta")
    testthat::expect_identical(
      unique(df.clm4$coefficient_type),
      unique(df.clm3$coefficient_type)
    )

    # polr model
    set.seed(123)
    library(MASS)
    mod.polr <- MASS::polr(
      formula = Sat ~ Infl + Type + Cont,
      weights = Freq,
      data = housing
    )

    # dataframes
    df.polr1 <-
      ggstatsplot::ggcoefstats(
        x = mod.polr,
        coefficient.type = "both",
        output = "tidy"
      )
    df.polr2 <-
      ggstatsplot::ggcoefstats(
        x = mod.polr,
        coefficient.type = "zeta",
        output = "tidy"
      )
    df.polr3 <-
      ggstatsplot::ggcoefstats(
        x = mod.polr,
        coefficient.type = "coefficient",
        output = "tidy"
      )
    df.polr4 <-
      ggstatsplot::ggcoefstats(
        x = mod.polr,
        coefficient.type = NULL,
        output = "tidy"
      )

    # tests
    testthat::expect_equal(dim(df.polr1), c(8L, 13L))
    testthat::expect_equal(dim(df.polr2), c(2L, 13L))
    testthat::expect_equal(dim(df.polr3), c(6L, 13L))
    testthat::expect_equal(dim(df.polr4), c(6L, 13L))
    testthat::expect_identical(
      unique(df.polr1$coefficient_type),
      c("coefficient", "zeta")
    )
    testthat::expect_identical(unique(df.polr2$coefficient_type), "zeta")
    testthat::expect_identical(unique(df.polr3$coefficient_type), "coefficient")
    testthat::expect_identical(
      unique(df.polr4$coefficient_type),
      unique(df.polr3$coefficient_type)
    )
  }
)

# check clm models (detailed) -------------------------------------------------

testthat::test_that(
  desc = "check clm models",
  code = {
    testthat::skip_on_cran()
    testthat::skip_on_appveyor()
    testthat::skip_on_travis()

    # creating broom dataframes
    set.seed(123)
    mod <- ordinal::clm(
      data = ggstatsplot::intent_morality,
      formula = as.factor(rating) ~ belief * outcome * question
    )

    # selecting alpha terms
    df1 <- broom::tidy(
      x = mod,
      conf.int = TRUE
    ) %>%
      dplyr::filter(.data = ., coefficient_type == "alpha")

    # selecting beta terms
    df2 <- broom::tidy(
      x = mod,
      conf.int = TRUE,
      exponentiate = TRUE
    ) %>%
      dplyr::filter(.data = ., coefficient_type == "beta")

    # computed dataframes
    tidy_df1 <-
      ggstatsplot::ggcoefstats(
        x = df1,
        statistic = "z",
        output = "tidy"
      )
    tidy_df2 <-
      ggstatsplot::ggcoefstats(
        x = df2,
        exponentiate = TRUE,
        statistic = "z",
        output = "tidy"
      )

    # checking confidence intervals
    testthat::expect_identical(df1$conf.low[1], NA_real_)
    testthat::expect_identical(df1$conf.high[1], NA_real_)
    testthat::expect_identical(tidy_df1$conf.low[1], NA_real_)
    testthat::expect_identical(tidy_df1$conf.high[1], NA_real_)
    testthat::expect_identical(
      tidy_df1$label,
      c(
        "list(~italic(beta)==-2.49, ~italic(z)==-27.31, ~italic(p)<= 0.001)",
        "list(~italic(beta)==-1.86, ~italic(z)==-21.29, ~italic(p)<= 0.001)",
        "list(~italic(beta)==-1.41, ~italic(z)==-16.57, ~italic(p)<= 0.001)",
        "list(~italic(beta)==-0.59, ~italic(z)==-7.21, ~italic(p)<= 0.001)",
        "list(~italic(beta)==-0.08, ~italic(z)==-0.95, ~italic(p)==0.343)",
        "list(~italic(beta)==0.61, ~italic(z)==7.39, ~italic(p)<= 0.001)"
      )
    )

    testthat::expect_identical(
      tidy_df2$label,
      c(
        "list(~italic(beta)==1.11, ~italic(z)==-19.34, ~italic(p)<= 0.001)",
        "list(~italic(beta)==1.20, ~italic(z)==-14.85, ~italic(p)<= 0.001)",
        "list(~italic(beta)==9.04, ~italic(z)==6.84, ~italic(p)<= 0.001)",
        "list(~italic(beta)==3.31, ~italic(z)==1.03, ~italic(p)==0.305)",
        "list(~italic(beta)==2.00, ~italic(z)==-2.26, ~italic(p)==0.024)",
        "list(~italic(beta)==4.02, ~italic(z)==2.04, ~italic(p)==0.041)",
        "list(~italic(beta)==1.85, ~italic(z)==-1.96, ~italic(p)==0.050)"
      )
    )

    # checking statistics
    testthat::expect_equal(df1$estimate, tidy_df1$estimate, tolerance = 0.001)
    testthat::expect_equal(df1$std.error, tidy_df1$std.error, tolerance = 0.001)
    testthat::expect_equal(df1$p.value, tidy_df1$p.value, tolerance = 0.001)
    testthat::expect_identical(
      tidy_df1$significance,
      c("***", "***", "***", "***", "ns", "***")
    )
    testthat::expect_identical(
      tidy_df1$p.value.formatted2,
      c(
        "<= 0.001",
        "<= 0.001",
        "<= 0.001",
        "<= 0.001",
        "==0.343",
        "<= 0.001"
      )
    )
  }
)

# dataframe as input ----------------------------------------------------

testthat::test_that(
  desc = "ggcoefstats works with data frames",
  code = {
    set.seed(123)

    # creating dataframe
    df1 <- tibble::tribble(
      ~term, ~statistic, ~estimate, ~conf.low, ~conf.high, ~p.value, ~df.residual,
      "level2", 0.158, 0.0665, -0.778, 0.911, 0.875, 5L,
      "level1", 1.33, 0.542, -0.280, 1.36, 0.191, 10L,
      "level3", 1.24, 0.045, 0.030, 0.65, 0.001, 12L
    )
    df2 <- dplyr::select(.data = df1, -p.value)
    df3 <- dplyr::select(.data = df1, -statistic)
    df4 <- dplyr::select(.data = df1, -df.residual)
    df5 <- tibble::add_column(df1, std.error = c(0.015, 0.2, 0.09))
    df6 <- dplyr::select(.data = df5, -term, -estimate, -std.error)

    # repeated term dataframe
    df7 <- tibble::tribble(
      ~term, ~statistic, ~estimate, ~conf.low, ~conf.high, ~p.value, ~df.residual,
      "x", 0.158, 0.0665, -0.778, 0.911, 0.875, 5L,
      "x", 1.33, 0.542, -0.280, 1.36, 0.191, 10L,
      "x", 1.24, 0.045, 0.030, 0.65, 0.001, 12L
    )

    # expect errors
    testthat::expect_message(ggstatsplot::ggcoefstats(x = df1))
    testthat::expect_error(ggstatsplot::ggcoefstats(
      x = df6,
      meta.analytic.effect = TRUE
    ))
    testthat::expect_message(ggstatsplot::ggcoefstats(x = df7))

    # plotting the dataframe
    p1 <- ggstatsplot::ggcoefstats(x = df1, statistic = "t", sort = "none")
    p2 <- ggstatsplot::ggcoefstats(x = df1, statistic = "z", sort = "descending")
    p3 <- ggstatsplot::ggcoefstats(x = df2, statistic = "t")
    p4 <- ggstatsplot::ggcoefstats(x = df3, statistic = "t") +
      ggplot2::scale_y_discrete(labels = c("x1", "x2", "x3")) +
      ggplot2::labs(x = "beta", y = NULL)
    p5 <- ggstatsplot::ggcoefstats(x = df4, statistic = "t")
    p6 <-
      ggstatsplot::ggcoefstats(
        x = df5,
        statistic = "t",
        k = 3,
        meta.analytic.effect = TRUE,
        messages = FALSE
      )

    # meta subtitle
    meta_subtitle <-
      ggstatsplot::subtitle_meta_ggcoefstats(data = df5, k = 3, messages = FALSE)

    # build plots
    pb1 <- ggplot2::ggplot_build(p1)
    pb2 <- ggplot2::ggplot_build(p2)
    pb3 <- ggplot2::ggplot_build(p3)
    pb4 <- ggplot2::ggplot_build(p4)
    pb5 <- ggplot2::ggplot_build(p5)
    pb6 <- ggplot2::ggplot_build(p6)

    # stats labels
    testthat::expect_identical(
      pb1$data[[4]]$label,
      c(
        "list(~italic(beta)==0.07, ~italic(t)(5)==0.16, ~italic(p)==0.875)",
        "list(~italic(beta)==0.54, ~italic(t)(10)==1.33, ~italic(p)==0.191)",
        "list(~italic(beta)==0.04, ~italic(t)(12)==1.24, ~italic(p)==0.001)"
      )
    )
    testthat::expect_identical(
      pb5$data[[4]]$label,
      c(
        "list(~italic(beta)==0.07, ~italic(t)==0.16, ~italic(p)==0.875)",
        "list(~italic(beta)==0.54, ~italic(t)==1.33, ~italic(p)==0.191)",
        "list(~italic(beta)==0.04, ~italic(t)==1.24, ~italic(p)==0.001)"
      )
    )

    testthat::expect_equal(pb2$data[[3]]$y, c(2L, 1L, 3L))
    testthat::expect_identical(
      pb2$data[[4]]$label,
      c(
        "list(~italic(beta)==0.07, ~italic(z)==0.16, ~italic(p)==0.875)",
        "list(~italic(beta)==0.54, ~italic(z)==1.33, ~italic(p)==0.191)",
        "list(~italic(beta)==0.04, ~italic(z)==1.24, ~italic(p)==0.001)"
      )
    )

    # checking number of data layers
    testthat::expect_equal(length(pb1$data), 4L)
    testthat::expect_equal(length(pb2$data), 4L)
    testthat::expect_equal(length(pb3$data), 3L)
    testthat::expect_equal(length(pb4$data), 3L)

    # confidence intervals used for each layer should be the same as df
    testthat::expect_equal(pb3$data[[2]]$xmin, df1$conf.low)
    testthat::expect_equal(pb3$data[[2]]$xmax, df2$conf.high)
    testthat::expect_equal(pb2$data[[2]]$xmin, df3$conf.low)
    testthat::expect_equal(pb2$data[[2]]$xmax, df3$conf.high)

    # checking labels (order)
    testthat::expect_identical(
      pb1$layout$panel_params[[1]]$y.labels,
      c("level2", "level1", "level3")
    )

    testthat::expect_identical(
      pb2$layout$panel_params[[1]]$y.labels,
      c("level1", "level2", "level3")
    )

    testthat::expect_identical(
      pb4$layout$panel_params[[1]]$y.labels,
      c("x1", "x2", "x3")
    )

    # annotations
    testthat::expect_identical(p4$labels$x, "beta")
    testthat::expect_null(p4$labels$y, NULL)
    testthat::expect_null(p4$labels$title, NULL)
    testthat::expect_null(p4$labels$subtitle, NULL)
    testthat::expect_null(p4$labels$caption, NULL)

    # checking meta-analysis
    testthat::expect_error(ggstatsplot::ggcoefstats(
      x = df1,
      statistic = "t",
      meta.analytic.effect = TRUE
    ))

    testthat::expect_identical(pb6$plot$labels$subtitle, meta_subtitle)
  }
)

# check confidence intervals ----------------------------------------------

testthat::test_that(
  desc = "check computing confidence intervals",
  code = {
    testthat::skip_on_cran()

    # creating broom dataframes
    set.seed(123)
    mod <- stats::lm(data = iris, formula = Sepal.Length ~ Species)
    df1 <- broom::tidy(
      x = mod,
      conf.int = TRUE,
      conf.level = 0.95
    )
    df2 <- broom::tidy(
      x = mod,
      conf.int = TRUE,
      conf.level = 0.50
    )

    # computed dataframes
    tidy_df1 <-
      ggstatsplot::ggcoefstats(
        x = dplyr::select(df1, -conf.low, -conf.high),
        exclude.intercept = FALSE,
        statistic = "t",
        output = "tidy"
      )
    tidy_df2 <-
      ggstatsplot::ggcoefstats(
        x = dplyr::select(df2, -conf.low, -conf.high),
        exclude.intercept = FALSE,
        statistic = "t",
        output = "tidy",
        conf.level = 0.50
      )
    tidy_df3 <-
      ggstatsplot::ggcoefstats(
        x = dplyr::select(df2, -conf.low, -conf.high, -std.error),
        exclude.intercept = FALSE,
        statistic = "t",
        output = "tidy",
        conf.level = 0.50
      )

    # checking confidence intervals
    testthat::expect_equal(df1$conf.low, tidy_df1$conf.low, tolerance = 0.001)
    testthat::expect_equal(df2$conf.high, tidy_df2$conf.high, tolerance = 0.001)
    testthat::expect_identical(tidy_df3$conf.low[1], NA_character_)
    testthat::expect_identical(tidy_df3$conf.high[1], NA_character_)
    testthat::expect_true(inherits(tidy_df1, what = "tbl_df"))
    testthat::expect_true(inherits(tidy_df2, what = "tbl_df"))
  }
)

# check if glance works ----------------------------------------------

testthat::test_that(
  desc = "check if glance works",
  code = {

    # creating broom and ggstatsplot output
    # lm
    set.seed(123)
    mod1 <- stats::lm(data = iris, formula = Sepal.Length ~ Species)
    broom_df1 <- broom::glance(mod1)
    glance_df1 <- ggstatsplot::ggcoefstats(x = mod1, output = "glance")

    # lmer
    set.seed(123)
    mod2 <-
      lme4::lmer(
        formula = Reaction ~ Days + (Days | Subject),
        data = sleepstudy
      )
    broom_df2 <- broom.mixed::glance(x = mod2)
    glance_df2 <- ggstatsplot::ggcoefstats(x = mod2, output = "glance")

    # checking if they are equal
    testthat::expect_identical(broom_df1, glance_df1)
    testthat::expect_identical(broom_df2, glance_df2)

    testthat::expect_true(inherits(glance_df1, what = "tbl_df"))
    testthat::expect_true(inherits(glance_df2, what = "tbl_df"))
  }
)


# check if augment works ----------------------------------------------

testthat::test_that(
  desc = "check if augment works",
  code = {
    # set up
    library(lme4)

    # linear model
    set.seed(123)
    mod1 <- stats::lm(
      formula = mpg ~ wt + qsec,
      data = mtcars
    )
    df1.broom <- broom::augment(mod1)
    df1.ggstats <- ggstatsplot::ggcoefstats(x = mod1, output = "augment")

    # mixed-effects model
    set.seed(123)
    mod2 <- lme4::lmer(
      formula = Reaction ~ Days + (Days | Subject),
      data = sleepstudy
    )
    df2.broom <- tibble::as_tibble(broom.mixed::augment(mod2))
    df2.ggstats <- ggstatsplot::ggcoefstats(x = mod2, output = "augment")

    # model with F-statistic
    set.seed(123)
    mod3 <- stats::aov(
      data = ggplot2::msleep,
      formula = sleep_rem ~ brainwt * vore
    )
    df3.broom <- tibble::as_tibble(broom::augment(mod3))
    df3.ggstats <- ggstatsplot::ggcoefstats(x = mod3, output = "augment")

    # checking if they are equal
    testthat::expect_identical(df1.broom, df1.ggstats)
    testthat::expect_identical(df2.broom, df2.ggstats)
    testthat::expect_identical(df3.broom, df3.ggstats)
    testthat::expect_true(inherits(df1.ggstats, what = "tbl_df"))
    testthat::expect_true(inherits(df2.ggstats, what = "tbl_df"))
    testthat::expect_true(inherits(df3.ggstats, what = "tbl_df"))
  }
)

# check if p-value adjustment works ----------------------------------------

testthat::test_that(
  desc = "check if p-value adjustment works",
  code = {
    testthat::skip_on_cran()

    set.seed(123)

    # model
    mod <- stats::aov(
      data = mtcars,
      formula = wt ~ am * cyl * carb
    )

    # tidy output
    df <-
      ggstatsplot::ggcoefstats(
        x = mod,
        output = "tidy",
        p.adjust.method = "holm"
      )

    # checking adjusted p-values
    testthat::expect_identical(
      df$p.value.formatted,
      c("< 0.001", "< 0.001", "0.200", "0.570", "0.570", "0.570", "0.570")
    )
  }
)

# testing aesthetic modifications --------------------------------------------

testthat::test_that(
  desc = "testing aesthetic modifications",
  code = {
    testthat::skip_on_cran()

    # model
    set.seed(123)
    mod <-
      stats::lm(
        formula = rating ~ belief * outcome * question * item,
        data = ggstatsplot::intent_morality
      )

    # plot
    p <-
      ggstatsplot::ggcoefstats(
        x = mod,
        exclude.intercept = FALSE,
        point.shape = 5,
        point.color = "red",
        point.size = 6
      )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # checking layered data
    testthat::expect_identical(unique(pb$data[[4]]$colour), "black")
    testthat::expect_identical(unique(pb$data[[3]]$colour), "red")
    testthat::expect_equal(unique(pb$data[[3]]$shape), 5L)
    testthat::expect_equal(unique(pb$data[[3]]$size), 6L)
  }
)

# unsupported model objects -----------------------------------------------

testthat::test_that(
  desc = "unsupported model objects",
  code = {

    # mod-1
    set.seed(123)
    mod1 <- stats::kmeans(
      x = dplyr::select(iris, -Species),
      centers = 3
    )

    # mod-2
    set.seed(123)
    library(survival)
    cfit <- coxph(Surv(time, status) ~ age + sex, lung)
    mod2 <- survfit(cfit)

    # test failures
    testthat::expect_error(
      ggstatsplot::ggcoefstats(x = mod1)
    )
    testthat::expect_error(
      ggstatsplot::ggcoefstats(x = mod2)
    )
  }
)
