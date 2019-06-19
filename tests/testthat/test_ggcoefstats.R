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
    testthat::skip_on_cran()
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
        exponentiate = TRUE,
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
      exponentiate = TRUE,
      conf.int = TRUE,
      conf.level = 0.90,
      effects = "fixed"
    )

    testthat::expect_equal(tidy_df$estimate, broom_df$estimate, tolerance = 1e-3)
    # testthat::expect_equal(tidy_df$std.error, broom_df$std.error, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$conf.low, broom_df$conf.low, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$conf.high, broom_df$conf.high, tolerance = 1e-3)
    testthat::expect_equal(tidy_df$p.value, broom_df$p.value, tolerance = 1e-3)
    testthat::expect_identical(
      tidy_df$statistic,
      trimws(as.character(format(broom_df$statistic, digits = 3)))
    )
    testthat::expect_identical(tidy_df$label, pb_df$label)

    # checking panel parameters
    testthat::expect_equal(pb$layout$panel_params[[1]]$y.range,
      c(0.4, 4.6),
      tolerance = 0.001
    )
  }
)

# f-statistic and partial eta- and omega-squared -----------------------------

testthat::test_that(
  desc = "ggcoefstats with partial variants of effect size for f-statistic",
  code = {
    testthat::skip_on_cran()

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

    # tests
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
      tolerance = 0.001
    )
    testthat::expect_equal(tidy_df$df1[1], 3L)
    testthat::expect_equal(tidy_df$df2[1], 35L)
    testthat::expect_equal(tidy_df$p.value,
      c(0.0005838887, 0.1626797382, 0.0148476585),
      tolerance = 0.001
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
      c(-0.1648929, 0.7565467),
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
    testthat::skip_on_cran()

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

    set.seed(123)
    mod2 <- lme4::glmer(y ~ x + (1 | f), data = d, family = poisson)

    # broom output
    set.seed(123)
    broom_df1 <- broom.mixed::tidy(
      x = mod1,
      conf.int = TRUE,
      conf.level = 0.99,
      effects = "fixed"
    )

    set.seed(123)
    broom_df2 <- broom.mixed::tidy(
      x = mod2,
      conf.int = TRUE,
      conf.level = 0.50,
      effects = "fixed"
    )

    # ggstatsplot output
    set.seed(123)
    tidy_df1 <- ggstatsplot::ggcoefstats(
      x = mod1,
      conf.int = TRUE,
      conf.level = 0.99,
      return = "tidy",
      exclude.intercept = FALSE
    )

    set.seed(123)
    tidy_df2 <- ggstatsplot::ggcoefstats(
      x = mod2,
      conf.int = TRUE,
      conf.level = 0.50,
      output = "tidy",
      exclude.intercept = FALSE
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
    # testthat::expect_equal(broom_df1$p.value, tidy_df1$p.value, tolerance = 0.001)
    testthat::expect_equal(broom_df2$p.value, tidy_df2$p.value, tolerance = 0.001)
  }
)

# check glm output ----------------------------------------------

testthat::test_that(
  desc = "check glm output",
  code = {
    testthat::skip_on_cran()

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
    testthat::skip_on_cran()

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
    testthat::skip_on_cran()

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

# check clm models (minimal) ----------------------------------------

testthat::test_that(
  desc = "check clm models (minimal)",
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
        exponentiate = TRUE,
        output = "tidy"
      )
    df.clm2 <-
      ggstatsplot::ggcoefstats(
        x = mod.clm,
        exponentiate = TRUE,
        coefficient.type = c("intercept", "alpha"),
        output = "tidy"
      )
    df.clm3 <-
      ggstatsplot::ggcoefstats(
        x = mod.clm,
        coefficient.type = c("location", "beta"),
        output = "tidy"
      )
    df.clm4 <-
      ggstatsplot::ggcoefstats(
        x = mod.clm,
        coefficient.type = NULL,
        output = "tidy"
      )

    # dimensions
    testthat::expect_equal(dim(df.clm1), c(9L, 12L))
    testthat::expect_equal(dim(df.clm2), c(6L, 12L))
    testthat::expect_equal(dim(df.clm3), c(3L, 12L))
    testthat::expect_equal(dim(df.clm4), c(9L, 12L))
  }
)

# checking bayesian models work ------------------------------------------

testthat::test_that(
  desc = "ggcoefstats works with data frames",
  code = {
    testthat::skip_on_cran()

    # setup
    library(lme4)
    suppressPackageStartupMessages(library(MCMCglmm))

    # model
    set.seed(123)
    mm0 <- MCMCglmm::MCMCglmm(
      fixed = scale(Reaction) ~ scale(Days),
      random = ~Subject,
      data = sleepstudy,
      nitt = 4000,
      pr = TRUE,
      verbose = FALSE
    )

    # output from broom.mixed
    broom_df <- broom.mixed::tidy(
      x = mm0,
      conf.int = TRUE,
      effects = "fixed"
    )

    # sticking to defaults
    df1 <- ggstatsplot::ggcoefstats(
      x = mm0,
      exclude.intercept = FALSE,
      output = "tidy"
    )

    # customizing
    df2 <- ggstatsplot::ggcoefstats(
      x = mm0,
      title = "multivariate generalized linear mixed model",
      conf.method = "HPDinterval",
      exclude.intercept = FALSE,
      robust = TRUE,
      output = "tidy"
    )

    # default
    testthat::expect_equal(df1$estimate, broom_df$estimate, tolerance = 0.001)
    testthat::expect_equal(df1$std.error, broom_df$std.error, tolerance = 0.001)
    testthat::expect_equal(df1$conf.low, broom_df$conf.low, tolerance = 0.001)
    testthat::expect_equal(df1$conf.high, broom_df$conf.high, tolerance = 0.001)

    # custom
    testthat::expect_identical(as.character(df2$term), c("(Intercept)", "scale(Days)"))
    testthat::expect_equal(df2$estimate, c(0.01964504, 0.53422489), tolerance = 0.001)
    testthat::expect_equal(df2$std.error, c(0.16551929, 0.04574515), tolerance = 0.001)
    testthat::expect_equal(df2$conf.low, c(-0.2679899, 0.4493697), tolerance = 0.001)
    testthat::expect_equal(df2$conf.high, c(0.3007959, 0.6163155), tolerance = 0.001)
  }
)

# dataframe as input ----------------------------------------------------

testthat::test_that(
  desc = "ggcoefstats works with data frames",
  code = {
    testthat::skip_on_cran()
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
    df3$p.value <- as.factor(df3$p.value)
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

    # check that term column is generated
    df8 <- tibble::tribble(
      ~statistic, ~estimate, ~conf.low, ~conf.high, ~p.value, ~df.residual,
      0.158, 0.0665, -0.778, 0.911, 0.875, 5L,
      1.33, 0.542, -0.280, 1.36, 0.191, 10L,
      1.24, 0.045, 0.030, 0.65, 0.001, 12L
    )

    testthat::expect_identical(
      colnames(ggstatsplot::ggcoefstats(df8, output = "tidy"))[[1]],
      "term"
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
      ggplot2::labs(x = "location", y = NULL)
    p5 <- ggstatsplot::ggcoefstats(x = df4, statistic = "t")
    p6 <-
      ggstatsplot::ggcoefstats(
        x = df5,
        statistic = "t",
        k = 3,
        meta.analytic.effect = TRUE,
        bf.message = TRUE,
        sample = 1000,
        messages = FALSE
      )

    # meta subtitle
    meta_subtitle <-
      ggstatsplot::subtitle_meta_ggcoefstats(
        data = df5,
        k = 3,
        messages = FALSE,
        output = "subtitle"
      )

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
    testthat::expect_identical(p4$labels$x, "location")
    testthat::expect_null(p4$labels$y, NULL)
    testthat::expect_null(p4$labels$title, NULL)
    testthat::expect_null(p4$labels$subtitle, NULL)

    # checking meta-analysis
    testthat::expect_error(ggstatsplot::ggcoefstats(
      x = df1,
      statistic = "t",
      meta.analytic.effect = TRUE
    ))

    testthat::expect_identical(pb6$plot$labels$subtitle, meta_subtitle)
    testthat::expect_identical(pb6$plot$labels$caption, ggplot2::expr(atop(
      displaystyle(atop(
        displaystyle(NULL),
        expr = paste(
          "In favor of null: ",
          "log"["e"],
          "(BF"["01"],
          ") = ",
          "0.174",
          ", ",
          italic("d")["mean"]^"posterior",
          " = ",
          "0.110",
          ", CI"["95%"],
          " [",
          "-0.175",
          ", ",
          "0.415",
          "]"
        )
      )),
      expr = paste(
        "Heterogeneity: ",
        italic("Q"),
        "(",
        "2",
        ") = ",
        "6",
        ", ",
        italic("p"),
        " = ",
        "0.058",
        ", ",
        tau["REML"]^2,
        " = ",
        "0.030",
        ", ",
        "I"^2,
        " = ",
        "81.42%"
      )
    )))
  }
)

# dataframe as input (with NAs) --------------------------------------------

testthat::test_that(
  desc = "ggcoefstats works with data frames (with NAs)",
  code = {
    testthat::skip_on_cran()
    set.seed(123)

    # creating dataframe
    df <- tibble::tribble(
      ~term, ~statistic, ~estimate, ~std.error, ~p.value,
      "level2", 0.158, 0.0665, 0.911, 0.875,
      "level1", NA, 0.542, NA, NA,
      "level3", 1.24, 0.045, 0.65, 0.001
    )

    # coefficient plot
    p <- ggstatsplot::ggcoefstats(
      x = df,
      statistic = "t",
      meta.analytic.effect = TRUE,
      bf.message = TRUE,
      messages = FALSE
    )

    # build the plot
    pb <- ggplot2::ggplot_build(p)

    # checking annotations
    testthat::expect_null(p$labels$caption, NULL)
    testthat::expect_null(p$labels$subtitle, NULL)

    # labels
    testthat::expect_identical(
      pb$data[[4]]$label,
      c(
        "list(~italic(beta)==0.07, ~italic(t)==0.16, ~italic(p)==0.875)",
        "list(~italic(beta)==0.04, ~italic(t)==1.24, ~italic(p)==0.001)"
      )
    )
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
    testthat::skip_on_cran()
    library(lme4)

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
    testthat::skip_on_cran()

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

# augment with clm works ----------------------------------------

testthat::test_that(
  desc = "augment with clm works",
  code = {
    testthat::skip_on_cran()
    testthat::skip_on_appveyor()
    testthat::skip_on_travis()

    # augment
    set.seed(123)
    mod4 <- stats::lm(formula = mpg ~ wt + qsec, data = mtcars)
    newdata <- mtcars %>%
      head(6) %>%
      dplyr::mutate(.data = ., wt = wt + 1)
    df4.broom <- tibble::as_tibble(broom::augment(x = mod4, newdata = newdata))
    df4.ggstats <-
      ggstatsplot::ggcoefstats(
        x = mod4,
        output = "augment",
        newdata = newdata
      )

    # tests
    testthat::expect_identical(df4.broom, df4.ggstats)
    testthat::expect_equal(dim(df4.ggstats), c(6L, 13L))
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
    testthat::skip_on_cran()

    # mod-1
    set.seed(123)
    mod1 <- stats::kmeans(
      x = dplyr::select(iris, -Species),
      centers = 3
    )

    # mod-2
    mod2 <- stats::aov(
      formula = value ~ attribute * measure + Error(id / (attribute * measure)),
      data = ggstatsplot::iris_long
    )

    # plot
    p <- ggstatsplot::ggcoefstats(mod2)
    pb <- ggplot2::ggplot_build(p)

    # test failures
    testthat::expect_error(ggstatsplot::ggcoefstats(mod1))
    testthat::expect_error(ggstatsplot::ggcoefstats(stats::acf(lh, plot = FALSE)))
    testthat::expect_null(pb$plot$labels$subtitle, NULL)
    testthat::expect_null(pb$plot$labels$caption, NULL)
  }
)
