# pearson's r without NAs ------------------------------------------------

test_that(
  desc = "checking ggcorrmat - without NAs - pearson's r",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggcorrmat(
        data = iris,
        cor.vars.names = "x",
        type = "p",
        title = "Iris dataset",
        subtitle = "By Edgar Anderson",
        ggstatsplot.layer = FALSE,
        sig.level = 0.001,
        matrix.type = "full",
        p.adjust.method = "fdr",
        colors = NULL,
        k = 4,
        ggcorrplot.args = list(
          lab_col = "white",
          pch.col = "white"
        )
      )

    # checking legend title
    pb <- ggplot2::ggplot_build(p)
    p_legend_title <- pb$plot$plot_env$legend.title

    # check data
    set.seed(123)
    expect_snapshot(pb$data)
    expect_snapshot(p$labels)
  }
)

# robust r without NAs ---------------------------------------------------

test_that(
  desc = "checking ggcorrmat - without NAs - robust r",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggcorrmat(
        data = anscombe,
        type = "r",
        partial = TRUE,
        cor.vars.names = names(anscombe)
      )

    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data)
    expect_snapshot(p$labels)
  }
)

# robust r with NAs ---------------------------------------------------

test_that(
  desc = "checking ggcorrmat - with NAs - robust r - partial",
  code = {
    skip_on_cran()
    skip_on_ci()

    # creating the plot
    set.seed(123)
    p <-
      ggcorrmat(
        data = ggplot2::msleep,
        type = "r",
        sig.level = 0.01,
        partial = TRUE,
        p.adjust.method = "hommel",
        matrix.type = "upper"
      ) +
      labs(caption = NULL)

    # checking legend title
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data)
    expect_snapshot(p$labels)
  }
)

# spearman's rho with NAs ---------------------------------------------------

test_that(
  desc = "checking ggcorrmat - with NAs - spearman's rho",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      suppressWarnings(ggcorrmat(
        data = ggplot2::msleep,
        cor.vars = sleep_total:awake,
        cor.vars.names = "sleep_total",
        type = "np",
        sig.level = 0.01,
        matrix.type = "full",
        p.adjust.method = "hommel",
        caption.default = FALSE,
        colors = NULL,
        package = "wesanderson",
        palette = "Rushmore1"
      ))

    # checking legend title
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data)
    expect_snapshot(p$labels)
  }
)

# Bayesian pearson (with NA) ---------------------------------------------------

test_that(
  desc = "checking Bayesian pearson (with NA)",
  code = {
    skip_on_cran()

    set.seed(123)
    p <- suppressWarnings(ggcorrmat(ggplot2::msleep, type = "bayes"))
    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data)
    expect_snapshot(p$labels)
  }
)

# checking all dataframe outputs -------------------------------------------

test_that(
  desc = "checking all dataframe outputs",
  code = {
    skip_on_cran()
    skip_on_ci()
    skip_on_appveyor()
    skip_on_travis()
    skip_on_covr()

    set.seed(123)
    expect_snapshot(suppressWarnings(purrr::pmap_dfr(
      .l = list(
        data = list(ggplot2::msleep),
        type = list("p", "p", "np", "np", "r", "r", "bf", "bayes"),
        output = list("dataframe"),
        partial = list(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
      ),
      .f = ggcorrmat
    )))
  }
)
