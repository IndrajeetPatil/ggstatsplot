# pearson's r without NAs ------------------------------------------------

test_that(
  desc = "checking ggcorrmat - without NAs - pearson's r",
  code = {
    skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggcorrmat(
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
          pch.col = "white",
          pch.cex = 14
        )
      )

    # checking legend title
    pb <- ggplot2::ggplot_build(p)
    p_legend_title <- pb$plot$plot_env$legend.title

    # check data
    set.seed(123)
    expect_snapshot(pb$data)

    # checking layers
    expect_equal(pb$plot$layers[[3]]$aes_params$shape, "cross")
    expect_equal(pb$plot$layers[[3]]$aes_params$size, 14L)
    expect_identical(pb$plot$layers[[3]]$aes_params$colour, "white")

    # checking plot labels
    expect_identical(p$labels$title, "Iris dataset")
    expect_identical(p$labels$subtitle, "By Edgar Anderson")
    expect_identical(
      p_legend_title,
      ggplot2::expr(atop(
        atop(scriptstyle(bold("sample sizes:")), italic(n) ~
        "=" ~ "150"),
        atop(
          scriptstyle(bold("correlation:")),
          "Pearson"
        )
      ))
    )
    expect_identical(
      p$labels$caption,
      ggplot2::expr(atop(
        displaystyle(NULL),
        expr = paste(
          bold("X"),
          " = non-significant at ",
          italic("p"),
          " < ",
          0.001,
          " (Adjustment: ",
          "FDR",
          ")"
        )
      ))
    )
    expect_null(p$labels$xlab, NULL)
    expect_null(p$labels$ylab, NULL)
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
      ggstatsplot::ggcorrmat(
        data = anscombe,
        type = "r",
        partial = TRUE,
        cor.vars.names = names(anscombe)
      )

    pb <- ggplot2::ggplot_build(p)

    # check data
    set.seed(123)
    expect_snapshot(pb$data)

    expect_identical(
      pb$plot$plot_env$legend.title,
      ggplot2::expr(
        atop(atop(scriptstyle(bold("sample sizes:")), italic(n) ~
        "=" ~ "11"), atop(
          scriptstyle(bold("correlation (partial):")),
          "Pearson (Winsorized)"
        ))
      )
    )
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
      ggstatsplot::ggcorrmat(
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

    # checking plot labels
    expect_identical(
      pb$plot$plot_env$legend.title,
      ggplot2::expr(atop(atop(scriptstyle(bold("sample sizes:")), italic(n) ~
      "=" ~ "30"), atop(
        scriptstyle(bold("correlation (partial):")),
        "Pearson (Winsorized)"
      )))
    )
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
      suppressWarnings(ggstatsplot::ggcorrmat(
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

    expect_identical(
      pb$plot$plot_env$legend.title,
      ggplot2::expr(atop(atop(
        atop(scriptstyle(bold("sample sizes:")), italic(n)[min] ~
        "=" ~ "32"),
        atop(
          italic(n)[mode] ~ "=" ~ "32",
          italic(n)[max] ~ "=" ~ "83"
        )
      ), atop(
        scriptstyle(
          bold("correlation:")
        ),
        "Spearman"
      )))
    )
  }
)

# Bayesian pearson (with NA) ---------------------------------------------------

test_that(
  desc = "checking Bayesian pearson (with NA)",
  code = {
    skip_on_cran()

    set.seed(123)
    p <- suppressWarnings(ggcorrmat(ggplot2::msleep, type = "bf"))
    pb <- ggplot2::ggplot_build(p)

    # check legend
    expect_identical(
      p$plot_env$legend.title,
      ggplot2::expr(atop(atop(atop(scriptstyle(bold("sample sizes:")), italic(n)[min] ~
      "=" ~ "30"), atop(italic(n)[mode] ~ "=" ~
      "32", italic(n)[max] ~ "=" ~ "83")), atop(
        scriptstyle(bold("correlation:")),
        "Pearson (Bayesian)"
      )))
    )

    # check data
    set.seed(123)
    expect_snapshot(pb$data)
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
