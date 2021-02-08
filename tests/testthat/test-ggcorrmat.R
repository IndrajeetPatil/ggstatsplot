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

    # checking data used to create a plot
    dat <- tibble::as_tibble(p$data) %>%
      dplyr::mutate_if(
        .tbl = .,
        .predicate = is.factor,
        .funs = ~ as.character(.)
      )

    # checking dimensions of data
    data_dims <- dim(dat)

    # testing everything is okay with data
    expect_equal(data_dims, c(16L, 7L))
    expect_equal(dim(pb$data[[1]]), c(16L, 15L))
    expect_equal(length(pb$data), 3L)
    expect_equal(
      dat$coef,
      c(
        1.0000,
        -0.1176,
        0.8718,
        0.8179,
        -0.1176,
        1.0000,
        -0.4284,
        -0.3661,
        0.8718,
        -0.4284,
        1.0000,
        0.9629,
        0.8179,
        -0.3661,
        0.9629,
        1.0000
      ),
      tolerance = 1e-3
    )
    expect_equal(dat$Var1[3], "Petal.Length")
    expect_equal(dat$Var2[4], "Sepal.Length")
    expect_equal(dat$signif[1], 1L)
    expect_equal(dat$signif[2], 0L)
    expect_equal(dat$signif[4], 1L)
    expect_equal(dat$signif[5], 0L)
    expect_identical(
      unclass(p$plot_env$colors),
      c("#1B9E77FF", "#D95F02FF", "#7570B3FF")
    )

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

    # checking layer data
    expect_equal(dim(pb$data[[1]]), c(16L, 15L))
    expect_equal(dim(pb$data[[2]]), c(16L, 15L))
    expect_equal(dim(pb$data[[3]]), c(2L, 10L))

    # check if number of cells is equal to number of correlations
    expect_equal(
      length(unique(dat$Var1))^2,
      max(pb$data[[1]]$group)
    )

    # checking unique number of correlations
    expect_equal(length(unique(pb$data[[1]]$fill)), 7L)

    # checking if aesthetic modifications worked
    expect_equal(pb$data[[3]]$shape[1], "cross")
    expect_equal(pb$data[[3]]$size[1], 14L)
    expect_identical(pb$data[[2]]$colour[1], "white")
    expect_identical(pb$data[[3]]$colour[1], "white")
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
    p_legend_title <- pb$plot$plot_env$legend.title

    # geom data
    expect_equal(
      pb$data,
      list(
        structure(list(
          fill = c(
            "#D0E9DD", "#FFFEFD", "#FFF9F1",
            "#E69F00", "#DEF0E7", "#FFFEFD", "#DCEFE6", "#F5FAF8", "#71BE9E",
            "#BFE1D2", "#C3E3D5", "#F5FAF8", "#FFFDFA", "#FFE7C9", "#A7D6C1"
          ), x = structure(c(
            1L, 1L, 2L, 1L, 2L, 3L, 1L, 2L, 3L, 4L, 1L,
            2L, 3L, 4L, 5L
          ), class = c("mapped_discrete", "numeric")), y = structure(c(
            1L,
            2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L
          ), class = c(
            "mapped_discrete",
            "numeric"
          )), PANEL = structure(c(
            1L, 1L, 1L, 1L, 1L, 1L, 1L,
            1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
          ), class = "factor", .Label = "1"),
          group = structure(c(
            1L, 2L, 6L, 3L, 7L, 10L, 4L, 8L, 11L,
            13L, 5L, 9L, 12L, 14L, 15L
          ), n = 15L), xmin = structure(c(
            0.5,
            0.5, 1.5, 0.5, 1.5, 2.5, 0.5, 1.5, 2.5, 3.5, 0.5, 1.5, 2.5,
            3.5, 4.5
          ), class = c("mapped_discrete", "numeric")), xmax = structure(c(
            1.5,
            1.5, 2.5, 1.5, 2.5, 3.5, 1.5, 2.5, 3.5, 4.5, 1.5, 2.5, 3.5,
            4.5, 5.5
          ), class = c("mapped_discrete", "numeric")), ymin = structure(c(
            0.5,
            1.5, 1.5, 2.5, 2.5, 2.5, 3.5, 3.5, 3.5, 3.5, 4.5, 4.5, 4.5,
            4.5, 4.5
          ), class = c("mapped_discrete", "numeric")), ymax = structure(c(
            1.5,
            2.5, 2.5, 3.5, 3.5, 3.5, 4.5, 4.5, 4.5, 4.5, 5.5, 5.5, 5.5,
            5.5, 5.5
          ), class = c("mapped_discrete", "numeric")), colour = c(
            "black",
            "black", "black", "black", "black", "black", "black", "black",
            "black", "black", "black", "black", "black", "black", "black"
          ), size = c(
            0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
            0.1, 0.1, 0.1, 0.1, 0.1, 0.1
          ), linetype = c(
            1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1
          ), alpha = c(
            NA, NA, NA, NA,
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
          ), width = c(
            NA,
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
          ),
          height = c(
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
            NA, NA, NA
          )
        ), row.names = c(NA, -15L), class = "data.frame"),
        structure(list(
          fill = c(
            "#D0E9DD", "#FFFEFD", "#FFF9F1",
            "#E69F00", "#DEF0E7", "#FFFEFD", "#DCEFE6", "#F5FAF8", "#71BE9E",
            "#BFE1D2", "#C3E3D5", "#F5FAF8", "#FFFDFA", "#FFE7C9", "#A7D6C1"
          ), x = structure(c(
            1L, 1L, 2L, 1L, 2L, 3L, 1L, 2L, 3L, 4L,
            1L, 2L, 3L, 4L, 5L
          ), class = c("mapped_discrete", "numeric")), y = structure(c(
            1L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 4L,
            5L, 5L, 5L, 5L, 5L
          ), class = c("mapped_discrete", "numeric")), PANEL = structure(c(
            1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
            1L, 1L, 1L, 1L, 1L, 1L
          ), class = "factor", .Label = "1"),
          group = structure(c(
            1L, 2L, 6L, 3L, 7L, 10L, 4L, 8L,
            11L, 13L, 5L, 9L, 12L, 14L, 15L
          ), n = 15L), colour = c(
            "black",
            "black", "black", "black", "black", "black", "black",
            "black", "black", "black", "black", "black", "black",
            "black", "black"
          ), size = c(
            4, 4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4
          ), angle = c(
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0
          ), hjust = c(
            0.5, 0.5, 0.5, 0.5, 0.5,
            0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5
          ), vjust = c(
            0.5,
            0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
            0.5, 0.5, 0.5
          ), alpha = c(
            NA, NA, NA, NA, NA, NA, NA,
            NA, NA, NA, NA, NA, NA, NA, NA
          ), family = c(
            "", "", "",
            "", "", "", "", "", "", "", "", "", "", "", ""
          ), fontface = c(
            1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
          ), lineheight = c(
            1.2,
            1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2,
            1.2, 1.2, 1.2
          ), label = c(
            0.23, -0.01, -0.06, -1, 0.16,
            -0.01, 0.17, 0.05, 0.68, 0.31, 0.29, 0.05, -0.02, -0.24,
            0.43
          )
        ), row.names = c(NA, -15L), class = "data.frame"),
        structure(list(
          fill = c(
            "#1FA278", "#1FA278", "#1FA278",
            "#1FA278", "#1FA278", "#1FA278", "#1FA278", "#1FA278", "#1FA278",
            "#1FA278", "#1FA278", "#1FA278", "#D2EADF"
          ), x = structure(c(
            1L,
            1L, 2L, 2L, 3L, 1L, 2L, 4L, 1L, 2L, 3L, 4L, 5L
          ), class = c(
            "mapped_discrete",
            "numeric"
          )), y = structure(c(
            1L, 2L, 2L, 3L, 3L, 4L, 4L,
            4L, 5L, 5L, 5L, 5L, 5L
          ), class = c("mapped_discrete", "numeric")), PANEL = structure(c(
            1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
            1L, 1L, 1L, 1L
          ), class = "factor", .Label = "1"), group = structure(c(
            1L,
            2L, 5L, 6L, 9L, 3L, 7L, 11L, 4L, 8L, 10L, 12L, 13L
          ), n = 13L),
          shape = c(
            "cross", "cross", "cross", "cross", "cross",
            "cross", "cross", "cross", "cross", "cross", "cross",
            "cross", "cross"
          ), colour = c(
            "black", "black", "black",
            "black", "black", "black", "black", "black", "black",
            "black", "black", "black", "black"
          ), size = c(
            5, 5, 5,
            5, 5, 5, 5, 5, 5, 5, 5, 5, 5
          ), alpha = c(
            NA, NA, NA,
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
          ), stroke = c(
            0.5,
            0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
            0.5
          )
        ), row.names = c(NA, -13L), class = "data.frame")
      ),
      tolerance = 0.001
    )

    # checking plot labels
    expect_identical(
      p_legend_title,
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
    p_legend_title <- pb$plot$plot_env$legend.title

    # checking data used to create a plot
    dat <- tibble::as_tibble(p$data) %>%
      dplyr::mutate_if(
        .tbl = .,
        .predicate = is.factor,
        .funs = ~ as.character(.)
      )

    # checking dimensions of data
    data_dims <- dim(dat)

    # testing everything is okay with data
    expect_equal(data_dims, c(16L, 7L))
    expect_equal(dat$coef[2], 0.76, tolerance = 1e-3)
    expect_equal(dat$coef[7], -0.33, tolerance = 1e-3)
    expect_equal(dat$coef[9], -0.49, tolerance = 1e-3)
    expect_equal(dat$coef[14], -0.76, tolerance = 1e-3)
    expect_equal(dat$Var1[15], "sleep_cycle")
    expect_equal(dat$Var2[16], "awake")
    expect_equal(dat$signif[1], 1L)
    expect_equal(dat$signif[7], 0L)
    expect_equal(dat$signif[10], 0L)
    expect_equal(dat$signif[11], 1L)

    # checking aesthetics
    expect_identical(
      unique(pb$data[[1]]$fill),
      c("#0B775E", "#57896B", "#E6BE81", "#E1BD6D", "#E7BE87", "#E3BD77", "#8E9C79")
    )

    expect_identical(
      p_legend_title,
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

    # checking geom data
    expect_equal(
      pb$data,
      list(
        structure(list(
          fill = c(
            "#65B997", "#FDD49E", "#FFE0B9",
            "#E69F00", "#F4B85B", "#A4D5BF", "#FFDDB2", "#FFEBD2", "#4FB08B",
            "#B9DECD", "#FFE1BB", "#FFDFB7", "#AFDAC6", "#C1E2D3", "#2DA57C"
          ), x = structure(c(
            1L, 1L, 2L, 1L, 2L, 3L, 1L, 2L, 3L, 4L, 1L,
            2L, 3L, 4L, 5L
          ), class = c("mapped_discrete", "numeric")), y = structure(c(
            1L,
            2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L
          ), class = c(
            "mapped_discrete",
            "numeric"
          )), PANEL = structure(c(
            1L, 1L, 1L, 1L, 1L, 1L, 1L,
            1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L
          ), class = "factor", .Label = "1"),
          group = structure(c(
            1L, 2L, 6L, 3L, 7L, 10L, 4L, 8L, 11L,
            13L, 5L, 9L, 12L, 14L, 15L
          ), n = 15L), xmin = structure(c(
            0.5,
            0.5, 1.5, 0.5, 1.5, 2.5, 0.5, 1.5, 2.5, 3.5, 0.5, 1.5, 2.5,
            3.5, 4.5
          ), class = c("mapped_discrete", "numeric")), xmax = structure(c(
            1.5,
            1.5, 2.5, 1.5, 2.5, 3.5, 1.5, 2.5, 3.5, 4.5, 1.5, 2.5, 3.5,
            4.5, 5.5
          ), class = c("mapped_discrete", "numeric")), ymin = structure(c(
            0.5,
            1.5, 1.5, 2.5, 2.5, 2.5, 3.5, 3.5, 3.5, 3.5, 4.5, 4.5, 4.5,
            4.5, 4.5
          ), class = c("mapped_discrete", "numeric")), ymax = structure(c(
            1.5,
            2.5, 2.5, 3.5, 3.5, 3.5, 4.5, 4.5, 4.5, 4.5, 5.5, 5.5, 5.5,
            5.5, 5.5
          ), class = c("mapped_discrete", "numeric")), colour = c(
            "black",
            "black", "black", "black", "black", "black", "black", "black",
            "black", "black", "black", "black", "black", "black", "black"
          ), size = c(
            0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
            0.1, 0.1, 0.1, 0.1, 0.1, 0.1
          ), linetype = c(
            1, 1, 1, 1, 1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1
          ), alpha = c(
            NA, NA, NA, NA,
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
          ), width = c(
            NA,
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
          ),
          height = c(
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
            NA, NA, NA
          )
        ), row.names = c(NA, -15L), class = "data.frame"),
        structure(list(
          fill = c(
            "#65B997", "#FDD49E", "#FFE0B9",
            "#E69F00", "#F4B85B", "#A4D5BF", "#FFDDB2", "#FFEBD2", "#4FB08B",
            "#B9DECD", "#FFE1BB", "#FFDFB7", "#AFDAC6", "#C1E2D3", "#2DA57C"
          ), x = structure(c(
            1L, 1L, 2L, 1L, 2L, 3L, 1L, 2L, 3L, 4L,
            1L, 2L, 3L, 4L, 5L
          ), class = c("mapped_discrete", "numeric")), y = structure(c(
            1L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 4L,
            5L, 5L, 5L, 5L, 5L
          ), class = c("mapped_discrete", "numeric")), PANEL = structure(c(
            1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
            1L, 1L, 1L, 1L, 1L, 1L
          ), class = "factor", .Label = "1"),
          group = structure(c(
            1L, 2L, 6L, 3L, 7L, 10L, 4L, 8L,
            11L, 13L, 5L, 9L, 12L, 14L, 15L
          ), n = 15L), colour = c(
            "black",
            "black", "black", "black", "black", "black", "black",
            "black", "black", "black", "black", "black", "black",
            "black", "black"
          ), size = c(
            4, 4, 4, 4, 4, 4, 4, 4, 4,
            4, 4, 4, 4, 4, 4
          ), angle = c(
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0
          ), hjust = c(
            0.5, 0.5, 0.5, 0.5, 0.5,
            0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5
          ), vjust = c(
            0.5,
            0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
            0.5, 0.5, 0.5
          ), alpha = c(
            NA, NA, NA, NA, NA, NA, NA,
            NA, NA, NA, NA, NA, NA, NA, NA
          ), family = c(
            "", "", "",
            "", "", "", "", "", "", "", "", "", "", "", ""
          ), fontface = c(
            1,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
          ), lineheight = c(
            1.2,
            1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2,
            1.2, 1.2, 1.2
          ), label = c(
            0.73, -0.43, -0.31, -1, -0.73,
            0.44, -0.34, -0.2, 0.82, 0.34, -0.3, -0.32, 0.39, 0.3,
            0.93
          )
        ), row.names = c(NA, -15L), class = "data.frame"),
        structure(list(), class = "data.frame", row.names = integer(0))
      ),
      tolerance = 0.001
    )
  }
)

# checking all dataframe outputs -------------------------------------------

test_that(
  desc = "checking ggcorrmat - with NAs - spearman's rho",
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
    )),
    cran = FALSE
    )
  }
)
