# pearson's r without NAs ------------------------------------------------

testthat::test_that(
  desc = "checking ggcorrmat - without NAs - pearson's r",
  code = {
    testthat::skip_on_cran()

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggcorrmat(
        data = iris,
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
    testthat::expect_equal(data_dims, c(16L, 7L))
    testthat::expect_equal(dim(pb$data[[1]]), c(16L, 15L))
    testthat::expect_equal(length(pb$data), 3L)
    testthat::expect_equal(
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
    testthat::expect_equal(dat$Var1[3], "Petal.Length")
    testthat::expect_equal(dat$Var2[4], "Sepal.Length")
    testthat::expect_equal(dat$signif[1], 1L)
    testthat::expect_equal(dat$signif[2], 0L)
    testthat::expect_equal(dat$signif[4], 1L)
    testthat::expect_equal(dat$signif[5], 0L)
    testthat::expect_identical(
      unclass(p$plot_env$colors),
      c("#1B9E77FF", "#D95F02FF", "#7570B3FF")
    )

    # checking layers
    testthat::expect_equal(pb$plot$layers[[3]]$aes_params$shape, "cross")
    testthat::expect_equal(pb$plot$layers[[3]]$aes_params$size, 14L)
    testthat::expect_identical(pb$plot$layers[[3]]$aes_params$colour, "white")

    # checking plot labels
    testthat::expect_identical(p$labels$title, "Iris dataset")
    testthat::expect_identical(p$labels$subtitle, "By Edgar Anderson")
    testthat::expect_identical(
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
    testthat::expect_identical(
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
    testthat::expect_null(p$labels$xlab, NULL)
    testthat::expect_null(p$labels$ylab, NULL)

    # checking layer data
    testthat::expect_equal(dim(pb$data[[1]]), c(16L, 15L))
    testthat::expect_equal(dim(pb$data[[2]]), c(16L, 15L))
    testthat::expect_equal(dim(pb$data[[3]]), c(2L, 10L))

    # check if number of cells is equal to number of correlations
    testthat::expect_equal(
      length(unique(dat$Var1))^2,
      max(pb$data[[1]]$group)
    )

    # checking unique number of correlations
    testthat::expect_equal(length(unique(pb$data[[1]]$fill)), 7L)

    # checking if aesthetic modifications worked
    testthat::expect_equal(pb$data[[3]]$shape[1], "cross")
    testthat::expect_equal(pb$data[[3]]$size[1], 14L)
    testthat::expect_identical(pb$data[[2]]$colour[1], "white")
    testthat::expect_identical(pb$data[[3]]$colour[1], "white")
  }
)

# robust r with NAs ---------------------------------------------------

testthat::test_that(
  desc = "checking ggcorrmat - with NAs - robust r",
  code = {
    testthat::skip_on_cran()

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
    testthat::expect_equal(
      pb$data,
      list(
        structure(list(
          fill = c(
            "#CAE6D9", "#FFFCF8", "#FFF9F1",
            "#E69F00", "#EFF7F3", "#FFFDFA", "#EAF5F0", "#EFF7F3", "#73BF9F",
            "#E0F1E9", "#E8F5EF", "#FDFEFE", "#FBFDFC", "#FFEED8", "#B1DBC8"
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
            "#CAE6D9", "#FFFCF8", "#FFF9F1",
            "#E69F00", "#EFF7F3", "#FFFDFA", "#EAF5F0", "#EFF7F3", "#73BF9F",
            "#E0F1E9", "#E8F5EF", "#FDFEFE", "#FBFDFC", "#FFEED8", "#B1DBC8"
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
            0.26, -0.03, -0.06, -1, 0.08,
            -0.02, 0.1, 0.08, 0.67, 0.15, 0.11, 0.01, 0.02, -0.17,
            0.38
          )
        ), row.names = c(NA, -15L), class = "data.frame"),
        structure(list(
          fill = c(
            "#27A47A", "#27A47A", "#27A47A",
            "#27A47A", "#27A47A", "#27A47A", "#27A47A", "#27A47A", "#27A47A",
            "#27A47A", "#27A47A", "#27A47A", "#93CDB4"
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
    testthat::expect_identical(
      p_legend_title,
      ggplot2::expr(atop(atop(scriptstyle(bold("sample sizes:")), italic(n) ~
      "=" ~ "30"), atop(
        scriptstyle(bold("correlation (partial):")),
        "robust (% bend)"
      )))
    )
  }
)

# spearman's rho with NAs ---------------------------------------------------

testthat::test_that(
  desc = "checking ggcorrmat - with NAs - spearman's rho",
  code = {
    testthat::skip_on_cran()

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
    testthat::expect_equal(data_dims, c(16L, 7L))
    testthat::expect_equal(dat$coef[2], 0.76, tolerance = 1e-3)
    testthat::expect_equal(dat$coef[7], -0.33, tolerance = 1e-3)
    testthat::expect_equal(dat$coef[9], -0.49, tolerance = 1e-3)
    testthat::expect_equal(dat$coef[14], -0.76, tolerance = 1e-3)
    testthat::expect_equal(dat$Var1[15], "sleep_cycle")
    testthat::expect_equal(dat$Var2[16], "awake")
    testthat::expect_equal(dat$signif[1], 1L)
    testthat::expect_equal(dat$signif[7], 0L)
    testthat::expect_equal(dat$signif[10], 0L)
    testthat::expect_equal(dat$signif[11], 1L)

    # checking aesthetics
    testthat::expect_identical(
      unique(pb$data[[1]]$fill),
      c("#0B775E", "#57896B", "#E6BE81", "#E1BD6D", "#E7BE87", "#E3BD77", "#8E9C79")
    )

    testthat::expect_identical(
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

testthat::test_that(
  desc = "checking Bayesian pearson (with NA)",
  code = {
    testthat::skip_on_cran()

    set.seed(123)
    p <- suppressWarnings(ggcorrmat(ggplot2::msleep, type = "bf"))
    pb <- ggplot2::ggplot_build(p)

    # check legend
    testthat::expect_identical(
      p$plot_env$legend.title,
      ggplot2::expr(atop(atop(atop(scriptstyle(bold("sample sizes:")), italic(n)[min] ~
      "=" ~ "30"), atop(italic(n)[mode] ~ "=" ~
      "32", italic(n)[max] ~ "=" ~ "83")), atop(
        scriptstyle(bold("correlation:")),
        "Pearson (Bayesian)"
      )))
    )

    # checking geom data
    testthat::expect_equal(
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

testthat::test_that(
  desc = "checking ggcorrmat - with NAs - spearman's rho",
  code = {
    testthat::skip_on_cran()
    testthat::skip_on_ci()

    set.seed(123)
    df <-
      suppressWarnings(purrr::pmap_dfr(
        .l = list(
          data = list(ggplot2::msleep),
          type = list("p", "p", "np", "np", "r", "r", "bf", "bayes"),
          output = list("dataframe"),
          partial = list(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
        ),
        .f = ggcorrmat
      ))

    testthat::expect_equal(
      df,
      structure(list(
        parameter1 = c(
          "sleep_total", "sleep_total", "sleep_total",
          "sleep_total", "sleep_total", "sleep_rem", "sleep_rem", "sleep_rem",
          "sleep_rem", "sleep_cycle", "sleep_cycle", "sleep_cycle", "awake",
          "awake", "brainwt", "sleep_total", "sleep_total", "sleep_total",
          "sleep_total", "sleep_total", "sleep_rem", "sleep_rem", "sleep_rem",
          "sleep_rem", "sleep_cycle", "sleep_cycle", "sleep_cycle", "awake",
          "awake", "brainwt", "sleep_total", "sleep_total", "sleep_total",
          "sleep_total", "sleep_total", "sleep_rem", "sleep_rem", "sleep_rem",
          "sleep_rem", "sleep_cycle", "sleep_cycle", "sleep_cycle", "awake",
          "awake", "brainwt", "sleep_total", "sleep_total", "sleep_total",
          "sleep_total", "sleep_total", "sleep_rem", "sleep_rem", "sleep_rem",
          "sleep_rem", "sleep_cycle", "sleep_cycle", "sleep_cycle", "awake",
          "awake", "brainwt", "sleep_total", "sleep_total", "sleep_total",
          "sleep_total", "sleep_total", "sleep_rem", "sleep_rem", "sleep_rem",
          "sleep_rem", "sleep_cycle", "sleep_cycle", "sleep_cycle", "awake",
          "awake", "brainwt", "sleep_total", "sleep_total", "sleep_total",
          "sleep_total", "sleep_total", "sleep_rem", "sleep_rem", "sleep_rem",
          "sleep_rem", "sleep_cycle", "sleep_cycle", "sleep_cycle", "awake",
          "awake", "brainwt", "sleep_total", "sleep_total", "sleep_total",
          "sleep_total", "sleep_total", "sleep_rem", "sleep_rem", "sleep_rem",
          "sleep_rem", "sleep_cycle", "sleep_cycle", "sleep_cycle", "awake",
          "awake", "brainwt", "sleep_total", "sleep_total", "sleep_total",
          "sleep_total", "sleep_total", "sleep_rem", "sleep_rem", "sleep_rem",
          "sleep_rem", "sleep_cycle", "sleep_cycle", "sleep_cycle", "awake",
          "awake", "brainwt"
        ), parameter2 = c(
          "sleep_rem", "sleep_cycle",
          "awake", "brainwt", "bodywt", "sleep_cycle", "awake", "brainwt",
          "bodywt", "awake", "brainwt", "bodywt", "brainwt", "bodywt",
          "bodywt", "sleep_rem", "sleep_cycle", "awake", "brainwt", "bodywt",
          "sleep_cycle", "awake", "brainwt", "bodywt", "awake", "brainwt",
          "bodywt", "brainwt", "bodywt", "bodywt", "sleep_rem", "sleep_cycle",
          "awake", "brainwt", "bodywt", "sleep_cycle", "awake", "brainwt",
          "bodywt", "awake", "brainwt", "bodywt", "brainwt", "bodywt",
          "bodywt", "sleep_rem", "sleep_cycle", "awake", "brainwt", "bodywt",
          "sleep_cycle", "awake", "brainwt", "bodywt", "awake", "brainwt",
          "bodywt", "brainwt", "bodywt", "bodywt", "sleep_rem", "sleep_cycle",
          "awake", "brainwt", "bodywt", "sleep_cycle", "awake", "brainwt",
          "bodywt", "awake", "brainwt", "bodywt", "brainwt", "bodywt",
          "bodywt", "sleep_rem", "sleep_cycle", "awake", "brainwt", "bodywt",
          "sleep_cycle", "awake", "brainwt", "bodywt", "awake", "brainwt",
          "bodywt", "brainwt", "bodywt", "bodywt", "sleep_rem", "sleep_cycle",
          "awake", "brainwt", "bodywt", "sleep_cycle", "awake", "brainwt",
          "bodywt", "awake", "brainwt", "bodywt", "brainwt", "bodywt",
          "bodywt", "sleep_rem", "sleep_cycle", "awake", "brainwt", "bodywt",
          "sleep_cycle", "awake", "brainwt", "bodywt", "awake", "brainwt",
          "bodywt", "brainwt", "bodywt", "bodywt"
        ), estimate = c(
          0.314198776569291,
          -0.0224751311427816, -1, -0.0970478079779942, -0.178630569276781,
          -0.0766002920838023, 0.0559650710918182, 0.0857321849455498,
          -0.0340923680283615, -0.00478591672843335, 0.801009895839906,
          -0.0949315225691833, -0.0956962403850673, -0.447684495624958,
          0.251687191889523, 0.751754999228714, -0.473712677698895, -0.9999985737041,
          -0.360487369330295, -0.312010598447784, -0.338123478333359, -0.751771275094598,
          -0.221334750192969, -0.327650730899231, 0.473712677698895, 0.851620348196375,
          0.417802887787044, 0.360487369330295, 0.311980149735026, 0.933782200562096,
          0.280533926585095, 0.0380422691879867, -1, 0.152836484983315,
          0.282758620689655, 0.0478309232480534, 0.175528364849833, -0.118576195773081,
          0.0745272525027809, 0.0104560622914349, 0.659176863181313, -0.025139043381535,
          0.247608453837597, -0.146607341490545, 0.407786429365962, 0.76414395786588,
          -0.488795003673769, -1, -0.593544885868695, -0.534601663421502,
          -0.33440549100742, -0.76414395786588, -0.413902053018504, -0.451708771406872,
          0.488795003673769, 0.872689336172418, 0.846422531288955, 0.593544885868695,
          0.534601663421502, 0.957157750288055, 0.261366405780764, -0.0342869377978439,
          -1, 0.0995856154368192, 0.11320032359815, -0.0565022940756734,
          0.077160353453414, 0.0797046787890234, 0.0133968737216177, -0.0170594230500481,
          0.671406155456209, 0.0172365945609237, 0.153418242541828, -0.172955785777577,
          0.376852001378535, 0.766715614960821, -0.524038139034508, -0.999999167843595,
          -0.569621956005504, -0.530375450657497, -0.410512529569298, -0.766868861491995,
          -0.395604332407832, -0.422636186907729, 0.524038139034508, 0.879121059223659,
          0.731648736168645, 0.569657504182276, 0.530343474131462, 0.867995206609151,
          0.279024814404289, -0.0181238788859881, -1, -0.0818154778196596,
          -0.162698048653251, -0.0665642379253442, 0.050532371737122, 0.0810867075891976,
          -0.0190351860377995, -0.00602878516289918, 0.763510730630958,
          -0.0864552161321, -0.085385990930527, -0.406737787396641, 0.229097288591127,
          0.734019132039742, -0.437164964457016, -0.999998461147014, -0.343026776463413,
          -0.301567179861619, -0.308197388762117, -0.732931553021478, -0.210184357588808,
          -0.306247619814812, 0.435575977276605, 0.822618903922315, 0.38656307053656,
          0.341326225717577, 0.297581166254415, 0.926373253093894
        ), conf.low = c(
          -0.0519511118191786,
          -0.379670127498996, -1, -0.441867855251125, -0.506315790409592,
          -0.425137121315912, -0.310565934884675, -0.283286823612558, -0.389576642422975,
          -0.364426787914686, 0.619521780874344, -0.440147308512428, -0.440769319161024,
          -0.695739947439588, -0.119409505394878, 0.616675570444065, -0.705818942495566,
          -0.999999079814697, -0.569422416796032, -0.494426316043698, -0.614380941351003,
          -0.843842787488684, -0.475561892358216, -0.535303943436836, 0.149755420997765,
          0.708828703697856, 0.0808939861910069, 0.107803638857133, 0.1032378053581,
          0.889164228839033, -0.0997519110374109, -0.336628836810346, -1,
          -0.230107326044402, -0.0973590095511379, -0.327904801811441,
          -0.207905424979848, -0.467981712729762, -0.30378174209606, -0.360873503352983,
          0.382521768758968, -0.391432496646883, -0.134659908942176, -0.489967638240629,
          0.0445774196767684, 0.629830720985725, -0.720744046845329, -1,
          -0.744411618060543, -0.676251959042727, -0.618453045398219, -0.854085686114354,
          -0.629822904637602, -0.636226998686945, 0.158414555405773, 0.742444263614606,
          0.700705021223084, 0.385020492859459, 0.354828879571721, 0.926584029289573,
          -0.109183812661312, -0.389741862952635, -1, -0.270384357712284,
          -0.257573410455534, -0.408456951621766, -0.291203900943824, -0.288859176262895,
          -0.348554641325675, -0.375023755241191, 0.410409690976692, -0.345176107640743,
          -0.218952917456446, -0.501948286160214, 0.0191878826405035, 0.638094999577311,
          -0.73789697725292, -0.999999463128224, -0.724087543149073, -0.669478839854689,
          -0.664139427708968, -0.853807801730548, -0.611053600878612, -0.609580186349078,
          0.214548363869887, 0.759354926964299, 0.514117090736725, 0.360790069921279,
          0.355296313407861, 0.784000846851608, 0.0202045068358928, -0.305886590074508,
          -1, -0.35202856783261, -0.425326067109479, -0.334768219338722,
          -0.212281119732702, -0.235255089368236, -0.295835000425703, -0.27761733765919,
          0.636997906814114, -0.350659204530818, -0.348829806514656, -0.629905364849293,
          -0.0340626556647824, 0.64141468296768, -0.646812429691077, -0.999998967235305,
          -0.525295189977712, -0.451890984518497, -0.5473860613241, -0.825398973222598,
          -0.411355697416063, -0.479511926598534, 0.211232337251529, 0.732523250180857,
          0.141880148377052, 0.157428313216245, 0.143976604525615, 0.894206123545546
        ), conf.high = c(
          0.605884236830885, 0.340551570904811, -1, 0.2727579428572,
          0.194132039415142, 0.291719432242205, 0.408007838637766, 0.432638641617158,
          0.330232917566714, 0.35609729541138, 0.901208949516927, 0.274733846438329,
          0.274020215545229, -0.104225482755788, 0.561080368493776, 0.843832010384061,
          -0.149755420997765, -0.999997789228207, -0.107803638857133, -0.103271177969349,
          0.0119833478677744, -0.616698764772103, 0.0670144092080866, -0.0826493257114811,
          0.705818942495565, 0.927362936398182, 0.669029118204181, 0.569422416796032,
          0.494400829468226, 0.960811381236503, 0.589309675979673, 0.402314144174387,
          -1, 0.494794790849554, 0.590884631161417, 0.410501095082924,
          0.512202754204274, 0.262889215284755, 0.43253586648149, 0.378924054218277,
          0.827357898402062, 0.348031259435335, 0.565722676184822, 0.236133020624429,
          0.67577656259569, 0.854085686114354, -0.158414555405773, -1,
          -0.385020492859459, -0.354828879571721, 0.0269290508083673, -0.629830720985725,
          -0.138601201906293, -0.2183100672115, 0.720744046845329, 0.939362030951896,
          0.924350415109458, 0.744411618060543, 0.676251959042727, 0.975163333951022,
          0.568138423691469, 0.33005935232951, -1, 0.443927761072774, 0.454916857342805,
          0.310078906385302, 0.425598573857868, 0.427692646037055, 0.371871266609985,
          0.345332206132106, 0.830732103583673, 0.375176043512928, 0.4867821073104,
          0.199760652198862, 0.649006759065075, 0.853706971908529, -0.214548363869887,
          -0.999998710149691, -0.360744294951411, -0.355335187458447, -0.0721457338319264,
          -0.638315440781038, -0.1255861332639, -0.191160370255085, 0.73789697725292,
          0.941270761094715, 0.860747508577743, 0.724112575362384, 0.669454289070243,
          0.920777602875286, 0.549741741482282, 0.253821812442424, -1,
          0.191693647091165, 0.121023646310055, 0.221576454156952, 0.328306887374677,
          0.325837926418885, 0.265498019934224, 0.278942291059749, 0.870561315244025,
          0.187336056811877, 0.204503844391491, -0.146106143658878, 0.483871657553288,
          0.830622340313033, -0.203047603845407, -0.999997871970694, -0.159916103173755,
          -0.140042686562347, -0.0588939169580219, -0.638234440672769,
          0.0256519037466994, -0.115409672982653, 0.655039342347737, 0.911562069500045,
          0.610590373661106, 0.541747682896723, 0.452455494715481, 0.953566255310192
        ), statistic = c(
          1.7512729847532, -0.118957263701502, -Inf, -0.515964225503251,
          -0.960675421889671, -0.406525065290985, 0.296604180082872, 0.455328501505056,
          -0.180504784728478, -0.0253249809537887, 7.08016058335592, -0.504609314659997,
          -0.508711596362414, -2.64923504058615, 1.37610203630925, 8.7563959665808,
          -2.94616978356295, -5328.71177213626, -2.83997889229681, -2.95564527332603,
          -1.96788344413517, -8.75683193760191, -1.53934429618568, -2.66377635085373,
          2.94616978356295, 8.59729638965135, 2.5187734859585, 2.83997889229681,
          2.95532573279422, 19.1757041195636, 3234, 4324, 8990, 3808, 3224,
          4280, 3706, 5028, 4160, 4448, 1532, 4608, 3382, 5154, 2662, 8920.07551351241,
          8122.86554004409, 190568, 46627.123360518, 146222.984897454,
          7280.51635893648, 66719.9244864876, 26049.7314248129, 54903.6257346079,
          2789.13445995591, 572.261433904979, 837.918669287463, 11892.876639482,
          44345.0151025456, 1253.56422657151, 1.43282655937649, -0.181536159060464,
          -251098376.703608, 0.529590139294394, 0.6028749779929, -0.299460434486292,
          0.409515101699341, 0.423103611985806, 0.0708959547809431, -0.0902831200308109,
          4.79395136489097, 0.0912210372161867, 0.821538949254626, -0.929199410558207,
          2.15283432559795, 9.17348077613296, -3.37007470794261, -6976.29377188071,
          -5.09285151745203, -5.63055924343947, -2.46581949020715, -9.17793143142042,
          -2.92144778875188, -3.58196204149372, 3.37007470794261, 9.76056586803812,
          5.87870316081028, 5.09332202433417, 5.63008692462593, 12.8449221712689,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
        ), df = c(
          28,
          28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 59, 30,
          81, 54, 81, 30, 59, 46, 59, 30, 28, 30, 54, 81, 54, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 28, 28, 28, 28, 28,
          28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 59, 30, 81, 54, 81, 30,
          59, 46, 59, 30, 28, 30, 54, 81, 54, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA
        ), p.value = c(
          1, 1, 0, 1, 1, 1, 1,
          1, 1, 1, 1.47937232398697e-06, 1, 1, 0.17044644526461, 1, 3.78380968819173e-11,
          0.0493483700873962, 3.62778457567608e-225, 0.0493483700873962,
          0.0408533174269959, 0.116770875757661, 3.78380968819173e-11,
          0.13057159546892, 0.0493483700873962, 0.0493483700873962, 2.66236158675713e-08,
          0.0520221097892513, 0.0493483700873962, 0.0408533174269959, 1.28175628644446e-24,
          1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0.00104299439393255, 1, 1, 1, 0.328828474358261,
          1.01471612029873e-11, 0.0138047898269776, 0, 9.9833621795509e-06,
          1.7380681480841e-06, 0.0613889761872692, 1.01471612029873e-11,
          0.0138047898269776, 0.00129019441264808, 0.0138047898269776,
          3.57451758992576e-09, 1.03950744535194e-08, 9.9833621795509e-06,
          1.7380681480841e-06, 1.35712192788287e-29, 1, 1, 2.60110925620787e-215,
          1, 1, 1, 1, 1, 1, 1, 0.000682010330889073, 1, 1, 1, 0.521226291864583,
          7.51235473445048e-12, 0.00832373294771808, 1.20908595584487e-234,
          3.22128738371129e-05, 2.50905386465852e-06, 0.0196088620726675,
          7.51235473445048e-12, 0.0107680358577365, 0.00345529728316837,
          0.00832373294771808, 1.80277593673956e-09, 1.56633965141154e-05,
          3.22128738371129e-05, 2.50905386465852e-06, 6.63601270365033e-17,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
        ), method = c(
          "Pearson",
          "Pearson", "Pearson", "Pearson", "Pearson", "Pearson", "Pearson",
          "Pearson", "Pearson", "Pearson", "Pearson", "Pearson", "Pearson",
          "Pearson", "Pearson", "Pearson", "Pearson", "Pearson", "Pearson",
          "Pearson", "Pearson", "Pearson", "Pearson", "Pearson", "Pearson",
          "Pearson", "Pearson", "Pearson", "Pearson", "Pearson", "Spearman",
          "Spearman", "Spearman", "Spearman", "Spearman", "Spearman", "Spearman",
          "Spearman", "Spearman", "Spearman", "Spearman", "Spearman", "Spearman",
          "Spearman", "Spearman", "Spearman", "Spearman", "Spearman", "Spearman",
          "Spearman", "Spearman", "Spearman", "Spearman", "Spearman", "Spearman",
          "Spearman", "Spearman", "Spearman", "Spearman", "Spearman", "Percentage Bend",
          "Percentage Bend", "Percentage Bend", "Percentage Bend", "Percentage Bend",
          "Percentage Bend", "Percentage Bend", "Percentage Bend", "Percentage Bend",
          "Percentage Bend", "Percentage Bend", "Percentage Bend", "Percentage Bend",
          "Percentage Bend", "Percentage Bend", "Percentage Bend", "Percentage Bend",
          "Percentage Bend", "Percentage Bend", "Percentage Bend", "Percentage Bend",
          "Percentage Bend", "Percentage Bend", "Percentage Bend", "Percentage Bend",
          "Percentage Bend", "Percentage Bend", "Percentage Bend", "Percentage Bend",
          "Percentage Bend", "Bayesian Pearson", "Bayesian Pearson", "Bayesian Pearson",
          "Bayesian Pearson", "Bayesian Pearson", "Bayesian Pearson", "Bayesian Pearson",
          "Bayesian Pearson", "Bayesian Pearson", "Bayesian Pearson", "Bayesian Pearson",
          "Bayesian Pearson", "Bayesian Pearson", "Bayesian Pearson", "Bayesian Pearson",
          "Bayesian Pearson", "Bayesian Pearson", "Bayesian Pearson", "Bayesian Pearson",
          "Bayesian Pearson", "Bayesian Pearson", "Bayesian Pearson", "Bayesian Pearson",
          "Bayesian Pearson", "Bayesian Pearson", "Bayesian Pearson", "Bayesian Pearson",
          "Bayesian Pearson", "Bayesian Pearson", "Bayesian Pearson"
        ),
        n.obs = c(
          30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L,
          30L, 30L, 30L, 30L, 30L, 61L, 32L, 83L, 56L, 83L, 32L, 61L,
          48L, 61L, 32L, 30L, 32L, 56L, 83L, 56L, 30L, 30L, 30L, 30L,
          30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 61L,
          32L, 83L, 56L, 83L, 32L, 61L, 48L, 61L, 32L, 30L, 32L, 56L,
          83L, 56L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L,
          30L, 30L, 30L, 30L, 30L, 61L, 32L, 83L, 56L, 83L, 32L, 61L,
          48L, 61L, 32L, 30L, 32L, 56L, 83L, 56L, 30L, 30L, 30L, 30L,
          30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 30L, 61L,
          32L, 83L, 56L, 83L, 32L, 61L, 48L, 61L, 32L, 30L, 32L, 56L,
          83L, 56L
        ), pd = c(
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, 0.93975, 0.543, 1, 0.678, 0.81775, 0.64275,
          0.611, 0.66775, 0.5435, 0.516, 1, 0.69075, 0.6895, 0.99075,
          0.904, 1, 0.996, 1, 0.996, 0.99725, 0.96975, 1, 0.931, 0.99075,
          0.996, 1, 0.98775, 0.996, 0.9985, 1
        ), rope.percentage = c(
          NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.13275,
          0.41825, 0, 0.3895, 0.294, 0.404, 0.411, 0.3805, 0.4245,
          0.4225, 0, 0.393, 0.3895, 0.033, 0.20625, 0, 0.0185, 0, 0.021,
          0.02875, 0.10025, 0, 0.20775, 0.04525, 0.01825, 0, 0.0395,
          0.0345, 0.028, 0
        ), prior.distribution = c(
          NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "cauchy", "cauchy",
          "cauchy", "cauchy", "cauchy", "cauchy", "cauchy", "cauchy",
          "cauchy", "cauchy", "cauchy", "cauchy", "cauchy", "cauchy",
          "cauchy", "cauchy", "cauchy", "cauchy", "cauchy", "cauchy",
          "cauchy", "cauchy", "cauchy", "cauchy", "cauchy", "cauchy",
          "cauchy", "cauchy", "cauchy", "cauchy"
        ), prior.location = c(
          NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        ), prior.scale = c(
          NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.707, 0.707,
          0.707, 0.707, 0.707, 0.707, 0.707, 0.707, 0.707, 0.707, 0.707,
          0.707, 0.707, 0.707, 0.707, 0.707, 0.707, 0.707, 0.707, 0.707,
          0.707, 0.707, 0.707, 0.707, 0.707, 0.707, 0.707, 0.707, 0.707,
          0.707
        ), bayes.factor = c(
          NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, NA, NA, 1.04220951845262, 0.277295951280714,
          NA, 0.311006952321971, 0.417342347547666, 0.29707641543189,
          0.286802609245689, 0.302808920409366, 0.279641863942762,
          0.275587993303019, 131029.026702124, 0.309377887446125, 0.30996134456914,
          4.81565605879391, 0.636828508733277, 3000790805.91831, 8.85477763270537,
          NA, 7.29209737517349, 9.27539818814149, 1.42437942505901,
          3005546543.91408, 0.653929649077368, 4.80166659272304, 8.85477763270537,
          3802376.31789373, 3.75738626630023, 7.29209737517349, 9.26775528548353,
          1.57719988795645e+22
        )
      ),
      row.names = c(NA, -120L), coefficient_name = "r", ci = 0.95, data = structure(list(
        sleep_total = c(
          12.1, 17, 14.4, 14.9, 4, 14.4, 8.7, 7, 10.1,
          3, 5.3, 9.4, 10, 12.5, 10.3, 8.3, 9.1, 17.4, 5.3, 18, 3.9,
          19.7, 2.9, 3.1, 10.1, 10.9, 14.9, 12.5, 9.8, 1.9, 2.7, 6.2,
          6.3, 8, 9.5, 3.3, 19.4, 10.1, 14.2, 14.3, 12.8, 12.5, 19.9,
          14.6, 11, 7.7, 14.5, 8.4, 3.8, 9.7, 15.8, 10.4, 13.5, 9.4,
          10.3, 11, 11.5, 13.7, 3.5, 5.6, 11.1, 18.1, 5.4, 13, 8.7,
          9.6, 8.4, 11.3, 10.6, 16.6, 13.8, 15.9, 12.8, 9.1, 8.6, 15.8,
          4.4, 15.6, 8.9, 5.2, 6.3, 12.5, 9.8
        ), sleep_rem = c(
          NA, 1.8,
          2.4, 2.3, 0.7, 2.2, 1.4, NA, 2.9, NA, 0.6, 0.8, 0.7, 1.5,
          2.2, 2, 1.4, 3.1, 0.5, 4.9, NA, 3.9, 0.6, 0.4, 3.5, 1.1,
          NA, 3.2, 1.1, 0.4, 0.1, 1.5, 0.6, 1.9, 0.9, NA, 6.6, 1.2,
          1.9, 3.1, NA, 1.4, 2, NA, NA, 0.9, NA, 0.9, 0.6, 1.4, NA,
          NA, NA, 1, 2.7, NA, NA, 1.8, 0.4, NA, 1.5, 6.1, 0.5, 2.4,
          NA, 1.4, 2.1, 1.1, 2.4, NA, 3.4, 3, 2, 2.4, NA, NA, 1, 2.3,
          2.6, NA, 1.3, NA, 2.4
        ), sleep_cycle = c(
          NA, NA, NA, 0.133333333,
          0.666666667, 0.766666667, 0.383333333, NA, 0.333333333, NA,
          NA, 0.216666667, NA, 0.116666667, NA, NA, 0.15, 0.383333333,
          NA, 0.333333333, NA, 0.116666667, 1, NA, 0.283333333, NA,
          NA, 0.416666667, 0.55, NA, NA, NA, NA, 1.5, NA, NA, NA, 0.75,
          NA, 0.2, NA, 0.183333333, 0.2, NA, NA, NA, NA, 0.416666667,
          NA, 1.416666667, NA, NA, NA, 0.666666667, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, 0.183333333, NA, NA, 0.166666667, 0.15,
          NA, NA, 0.216666667, NA, 0.183333333, 0.5, NA, NA, 0.9, NA,
          0.233333333, NA, NA, NA, 0.35
        ), awake = c(
          11.9, 7, 9.6, 9.1,
          20, 9.6, 15.3, 17, 13.9, 21, 18.7, 14.6, 14, 11.5, 13.7,
          15.7, 14.9, 6.6, 18.7, 6, 20.1, 4.3, 21.1, 20.9, 13.9, 13.1,
          9.1, 11.5, 14.2, 22.1, 21.35, 17.8, 17.7, 16, 14.5, 20.7,
          4.6, 13.9, 9.8, 9.7, 11.2, 11.5, 4.1, 9.4, 13, 16.3, 9.5,
          15.6, 20.2, 14.3, 8.2, 13.6, 10.5, 14.6, 13.7, 13, 12.5,
          10.3, 20.5, 18.45, 12.9, 5.9, 18.6, 11, 15.3, 14.4, 15.6,
          12.7, 13.4, 7.4, 10.2, 8.1, 11.2, 14.9, 15.4, 8.2, 19.6,
          8.4, 15.1, 18.8, 17.7, 11.5, 14.2
        ), brainwt = c(
          NA, 0.0155,
          NA, 0.00029, 0.423, NA, NA, NA, 0.07, 0.0982, 0.115, 0.0055,
          NA, 0.0064, 0.001, 0.0066, 0.00014, 0.0108, 0.0123, 0.0063,
          4.603, 3e-04, 0.655, 0.419, 0.0035, 0.115, NA, 0.0256, 0.005,
          NA, NA, 0.325, 0.01227, 1.32, NA, 5.712, NA, 0.179, NA, 0.001,
          NA, 4e-04, 0.00025, NA, 0.0125, NA, NA, 0.0121, 0.175, 0.44,
          NA, 0.157, NA, 0.18, 0.0024, NA, NA, 0.0114, NA, NA, NA,
          0.081, 0.021, 0.0019, NA, 0.02, 0.0012, 0.00118, 0.003, 0.0057,
          0.004, NA, 0.00033, 0.18, 0.025, NA, 0.169, 0.0026, 0.0025,
          NA, 0.0175, 0.0445, 0.0504
        ), bodywt = c(
          50, 0.48, 1.35, 0.019,
          600, 3.85, 20.49, 0.045, 14, 14.8, 33.5, 0.728, 4.75, 0.42,
          0.06, 1, 0.005, 3.5, 2.95, 1.7, 2547, 0.023, 521, 187, 0.77,
          10, 0.071, 3.3, 0.2, 899.995, 800, 85, 2.625, 62, 1.67, 6654,
          0.37, 6.8, 0.053, 0.12, 0.035, 0.022, 0.01, 0.266, 1.4, 0.21,
          0.028, 2.5, 55.5, 52.2, 162.564, 100, 161.499, 25.235, 0.55,
          1.1, 0.021, 1.62, 86, 53.18, 1.1, 60, 3.6, 0.32, 0.044, 0.743,
          0.075, 0.148, 0.122, 0.92, 0.101, 0.205, 0.048, 86.25, 4.5,
          0.112, 207.501, 0.9, 0.104, 173.33, 2, 3.38, 4.23
        )
      ), row.names = c(
        NA,
        -83L
      ), class = c("tbl_df", "tbl", "data.frame")), modelframe = structure(list(
        sleep_total = c(
          12.1, 17, 14.4, 14.9, 4, 14.4, 8.7, 7, 10.1,
          3, 5.3, 9.4, 10, 12.5, 10.3, 8.3, 9.1, 17.4, 5.3, 18, 3.9,
          19.7, 2.9, 3.1, 10.1, 10.9, 14.9, 12.5, 9.8, 1.9, 2.7, 6.2,
          6.3, 8, 9.5, 3.3, 19.4, 10.1, 14.2, 14.3, 12.8, 12.5, 19.9,
          14.6, 11, 7.7, 14.5, 8.4, 3.8, 9.7, 15.8, 10.4, 13.5, 9.4,
          10.3, 11, 11.5, 13.7, 3.5, 5.6, 11.1, 18.1, 5.4, 13, 8.7,
          9.6, 8.4, 11.3, 10.6, 16.6, 13.8, 15.9, 12.8, 9.1, 8.6, 15.8,
          4.4, 15.6, 8.9, 5.2, 6.3, 12.5, 9.8
        ), sleep_rem = c(
          NA, 1.8,
          2.4, 2.3, 0.7, 2.2, 1.4, NA, 2.9, NA, 0.6, 0.8, 0.7, 1.5,
          2.2, 2, 1.4, 3.1, 0.5, 4.9, NA, 3.9, 0.6, 0.4, 3.5, 1.1,
          NA, 3.2, 1.1, 0.4, 0.1, 1.5, 0.6, 1.9, 0.9, NA, 6.6, 1.2,
          1.9, 3.1, NA, 1.4, 2, NA, NA, 0.9, NA, 0.9, 0.6, 1.4, NA,
          NA, NA, 1, 2.7, NA, NA, 1.8, 0.4, NA, 1.5, 6.1, 0.5, 2.4,
          NA, 1.4, 2.1, 1.1, 2.4, NA, 3.4, 3, 2, 2.4, NA, NA, 1, 2.3,
          2.6, NA, 1.3, NA, 2.4
        ), sleep_cycle = c(
          NA, NA, NA, 0.133333333,
          0.666666667, 0.766666667, 0.383333333, NA, 0.333333333, NA,
          NA, 0.216666667, NA, 0.116666667, NA, NA, 0.15, 0.383333333,
          NA, 0.333333333, NA, 0.116666667, 1, NA, 0.283333333, NA,
          NA, 0.416666667, 0.55, NA, NA, NA, NA, 1.5, NA, NA, NA, 0.75,
          NA, 0.2, NA, 0.183333333, 0.2, NA, NA, NA, NA, 0.416666667,
          NA, 1.416666667, NA, NA, NA, 0.666666667, NA, NA, NA, NA,
          NA, NA, NA, NA, NA, 0.183333333, NA, NA, 0.166666667, 0.15,
          NA, NA, 0.216666667, NA, 0.183333333, 0.5, NA, NA, 0.9, NA,
          0.233333333, NA, NA, NA, 0.35
        ), awake = c(
          11.9, 7, 9.6, 9.1,
          20, 9.6, 15.3, 17, 13.9, 21, 18.7, 14.6, 14, 11.5, 13.7,
          15.7, 14.9, 6.6, 18.7, 6, 20.1, 4.3, 21.1, 20.9, 13.9, 13.1,
          9.1, 11.5, 14.2, 22.1, 21.35, 17.8, 17.7, 16, 14.5, 20.7,
          4.6, 13.9, 9.8, 9.7, 11.2, 11.5, 4.1, 9.4, 13, 16.3, 9.5,
          15.6, 20.2, 14.3, 8.2, 13.6, 10.5, 14.6, 13.7, 13, 12.5,
          10.3, 20.5, 18.45, 12.9, 5.9, 18.6, 11, 15.3, 14.4, 15.6,
          12.7, 13.4, 7.4, 10.2, 8.1, 11.2, 14.9, 15.4, 8.2, 19.6,
          8.4, 15.1, 18.8, 17.7, 11.5, 14.2
        ), brainwt = c(
          NA, 0.0155,
          NA, 0.00029, 0.423, NA, NA, NA, 0.07, 0.0982, 0.115, 0.0055,
          NA, 0.0064, 0.001, 0.0066, 0.00014, 0.0108, 0.0123, 0.0063,
          4.603, 3e-04, 0.655, 0.419, 0.0035, 0.115, NA, 0.0256, 0.005,
          NA, NA, 0.325, 0.01227, 1.32, NA, 5.712, NA, 0.179, NA, 0.001,
          NA, 4e-04, 0.00025, NA, 0.0125, NA, NA, 0.0121, 0.175, 0.44,
          NA, 0.157, NA, 0.18, 0.0024, NA, NA, 0.0114, NA, NA, NA,
          0.081, 0.021, 0.0019, NA, 0.02, 0.0012, 0.00118, 0.003, 0.0057,
          0.004, NA, 0.00033, 0.18, 0.025, NA, 0.169, 0.0026, 0.0025,
          NA, 0.0175, 0.0445, 0.0504
        ), bodywt = c(
          50, 0.48, 1.35, 0.019,
          600, 3.85, 20.49, 0.045, 14, 14.8, 33.5, 0.728, 4.75, 0.42,
          0.06, 1, 0.005, 3.5, 2.95, 1.7, 2547, 0.023, 521, 187, 0.77,
          10, 0.071, 3.3, 0.2, 899.995, 800, 85, 2.625, 62, 1.67, 6654,
          0.37, 6.8, 0.053, 0.12, 0.035, 0.022, 0.01, 0.266, 1.4, 0.21,
          0.028, 2.5, 55.5, 52.2, 162.564, 100, 161.499, 25.235, 0.55,
          1.1, 0.021, 1.62, 86, 53.18, 1.1, 60, 3.6, 0.32, 0.044, 0.743,
          0.075, 0.148, 0.122, 0.92, 0.101, 0.205, 0.048, 86.25, 4.5,
          0.112, 207.501, 0.9, 0.104, 173.33, 2, 3.38, 4.23
        )
      ),
      row.names = c(
        NA,
        -83L
      ),
      class = c("tbl_df", "tbl", "data.frame")
      ),
      n = 83L, method = "pearson", bayesian = FALSE,
      p_adjust = "holm",
      partial = TRUE,
      multilevel = FALSE,
      partial_bayesian = FALSE,
      bayesian_prior = 0.707,
      include_factors = FALSE,
      additional_arguments = list(
        beta = 0.1
      ), class = c("tbl_df", "tbl", "data.frame")
      ),
      tolerance = 0.001
    )
  }
)
