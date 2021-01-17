# checking labels and data from plot -------------------------------------

test_that(
  desc = "checking labels and data from plot",
  code = {
    skip_on_cran()
    skip_if(getRversion() < "3.6")

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggbetweenstats(
        data = ggplot2::msleep,
        x = vore,
        y = brainwt,
        title = "mammalian sleep",
        caption = "From ggplot2 package",
        xlab = "vore",
        ylab = "brain weight",
        ggsignif.args = list(textsize = 6, tip_length = 0.01),
        ggstatsplot.layer = FALSE,
        outlier.tagging = TRUE,
        outlier.label = name,
        outlier.label.args = list(color = "darkgreen"),
        conf.level = 0.99,
        k = 5
      )

    # subtitle
    set.seed(123)
    p_subtitle <-
      statsExpressions::expr_oneway_anova(
        data = ggplot2::msleep,
        x = vore,
        y = brainwt,
        k = 5,
        conf.level = 0.99
      )

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # dataframe used for visualization
    expect_equal(length(pb$data), 6L)
    expect_equal(dim(pb$data[[1]]), c(44L, 13L))
    expect_equal(dim(pb$data[[2]]), c(4L, 26L))
    expect_equal(dim(pb$data[[3]]), c(2048L, 21L))
    expect_equal(dim(pb$data[[5]]), c(4L, 10L))

    # data from difference layers
    expect_equal(length(pb$data), 6L)
    expect_equal(pb$data[[5]]$y,
      c(0.07925556, 0.62159750, 0.02155000, 0.14573118),
      tolerance = 0.001
    )

    # checking displayed outlier labels
    expect_equal(
      pb$data[[4]],
      structure(list(
        x = structure(c(2L, 2L, 1L, 4L, 2L, 1L, 3L), class = c(
          "mapped_discrete",
          "numeric"
        )), y = c(4.603, 0.655, 0.325, 1.32, 5.712, 0.157, 0.081), label = c(
          "Asian elephant", "Horse", "Gray seal", "Human",
          "African elephant", "Jaguar", "Giant armadillo"
        ), PANEL = structure(c(
          1L,
          1L, 1L, 1L, 1L, 1L, 1L
        ), .Label = "1", class = "factor"),
        group = structure(c(
          2L, 2L, 1L, 4L, 2L, 1L, 3L
        ), n = 4L),
        colour = c(
          "darkgreen", "darkgreen",
          "darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen"
        ),
        fill = c("white", "white", "white", "white", "white", "white", "white"),
        size = c(3.88, 3.88, 3.88, 3.88, 3.88, 3.88, 3.88),
        angle = c(0, 0, 0, 0, 0, 0, 0),
        alpha = c(NA, NA, NA, NA, NA, NA, NA),
        family = c("", "", "", "", "", "", ""),
        fontface = c(1, 1, 1, 1, 1, 1, 1),
        lineheight = c(1.2, 1.2, 1.2, 1.2, 1.2, 1.2, 1.2),
        hjust = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
        vjust = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
        point.size = c(1, 1, 1, 1, 1, 1, 1),
        segment.linetype = c(1, 1, 1, 1, 1, 1, 1),
        segment.size = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
        segment.curvature = c(0, 0, 0, 0, 0, 0, 0),
        segment.angle = c(90, 90, 90, 90, 90, 90, 90),
        segment.ncp = c(1, 1, 1, 1, 1, 1, 1),
        segment.shape = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
        segment.square = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
        segment.squareShape = c(1, 1, 1, 1, 1, 1, 1),
        segment.inflect = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
        segment.debug = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
      ),
      row.names = c(
        NA,
        -7L
      ),
      class = "data.frame"
      )
    )

    # range of y variable
    expect_equal(ggplot2::layer_scales(p)$y$range$range,
      c(0.00014, 5.71200000),
      tolerance = 1e-5
    )

    # checking centrality
    expect_equal(
      pb$data[[6]]$label,
      c(
        "list(~widehat(mu)[mean]=='0.07926')",
        "list(~widehat(mu)[mean]=='0.62160')",
        "list(~widehat(mu)[mean]=='0.02155')",
        "list(~widehat(mu)[mean]=='0.14573')"
      )
    )

    # checking x-axis sample size labels
    expect_identical(
      ggplot2::layer_scales(p)$x$labels,
      c(
        "carni\n(n = 9)",
        "herbi\n(n = 20)",
        "insecti\n(n = 5)",
        "omni\n(n = 17)"
      )
    )

    expect_identical(pb$plot$labels$subtitle, p_subtitle)

    # checking plot labels
    # expect_equal(
    #   pb$plot$labels$caption,
    #   ggplot2::expr(atop(
    #     displaystyle(atop(
    #       displaystyle("From ggplot2 package"),
    #       expr = paste(
    #         "log"["e"],
    #         "(BF"["01"],
    #         ") = ",
    #         "1.54274",
    #         ", ",
    #         widehat(italic(R^"2"))["median"]^"posterior",
    #         " = ",
    #         "0.00000",
    #         ", CI"["95%"]^"HDI",
    #         " [",
    #         "0.00000",
    #         ", ",
    #         "0.10131",
    #         "]",
    #         ", ",
    #         italic("r")["Cauchy"]^"JZS",
    #         " = ",
    #         "0.70700"
    #       )
    #     )),
    #     expr = paste(
    #       "Pairwise test: ",
    #       bold("Games-Howell test"),
    #       "; Comparisons shown: ",
    #       bold("only significant")
    #     )
    #   ))
    # )
  }
)

# mean labelling tests work ------------------------------------------

test_that(
  desc = "checking mean labels are working",
  code = {
    skip_on_cran()
    skip_if(getRversion() < "3.6")

    # creating the plot
    set.seed(123)
    p <-
      ggstatsplot::ggbetweenstats(
        data = tibble::as_tibble(mtcars, rownames = "name") %>%
          dplyr::rename(.data = ., n = wt),
        x = "cyl",
        y = "n",
        type = "np",
        k = 2L,
        pairwise.comparisons = FALSE,
        conf.level = 0.90,
        outlier.tagging = TRUE,
        outlier.label = "name",
        outlier.coef = 2.5,
        nboot = 5,
        results.subtitle = FALSE
      ) +
      ggplot2::coord_cartesian(ylim = c(1, 6)) +
      ggplot2::scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, 1))

    # plot build
    pb <- ggplot2::ggplot_build(p)

    # checking dimensions of data for `geom_point`
    expect_equal(dim(pb$data[[1]]), c(29L, 13L))

    # checking displayed mean labels
    expect_identical(
      pb$data[[4]]$label,
      c(
        "Cadillac Fleetwood",
        "Lincoln Continental",
        "Chrysler Imperial"
      )
    )

    # check if the y-axis labels have changed
    expect_identical(
      pb$layout$panel_params[[1]]$x$scale$labels,
      c("4\n(n = 11)", "6\n(n = 7)", "8\n(n = 14)")
    )

    expect_identical(
      pb$layout$panel_params[[1]]$y$breaks,
      c(1, 2, 3, 4, 5, 6)
    )

    expect_identical(
      pb$data[[6]]$label[1],
      "list(~widehat(mu)[median]=='2.20')"
    )

    # edge case
    a <- data.frame(
      centrality.a = c(1.1, 0.9, 0.94, 1.58, 1.2, 1.4),
      group = c("a", "a", "a", "b", "b", "b")
    )

    # plot
    p1 <-
      suppressWarnings(ggstatsplot::ggbetweenstats(
        data = a,
        x = "group",
        y = "centrality.a",
        results.subtitle = FALSE
      ))

    # build
    pb1 <- ggplot2::ggplot_build(p1)

    expect_identical(
      pb1$data[[6]]$label,
      c(
        "list(~widehat(mu)[mean]=='0.98')",
        "list(~widehat(mu)[mean]=='1.39')"
      )
    )
  }
)

# checking if plot.type argument works --------------------------------------

test_that(
  desc = "checking if plot.type argument works",
  code = {
    skip_on_cran()
    skip_if(getRversion() < "3.6")

    # boxplot
    set.seed(123)
    p1 <-
      ggstatsplot::ggbetweenstats(
        data = ToothGrowth,
        x = supp,
        y = len,
        type = "bf",
        plot.type = "box",
        results.subtitle = FALSE,
        outlier.tagging = TRUE,
        bf.message = TRUE,
        outlier.coef = 0.75,
        outlier.color = "blue",
        centrality.point.args = list(size = 5, color = "darkgreen"),
        centrality.label.args = list(color = "blue", nudge_x = 0.4, segment.linetype = 4)
      )

    # violin
    set.seed(123)
    p2 <-
      ggstatsplot::ggbetweenstats(
        data = ToothGrowth,
        x = supp,
        y = len,
        type = "r",
        results.subtitle = FALSE,
        effsize.noncentral = FALSE,
        plot.type = "violin",
        outlier.tagging = TRUE,
        outlier.coef = 0.75,
        outlier.color = "blue",
        bf.message = FALSE,
        sample.size.label = FALSE,
        package = "wesanderson",
        palette = "Royal1"
      ) +
      ggplot2::scale_y_continuous(breaks = seq(0, 30, 5))

    # build the plots
    pb1 <- ggplot2::ggplot_build(p1)
    pb2 <- ggplot2::ggplot_build(p2)

    # tests for labels
    expect_null(pb1$plot$labels$subtitle, NULL)
    expect_null(pb1$plot$labels$caption, NULL)
    expect_null(pb2$plot$labels$caption, NULL)
    expect_identical(
      unique(pb1$data[[1]]$colour),
      c("#D95F02FF", "#1B9E77FF")
    )
    expect_identical(
      unique(pb2$data[[1]]$colour),
      c("#C93312FF", "#899DA4FF")
    )

    # dims for data
    expect_equal(dim(pb1$data[[1]]), c(58L, 13L))
    expect_equal(dim(pb1$data[[2]]), c(2L, 26L))
    expect_equal(dim(pb1$data[[4]]), c(2L, 10L))
    expect_equal(dim(pb1$data[[2]]), c(2L, 26L))
    expect_equal(dim(pb1$data[[4]]), c(2L, 10L))
    expect_equal(dim(pb2$data[[1]]), c(58L, 13L))
    expect_equal(dim(pb2$data[[2]]), c(2L, 10L))
    expect_equal(dim(pb2$data[[3]]), c(1024L, 21L))

    # checking geom data
    expect_equal(
      pb1$data[[2]],
      structure(list(
        ymin = c(8.2, 4.2), lower = c(15.525, 11.2), middle = c(
          22.7,
          16.5
        ), upper = c(25.725, 23.1), ymax = c(30.9, 29.5), outliers = list(
          numeric(0), c(33.9, 32.5)
        ), notchupper = c(
          25.6423655789178,
          19.9327598420707
        ), notchlower = c(19.7576344210822, 13.0672401579293),
        x = structure(c(1, 2), class = c("mapped_discrete", "numeric")),
        flipped_aes = c(FALSE, FALSE),
        PANEL = structure(c(1L, 1L), .Label = "1", class = "factor"),
        group = 1:2, ymin_final = c(
          8.2,
          4.2
        ),
        ymax_final = c(30.9, 33.9),
        xmin = structure(c(0.85, 1.85), class = c("mapped_discrete", "numeric")),
        xmax = structure(c(
          1.15,
          2.15
        ), class = c("mapped_discrete", "numeric")),
        xid = c(1, 2), newx = structure(c(1, 2), .Dim = 2L),
        new_width = c(0.3, 0.3),
        weight = c(1, 1), colour = c("grey20", "grey20"), fill = c(
          "white",
          "white"
        ), size = c(0.5, 0.5),
        alpha = c(0.2, 0.2),
        shape = c(
          19,
          19
        ), linetype = c("solid", "solid")
      ), row.names = c(NA, -2L), class = "data.frame")
    )

    expect_equal(
      pb1$data[[3]],
      structure(
        list(
          x = structure(c(2L, 2L), class = c(
            "mapped_discrete",
            "numeric"
          )),
          y = c(33.9, 32.5),
          label = c(33.9, 32.5),
          PANEL = structure(c(
            1L,
            1L
          ), .Label = "1", class = "factor"),
          group = structure(c(
            1L,
            1L
          ), n = 1L),
          colour = c("black", "black"),
          fill = c(
            "white",
            "white"
          ),
          size = c(3, 3),
          angle = c(0, 0),
          alpha = c(NA, NA),
          family = c("", ""),
          fontface = c(1, 1),
          lineheight = c(
            1.2,
            1.2
          ),
          hjust = c(0.5, 0.5),
          vjust = c(0.5, 0.5),
          point.size = c(
            1,
            1
          ),
          segment.linetype = c(1, 1),
          segment.size = c(0.5, 0.5),
          segment.curvature = c(0, 0),
          segment.angle = c(90, 90),
          segment.ncp = c(1, 1),
          segment.shape = c(0.5, 0.5),
          segment.square = c(TRUE, TRUE),
          segment.squareShape = c(1, 1),
          segment.inflect = c(FALSE, FALSE),
          segment.debug = c(FALSE, FALSE)
        ),
        row.names = c(
          NA,
          -2L
        ),
        class = "data.frame"
      )
    )

    expect_equal(
      pb1$data[[4]],
      structure(
        list(
          x = structure(1:2, class = c(
            "mapped_discrete",
            "numeric"
          )),
          y = c(24.9087976539589, 15.4354838709677),
          PANEL = structure(c(
            1L,
            1L
          ), .Label = "1", class = "factor"),
          group = structure(1:2, n = 2L),
          shape = c(19, 19),
          colour = c("darkgreen", "darkgreen"),
          size = c(5, 5),
          fill = c(NA, NA),
          alpha = c(NA, NA),
          stroke = c(
            0.5,
            0.5
          )
        ),
        row.names = c(NA, -2L),
        class = "data.frame"
      )
    )

    expect_equal(
      pb1$data[[5]],
      structure(
        list(
          x = structure(1:2, class = c(
            "mapped_discrete",
            "numeric"
          )),
          y = c(24.9087976539589, 15.4354838709677),
          label = c(
            "list(~widehat(mu)[MAP]=='24.91')",
            "list(~widehat(mu)[MAP]=='15.44')"
          ),
          PANEL = structure(c(
            1L,
            1L
          ), .Label = "1", class = "factor"),
          group = structure(1:2, n = 2L),
          nudge_x = structure(c(1.4, 2.4), class = c(
            "mapped_discrete",
            "numeric"
          )),
          nudge_y = c(24.9087976539589, 15.4354838709677),
          colour = c("blue", "blue"),
          fill = c("white", "white"),
          size = c(3.88, 3.88),
          angle = c(0, 0),
          alpha = c(NA, NA),
          family = c("", ""),
          fontface = c(1, 1),
          lineheight = c(
            1.2,
            1.2
          ),
          hjust = c(0.5, 0.5),
          vjust = c(0.5, 0.5),
          point.size = c(
            1,
            1
          ),
          segment.linetype = c(4, 4),
          segment.size = c(0.5, 0.5),
          segment.curvature = c(0, 0),
          segment.angle = c(90, 90),
          segment.ncp = c(1, 1),
          segment.shape = c(0.5, 0.5),
          segment.square = c(
            TRUE,
            TRUE
          ),
          segment.squareShape = c(1, 1),
          segment.inflect = c(
            FALSE,
            FALSE
          ),
          segment.debug = c(FALSE, FALSE)
        ),
        row.names = c(
          NA,
          -2L
        ),
        class = "data.frame"
      )
    )

    expect_equal(
      pb2$data[[2]],
      structure(
        list(
          y = c(33.9, 32.5),
          x = structure(c(2L, 2L), class = c(
            "mapped_discrete",
            "numeric"
          )),
          PANEL = structure(c(1L, 1L), class = "factor", .Label = "1"),
          group = structure(c(1L, 1L), n = 1L),
          shape = c(19, 19),
          colour = c("blue", "blue"),
          size = c(3, 3),
          fill = c(NA, NA),
          alpha = c(0.7, 0.7),
          stroke = c(0, 0)
        ),
        row.names = c(NA, -2L),
        class = "data.frame"
      )
    )

    expect_equal(
      pb2$data[[4]],
      structure(
        list(
          y = c(33.9, 32.5),
          x = structure(c(2L, 2L), class = c("mapped_discrete", "numeric")),
          label = c(33.9, 32.5),
          PANEL = structure(c(1L, 1L), .Label = "1", class = "factor"),
          group = structure(c(1L, 1L), n = 1L),
          colour = c("black", "black"),
          fill = c("white", "white"),
          size = c(3, 3),
          angle = c(0, 0),
          alpha = c(NA, NA),
          family = c("", ""),
          fontface = c(1, 1),
          lineheight = c(1.2, 1.2),
          hjust = c(0.5, 0.5),
          vjust = c(0.5, 0.5),
          point.size = c(1, 1),
          segment.linetype = c(1, 1),
          segment.size = c(0.5, 0.5),
          segment.curvature = c(0, 0),
          segment.angle = c(90, 90),
          segment.ncp = c(1, 1),
          segment.shape = c(0.5, 0.5),
          segment.square = c(TRUE, TRUE),
          segment.squareShape = c(1, 1),
          segment.inflect = c(FALSE, FALSE),
          segment.debug = c(FALSE, FALSE)
        ),
        row.names = c(NA, -2L),
        class = "data.frame"
      )
    )

    expect_equal(
      pb2$data[[6]]$label[1],
      "list(~widehat(mu)[trimmed]=='21.04')"
    )
  }
)

# subtitle output works ------------------------------------------------

test_that(
  desc = "subtitle output works",
  code = {
    skip_on_cran()
    skip_if(getRversion() < "3.6")

    df <- mtcars
    df$wt[3] <- NA

    # plot
    set.seed(123)
    subtitle_exp <-
      ggstatsplot::ggbetweenstats(
        data = df,
        x = am,
        y = wt,
        output = "subtitle"
      )

    set.seed(123)
    sub <-
      statsExpressions::expr_t_twosample(
        data = df,
        x = am,
        y = wt,
        output = "subtitle"
      )

    # test
    expect_identical(subtitle_exp, sub)
  }
)
