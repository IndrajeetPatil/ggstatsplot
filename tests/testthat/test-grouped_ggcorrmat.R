
# output: plot ---------------------------------------------------------------

test_that(
  desc = "grouped_ggcorrmat plots work",
  code = {
    skip_on_cran()
    skip_if_not_installed("ggcorrplot")
    skip_if_not_installed("vdiffr")

    # with grouping.var missing ---------------------------------------------

    expect_error(grouped_ggcorrmat(iris))

    # with cor.vars specified -----------------------------------------------

    # creating a smaller dataframe
    set.seed(123)
    movies_filtered <- movies_long %>%
      dplyr::filter(mpaa != "NC-17") %>%
      dplyr::select(-year) %>%
      dplyr::sample_frac(size = 0.1)

    # basic
    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "grouped_ggcorrmat works",
      fig = grouped_ggcorrmat(
        data = movies_filtered,
        grouping.var = mpaa,
        cor.vars = length:votes
      )
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "grouped_ggcorrmat works - with NAs",
      fig = grouped_ggcorrmat(
        data = dplyr::select(ggplot2::msleep, dplyr::matches("sleep|awake|vore")),
        grouping.var = vore,
        tr = 0.2
      )
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "grouped_ggcorrmat works - np",
      fig = grouped_ggcorrmat(
        data = movies_filtered,
        grouping.var = mpaa,
        matrix.type = "lower",
        cor.vars = c(length:votes),
        cor.vars.names = c("w", "x", "y", "z"),
        type = "np"
      )
    )

    # without cor.vars specified -------------------------------------------

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "grouped_ggcorrmat - entire data",
      fig = grouped_ggcorrmat(
        data = movies_filtered,
        grouping.var = mpaa,
        ggtheme = ggplot2::theme_dark(),
        colors = NULL,
        package = "RColorBrewer",
        palette = "Paired"
      )
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "grouped_ggcorrmat works - r",
      fig = grouped_ggcorrmat(
        data = movies_filtered,
        grouping.var = mpaa,
        type = "r"
      )
    )
  }
)

# output: stats ---------------------------------------------------------------

test_that(
  desc = "grouped_ggcorrmat stats work",
  code = {
    skip_on_cran()
    skip_if_not_installed("ggcorrplot")

    options(tibble.width = Inf)

    # tidy dataframe
    set.seed(123)
    expect_snapshot(grouped_ggcorrmat(
      data = dplyr::select(ggplot2::msleep, dplyr::matches("sleep|awake|vore")),
      grouping.var = vore,
      type = "r",
      output = "data",
      tr = 0.2
    ))
  }
)
