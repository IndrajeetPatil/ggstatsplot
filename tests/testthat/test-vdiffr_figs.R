# wait for https://github.com/r-lib/vdiffr/issues/86
if (getRversion() > "4.3") {
  test_that("plots are rendered correctly", {
    skip_on_cran()
    skip_on_os("windows")
    skip_if_not_installed("vdiffr")

    ## ----ggbetweenstats-------------------------------

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "ggbetweenstats works",
      fig = ggbetweenstats(
        data = dplyr::filter(
          .data = movies_long,
          genre %in% c("Action", "Action Comedy", "Action Drama", "Comedy")
        ),
        x = mpaa,
        y = length
      )
    )


    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "grouped_ggbetweenstats works",
      fig = grouped_ggbetweenstats(
        data = dplyr::filter(
          .data = movies_long,
          genre %in% c("Action", "Action Comedy", "Action Drama", "Comedy")
        ),
        x = mpaa,
        y = length,
        grouping.var = genre
      )
    )

    ## ----ggwithinstats--------------------------------

    if (require("afex")) {
      set.seed(123)
      vdiffr::expect_doppelganger(
        title = "ggwithinstats works",
        fig = ggwithinstats(
          data = dplyr::filter(bugs_long, condition %in% c("HDHF", "LDLF")),
          x = condition,
          y = desire
        )
      )

      set.seed(123)
      vdiffr::expect_doppelganger(
        title = "grouped_ggwithinstats works",
        fig = grouped_ggwithinstats(
          data = dplyr::filter(bugs_long, condition %in% c("HDHF", "LDLF")),
          x = condition,
          y = desire,
          grouping.var = gender
        )
      )
    }

    ## ----gghistostats---------------------------------

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "gghistostats works",
      fig = gghistostats(mtcars, wt, test.value = 3)
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "grouped_gghistostats works",
      fig = grouped_gghistostats(mtcars, wt, test.value = 3, grouping.var = am)
    )

    ## ----ggdotplotstats-------------------------------

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "ggdotplotstats works",
      fig = ggdotplotstats(dplyr::filter(.data = ggplot2::mpg, cyl %in% c("4", "6")),
        cty, manufacturer,
        test.value = 15
      )
    )


    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "grouped_ggdotplotstats works",
      fig = grouped_ggdotplotstats(dplyr::filter(.data = ggplot2::mpg, cyl %in% c("4", "6")),
        cty, manufacturer,
        test.value = 15, grouping.var = cyl
      )
    )

    ## ----ggscatterstats-------------------------------

    if (require("ggExtra")) {
      set.seed(123)
      vdiffr::expect_doppelganger(
        title = "ggscatterstats works",
        fig = ggscatterstats(mtcars, wt, mpg)
      )

      set.seed(123)
      vdiffr::expect_doppelganger(
        title = "grouped_ggscatterstats works",
        fig = grouped_ggscatterstats(mtcars, wt, mpg, grouping.var = am)
      )
    }

    ## ----ggcorrmat------------------------------------

    if (require("ggcorrplot")) {
      set.seed(123)
      vdiffr::expect_doppelganger(
        title = "ggcorrmat works",
        fig = ggcorrmat(iris)
      )


      set.seed(123)
      vdiffr::expect_doppelganger(
        title = "grouped_ggcorrmat works",
        fig = grouped_ggcorrmat(iris, grouping.var = Species)
      )
    }

    ## ----ggpiestats-----------------------------------

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "ggpiestats works",
      fig = ggpiestats(mtcars, cyl)
    )


    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "grouped_ggpiestats works",
      fig = grouped_ggpiestats(mtcars, cyl, grouping.var = am)
    )

    ## ----ggbarstats-----------------------------------

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "ggbarstats works",
      fig = ggbarstats(ggplot2::mpg, fl, class)
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "grouped_ggbarstats works",
      fig = grouped_ggbarstats(ggplot2::mpg, fl, class, grouping.var = drv)
    )

    ## ----ggcoefstats----------------------------------

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "ggcoefstats works",
      fig = ggcoefstats(stats::lm(formula = wt ~ am * cyl, data = mtcars))
    )
  })
}
