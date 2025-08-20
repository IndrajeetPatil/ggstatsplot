morley_new <- dplyr::mutate(
  morley,
  Expt = dplyr::case_when(
    Expt == 1L ~ "1st",
    Expt == 2L ~ "2nd",
    Expt == 3L ~ "3rd",
    Expt == 4L ~ "4th",
    Expt == 5L ~ "5th"
  )
) %>%
  tibble::as_tibble()

morley_new[3L, 3L] <- NA_integer_
morley_new[23L, 3L] <- NA_integer_
morley_new[87L, 3L] <- NA_integer_

# checking default outputs -----------------------------------------

test_that(
  "checking default outputs",
  {
    set.seed(123)
    expect_doppelganger(
      title = "parametric - without NA",
      fig = ggdotplotstats(ggplot2::mpg, cty, cyl, test.value = 16, type = "p")
    )

    set.seed(123)
    expect_doppelganger(
      title = "robust - with NA",
      fig = ggdotplotstats(morley_new, Speed, Expt, test.value = 800, type = "r")
    )
  }
)


# modification with ggplot2 ----------------------------------------------

test_that(
  "modification with ggplot2 works as expected",
  {
    set.seed(123)
    expect_doppelganger(
      title = "modification with ggplot2 ",
      fig = suppressMessages(ggdotplotstats(
        data = morley_new,
        x = Speed,
        y = Expt,
        conf.int = FALSE,
        results.subtitle = FALSE,
        title = "Michelson-Morley experiment",
        xlab = substitute(paste("Speed of light (", italic("c"), ")")),
        ylab = "Experimental run",
        ggplot.component = ggplot2::scale_x_continuous(
          breaks = seq(800, 900, 10),
          sec.axis = ggplot2::dup_axis()
        )
      ))
    )
  }
)

# subtitle output -------------------------------------------------------

test_that(
  "subtitle output",
  {
    set.seed(123)
    p_sub_ggdot <- ggdotplotstats(
      data = morley,
      x = Speed,
      y = Expt,
      test.value = 800,
      type = "np"
    ) %>%
      extract_subtitle()

    set.seed(123)
    p_sub_gghist <-
      morley %>%
      dplyr::group_by(Expt) %>%
      dplyr::summarise(mean = mean(Speed), .groups = "drop") %>%
      gghistostats(
        x = mean,
        test.value = 800,
        type = "np"
      ) %>%
      extract_subtitle()

    set.seed(123)
    expect_identical(p_sub_ggdot, p_sub_gghist)
  }
)

# grouped_ggdotplotstats works -----------------------------------------------

test_that(
  "grouped_ggdotplotstats works",
  {
    # removing factor level with very few no. of observations
    df <- dplyr::filter(ggplot2::mpg, cyl %in% c("4", "6", "8"))

    set.seed(123)
    expect_doppelganger(
      title = "defaults work as expected",
      fig = grouped_ggdotplotstats(
        data = df,
        x = cty,
        y = manufacturer,
        xlab = "city miles per gallon",
        ylab = "car manufacturer",
        grouping.var = cyl,
        test.value = 15.5,
        point.args = list(color = "red", size = 5, shape = 13),
        results.subtitle = FALSE,
        ggtheme = ggplot2::theme_classic()
      )
    )

    set.seed(123)
    expect_doppelganger(
      title = "further modification with ggplot works",
      fig = grouped_ggdotplotstats(
        data = df,
        x = cty,
        y = manufacturer,
        grouping.var = cyl,
        test.value = 15.5,
        results.subtitle = FALSE,
        effsize.type = "d",
        ggplot.component = ggplot2::scale_y_continuous(
          sec.axis = ggplot2::dup_axis(name = "percentile score"),
          breaks = seq(0, 12, 2)
        )
      )
    )
  }
)
