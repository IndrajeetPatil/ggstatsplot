# creating a new dataset
morley_new <- dplyr::mutate(
  morley,
  Expt = dplyr::case_when(
    Expt == 1 ~ "1st",
    Expt == 2 ~ "2nd",
    Expt == 3 ~ "3rd",
    Expt == 4 ~ "4th",
    Expt == 5 ~ "5th"
  )
) %>%
  dplyr::as_tibble()

morley_new[3, 3] <- NA_integer_
morley_new[23, 3] <- NA_integer_
morley_new[87, 3] <- NA_integer_

# checking default outputs -----------------------------------------

test_that(
  desc = "checking default outputs",
  code = {
    skip_if_not_installed("vdiffr")
    skip_if(getRversion() < "4.1")


    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "parametric - without NA",
      fig = ggdotplotstats(ggplot2::mpg, cty, cyl, test.value = 16, type = "p")
    )

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "robust - with NA",
      fig = ggdotplotstats(morley_new, Speed, Expt, test.value = 800, type = "r")
    )
  }
)


# modification with ggplot2 ----------------------------------------------

test_that(
  desc = "modification with ggplot2 works as expected",
  code = {
    skip_if_not_installed("vdiffr")
    skip_if(getRversion() < "4.1")


    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "modification with ggplot2 ",
      fig = suppressMessages(ggdotplotstats(
        data = morley_new,
        x = Speed,
        y = Expt,
        results.subtitle = FALSE,
        title = "Michelson-Morley experiment",
        xlab = substitute(paste("Speed of light (", italic("c"), ")")),
        ylab = "Experimental run",
        ggplot.component = ggplot2::scale_x_continuous(
          breaks = seq(800, 900, 10),
          sec.axis = ggplot2::dup_axis()
        ),
      ))
    )
  }
)

# subtitle output -------------------------------------------------------

test_that(
  desc = "subtitle output",
  code = {
    # should output a list of length 3
    set.seed(123)
    p_sub <- suppressWarnings(ggdotplotstats(
      data = morley,
      x = Speed,
      y = Expt,
      test.value = 800,
      type = "np",
      output = "subtitle"
    ))

    set.seed(123)
    expect_equal(
      p_sub,
      suppressWarnings(gghistostats(
        data = dplyr::group_by(morley, Expt) %>%
          dplyr::summarise(mean = mean(Speed)),
        x = mean,
        test.value = 800,
        type = "np",
        output = "subtitle"
      ))
    )
  }
)
