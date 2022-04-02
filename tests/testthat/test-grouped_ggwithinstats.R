# outlier labeling works --------------------------------------------------

test_that(
  desc = "grouping.var works across vector types",
  code = {
    skip_if_not_installed("afex")
    set.seed(123)
    library(afex)

    # expect error when no grouping.var is specified
    expect_error(
      grouped_ggwithinstats(
        data = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
        x = condition,
        y = desire,
      )
    )

    # outlier tagging is not required
    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "no outlier tagging",
      fig = grouped_ggwithinstats(
        data = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
        x = condition,
        y = desire,
        grouping.var = gender,
        results.subtitle = FALSE
      )
    )

    # `outlier.label` is not specified
    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "outlier.label not specified",
      fig = grouped_ggwithinstats(
        data = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
        x = condition,
        y = desire,
        grouping.var = gender,
        ggtheme = ggplot2::theme_linedraw(),
        results.subtitle = FALSE,
        outlier.tagging = TRUE
      )
    )

    # `outlier.label` is character
    # also x, y, and outlier.label arguments as characters
    set.seed(123)
    dat <- iris_long
    dat$id <- as.character(dat$id)

    set.seed(123)
    vdiffr::expect_doppelganger(
      title = "outlier.label specified",
      fig = grouped_ggwithinstats(
        data = dat,
        x = attribute,
        y = value,
        grouping.var = Species,
        palette = "default_jama",
        package = "ggsci",
        results.subtitle = FALSE,
        outlier.tagging = TRUE,
        outlier.label = id,
        outlier.coef = 2
      )
    )
  }
)

# subtitle output with NA --------------------------------------------------

test_that(
  desc = "subtitle output with NA",
  code = {
    skip_if_not_installed("PMCMRplus")

    # data
    df <- dplyr::filter(bugs_long, region %in% c("North America"))

    # should output a list of length 2
    set.seed(123)
    ls_results <- grouped_ggwithinstats(
      data = df,
      x = condition,
      y = desire,
      grouping.var = region,
      output = "subtitle",
      bf.message = FALSE
    )

    set.seed(123)
    basic_results <- ggwithinstats(
      data = df,
      x = condition,
      y = desire,
      output = "subtitle",
      bf.message = FALSE
    )


    expect_equal(length(ls_results), 1L)
    expect_equal(ls_results$`North America`, basic_results)
  }
)
