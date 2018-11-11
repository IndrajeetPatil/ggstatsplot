#' @title Games-Howell post-hoc test
#' @name games_howell
#' @description This function produces results from Games-Howell post-hoc tests
#'   for Welch's one-way analysis of variance (ANOVA) (`stats::oneway.test()`).
#'
#' @inheritParams ggbetweenstats
#'
#' @importFrom stats ptukey qtukey
#' @importFrom utils combn
#'
#' @note This is based on the implementation of Games-Howell test by Aaron
#'   Schlegel (https://rpubs.com/aaronsc32)  and published on RPubs
#'   (https://rpubs.com/aaronsc32/games-howell-test).
#'
#' @keywords internal

# function body
games_howell <- function(data,
                         x,
                         y) {

  # ============================ data preparation ==========================

  # creating a dataframe
  data <-
    dplyr::select(
      .data = data,
      x = !!rlang::enquo(x),
      y = !!rlang::enquo(y)
    )

  # convert the grouping variable to factor and drop unused levels
  data %<>%
    stats::na.omit(.) %>%
    dplyr::mutate_at(
      .tbl = .,
      .vars = "x",
      .funs = ~ base::droplevels(x = base::as.factor(x = .))
    )

  # variables of interest for running the test
  grp <- data$x
  obs <- data$y

  # create combinations
  combs <- utils::combn(x = unique(grp), m = 2)

  # Statistics that will be used throughout the calculations:
  # n = sample size of each group
  # groups = number of groups in data
  # Mean = means of each group sample
  # std = variance of each group sample
  n <- tapply(X = obs, INDEX = grp, FUN = length)
  groups <- length(tapply(X = obs, INDEX = grp, FUN = length))
  Mean <- tapply(X = obs, INDEX = grp, FUN = mean)
  std <- tapply(X = obs, INDEX = grp, FUN = var)

  # ============================ analysis ===============================

  statistics <- lapply(X = 1:ncol(combs), FUN = function(x) {

    # mean difference
    mean.diff <- Mean[combs[2, x]] - Mean[combs[1, x]]

    # t-values
    t <-
      (abs(Mean[combs[1, x]] - Mean[combs[2, x]])) /
        (sqrt((std[combs[1, x]] / n[combs[1, x]]) +
          (std[combs[2, x]] / n[combs[2, x]])))

    # degrees of freedom (df)
    df <-
      ((std[combs[1, x]] / n[combs[1, x]] +
        std[combs[2, x]] / n[combs[2, x]])^2) /
        ((((std[combs[1, x]] / n[combs[1, x]])^2 / (n[combs[1, x]] - 1)) +
          ((std[combs[2, x]] / n[combs[2, x]])^2 / (n[combs[2, x]] - 1))))

    # p-values
    p <-
      stats::ptukey(
        q = t * sqrt(2),
        nmeans = groups,
        df = df,
        lower.tail = FALSE
      )

    # sigma standard error
    se <-
      sqrt(x = 0.5 * (std[combs[1, x]] / n[combs[1, x]] +
        std[combs[2, x]] / n[combs[2, x]]))

    # upper confidence limit for mean difference
    high.conf <- lapply(X = 1:ncol(combs), FUN = function(x) {
      mean.diff + stats::qtukey(
        p = 0.95,
        nmeans = groups,
        df = df
      ) * se
    })[[1]]

    # lower confidence limit for mean difference
    low.conf <- lapply(X = 1:ncol(combs), FUN = function(x) {
      mean.diff - stats::qtukey(
        p = 0.95,
        nmeans = groups,
        df = df
      ) * se
    })[[1]]

    # Group Combinations
    group1 <- as.character(combs[1, x])
    group2 <- as.character(combs[2, x])

    # Collect all statistics into list
    stats <-
      list(group1, group2, mean.diff, se, t, df, p, high.conf, low.conf)
  })

  # unlist statistics collected earlier
  stats.unlisted <- lapply(statistics, function(x) {
    unlist(x)
  })

  # create dataframe from flattened list
  results <-
    data.frame(matrix(
      unlist(stats.unlisted),
      nrow = length(stats.unlisted),
      byrow = TRUE
    ))

  # select columns that should be numeric and change with as.numeric
  results[, 3:ncol(results)] <-
    round(as.numeric(as.matrix(results[, 3:ncol(results)])), digits = 3)

  # Rename data frame columns
  colnames(results) <-
    c(
      "group1",
      "group2",
      "mean.difference",
      "se",
      "t.value",
      "df",
      "p.value",
      "high.conf",
      "low.conf"
    )

  # converting it to tibble
  results %<>%
    tibble::as.tibble(x = .) %>%
    dplyr::select(
      .data = .,
      group1:mean.difference,
      low.conf,
      high.conf,
      dplyr::everything()
    )

  # select the final dataframe
  return(results)
}

#' @title Pairwise comparison tests
#' @name pairwise_p
#' @aliases pairwise_p
#' @description Calculate pairwise comparisons between group levels with
#'   corrections for multiple testing.
#' @author Indrajeet Patil
#'
#' @inheritParams ggbetweenstats
#' @inheritParams stats::t.test
#' @inheritParams WRS2::rmmcp
#'
#' @importFrom dplyr select rename mutate mutate_if everything full_join
#' @importFrom stats p.adjust pairwise.t.test na.omit
#' @importFrom stats aov TukeyHSD var sd
#' @importFrom WRS2 lincon rmmcp
#' @importFrom tidyr gather spread separate
#' @importFrom rlang !! enquo
#' @importFrom tibble as.tibble rowid_to_column
#' @importFrom broom tidy
#' @importFrom jmv anovaNP anovaRMNP
#'
#' @seealso \code{\link{ggbetweenstats}}, \code{\link{grouped_ggbetweenstats}}
#'
#' @family helper_messages
#'
#' @examples
#' \dontrun{
#'
#' # for reproducibility
#' set.seed(123)
#'
#' # parametric
#' # if `var.equal = TRUE`, then Student's *t*-test will be run
#' ggstatsplot::pairwise_p(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = brainwt,
#'   type = "p",
#'   var.equal = TRUE,
#'   paired = FALSE,
#'   p.adjust.method = "bonferroni"
#' )
#'
#' # if `var.equal = FALSE`, then Games-Howell test will be run
#' ggstatsplot::pairwise_p(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = brainwt,
#'   type = "p",
#'   var.equal = FALSE,
#'   paired = FALSE,
#'   p.adjust.method = "bonferroni"
#' )
#'
#' # non-parametric
#' ggstatsplot::pairwise_p(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = brainwt,
#'   type = "np",
#'   p.adjust.method = "none"
#' )
#'
#' # robust
#' ggstatsplot::pairwise_p(
#'   data = ggplot2::msleep,
#'   x = vore,
#'   y = brainwt,
#'   type = "r",
#'   p.adjust.method = "fdr"
#' )
#' }
#' @export

# function body
pairwise_p <-
  function(data,
             x,
             y,
             type = "parametric",
             tr = 0.1,
             paired = FALSE,
             var.equal = FALSE,
             p.adjust.method = "holm",
             k = 3,
             messages = TRUE,
             ...) {
    # ---------------------------- data cleanup -------------------------------
    # creating a dataframe
    data <-
      dplyr::select(
        .data = data,
        x = !!rlang::enquo(x),
        y = !!rlang::enquo(y)
      )

    # convert the grouping variable to factor and drop unused levels
    data %<>%
      dplyr::mutate_at(
        .tbl = .,
        .vars = "x",
        .funs = ~ base::droplevels(x = base::as.factor(x = .))
      ) %>%
      # stats::na.omit(.) %>%
      tibble::as.tibble(x = .)

    # ---------------------------- parametric ---------------------------------
    #
    if (type %in% c("parametric", "p")) {
      if (isTRUE(var.equal)) {
        df <-
          dplyr::full_join(
            # mean difference and its confidence intervals
            x = stats::aov(formula = y ~ x, data = data) %>%
              stats::TukeyHSD(x = .) %>%
              broom::tidy(x = .) %>%
              dplyr::select(
                .data = .,
                comparison, estimate, conf.low, conf.high
              ) %>%
              tidyr::separate(
                data = .,
                col = comparison,
                into = c("group1", "group2"),
                sep = "-"
              ) %>%
              dplyr::rename(.data = ., mean.difference = estimate),
            y = broom::tidy(
              stats::pairwise.t.test(
                x = data$y,
                g = data$x,
                p.adjust.method = p.adjust.method,
                paired = paired,
                alternative = "two.sided",
                na.action = na.omit
              )
            ) %>%
              ggstatsplot::signif_column(data = ., p = p.value),
            by = c("group1", "group2")
          )

        # display message about the post hoc tests run
        if (isTRUE(messages)) {
          base::message(cat(
            crayon::green("Note: "),
            crayon::blue(
              "The parametric pairwise multiple comparisons test used-\n",
              "Student's t-test.\n",
              "Adjustment method for p-values: "
            ),
            crayon::yellow(p.adjust.method),
            sep = ""
          ))
        }
      } else if (!isTRUE(var.equal)) {

        # dataframe with Games-Howell test results
        df <-
          games_howell(data = data, x = x, y = y) %>%
          dplyr::mutate(
            .data = .,
            p.value = stats::p.adjust(p = p.value, method = p.adjust.method)
          ) %>%
          ggstatsplot::signif_column(data = ., p = p.value)

        # display message about the post hoc tests run
        if (isTRUE(messages)) {
          base::message(cat(
            crayon::green("Note: "),
            crayon::blue(
              "The parametric pairwise multiple comparisons test used-\n",
              "Games-Howell test.\n",
              "Adjustment method for p-values: "
            ),
            crayon::yellow(p.adjust.method),
            sep = ""
          ))
        }
      }
      # ---------------------------- nonparametric ----------------------------
      #
    } else if (type %in% c("nonparametric", "np")) {
      if (!isTRUE(paired)) {
        # running Dwass-Steel-Crichtlow-Fligner test using `jmv` package
        jmv_pairs <-
          jmv::anovaNP(
            data = data,
            deps = "y",
            group = "x",
            pairs = TRUE
          )

        # extracting the pairwise tests and formatting the output
        df <-
          as.data.frame(x = jmv_pairs$comparisons[[1]]) %>%
          tibble::as.tibble(x = .) %>%
          dplyr::rename(
            .data = .,
            group1 = p1,
            group2 = p2,
            p.value = p
          ) %>%
          dplyr::mutate(
            .data = .,
            p.value = stats::p.adjust(p = p.value, method = p.adjust.method)
          ) %>%
          ggstatsplot::signif_column(data = ., p = p.value)

        # letting the user know which test was run
        if (isTRUE(messages)) {
          base::message(cat(
            crayon::green("Note: "),
            crayon::blue(
              "The nonparametric pairwise multiple comparisons test used-\n",
              "Dwass-Steel-Crichtlow-Fligner test.\n",
              "Adjustment method for p-values: "
            ),
            crayon::yellow(p.adjust.method),
            sep = ""
          ))
        }
      } else if (isTRUE(paired)) {

        # converting the entered long format data to wide format
        data_wide <- long_to_wide_converter(
          data = data,
          x = x,
          y = y
        )

        # running Durbin-Conover test using `jmv` package
        jmv_pairs <-
          jmv::anovaRMNP(
            data = data_wide,
            measures = names(data_wide[, -1]),
            pairs = TRUE
          )

        # extracting the pairwise tests and formatting the output
        df <-
          as.data.frame(x = jmv_pairs$comp) %>%
          tibble::as.tibble(x = .) %>%
          dplyr::select(.data = ., -sep) %>%
          dplyr::rename(
            .data = .,
            group1 = i1,
            group2 = i2,
            statistic = stat,
            p.value = p
          ) %>%
          dplyr::mutate(
            .data = .,
            p.value = stats::p.adjust(p = p.value, method = p.adjust.method)
          ) %>%
          ggstatsplot::signif_column(data = ., p = p.value)

        # letting the user know which test was run
        if (isTRUE(messages)) {
          base::message(cat(
            crayon::green("Note: "),
            crayon::blue(
              "The nonparametric pairwise multiple comparisons test used-\n",
              "Durbin-Conover test.\n",
              "Adjustment method for p-values: "
            ),
            crayon::yellow(p.adjust.method),
            sep = ""
          ))
        }
      }

      # ---------------------------- robust ----------------------------------
      #
    } else if (type %in% c("robust", "r")) {
      if (!isTRUE(paired)) {
        # object with all details about pairwise comparisons
        rob_pairwise_df <-
          WRS2::lincon(
            formula = y ~ x,
            data = data,
            tr = tr
          )
      } else if (isTRUE(paired)) {
        # converting to long format and then getting it back in wide so that the
        # rowid variable can be used as the block variable for WRS2 functions
        data_within <-
          long_to_wide_converter(
            data = data,
            x = x,
            y = y
          ) %>%
          tidyr::gather(data = ., key, value, -rowid) %>%
          dplyr::arrange(.data = ., rowid)

        # running pairwise multiple comparison tests
        rob_pairwise_df <-
          base::with(
            data = data_within,
            expr = WRS2::rmmcp(
              y = value,
              groups = key,
              blocks = rowid,
              tr = tr
            )
          )
      }

      # cleaning the raw object and getting it in the right format
      df <-
        dplyr::full_join(
          # dataframe comparing comparion details
          x = rob_pairwise_df$comp %>%
            tibble::as.tibble(x = .) %>%
            dplyr::rename(
              .data = .,
              group1 = Group,
              group2 = Group1
            ) %>%
            dplyr::mutate(
              .data = .,
              p.value = stats::p.adjust(p = p.value, method = p.adjust.method)
            ) %>%
            ggstatsplot::signif_column(data = ., p = p.value) %>%
            tidyr::gather(
              data = .,
              key = "key",
              value = "rowid",
              group1:group2
            ),
          # dataframe with factor level codings
          y = rob_pairwise_df$fnames %>%
            tibble::as.tibble(x = .) %>%
            tibble::rowid_to_column(.),
          by = "rowid"
        ) %>%
        dplyr::select(.data = ., -rowid) %>%
        tidyr::spread(data = ., key = "key", value = "value") %>%
        dplyr::select(.data = ., group1, group2, dplyr::everything())

      # for paired designs, there will be an unnecessary column to remove
      if (("p.crit") %in% names(df)) {
        df %<>%
          dplyr::select(.data = ., -p.crit)
      }

      # message about which test was run
      if (isTRUE(messages)) {
        base::message(cat(
          crayon::green("Note: "),
          crayon::blue(
            "The robust pairwise multiple comparisons test used-\n",
            "Yuen's trimmed means comparisons test.\n",
            "Adjustment method for p-values: "
          ),
          crayon::yellow(p.adjust.method),
          sep = ""
        ))
      }
    }

    # if there are factors, covert them to character to make life easy
    df %<>%
      dplyr::mutate_if(
        .tbl = .,
        .predicate = base::is.factor,
        .funs = ~ as.character(.)
      ) %>%
      purrrlyr::by_row(
        .d = .,
        ..f = ~ ggstatsplot::specify_decimal_p(
          x = .$p.value,
          k = k,
          p.value = TRUE
        ),
        .collate = "rows",
        .to = "label",
        .labels = TRUE
      ) %>%
      dplyr::mutate(
        .data = .,
        label2 = dplyr::case_when(
          label == "< 0.001" ~ "<= 0.001",
          label != "< 0.001" ~ paste(" = ", label,
            sep = ""
          )
        )
      ) %>%
      dplyr::select(.data = ., -label) %>%
      purrrlyr::by_row(
        .d = .,
        ..f = ~ paste("p ",
          .$label2,
          sep = ""
        ),
        .collate = "rows",
        .to = "p.value.label",
        .labels = TRUE
      ) %>%
      dplyr::select(.data = ., -label2)

    # return
    return(df)
  }


#' @title Preparing caption in case pairwise comparisons are displayed.
#' @name pairwise_p_caption
#'
#' @inheritParams pairwise_p
#' @inheritParams ggbetweenstats
#'
#' @keywords internal

pairwise_p_caption <- function(type,
                               var.equal,
                               paired,
                               p.adjust.method,
                               caption = NULL) {

  # ======================= pairwise test run ==============================

  # figuring out type of test needed to run
  test.type <- switch(
    EXPR = type,
    parametric = "p",
    p = "p",
    robust = "r",
    r = "r",
    nonparametric = "np",
    np = "np",
    bayes = "bf",
    bf = "bf"
  )

  # figuring out which pairwise comparison test was run
  if (test.type == "p") {
    if (isTRUE(paired)) {
      test.description <- "Student's t-test"
    } else if (!isTRUE(paired)) {
      if (!isTRUE(var.equal)) {
        test.description <- "Games-Howell test"
      } else {
        test.description <- "Student's t-test"
      }
    }
  } else if (test.type == "np") {
    if (isTRUE(paired)) {
      test.description <- "Durbin-Conover test"
    } else {
      test.description <- "Dwass-Steel-Crichtlow-Fligner test"
    }
  } else if (test.type == "r") {
    test.description <- "Yuen's trimmed means test"
  }

  # ======================= adjustment method ==============================

  # p value adjustment method description
  p.adjust.method.text <-
    p.adjust.method.description(p.adjust.method = p.adjust.method)

  # ==================== combining into a caption ==========================

  # prepare the bayes factor message
  pairwise_caption <-
    base::substitute(
      atop(
        displaystyle(top.text),
        expr =
          paste(
            "Pairwise comparisons: ",
            bold(test.description),
            "; Adjustment (p-value): ",
            bold(p.adjust.method.text)
          )
      ),
      env = base::list(
        top.text = caption,
        test.description = test.description,
        p.adjust.method.text = p.adjust.method.text
      )
    )

  # return the caption
  return(pairwise_caption)
}


#' @title Calculating `y` coordinates for the `ggsignif` comparison bars.
#' @inheritParams ggbetweenstats
#'
#' @keywords internal

ggsignif_position_calculator <- function(x, y) {
  # number of comparisons
  n_comparions <-
    length(x = utils::combn(
      x = unique(x),
      m = 2,
      simplify = FALSE
    ))

  # start position on y-axis for the ggsignif lines
  y_start <- max(y, na.rm = TRUE) * (1 + 0.025)

  # steps in which the y values need to increase
  step_length <- (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)) / 20

  # end position on y-axis for the ggsignif lines
  y_end <- y_start + (step_length * n_comparions)

  # creating a vector of positions for the ggsignif lines
  ggsignif_position <-
    seq(
      from = y_start,
      to = y_end,
      length.out = n_comparions
    )

  # return the position vector
  return(ggsignif_position)
}
