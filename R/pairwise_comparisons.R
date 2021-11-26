#' @title Multiple pairwise comparison tests with tidy data
#' @name pairwise_comparisons
#'
#' @description
#'
#' Calculate parametric, non-parametric, robust, and Bayes Factor pairwise
#' comparisons between group levels with corrections for multiple testing.
#'
#' @inheritParams statsExpressions::long_to_wide_converter
#' @inheritParams statsExpressions::stats_type_switch
#' @inheritParams statsExpressions::oneway_anova
#' @param p.adjust.method Adjustment method for *p*-values for multiple
#'   comparisons. Possible methods are: `"holm"` (default), `"hochberg"`,
#'   `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"`.
#' @param ... Additional arguments passed to other methods.
#' @inheritParams stats::t.test
#' @inheritParams WRS2::rmmcp
#'
#' @return A tibble dataframe containing two columns corresponding to group
#'   levels being compared with each other (`group1` and `group2`) and `p.value`
#'   column corresponding to this comparison. The dataframe will also contain a
#'   `p.value.label` column containing a *label* for this *p*-value, in case
#'   this needs to be displayed in `ggsignif::geom_ggsignif`. In addition to
#'   these common columns across the different types of statistics, there will
#'   be additional columns specific to the `type` of test being run.
#'
#'   This function provides a unified syntax to carry out pairwise comparison
#'   tests and internally relies on other packages to carry out these tests. For
#'   more details about the included tests, see the documentation for the
#'   respective functions:
#'   - *parametric* : [stats::pairwise.t.test()] (paired) and
#'   [PMCMRplus::gamesHowellTest()] (unpaired)
#'   - *non-parametric* :
#'   [PMCMRplus::durbinAllPairsTest()] (paired) and
#'   [PMCMRplus::kwAllPairsDunnTest()] (unpaired)
#'   - *robust* :
#'   [WRS2::rmmcp()] (paired) and [WRS2::lincon()] (unpaired)
#'   - *Bayes Factor* : [BayesFactor::ttestBF()]
#'
#' @importFrom stats p.adjust pairwise.t.test na.omit aov
#' @importFrom WRS2 lincon rmmcp
#'
#' @references For more, see:
#' <https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/pairwise.html>
#'
#' @examples
#' \donttest{
#' if (require("PMCMRplus")) {
#'   # for reproducibility
#'   set.seed(123)
#'   library(ggstatsplot)
#'   library(statsExpressions) # for data
#'
#'   # show all columns and make the column titles bold
#'   # as a user, you don't need to do this; this is just for the package website
#'   options(tibble.width = Inf, pillar.bold = TRUE, pillar.neg = TRUE, pillar.subtle_num = TRUE)
#'
#'   #------------------- between-subjects design ----------------------------
#'
#'   # parametric
#'   # if `var.equal = TRUE`, then Student's t-test will be run
#'   pairwise_comparisons(
#'     data            = mtcars,
#'     x               = cyl,
#'     y               = wt,
#'     type            = "parametric",
#'     var.equal       = TRUE,
#'     paired          = FALSE,
#'     p.adjust.method = "none"
#'   )
#'
#'   # if `var.equal = FALSE`, then Games-Howell test will be run
#'   pairwise_comparisons(
#'     data            = mtcars,
#'     x               = cyl,
#'     y               = wt,
#'     type            = "parametric",
#'     var.equal       = FALSE,
#'     paired          = FALSE,
#'     p.adjust.method = "bonferroni"
#'   )
#'
#'   # non-parametric (Dunn test)
#'   pairwise_comparisons(
#'     data            = mtcars,
#'     x               = cyl,
#'     y               = wt,
#'     type            = "nonparametric",
#'     paired          = FALSE,
#'     p.adjust.method = "none"
#'   )
#'
#'   # robust (Yuen's trimmed means *t*-test)
#'   pairwise_comparisons(
#'     data            = mtcars,
#'     x               = cyl,
#'     y               = wt,
#'     type            = "robust",
#'     paired          = FALSE,
#'     p.adjust.method = "fdr"
#'   )
#'
#'   # Bayes Factor (Student's *t*-test)
#'   pairwise_comparisons(
#'     data   = mtcars,
#'     x      = cyl,
#'     y      = wt,
#'     type   = "bayes",
#'     paired = FALSE
#'   )
#'
#'   #------------------- within-subjects design ----------------------------
#'
#'   # parametric (Student's *t*-test)
#'   pairwise_comparisons(
#'     data            = bugs_long,
#'     x               = condition,
#'     y               = desire,
#'     subject.id      = subject,
#'     type            = "parametric",
#'     paired          = TRUE,
#'     p.adjust.method = "BH"
#'   )
#'
#'   # non-parametric (Durbin-Conover test)
#'   pairwise_comparisons(
#'     data            = bugs_long,
#'     x               = condition,
#'     y               = desire,
#'     subject.id      = subject,
#'     type            = "nonparametric",
#'     paired          = TRUE,
#'     p.adjust.method = "BY"
#'   )
#'
#'   # robust (Yuen's trimmed means t-test)
#'   pairwise_comparisons(
#'     data            = bugs_long,
#'     x               = condition,
#'     y               = desire,
#'     subject.id      = subject,
#'     type            = "robust",
#'     paired          = TRUE,
#'     p.adjust.method = "hommel"
#'   )
#'
#'   # Bayes Factor (Student's *t*-test)
#'   pairwise_comparisons(
#'     data       = bugs_long,
#'     x          = condition,
#'     y          = desire,
#'     subject.id = subject,
#'     type       = "bayes",
#'     paired     = TRUE
#'   )
#' }
#' }
#' @export

# function body
pairwise_comparisons <- function(data,
                                 x,
                                 y,
                                 subject.id = NULL,
                                 type = "parametric",
                                 paired = FALSE,
                                 var.equal = FALSE,
                                 tr = 0.2,
                                 bf.prior = 0.707,
                                 p.adjust.method = "holm",
                                 k = 2L,
                                 ...) {
  # standardize stats type
  type <- stats_type_switch(type)

  # fail early if the needed package is not available
  if (type != "robust") check_if_installed("PMCMRplus", reason = "for pairwise comparisons")

  # ensure the arguments work quoted or unquoted
  c(x, y) %<-% c(ensym(x), ensym(y))

  # dataframe -------------------------------

  # cleaning up dataframe
  data %<>%
    long_to_wide_converter(
      x          = {{ x }},
      y          = {{ y }},
      subject.id = {{ subject.id }},
      paired     = paired,
      spread     = FALSE
    )

  # for some tests, it's better to have these as vectors
  x_vec <- data %>% pull({{ x }})
  y_vec <- data %>% pull({{ y }})
  g_vec <- data$rowid
  .f.args <- list(...)

  # parametric ---------------------------------

  if (type %in% c("parametric", "bayes")) {
    if (var.equal || paired) {
      c(.f, test.details) %<-% c(stats::pairwise.t.test, "Student's t-test")
    } else {
      c(.f, test.details) %<-% c(PMCMRplus::gamesHowellTest, "Games-Howell test")
    }
  }

  # nonparametric ----------------------------

  if (type == "nonparametric") {
    if (!paired) c(.f, test.details) %<-% c(PMCMRplus::kwAllPairsDunnTest, "Dunn test")
    if (paired) c(.f, test.details) %<-% c(PMCMRplus::durbinAllPairsTest, "Durbin-Conover test")

    # `exec` fails otherwise for `pairwise.t.test` because `y` is passed to `t.test`
    .f.args <- list(y = y_vec, ...)
  }

  # running the appropriate test
  if (type != "robust") {
    df <- suppressWarnings(exec(
      .fn             = .f,
      # Dunn, Games-Howell, Student's t-test
      x               = y_vec,
      g               = x_vec,
      # Durbin-Conover test
      groups          = x_vec,
      blocks          = g_vec,
      # Student
      paired          = paired,
      # common
      p.adjust.method = "none",
      # problematic for other methods
      !!!.f.args
    )) %>%
      tidy_model_parameters(.) %>%
      rename(group2 = group1, group1 = group2)
  }

  # robust ----------------------------------

  # extracting the robust pairwise comparisons
  if (type == "robust") {
    if (!paired) {
      c(.ns, .fn) %<-% c("WRS2", "lincon")
      .f.args <- list(formula = new_formula(y, x), data = data, method = "none")
    } else {
      c(.ns, .fn) %<-% c("WRS2", "rmmcp")
      .f.args <- list(y = quote(y_vec), groups = quote(x_vec), blocks = quote(g_vec))
    }

    # cleaning the raw object and getting it in the right format
    df <- eval(call2(.ns = .ns, .fn = .fn, tr = tr, !!!.f.args)) %>%
      tidy_model_parameters(.)

    # test details
    test.details <- "Yuen's trimmed means test"
  }

  # Bayesian --------------------------------

  if (type == "bayes") {
    # combining results into a single dataframe and returning it
    df_tidy <- purrr::map_dfr(
      # creating a list of dataframes with subsections of data
      .x = purrr::map2(
        .x = as.character(df$group1),
        .y = as.character(df$group2),
        .f = function(a, b) droplevels(filter(data, {{ x }} %in% c(a, b)))
      ),
      # internal function to carry out BF t-test
      .f = ~ two_sample_test(
        data     = .x,
        x        = {{ x }},
        y        = {{ y }},
        paired   = paired,
        bf.prior = bf.prior,
        type     = "bayes"
      )
    ) %>%
      filter(term == "Difference") %>%
      rowwise() %>%
      mutate(label = paste0("list(~log[e](BF['01'])==", format_value(-log_e_bf10, k), ")")) %>%
      ungroup() %>%
      mutate(test.details = "Student's t-test")

    # combine it with the other details
    df <- bind_cols(select(df, group1, group2), df_tidy)
  }

  # cleanup ----------------------------------

  # final cleanup for p-value labels
  df %<>%
    mutate_if(.predicate = is.factor, .funs = ~ as.character(.)) %>%
    arrange(group1, group2) %>%
    select(group1, group2, everything())

  # clean-up for non-Bayes tests
  if (type != "bayes") {
    df %<>%
      mutate(p.value = stats::p.adjust(p = p.value, method = p.adjust.method)) %>%
      mutate(
        test.details = test.details,
        p.value.adjustment = p_adjust_text(p.adjust.method)
      ) %>%
      rowwise() %>%
      mutate(
        label = case_when(
          p.value.adjustment != "None" ~ paste0(
            "list(~italic(p)[", p.value.adjustment, "-corrected]==", format_value(p.value, k), ")"
          ),
          TRUE ~ paste0("list(~italic(p)[uncorrected]==", format_value(p.value, k), ")")
        )
      ) %>%
      ungroup()
  }

  # return
  as_tibble(df)
}


#' @title *p*-value adjustment method text
#' @name p_adjust_text
#'
#' @description
#'
#' Preparing text to describe which *p*-value adjustment method was used
#'
#' @return Standardized text description for what method was used.
#'
#' @inheritParams pairwise_comparisons
#'
#' @examples
#' p_adjust_text("none")
#' p_adjust_text("BY")
#' @keywords internal
#' @noRd

p_adjust_text <- function(p.adjust.method) {
  case_when(
    grepl("^n|^bo|^h", p.adjust.method) ~ paste0(
      toupper(substr(p.adjust.method, 1, 1)),
      substr(p.adjust.method, 2, nchar(p.adjust.method))
    ),
    grepl("^BH|^f", p.adjust.method) ~ "FDR",
    grepl("^BY", p.adjust.method) ~ "BY",
    TRUE ~ "Holm"
  )
}


#' @name pairwise_caption
#' @title Pairwise comparison test expression
#'
#' @description
#'
#' This returns an expression containing details about the pairwise comparison
#' test and the *p*-value adjustment method. These details are typically
#' included in the `{ggstatsplot}` package plots as a caption.
#'
#' @param test.description Text describing the details of the test.
#' @param caption Additional text to be included in the plot.
#' @param pairwise.display Decides *which* pairwise comparisons to display.
#'   Available options are:
#'   - `"significant"` (abbreviation accepted: `"s"`)
#'   - `"non-significant"` (abbreviation accepted: `"ns"`)
#'   - `"all"`
#'
#'   You can use this argument to make sure that your plot is not uber-cluttered
#'   when you have multiple groups being compared and scores of pairwise
#'   comparisons being displayed.
#' @param ... Ignored.
#'
#' @examples
#' pairwise_caption("my caption", "Student's t-test")
#' @keywords internal
#' @noRd

pairwise_caption <- function(caption,
                             test.description,
                             bf.message,
                             pairwise.display = "significant",
                             ...) {

  # ' needs to be escaped inside glue
  test <- sub("'", "\\'", test.description, fixed = TRUE)
  display <- case_when(
    substr(pairwise.display, 1L, 1L) == "s" ~ "only significant",
    substr(pairwise.display, 1L, 1L) == "n" ~ "only non-significant",
    TRUE ~ "all"
  )

  # returned parsed glue expression
  if (bf.message) {
    parse(text = glue("atop(displaystyle({caption}),
                    list('Pairwise test:'~bold('{test}'), 'Comparisons shown:'~bold('{display}')))"))
  } else {
    if (!is.null(caption)) {
      parse(text = glue("atop(displaystyle('{caption}'),
                    list('Pairwise test:'~bold('{test}'), 'Comparisons shown:'~bold('{display}')))"))
    } else {
      parse(text = glue("list('Pairwise test:'~bold('{test}'), 'Comparisons shown:'~bold('{display}'))"))
    }
  }
}
