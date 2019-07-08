#' @title Switch function to use helper function to create subtitle for the
#'   `ggbetweenstats` plot.
#' @name ggbetweenstats_switch
#' @author Indrajeet Patil
#'
#' @inheritParams ggbetweenstats
#' @param test Decides which test to run (can be either `"t-test"` or
#'   `"anova"`).
#' @param ... Arguments for respective subtitle helper function.
#'
#' @keywords internal

ggbetweenstats_switch <- function(type, test, ...) {
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

  # either t-test or anova will be run
  if (test == "t-test") {
    subtitle <- switch(
      EXPR = test.type,
      p = {
        subtitle_t_parametric(...)
      },
      np = {
        subtitle_mann_nonparametric(...)
      },
      r = {
        subtitle_t_robust(...)
      },
      bf = {
        subtitle_t_bayes(...)
      }
    )
  } else {
    subtitle <- switch(
      EXPR = test.type,
      p = {
        subtitle_anova_parametric(...)
      },
      np = {
        subtitle_anova_nonparametric(...)
      },
      r = {
        subtitle_anova_robust(...)
      },
      bf = {
        subtitle_anova_bayes(...)
      }
    )
  }

  # return the text for the subtitle
  return(subtitle)
}

#' @rdname ggbetweenstats_switch
#' @aliases ggbetweenstats_switch
#' @export

ggwithinstats_switch <- ggbetweenstats_switch

#' @title Preparing text to describe which *p*-value adjustment method was used.
#' @name p.adjust.method.description
#' @return Standardized text description for what method was used.
#'
#' @inheritParams pairwise_p
#'
#' @keywords internal

p.adjust.method.description <- function(p.adjust.method) {
  base::switch(
    EXPR = p.adjust.method,
    none = "None",
    bonferroni = "Bonferroni",
    holm = "Holm",
    hochberg = "Hochberg",
    hommel = "Hommel",
    BH = "Benjamini & Hochberg",
    fdr = "Benjamini & Hochberg",
    BY = "Benjamini & Yekutieli",
    "Holm"
  )
}

#' @title Switch function to determine which effect size is to computed.
#' @name effsize_type_switch
#' @description Takes in all allowed characters describing the needed effect
#'   size (e.g., `"d"`, `"partial_eta"`, etc.) and converts it into standard
#'   terms (`"biased"` or `"unbiased"`) to reduce the complexity of conditional
#'   statements.
#' @author Indrajeet Patil
#'
#' @param effsize.type Character describing the needed effect size.
#'
#' @keywords internal

effsize_type_switch <- function(effsize.type = NULL) {
  # figuring out which effect size to use
  if (!is.null(effsize.type)) {
    effsize.type <-
      switch(
        EXPR = effsize.type,
        d = "biased",
        g = "unbiased",
        eta = "biased",
        omega = "unbiased",
        partial_eta = "biased",
        partial_omega = "unbiased",
        partial.eta = "biased",
        partial.omega = "unbiased",
        p_eta = "biased",
        p_omega = "unbiased",
        biased = "biased",
        unbiased = "unbiased",
        "unbiased"
      )
  } else {
    effsize.type <- "unbiased"
  }

  # return the value
  return(effsize.type)
}

#' @title Switch function to determine which type of statistics is to be run.
#' @name stats_type_switch
#' @description Takes in all allowed characters describing the needed type of
#'   test and converts it into standard terms to reduce the complexity of
#'   conditional statements.
#' @author Indrajeet Patil
#'
#' @param stats.type Character describing the needed type of statistics (e.g.,
#'   `"parametric"`, `"nonparametric"`, `"robust"`, `"bayes"``, etc.).
#'
#' @keywords internal

stats_type_switch <- function(stats.type) {
  # figuring out which effect size to use
  if (!is.null(stats.type)) {
    stats.type <-
      switch(
        EXPR = stats.type,
        parametric = "parametric",
        p = "parametric",
        pearson = "parametric",
        nonparametric = "nonparametric",
        np = "nonparametric",
        "non-parametric" = "nonparametric",
        spearman = "nonparametric",
        robust = "robust",
        r = "robust",
        pb = "robust",
        bayes = "bayes",
        bf = "bayes",
        "parametric"
      )
  } else {
    stats.type <- "parametric"
  }

  # return the value
  return(stats.type)
}
