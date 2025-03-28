#' @title Dot plot/chart for labeled numeric data.
#' @name ggdotplotstats
#'
#' @description
#'
#' A dot chart (as described by William S. Cleveland) with statistical details
#' from one-sample test.
#'
#' @param ... Currently ignored.
#' @param y Label or grouping variable.
#' @inheritParams gghistostats
#' @inheritParams ggcoefstats
#' @inheritParams ggbetweenstats
#'
#' @param conf.plotting Logical; if TRUE, adds 95% confidence intervals around the mean points.
#' @param conf.level Numeric; the confidence level for the intervals (default is 0.95).
#' @param conf.plot.args List; additional arguments passed to `ggdist::stat_pointinterval()` for customizing the confidence intervals.
#'
#' @seealso \code{\link{grouped_gghistostats}}, \code{\link{ggdotplotstats}},
#'  \code{\link{gghistostats}}
#' @export
ggdotplotstats <- function(
    data,
    x,
    y,
    xlab = NULL,
    ylab = NULL,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    type = "parametric",
    test.value = 0,
    bf.prior = 0.707,
    bf.message = TRUE,
    effsize.type = "g",
    conf.level = 0.95,  # Niveau de confiance pour l'intervalle
    tr = 0.2,
    digits = 2L,
    results.subtitle = TRUE,
    point.args = list(color = "black", size = 3, shape = 16),
    centrality.plotting = TRUE,
    centrality.type = type,
    centrality.line.args = list(color = "blue", linewidth = 1, linetype = "dashed"),  # Remplacement ici
    ggplot.component = NULL,
    ggtheme = ggstatsplot::theme_ggstatsplot(),
    conf.plotting = TRUE,
    conf.plot.args = list(),
    ...
) {
  # Data processing -----------------------------------
  c(x, y) %<-% c(ensym(x), ensym(y))
  type <- stats_type_switch(type)

  data %<>%
    select({{ x }}, {{ y }}) %>%
    tidyr::drop_na() %>%
    mutate({{ y }} := droplevels(as.factor({{ y }}))) %>%
    summarise(
      mean_x = mean({{ x }}),
      se_x = sd({{ x }}) / sqrt(n()),  # Calcul de l'erreur standard
      .by = {{ y }}
    ) %>%
    arrange(mean_x) %>%
    mutate(
      percent_rank = percent_rank(mean_x),
      rank = row_number(),
      ci_lower = mean_x - qnorm(1 - (1 - conf.level) / 2) * se_x,  # Calcul de la borne inférieure de l'intervalle de confiance
      ci_upper = mean_x + qnorm(1 - (1 - conf.level) / 2) * se_x   # Calcul de la borne supérieure de l'intervalle de confiance
    )

  # Statistical analysis ------------------------------------------
  if (results.subtitle) {
    .f.args <- list(
      data = data,
      x = {{ x }},
      test.value = test.value,
      effsize.type = effsize.type,
      conf.level = conf.level,
      digits = digits,
      tr = tr,
      bf.prior = bf.prior
    )
    #On utilise "ggstatsplot:::" pour accéder à la fonction interne
    #Attention l'implémentation peut changer dans les futures mises à jour du package
    #peut être à modifier plus tard.
    subtitle_df <- ggstatsplot:::.eval_f(one_sample_test, !!!.f.args, type = type)
    subtitle <- ggstatsplot:::.extract_expression(subtitle_df)

    if (type == "parametric" && bf.message) {
      #Idem pour "ggstatsplot:::"
      caption_df <- ggstatsplot:::.eval_f(one_sample_test, !!!.f.args, type = "bayes")
      caption <- ggstatsplot:::.extract_expression(caption_df)
    }
  }

  # Plot -----------------------------------
  plot_dot <- ggplot(data, mapping = aes(x = mean_x, y = rank)) +
    exec(geom_point, !!!point.args) +
    scale_y_continuous(
      name = ylab,
      labels = pull(data, {{ y }}),
      breaks = data$rank,
      sec.axis = dup_axis(
        name   = "percentile",
        breaks = seq(1, nrow(data), (nrow(data) - 1) / 4),
        labels = 25 * 0:4
      )
    )

  # Ajout des intervalles de confiance --------------------------
  if (conf.plotting) {
    plot_dot <- plot_dot +
      # Ici cette fonction permet l'ajout de barres horizontales représentant
      #les intervalles de confiance.
      geom_errorbarh(
        aes(xmin = ci_lower, xmax = ci_upper, y = rank),
        color = "red",  # Personnalisation de la couleur des barres
        linewidth = 1,  #Largeur des barres d'erreur
        height = 0.2  # Largeur des barres d'erreur (verticale)
      )
  }

  # Centrality plotting -------------------------------------
  if (isTRUE(centrality.plotting)) {
    #Idem pour "ggstatsplot:::"
    plot_dot <- ggstatsplot:::.histo_labeller(
      plot_dot,
      x = pull(data, mean_x),
      type = stats_type_switch(centrality.type),
      tr = tr,
      digits = digits,
      centrality.line.args = centrality.line.args
    )
  }

  # Annotations -------------------------
  plot_dot +
    labs(
      x = xlab %||% as_name(x),
      y = ylab %||% as_name(y),
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    ggtheme +
    ggplot.component
}


#' @title Grouped histograms for distribution of a labeled numeric variable
#' @name grouped_ggdotplotstats
#'
#' @description
#'
#' Helper function for `ggstatsplot::ggdotplotstats` to apply this function
#' across multiple levels of a given factor and combining the resulting plots
#' using `ggstatsplot::combine_plots`.
#'
#' @inheritParams ggdotplotstats
#' @inheritParams grouped_ggbetweenstats
#' @inheritDotParams ggdotplotstats -title
#'
#' @seealso \code{\link{grouped_gghistostats}}, \code{\link{ggdotplotstats}},
#'  \code{\link{gghistostats}}
#'
#' @autoglobal
#'
#' @inherit ggdotplotstats return references
#' @inherit ggdotplotstats return details
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' # for reproducibility
#' set.seed(123)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # removing factor level with very few no. of observations
#' df <- filter(ggplot2::mpg, cyl %in% c("4", "6", "8"))
#'
#' # plot
#' grouped_ggdotplotstats(
#'   data         = df,
#'   x            = cty,
#'   y            = manufacturer,
#'   grouping.var = cyl,
#'   test.value   = 15.5
#' )
#' @export
grouped_ggdotplotstats <- function(
    data,
    ...,
    grouping.var,
    plotgrid.args = list(),
    annotation.args = list()
) {
  .grouped_list(data, {{ grouping.var }}) %>%
    purrr::pmap(.f = ggdotplotstats, ...) %>%
    combine_plots(plotgrid.args, annotation.args)
}
