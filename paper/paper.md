---
title: "Enhancing 'ggplot2' : The 'ggstatsplot' approach"
tags:
  - R
  - parametric statistics
  - nonparametric statistics
  - robust statistics
  - Bayesian statistics
  - ggplot2
  - ggplot2-extension
authors:
  - name: Indrajeet Patil
    orcid: 0000-0003-1995-6531
    affiliation: 1
affiliations:
  - name: Center for Humans and Machines, Max Planck Institute for Human Development, Berlin, Germany
citation_author: Patil
date: "2021-03-22"
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
link-citations: yes
header-includes:
  - \usepackage{tabularx}
  - \usepackage{booktabs}
  - \usepackage{tikz}
  - \usepackage{float}
---



# Summary

[`ggstatsplot`](https://indrajeetpatil.github.io/ggstatsplot/) is an extension
of [`ggplot2`](https://github.com/tidyverse/ggplot2) package for creating
graphics with details from statistical tests included in the plots themselves
and targeted primarily at behavioral sciences community to provide a one-line
code to produce information-rich plots. In a typical exploratory data analysis
workflow, data visualization and statistical modeling are two different phases:
visualization informs modeling, and modeling in its turn can suggest a
different visualization method, and so on and so forth. The central idea of
`ggstatsplot` is simple: combine these two phases into one in the form of
graphics with statistical details, which makes data exploration simpler and
faster.

# Statement of Need



# `ggstatsplot` at a glance

## Summary of types of plots included

It produces a limited kinds of ready-made plots for the supported analyses:

Function | Plot | Description
------- | ---------- | ----------------- 
`ggbetweenstats`, `ggwithinstats` | **boxviolin plots** | for comparisons *between* and *within* groups/conditions
`gghistostats` | **histograms** | for distribution of a numeric variable
`ggdotplotstats` | **dot plots/charts** | for distribution about labeled numeric variable
`ggscatterstats` | **scatterplots** | for correlation between two variables
`ggcorrmat` | **correlation matrices** | for correlations between multiple variables
`ggpiestats` | **pie charts** | for categorical data 
`ggbarstats` | **bar charts** | for categorical data 
`ggcoefstats` | **dot-and-whisker plots** | for regression models and meta-analysis

In addition to these basic plots, `ggstatsplot` also provides `grouped_`
versions (see below) that makes it easy to repeat the same analysis for
any grouping variable.

## Summary of types of statistical analyses

Most functions provide a `type` (of test) argument that is helpful to specify the
type of statistical approaches:

  - `"parametric"` (for **parametric** statistics)
  - `"nonparametric"` (for **non-parametric** statistics)
  - `"robust"` (for **robust** statistics)
  - `"bayes"` (for **Bayesian** statistics)

In the following sections, we will discuss at depth justification for why the
plots have been designed in certain ways and what principles were followed to
report statistical details on the plots.

# Licensing and Availability

`ggstatsplot` is licensed under the GNU General Public License (v3.0), with
all source code stored at
[GitHub](https://github.com/IndrajeetPatil/ggstatsplot/), and with a
corresponding issue tracker for bug reporting and feature enhancements. In the
spirit of honest and open science, we encourage requests/tips for fixes, feature
updates, as well as general questions and concerns via direct interaction with
contributors and developers, by [filing an issue](https://github.com/IndrajeetPatil/ggstatsplot/issues). See the
package's [*Contribution Guidelines*](https://github.com/IndrajeetPatil/ggstatsplot/blob/master/.github/CODE_OF_CONDUCT.md).

# Acknowledgements

I would like to acknowledge the support of Mina Cikara, Fiery Cushman, and Iyad
Rahwan during the development of this project. `ggstatsplot` relies heavily
on the [`easystats`](https://github.com/easystats/easystats) ecosystem, a
collaborative project created to facilitate the usage of `R` for statistical
analyses. Thus, I would like to thank the [members of easystats](https://github.com/orgs/easystats/people) as well as the users.

\newpage

# References
