---
title: "Visualizations with statistical details: The 'ggstatsplot' approach"
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
date: "2021-03-27"
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
link-citations: yes
---



# Summary

During exploratory data analysis, the graphical displays can reveal problems in
a statistical model that might not be apparent from purely numerical summaries.
Such visualizations can also be helpful for the reader to evaluate validity of a
model if the said analysis is reported in a scholary publication/report. But
given the onerous cost of preparing information-rich graphics and exploring
several statistical approaches/tests available, researchers can avoid this
practice. The `ggstatsplot` package in R programming language [@base2021]
provides a one-line syntax to create densely informative `ggplot2`-based
visualizations with the results from statistical analysis embedded in the
visualization itself. In doing so, the package helps researchers adopt a
**rigorous, reliable, and robust** data exploratory and reporting workflow and
holds the potential to alleviate a few of the crises affecting scientific
research.

# Statement of Need

Recent meta-research has revealed a number of problems plaguing the credibility
of scientific research: findings are not replicable, codes are computationally
irreproducible, the statistical reporting is inaccurate, the effects do not
survive further robustness checks, etc. A *few* of these problems can be
alleviated simply by adopting good practices while exploring (analyzing and
visualizing) data and reporting results from statistical analysis. This is where
`ggstatsplot` comes in.

In a typical data analysis workflow, data visualization and statistical modeling
are two different phases: visualization informs modeling, and modeling in its
turn can suggest a different visualization method, and so on and so forth
[@wickham2016r]. The central idea of `ggstatsplot` is simple: combine these two
phases into one in the form of an informative graphic with statistical details.

Before discussing benefits of this approach, we will see one example (Figure 1).


```r
library(ggstatsplot)
library(palmerpenguins) # for 'penguins' dataset

ggbetweenstats(penguins, species, body_mass_g)
```

\begin{figure}
\includegraphics[width=1\linewidth]{paper_files/figure-latex/penguins-1} \caption{Example plot from the `ggstatsplot` package illustrates its philosophy of juxtaposing informative visualizations with details from statistical analysis. To see all supported plots and statistical analyses, see the package website: https://indrajeetpatil.github.io/ggstatsplot/}\label{fig:penguins}
\end{figure}

As can be seen, with a **single** line of code, the function produces details
about descriptive statistics, inferential statistics, effect size estimate and
its uncertainty, pairwise comparisons, Bayesian hypothesis testing, Bayesian
posterior estimate and its uncertainty. Moreover, these details are juxtaposed
with informative and well-labeled visualizations, designed to follow best
practices in **both** data visualization [@cleveland1985; @grant2018data;
@healy2018data; @tufte2001; @wilke2019fundamentals] and (Frequentist/Bayesian)
statistical reporting [@american1985publication; @van2020jasp]. Without
`ggstatsplot`, getting these statistical details and customizing a plot would
require significant amount of time and work. In other words, this package takes
away *an* excuse from researchers to thoroughly explore their data and instills
good data sanitation/exploration habits.

Behind the scenes, data cleaning is carried out using `tidyverse`
[@Wickham2019], while statistical analysis is carried out via `statsExpressions`
[@Patil2021] and `easystats` [@Ben-Shachar2020; @Lüdecke2020parameters;
@Lüdecke2020performance; @Lüdecke2019; @Makowski2019; @Makowski2020]. All
visualizations are constructed using `ggplot2` [@Wickham2016; @Wilkinson2012].

This package is an ambitious, ongoing, long-term project, and it will continue
to grow to support ever increasing collection of visualiazations and statistical
analyses.

# Benefits

We can now succinctly summarize the benefits of `ggstatsplot`'s approach. It-

a. produces charts displaying both raw data, and numerical plus graphical
   summary indices,

b. avoids errors in statistical reporting,

c. highlights the importance of the effect by providing effect size measures by
   default,

d. provides an easy way to evaluate *absence* of an effect using Bayesian
   framework,

e. forces to evaluate statistical assumptions of a model in the
   context of the underlying data (Figure 2), and

f. is easy and simple enough that somebody with little-to-no coding experience
   can use it without making an error.

\begin{figure}
\includegraphics[width=1\linewidth]{reporting} \caption{Comparing the standard approach of reporting statistical analysis in a scholarly publications with the `ggstatsplot` with approach of reporting the same analysis next to an informative graphic.}\label{fig:reporting}
\end{figure}

# Licensing and Availability

`ggstatsplot` is licensed under the GNU General Public License (v3.0), with all
source code stored at [GitHub](https://github.com/IndrajeetPatil/ggstatsplot/),
and with a corresponding issue tracker for bug reporting and feature
enhancements. In the spirit of honest and open science, we encourage
requests/tips for fixes, feature updates, as well as general questions and
concerns via direct interaction with contributors and developers, by filing an
[issue](https://github.com/IndrajeetPatil/ggstatsplot/issues). See the package's
[*Contribution
Guidelines*](https://indrajeetpatil.github.io/ggstatsplot/CONTRIBUTING.html).

# Acknowledgements

I would like to acknowledge the support of Mina Cikara, Fiery Cushman, and Iyad
Rahwan during the development of this project. `ggstatsplot` relies heavily on
the [`easystats`](https://github.com/easystats/easystats) ecosystem, a
collaborative project created to facilitate the usage of `R` for statistical
analyses. Thus, I would like to thank the
[members](https://github.com/orgs/easystats/people) of `easystats` as well as
the users. I would additionally like to thank the contributors to `ggstatsplot`
for reporting bugs, providing helpful feedback, or helping with enhancements.

# References

