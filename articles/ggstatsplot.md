# Visualizations with statistical details: The 'ggstatsplot' approach

This vignette can be cited as:

    To cite package 'ggstatsplot' in publications use:

      Patil, I. (2021). Visualizations with statistical details: The
      'ggstatsplot' approach. Journal of Open Source Software, 6(61), 3167,
      doi:10.21105/joss.03167

    A BibTeX entry for LaTeX users is

      @Article{,
        doi = {10.21105/joss.03167},
        url = {https://doi.org/10.21105/joss.03167},
        year = {2021},
        publisher = {{The Open Journal}},
        volume = {6},
        number = {61},
        pages = {3167},
        author = {Indrajeet Patil},
        title = {{Visualizations with statistical details: The {'ggstatsplot'} approach}},
        journal = {{Journal of Open Source Software}},
      }

## Summary

Graphical displays can reveal problems in a statistical model that might
not be apparent from purely numerical summaries. Such visualizations can
also be helpful for the reader to evaluate the validity of a model if it
is reported in a scholarly publication or report. But, given the onerous
costs involved, researchers often avoid preparing information-rich
graphics and exploring several statistical approaches or tests
available. The
[ggstatsplot](https://indrajeetpatil.github.io/ggstatsplot/) package in
the R programming language ([R Core Team, 2021](#ref-base2021)) provides
a one-line syntax to enrich
[ggplot2](https://ggplot2.tidyverse.org)-based visualizations with the
results from statistical analysis embedded in the visualization itself.
In doing so, the package helps researchers adopt a rigorous, reliable,
and robust data exploratory and reporting workflow.

## Statement of Need

In a typical data analysis workflow, data visualization and statistical
modeling are two different phases: visualization informs modeling, and
in turn, modeling can suggest a different visualization method, and so
on and so forth ([Wickham & Grolemund, 2016](#ref-wickham2016r)). The
central idea of
[ggstatsplot](https://indrajeetpatil.github.io/ggstatsplot/) is simple:
combine these two phases into one in the form of an informative graphic
with statistical details.

Before discussing benefits of this approach, we will show an example
(Figure 1).

``` r


ggbetweenstats(iris, Species, Sepal.Length)
```

![Example plot from the \`{ggstatsplot}\` package illustrating its
philosophy of juxtaposing informative visualizations with details from
statistical analysis. To see all supported plots and statistical
analyses, see the package website:
\url{https://indrajeetpatil.github.io/ggstatsplot/}](ggstatsplot_files/figure-html/iris-1.png)

Example plot from the
[ggstatsplot](https://indrajeetpatil.github.io/ggstatsplot/) package
illustrating its philosophy of juxtaposing informative visualizations
with details from statistical analysis. To see all supported plots and
statistical analyses, see the package website:

As can be seen, with a single line of code, the function produces
details about descriptive statistics, inferential statistics, effect
size estimate and its uncertainty, pairwise comparisons, Bayesian
hypothesis testing, Bayesian posterior estimate and its uncertainty.
Moreover, these details are juxtaposed with informative and well-labeled
visualizations. The defaults are designed to follow best practices in
both data visualization ([Cleveland, 1985](#ref-cleveland1985); [Grant,
2018](#ref-grant2018data); [Healy, 2018](#ref-healy2018data); [Tufte,
2001](#ref-tufte2001); [Wilke, 2019](#ref-wilke2019fundamentals)) and
(frequentist/Bayesian) statistical reporting ([American Psychological
Association, 2019](#ref-apa2019); [van Doorn et al.,
2020](#ref-van2020jasp)). Without
[ggstatsplot](https://indrajeetpatil.github.io/ggstatsplot/), getting
these statistical details and customizing a plot would require
significant amount of time and effort. In other words, this package
removes the trade-off often faced by researchers between ease and
thoroughness of data exploration and further cements good data
exploration habits.

Internally, data cleaning is carried out using the `tidyverse` ([Wickham
et al., 2019](#ref-Wickham2019)), while statistical analysis is carried
out via the
[statsExpressions](https://indrajeetpatil.github.io/statsExpressions/)
([Patil, 2021](#ref-Patil2021)) and `easystats` ([Ben-Shachar et al.,
2020](#ref-Ben-Shachar2020); [Lüdecke et al.,
2019](#ref-L%C3%BCdecke2019), [2020](#ref-L%C3%BCdecke2020parameters),
[2021](#ref-L%C3%BCdecke2020performance); [Makowski et al.,
2019](#ref-Makowski2019), [2020](#ref-Makowski2020)) packages. All
visualizations are constructed using the grammar of graphics framework
([Wilkinson, 2012](#ref-Wilkinson2012)), as implemented in the
[ggplot2](https://ggplot2.tidyverse.org) package ([Wickham,
2016](#ref-Wickham2016)).

## Benefits

In summary, the benefits of
[ggstatsplot](https://indrajeetpatil.github.io/ggstatsplot/)’s approach
are the following. It:

1.  produces charts displaying both raw data, and numerical plus
    graphical summary indices,

2.  avoids errors in and increases reproducibility of statistical
    reporting,

3.  highlights the importance of the effect by providing effect size
    measures by default,

4.  provides an easy way to evaluate *absence* of an effect using Bayes
    factors,

5.  encourages researchers and readers to evaluate statistical
    assumptions of a model in the context of the underlying data (Figure
    2),

6.  is easy and simple enough that someone with little to no coding
    experience can use it without making an error and may even encourage
    beginners to programmatically analyze data, instead of using GUI
    software.

![Comparing the 'Standard' approach of reporting statistical analysis in
a publication/report with the 'ggstatsplot' approach of reporting the
same analysis next to an informative graphic. Note that the results
described in the 'Standard' approach are about the 'Dinosaur' dataset
plotted on the right. Without the accompanying visualization, it is hard
to evaluate the validity of the results. The ideal reporting practice
will be a hybrid of these two approaches where the plot contains both
the visual and numerical summaries about a statistical model, while the
narrative provides interpretative context for the reported
statistics.](../reference/figures/stats_reporting_format.png)

Comparing the ‘Standard’ approach of reporting statistical analysis in a
publication/report with the ‘ggstatsplot’ approach of reporting the same
analysis next to an informative graphic. Note that the results described
in the ‘Standard’ approach are about the ‘Dinosaur’ dataset plotted on
the right. Without the accompanying visualization, it is hard to
evaluate the validity of the results. The ideal reporting practice will
be a hybrid of these two approaches where the plot contains both the
visual and numerical summaries about a statistical model, while the
narrative provides interpretative context for the reported statistics.

## Future Scope

This package is an ambitious, ongoing, and long-term project. It
currently supports common statistical tests (parametric, non-parametric,
robust, or Bayesian *t*-test, one-way ANOVA, contingency table analysis,
correlation analysis, meta-analysis, regression analyses, etc.) and
corresponding visualizations (box/violin plot, scatter plot,
dot-and-whisker plot, pie chart, bar chart, etc.). It will continue
expanding to support an increasing collection of statistical analyses
and visualizations.

## Licensing and Availability

[ggstatsplot](https://indrajeetpatil.github.io/ggstatsplot/) is licensed
under the GNU General Public License (v3.0), with all source code stored
at [GitHub](https://github.com/IndrajeetPatil/ggstatsplot/). In the
spirit of honest and open science, requests and suggestions for fixes,
feature updates, as well as general questions and concerns are
encouraged via direct interaction with contributors and developers by
filing an [issue](https://github.com/IndrajeetPatil/ggstatsplot/issues)
while respecting [*Contribution
Guidelines*](https://indrajeetpatil.github.io/ggstatsplot/CONTRIBUTING.html).

## Acknowledgements

I would like to acknowledge the support of Mina Cikara, Fiery Cushman,
and Iyad Rahwan during the development of this project.
[ggstatsplot](https://indrajeetpatil.github.io/ggstatsplot/) relies
heavily on the [`easystats`](https://github.com/easystats/easystats)
ecosystem, a collaborative project created to facilitate the usage of
`R` for statistical analyses. Thus, I would like to thank the
[members](https://github.com/orgs/easystats/people) of `easystats` as
well as the users. I would additionally like to thank the contributors
to [ggstatsplot](https://indrajeetpatil.github.io/ggstatsplot/) for
reporting bugs, providing helpful feedback, or helping with
enhancements.

## References

American Psychological Association. (2019). *Publication Manual of the
American Psychological Association, 7th Edition*. American Psychological
Association.

Ben-Shachar, M. S., Lüdecke, D., & Makowski, D. (2020). effectsize:
Estimation of effect size indices and standardized parameters. *Journal
of Open Source Software*, *5*(56), 2815.
<https://doi.org/10.21105/joss.02815>

Cleveland, W. S. (1985). *The Elements of Graphing Data* (1st edition).
Wadsworth, Inc.

Grant, R. (2018). *Data Visualization: Charts, Maps, and Interactive
Graphics*. CRC Press.

Healy, K. (2018). *Data Visualization: A Practical Introduction*.
Princeton University Press.

Lüdecke, D., Ben-Shachar, M. S., Patil, I., & Makowski, D. (2020).
Extracting, computing and exploring the parameters of statistical models
using R. *Journal of Open Source Software*, *5*(53), 2445.
<https://doi.org/10.21105/joss.02445>

Lüdecke, D., Ben-Shachar, M. S., Patil, I., Waggoner, P., & Makowski, D.
(2021). performance: An R package for assessment, comparison and testing
of statistical models. *Journal of Open Source Software*, *6*(60), 3139.
<https://doi.org/10.21105/joss.03139>

Lüdecke, D., Waggoner, P., & Makowski, D. (2019). Insight: A unified
interface to access information from model objects in R. *Journal of
Open Source Software*, *4*(38), 1412.
<https://doi.org/10.21105/joss.01412>

Makowski, D., Ben-Shachar, M. S., & Lüdecke, D. (2019). bayestestR:
Describing effects and their uncertainty, existence and significance
within the Bayesian framework. *Journal of Open Source Software*,
*4*(40), 1541. <https://doi.org/10.21105/joss.01541>

Makowski, D., Ben-Shachar, M. S., Patil, I., & Lüdecke, D. (2020).
Methods and algorithms for correlation analysis in R. *Journal of Open
Source Software*, *5*(51), 2306. <https://doi.org/10.21105/joss.02306>

Patil, I. (2021). statsExpressions: R Package for Tidy Dataframes and
Expressions with Statistical Details. *Journal of Open Source Software*,
*6*(61), 3236. <https://doi.org/10.21105/joss.03236>

R Core Team. (2021). *R: A language and environment for statistical
computing*. R Foundation for Statistical Computing.
<https://www.R-project.org/>

Tufte, E. R. (2001). *The Visual Display of Quantitative Information*
(2nd edition). Graphics Press.

van Doorn, J., van den Bergh, D., Böhm, U., Dablander, F., Derks, K.,
Draws, T., Etz, A., Evans, N. J., Gronau, Q. F., Haaf, J. M., Hinne, M.,
Kucharský, Š., Ly, A., Marsman, M., Matzke, D., Gupta, A. R. K. N.,
Sarafoglou, A., Stefan, A., Voelkel, J. G., & Wagenmakers, E.-J. (2020).
The JASP guidelines for conducting and reporting a Bayesian analysis.
*Psychonomic Bulletin & Review*, 1–14.
<https://doi.org/10.3758/s13423-020-01798-5>

Wickham, H. (2016). *ggplot2: Elegant graphics for data analysis*.
Springer-Verlag New York.

Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L. D.,
François, R., Grolemund, G., Hayes, A., Henry, L., Hester, J., Kuhn, M.,
Pedersen, T. L., Miller, E., Bache, S. M., Müller, K., Ooms, J.,
Robinson, D., Seidel, D. P., Spinu, V., … Yutani, H. (2019). Welcome to
the tidyverse. *Journal of Open Source Software*, *4*(43), 1686.
<https://doi.org/10.21105/joss.01686>

Wickham, H., & Grolemund, G. (2016). *R for Data Science*. O’Reilly
Media.

Wilke, C. O. (2019). *Fundamentals of Data Visualization*. O’Reilly
Media.

Wilkinson, L. (2012). The Grammar of Graphics. In *Handbook of
computational statistics* (pp. 375–414). Springer.
