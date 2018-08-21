# ggstatsplot 0.0.5.9000

MAJOR CHANGES
  - `gghistostats` now defaults to not showing (redundant) color gradient
  (`fill.gradient = FALSE`) and shows both `"count"` and `"proportion"` data. It
  also gains a new argument `bar.fill` that can be used to fill bars with a
  uniform color.

MINOR CHANGES
  - `gghistostats`: To be compatible with `JASP`, natural logarithm of Bayes
  Factors is displayed, and not base 10 logarithm.
  
# ggstatsplot 0.0.5

MAJOR CHANGES
  - For the sake of consistency, `theme_mprl` is now called `theme_ggstatsplot`.
    The `theme_mprl` function will still be around and will **not** be deprecated,
    so feel free to use either or both of them since they are identical.
  - `ggcoefstats` no longer has arguments `effects` and `ran_params` because
    only fixed effects are shown for mixed-effects models.
  - `ggpiestats` can now handle within-subjects designs (McNemar test).
     
BUG FIXES
  - `ggbetweenstats` was producing wrong axes labels when `sample.size.label`
    was set to `TRUE` and user had reordered factor levels before using this
    function. The new version fixes this.
  - `ggcoefstats` wasn't producing partial omega-squared for `aovlist` objects.
    Fixed that with new version of `sjstats`.

MINOR CHANGES
  - Removed the trailing comma from the robust correlation analyses.
  - `gghistostats` has a new argument to remove color fill gradient.
  - `ggbetweenstats` takes new argument `mean.ci` to show confidence intervals
    for the mean values.
  - For `lmer` models, p-values are now computed using `sjstats::p_value`. This
    removes `lmerTest` package from dependencies.
  - `sjstats` no longer suggests `apaTables` package to compute confidence
    intervals for partial eta- and omega-squared. Therefore, `apaTables` and
    `MBESS` are removed from dependencies.
  - `ggscatterstats` supports `densigram` with the development version of
    `ggExtra`. It additionally gains few extra arguments to change aesthetics of
    marginals (alpha, size, etc.).

# ggstatsplot 0.0.4

MAJOR CHANGES

  - New function: `ggcoefstats` for displaying model coefficients.
  - All functions now have `ggtheme` argument that can be used to change the
    default theme, which has now been changed from `theme_grey()` to
    `theme_bw()`.
  - The robust correlation is no longer `MASS::rlm`, but percentage bend
    correlation, as implemented in `WRS2::pbcor`. This was done to be consistent
    across different functions. `ggcorrmat` also uses percentage bend
    correlation as the robust correlation measure. This also means that
    `ggstatsplot` no longer imports `MASS` and `sfsmisc`.
  - The `data` argument is no longer `NULL` for all functions, except
    `gghistostats`. In other words, the user **must** provide a dataframe from
    which variables or formulas should be selected.
  - All subtitles containing results now also show sample size information
    (*n*). To adjust for the inflated length of the subtitle, the default
    subtitle text size has been changed from `12` to `11`.

MINOR CHANGES

  - Switched back to Shapiro-Wilk test of normality to remove `nortest` from
    imports.
  - `ggpiestats` can now handle dataframes with 
  - `ggbetweenstats` and `ggpiestats` now display sample sizes for each level of
    the groping factor by default. This behavior can be turned off by setting
    `sample.size.label` to `FALSE`.
  - Three new datasets added: `Titanic_full`, `movies_wide`, `movies_long`. 
  - Added confidence interval for effect size for robust ANOVA. 
  - The 95% CI for Cramer'V computed using `boot::boot`. Therefore,
    the package no longer imports `DescTools`. 
  - To be consistent across correlations covered, all correlations now show
    estimates for correlation coefficients, confidence intervals for the estimate,
    and *p*-values. Therefore, *t*-values and regression coefficients are no
    longer displayed for Pearson's *r*.
  - The `legend.title.margin` arguments for `gghistostats` and `ggcorrmat` now
    default to `FALSE`, since `ggplot2 3.0.0` has better legend title margins.
  - `ggpiestats` now sorts the summary dataframes not by percentages but by the
    levels of `main` variable. This was done to have the same legends across
    different levels of a grouping variable in `grouped_ggpiestats`.
  - To remove cluttered display of results in the subtitle, `ggpiestats` no
    longer shows titles for the tests run (these were "Proportion test" and
    "Chi-Square test"). From the pie charts, it should be obvious to the user or
    reader what test was run.
  - `gghistostats` also allows running robust version of one-sample test now
    (One-sample percentile bootstrap).

# ggstatsplot 0.0.3

NEW FEATURES

  - The `ggbetweenstats` function can now show notched box plots. Two new
    arguments `notch` and `notchwidth` control its behavior. The defaults are
    still standard box plots.
  - Removed warnings that were appearing when `outlier.label` argument was of
    `character` type.
  - The default color palette used for all plots is colorblind friendly.
  - `gghistostats` supports `proportion` and `density` as a value measure for
    bar heights to show proportions and density. New argument `bar.measure`
    controls this behavior.
  - `grouped_` variants of functions `ggcorrmat`, `ggscatterstats`,
    `ggbetweenstats`, and `ggpiestats` introduced to create multiple plots for
    different levels of a grouping variable.

MAJOR CHANGES
  - To be internally consistent, all functions in `ggstatsplot` use the spelling
    `color`, rather than `colour` in some functions, while `color` in others.
  - Removed the redundant argument `binwidth.adjust` from `gghistostats`
    function. This argument was relevant for the first avatar of this fucntion,
    but is no longer playing any role.
  - To be internally consistent, the argument `lab_col` and `lab_size` in
    `ggcorrmat` have been changed to `lab.col` and `lab.size`, respectively.

MINOR CHANGES

  - Added a new argument to `ggstatsplot.theme` function to control if
    `ggstatsplot::theme_mprl` is to be overlaid on top of the selected ggtheme
    (ggplot2 theme, i.e.).
  - Two new arguments added to `gghistostats` to allow user to change colorbar
    gradient. Defaults are colorblind friendly.
  - Both `gghistostats` and `ggcorrmat` have a new argument
    `legend.title.margin` to control margin adjustment between the title and the
    colorbar.
  - The vertical lines denoting test values and centrality parameters can be
    tagged with text labels with a new argument `line.labeller` in
    `gghistostats` function.

BUG FIXES

  - The `centrality.para` argument for `ggscatterstats` was not working
    properly. Choosing `"median"` didn't show median, but the mean. This is
    fixed now.
    
# ggstatsplot 0.0.2

NEW FEATURES

  - Bayesian test added to `gghistostats` and two new arguments to also display
    a vertical line for `test.value` argument.
  - Vignette added for `gghistostats`.
  - Added new function `grouped_gghistostats` to facilitate applying
    `gghistostats` for multiple levels of a grouping factor.
  - `ggbetweenstats` has a new argument `outlier.coef` to adjust threshold used
    to detect outliers. Removed bug from the same function when `outlier.label`
    argument is of factor/character type.

MAJOR CHANGES

  - Functions `signif_column` and `grouped_proptest` are now deprecated. They
    were exported in the first release by mistake.
  - Function `gghistostats` no longer displays both density and count since the
    density information was redundant. The `density.plot` argument has also been
    deprecated.
  - `ggscatterstats` argument `intercept` has now been changed to
    `centrality.para`. This was due to possible confusion about interpreation of
    these lines; they show central tendency measures and not intercept for the
    linear model. Thus the change.
  - The default for `effsize.type = "biased"` effect size for `ggbetweenstats`
    in case of ANOVA is **partial** omega-squared, and not omega-squared.
    Additionally, both partial eta- and omega-squared are not computed using
    bootstrapping with (default) 100 bootstrap samples.
    
MINOR CHANGES

  - More examples added to the `README` document.
  - 95% confidence intervals for Spearman's rho are now computed using `broom`
    package. `RVAideMemoire` package is thus removed from dependencies.
  - 95% confidence intervals for partial eta- and omega-squared for
    `ggbetweenstats` function are now computed using `sjstats` package, which
    allows bootstrapping. `apaTables` and `userfriendlyscience` packages are
    thus removed from dependencies.

# ggstatsplot 0.0.1

- First release of the package.
