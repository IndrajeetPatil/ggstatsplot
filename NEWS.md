# ggstatsplot 0.0.11.9000

MAJOR CHANGES

  - The effect size measure for Kruskal-Wallis test has been changed from the
    more obscure H-based eta-squared statistic to more common and interpretable
    epsilon-squared.

MINOR CHANGES

  - `ggcoefstats` defaults to `bf.message = TRUE` to be consistent with the rest
    of the functions in the package.
  - `ggcoefstats` supports the following class of objects: `epi.2by2`, `negbin`,
    `emmGrid`.

# ggstatsplot 0.0.11
 
BREAKING CHANGES

  - Instead of having two separate functions that dealt with repeated measures
    (`subtitle_friedman_nonparametric`) and between-subjects
    (`subtitle_kw_nonparametric`), a single function
    `subtitle_anova_nonparametric` handles both of these designs with the
    `paired` argument determining which test is run.
  - All functions that supported Bayes Factor analysis (`type = "bf"`) will only
    return BF value and the scale used. Previously, this was a mix of parametric
    statistics and BF, which was confusing and often times misleading since
    these two types of analyses relied on different tests.
  - The default for `bf.message` has been changed from `FALSE` to `TRUE`. This
    is to make the Bayes Factor analysis more visible to the user.
    
MAJOR CHANGES

  - `ggscatterstats` returns only plot (without any statistical details) when
    the specified model is not linear (i.e., either when `method` argument is
    not `"lm"` or when `formula` is not `y ~ x`).

NEW FEATURES

  - New functions `ggwithinstats` (and its `grouped_` variant) are introduced as
    a counterpart to `ggbetweenstats` to handle repeated measures designs.
  - For repeated measures ANOVA, `subtitle_anova_nonparametric` now returns
    confidence intervals for Kendall's *W*.
  - All functions get `return` argument that can be used to return either
    `"plot"`, `"subtitle"`, or `"caption"`. This makes it unnecessary to
    remember which subtitle function is to be used where. As a result, in the
    next release, all subtitle making functions will not be exported and are
    encouraged not be used either by other developers or by users.
  - Both `subtitle_anova_robust` and `subtitle_anova_parametric` gain a new
    argument `paired` to support repeated measures designs.
  - `ggcoefstats` can support following new model objects: `drc`, `mlm`.
  - `ggcoefstats` gains `bf.message` argument to display a caption containing
    results from Bayesian random-effects meta-analysis. It therefore gains a new
    dependency: `metaBMA`.
  - `ggpiestats` and `ggcatstats` will now display Cramer's *V* as effect size
    for one-sample proportion tests.
  - All functions gain `stat.title` argument (`NULL` by default) that can be
    used to prefix the subtitle with a string of interest. This is possibly
    useful for specifying the details of the statistical test.

MINOR CHANGES

  - `pairwise_p()` function no longer outputs `conf.low` and `conf.high` columns
    when parametric *post hoc* tests are run. This is because these values were
    accurate only when no *p*-value adjustment was carried out.
  - Instead of using the internal function `cor_test_ci`, `ggscatterstats`
    instead used `SpearmanRho` function from `DescTools` package. This was done
    to reduce number of custom internal functions used to compute CIs for
    various effect sizes. `ggstatsplot` therefore gains `DescTools` as a
    dependency.
  - The `sampling.plan` argument default for `ggbarstats` function has been
    changed from `"indepMulti"` to `"jointMulti"` to be consistent with its
    sister function `ggpiestats`.
  
# ggstatsplot 0.0.10

NEW FEATURES

  - `ggcoefstats` can support following new model objects: `rjags`.
  - New `VR_dilemma` dataset for toying around with within-subjects design.
  - `subtitle_t_onesample` supports both Cohen's *d* and Hedge's *g* as effect
    sizes and also produces their confidence intervals. Additionally,
    non-central variants of these effect sizes are also supported. Thus,
    `gghistostats` and its `grouped_` variant gets two new arguments:
    `effsize.type`, `effsize.noncentral`.
  - `ggpiestats` used to display odds ratio as effect size for paired designs
    (McNemar test). But this was only working when the analysis was a 2 x 2
    contingency table. It now instead displays Cohen's G as effect size, which
    generalizes to any kind of design.

MINOR CHANGES

  - The internal function `outlier_df` to add a column specifying outlier status
    of any given data point is now exported.
  - `ggstatsplot` previously relied on an internal function `chisq_v_ci` to
    compute confidence intervals for Cramer's *V* using bootstrapping but it was
    pretty slow. It now instead relies on `rcompanion` package to compute
    confidence intervals for *V*. `ggstatsplot`, therefore, gains a new
    dependency.
  - `subtitle_mann_nonparametric` and `subtitle_t_onesample` now computes effect
    size *r* and its confidence intervals as $Z/\sqrt{N}$ (with the help of
    `rcompanion` package), instead of using Spearman correlation.

# ggstatsplot 0.0.9

BREAKING CHANGES

  - `subtitle_t_onesample` no longer has `data` as the optional argument. This
    was done to be consistent with other subtitle helper functions.

NEW FEATURES

  - New function `ggbarstats` (and its `grouped_` variant) introduced for making
    bar charts (thanks to #78).
  - `ggcoefstats` also displays a caption with model summary when meta-analysis
    is required.
  - `gghistostats` and its `grouped_` variant has a new argument `normal.curve`
    to superpose a normal distribution curve on top of the histogram (#138).
  - `ggcoefstats` can support following new regression model objects: `brmsfit`,
    `gam`, `Gam`, `gamlss`, `mcmc`, `mjoint`, `stanreg`.
  - New function to convert plots which are not of `gg`/`ggplot` class to
    `ggplot` class objects.
  - Instead of using `effsize` to compute Cohen's *d* and Hedge's *g*,
    `ggstatsplot` now relies on a new (#159) internal function
    `effect_t_parametric` to compute them. This removes `effsize` from
    dependencies.
  - To be consistent with other functions in this package, both `ggbarstats` and
    `ggpiestats` gain `results.subtitle` which can be set to `FALSE` if
    statistical analysis is not required, in which case `subtitle` argument can
    be used to provide alternative subtitle.

MAJOR CHANGES
   
  - `ggbetweenstats` now defaults to using noncentral-*t* distribution for
    computing Cohen's *d* and Hedge's *g*. To get variants with central-*t*
    distribution, use `effsize.noncentral = FALSE`.
   
MINOR CHANGES

  - All `grouped_` functions had argument `title.prefix` that defaulted to
    `"Group"`. It now instead defaults to `NULL`, in which case the prefix will
    variable name for `grouping.var` argument.
  - To accommodate non-parametric tests, `subtitle_template` function can now
    work with `parameter = NULL`.
  - For `ggbetweenstats`, details contained in the subtitle for non-parametric
    test are modified. It now uses Spearman's *rho*-based effect size estimates.
    This removes `coin` from dependencies.
  - `ggbetweenstats` and its `grouped_` variant gain a new argument
    `axes.range.restrict` (which defaults to `FALSE`). This restricts `y`-axes
    limits to minimum and maximum of `y` variable. This is what these functions
    were doing by default in the past versions, which created issues for
    additional ggplot components using the `ggplot.component` argument.
  - All bayes factor related subtitle and captions replace `prior.width` with
    `r_{Cauchy}`.
  - `ggcoefstats` passes dots (`...`) to `augment` method from `broom`.

BUG FIXES

  - The helper function `bf_extractor` no longer provides option to extract
    information about posterior distribution because these details were
    incorrect. The `posterior = TRUE` details were not used anywhere in the
    package so nothing about the results changes.
  - `ggcorrmat` didn't output pair names when `output == "ci"` was used. This is
    fixed.

# ggstatsplot 0.0.8

NEW FEATURES

  - `ggcoefstats` gains `meta.analytic.effect` that can be used to carry out
    meta-analysis on regression estimates. This especially useful when a
    dataframe with regression estimates and standard error is available from
    prior analyses. The `subtitle` is prepared with the new function
    `subtitle_meta_ggcoefstats` which is also exported.
  - `ggbetweenstats`, `ggscatterstats`, `gghistostats`, and `ggdotplotstats`
    (and their `grouped_` variants) all gain a new `ggplot.component` argument.
    This argument will primarily be helpful to change the individual plots in a
    `grouped_` plot.
  - `ggcoefstats` can support following new regression model objects: `polr`,
    `survreg`, `cch`, `Arima`, `biglm`, `glmmTMB`, `coxph`, `ridgelm`, `aareg`,
    `plm`, `nlrq`, `ivreg`, `ergm`, `btergm`, `garch`, `gmm`, `lmodel2`,
    `svyolr`, `confusionMatrix`, `multinom`, `nlmerMod`, `svyglm`, `MCMCglmm`,
    `lm.beta`, `speedlm`, `fitdistr`, `mle2`, `orcutt`, `glmmadmb`.

BUG FIXES

  - `ggcoefstats` didn't work when `statistic` argument was set to `NULL`. This
    was not expected behavior. This has been fixed. Now, if `statistic` is not
    specified, only the dot-and-whiskers will be shown without any labels.
  - `subtitle_t_parametric` was producing incorrect sample size information when
    `paired = TRUE` and the data contained `NA`s. This has been fixed.

MAJOR CHANGES

  - `ggscatterstats` and its `grouped_` variant accept both character and bare
    exressions as input to arguments `label.var` and `labe.expression` (#110).
  - To be consistent with rest of the functions in the package, both Pearson's
    *r*, Spearman's *rho*, and robust percentage bend correlations also display
    information about statistic associated with these tests.
  - `ggscatterstats`, by default, showed jittered data points (because it relied
    on `position_jitter` defaults). This could be visually inaccurate and,
    therefore, `ggscatterstats` now displays points without any jitter. The user
    can introduce jitter if they wish to using `point.width.jitter` and
    `point.height.jitter` arguments. For similar reasons, for `ggbetweenstats`
    and its `grouped_` variant, `point.jitter.height` default has been changed
    from `0.1` to `0` (no vertical jitter, i.e.).

MINOR CHANGES

  - Confidence interval for Kendall's *W* is now computed using
    `stats::kruskal.test`. As a result, `PMCMRplus` removed from dependencies.
  - `ggcoefstats` gains a `caption` argument. If `caption.summary` is set to
    `TRUE`, the specified caption will be added on top of the `caption.summary`.
  
# ggstatsplot 0.0.7

BUG FIXES

  - `ggcoefstats` was showing wrong confidence intervals for `merMod` class
    objects due to a bug in the `broom.mixed` package
    (<https://github.com/bbolker/broom.mixed/issues/30#issuecomment-428385005>).
    This was fixed in `broom.mixed` and so `ggcoefstats` should no longer have
    any issues.
  - `specify_decimal_p` has been modified because it produced incorrect results
    when `k < 3` and `p.value = TRUE` (e.g., `0.002` was printed as `< 0.001`).
  - `ggpiestats` produced incorrect results if some levels of the factor had
    been filtered out prior to using this function. It now drops unused levels
    and produces correct results.
  - `gghistostats` wasn't filtering out `NA`s properly. This has been fixed.

MAJOR CHANGES

  - New function `ggdotplotstats` for creating a dot plot/chart for labelled
    numeric data.
  - All primary functions gain `conf.level` argument to control confidence level
    for effect size measures.
  - As per APA guidelines, all results show results with two decimal places.
    That is, the default value for `k` argument for all functions has been
    changed from `3` to `2`.
  - All helper functions for the `ggbetweenstats` subtitles have been renamed to
    remove `_ggbetween_` from their names as this was becoming confusing for the
    user. Some of these functions work both with the between- and
    within-subjects designs, so having `_ggbetween_` in their names made users
    suspect if they could use these functions for within-subjects designs.
  - `ggstatsplot` now depends on `R 3.5.0`. This is because some of its
    dependencies require 3.5.0 to work (e.g., `broom.mixed`).
  - All `theme_` functions are now exported (`theme_pie()`, `theme_corrmat()`).   
  - `ggbetweenstats` now supports multiple pairwise comparison tests
    (parametric, nonparametric, and robust variants). It gains a new dependency
    `ggsignif`.
  - `ggbetweenstats` now supports eta-squared and omega-squared effect sizes for
    anova models. This function gains a new argument `partial`. 
  - Following functions are now reexported from the `groupedstats` package to
    avoid repeating the same code in two packages: `specify_decimal_p`,
    `signif_column`, `lm_effsize_ci`, and `set_cwd`. Therefore, `groupedstats`
    is now added as a dependency.
  - `gghistostats` can now show both counts and proportions information on the
    same plot when `bar.measure` argument is set to `"mix"`.
  - `ggcoefstats` works with tidy dataframes.
  - The helper function `untable` has been deprecated in light of
    `tidyr::uncount`, which does exactly what `untable` was doing. The author
    wasn't aware of this function when `untable` was written.
  - All vignettes have been removed from `CRAN` to reduce the size of the
    package. They are now available on the package website:
    <https://indrajeetpatil.github.io/ggstatsplot/articles/>.
  - `subtitle_t_robust` function can now handle dependent samples and
    gains `paired` argument.
  - A number of tidyverse operators are now reexported by `ggstatsplot`: `%>%`,
    `%<>%`, `%$%`.

MINOR CHANGES

  - `ggscatterstats`, `ggpiestats`, and their `grouped_` variant support bayes
    factor tests and gain new arguments relevant to this test.
  - Effect size and their confidence intervals now available for Kruskal-Wallis
    test.
  - Minor stylistic changes to how symbols for partial-eta-/omega-squared were
    being displayed in subtitles.
  - `ggbetweenstats` supports bayes factor tests for anova designs.
  - `ggpiestats` (and its `grouped_` version) gain `slice.label` argument that
    decides what information needs to be displayed as a label on the slices of
    the pie chart: `"percentage"` (which has been the default thus far),
    `"counts"`, or `"both"`.
  - `ggcorrmat` can work with `cor.vars = NULL`. In such case, **all** numeric
    variables from the provided dataframe will be used for computing the
    correlation matrix.
  - Given the constant changes to the default behavior of functions, the
    lifecycle badge has been changed from `stable` to `maturing`.
  - When the number of colors needed by a function exceeds the number of colors
    contained in a given palette, informative message is displayed to the user
    (with the new internal function `palette_message()`).
  - Several users had requested an easier way to turn off subtitles with results
    from tests (which was already implemented in `ggscatterstats` and
    `gghistostats` with the argument `results.subtitle`), so `ggbetweenstats`
    also gains two new arguments to do this: `results.subtitle` and `subtitle`.
  - New dataset added: `iris_long`.
  - More tests added and the code coverage has now jumped to over 75%.
  - To avoid code repetition, there is a now a function that produces a generic
    message any time confidence intervals for effect size estimate are computed
    using bootstrapping.

# ggstatsplot 0.0.6

MAJOR CHANGES

  - The package now exports all functions used to create text expressions with
    results. This makes it easy for people to use these results in their own
    plots at any location they want (and not just in `subtitle`, the current
    default for `ggstatsplot`).
  - `ggcorrmat` gains `p.adjust.method` argument which allows *p*-values for
    correlations to be corrected for multiple comparisons.
  - `ggscatterstats` gains `label.var` and `label.expression` arguments to
    attach labels to points.
  - `gghistostats` now defaults to not showing (redundant) color gradient
    (`fill.gradient = FALSE`) and shows both `"count"` and `"proportion"` data.
    It also gains a new argument `bar.fill` that can be used to fill bars with a
    uniform color.
  - `ggbetweenstats`, `ggcoefstats`, `ggcorrmat`, `ggscatterstats`, and
    `ggpiestats` now support all palettes contained in the `paletteer` package.
    This helps avoid situations where people had large number of groups (\> 12)
    and there were not enough colors in any of the `RColorBrewer` palettes.
  - `ggbetweenstats` gains `bf.message` argument to display bayes factors in
    favor of the null (currently works only for parametric t-test).
  - `gghistostats` function no longer has `line.labeller.y` argument; this
    position is automatically determined now.

BREAKING CHANGES

  - `legend.title.margin` function has been deprecated since `ggplot2 3.0.0`
    has improved on the margin issues from previous versions. All functions that
    wrapped around this function now lose the relevant arguments
    (`legend.title.margin`, `t.margin`, `b.margin`).
  - The argument `ggstatsplot.theme` has been changed to `ggstatsplot.layer` for
    `ggcorrmat` function to be consistent across functions.
  - For consistency, `conf.level` and `conf.type` arguments for `ggbetweenstats`
    have been deprecated. No other function in the package allowed changing
    confidence interval or their type for effect size estimation. These
    arguments were relevant only for `robust` tests anyway.
  - `ggocorrmat` argument `type` has been changed to `matrix.type` because for
    all other functions `type` argument specifies the type of the test, while
    for this function it specified the display of the visualization matrix.
    This will make the syntax more consistent across functions.
  - `ggscatterstats` gains new arguments to specify aesthetics for geom point
    (`point.color`, `point.size`, `point.alpha`). To be consistent with this
    naming schema, the `width.jitter` and `height.jitter` arguments have been
    renamed to `point.width.jitter` and `point.height.jitter`, resp.

MINOR CHANGES

  - `gghistostats`: To be compatible with `JASP`, natural logarithm of Bayes
    Factors is displayed, and not base 10 logarithm.
  - `ggscatterstats` gains `method` and `formula` arguments to modify smoothing
    functions.
  - `ggcorrmat` can now show `robust` correlation coefficients in the matrix
    plot.
  - For `gghistostats`, `binwidth` value, if not specified, is computed with
    `(max-min)/sqrt(n)`. This is basically to get rid of the warnings ggplot2
    produces. Thanks to Chuck Powell's PR (\#43).
  - `ggcoefstats` gains a new argument `partial` and can display eta-squared and
    omega-squared effect sizes for anovas, in addition to the prior partial
    variants of these effect sizes.
  - `ggpiestats` gains `perc.k` argument to show desired number of decimal
    places in percentage labels.

BUG FIXES

  - `grouped_ggpiestats` wasn't working when only `main` variable was provided
    with `counts` data. Fixed that.
  
# ggstatsplot 0.0.5

MAJOR CHANGES

  - For the sake of consistency, `theme_mprl` is now called `theme_ggstatsplot`.
    The `theme_mprl` function will still be around and will **not** be deprecated,
    so feel free to use either or both of them since they are identical.
  - `ggcoefstats` no longer has arguments `effects` and `ran_params` because
    only fixed effects are shown for mixed-effects models.
  - `ggpiestats` can now handle within-subjects designs (McNemar test results
    will be displayed).
     
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
    function. This argument was relevant for the first avatar of this function,
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
    `centrality.para`. This was due to possible confusion about interpretation of
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
