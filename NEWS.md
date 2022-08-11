# ggstatsplot 0.9.4

N.B. All statistical analysis in `{ggstatsplot}` is carried out in
`{statsExpressions}`. Thus, to see changes related to statistical expressions,
read the `NEWS` for that package:
<https://indrajeetpatil.github.io/statsExpressions/news/index.html>

- Internal housekeeping to adjust to changes in upstream dependencies.

# ggstatsplot 0.9.3

- Hot fix release to correct a failing example in CRAN daily checks.

# ggstatsplot 0.9.2

MAJOR CHANGES

  - The `pairwise_comparions()` function implementation now lives in
    `{statsExpressions}` package, although it will continue to be exported from
    `{ggstatsplot}` package.

  - The details about pairwise test for `ggbetweenstats()` and `ggwithinstats()`
    functions are now displayed as a label for the secondary axis. Previously,
    this information was displayed in the caption. Given that caption already
    contained Bayesian test details, it was becoming difficult to stack
    different expressions on top of each other. To avoid unnecessary code
    complexity and also to avoid crowded caption, this decision was made.
    Additionally, the pairwise test label has been slightly abbreviated, and so
    is the label for significance bars. This is done to not let the text
    overwhelm the numeric values, the latter being more important.

# ggstatsplot 0.9.1

MAJOR CHANGES

  - Moves `{PMCMRplus}` package from Imports to Suggests. So, if, as a user, you
    wish to use pairwise comparisons in `ggbetweenstats()` and
    `ggwithinstats()`, you will need to download this package.

MINOR CHANGES

  - To keep the documentation maintainable, a number of vignettes have either
    been removed or they are no longer evaluated and only code is reported.

# ggstatsplot 0.9.0

NEW FEATURES

  - The `pairwise_comparisons()` function for carrying out one-way pairwise
    comparisons has now moved in `{ggstatsplot}` from `{pairwiseComparisons}`
    package.

BREAKING CHANGES

  - A number of effect size estimates and their confidence intervals have
    changed due to respective changes made in `{effectsize}` package version
    `0.5` release. For full details of these changes, see:
    <https://easystats.github.io/effectsize/news/index.html>

  - For the same reason, the effect size for one-way contingency table has
    changed from Cramer's *V* to Pearson's *C*.

MAJOR CHANGES

  - For plotting marginal distributions in `ggscatterstats`, `{ggstatsplot}` now
    relies on `ggside` package instead of `ggExtra`. This was done to remove a
    glaring inconsistency in the API. All functions in `{ggstatsplot}` produced
    `ggplot` objects and could be further modified with `ggplot2` functions,
    except `ggscatterstats`, which led to a lot of confusion among users (e.g.
    #28). This change gets rid of this inconsistency. But it comes at a cost:
    there is no more `marginal.type` argument that lets you change the type of
    marginal distribution graphic and histogram is the only possible option.
    Note that this is **not** a breaking change. Your past code will continue to
    work but it will now always produce a histogram instead of other marginal
    graphic you might have chosen.

  - Minimum needed R version is now `4.0`.

MINOR CHANGES

  - Online vignette about `combine_plots` has been removed. In case you want to
    create a grid of plots, it is highly recommended that you use `patchwork`
    package directly and not this wrapper around it which is mostly useful with
    `{ggstatsplot}` plots.

  - `ggscatterstats` labeling arguments accept only unquoted inputs now, and not
    quoted or string inputs. Allowing this was a bad design choice in the past
    since most functions in `{ggstatsplot}`, inspired by `tidyverse`, expect
    unquoted (`x`) - and not quoted (`"x"`) - arguments. So this function was
    the odd one out.

  - Gets rid of `ipmisc` dependency.

  - Removes `movies_wide` dataset, which was virtually identical to
    `movies_long` dataset and was not used anywhere in the package. Also removes
    the unused `VR_dilemma` dataset.

# ggstatsplot 0.8.0

NEW FUNCTIONS

  - Adds `extract_stats` function to extract dataframes containing statistical
    details.

MAJOR CHANGES

  - There is finally a publication for `{ggstatsplot}` package!
    <https://joss.theoj.org/papers/10.21105/joss.03167>

  - The `ggcoefstats` function defaults to `NULL` for `xlab` and `ylab`
    arguments, which lets users change these labels if they wish to do so.
    Additionally, the x-axis label, if not specified, now defaults to
    `"estimate"`. Whether this estimate corresponds to regression coefficient or
    effect size like partial eta-squared should be clear from the label itself.

  - To reduce the dependency load, `ggcorrplot` moves from `Imports` to
    `Suggests`.

  - The `bar.fill` argument in `gghistostats` is retired in favor of the new
    `bin.args` argument that can be used to pass aesthetic arguments to
    `ggplot2::stat_bin`.

  - `ggstatsplot.layer` argument has been retired. If the user _chooses_ a
    certain `ggplot2` theme, it means they _want_ that theme, and not
    `{ggstatsplot}`'s varnish on it. So the previous behavior was undesirable.
    This is a backward compatible change, so the plots should not look
    different.

MINOR CHANGES

  - The `pch` size for `ggcorrmat` has been increased to 14 (#579) to increase
    its visibility compared to the correlation value text.

  - `ggwithinstats` gains `point.args` to change `geom_point`.

  - Minor change to `ggcorrmat` legend title - content in parentheses is now
    shown outside of it.

BUG FIXES

  - `ggcoefstats` didn't work when statistic for the given model was
    chi-squared. This has been fixed.

# ggstatsplot 0.7.2

MAJOR CHANGES

  - To reduce the dependency load, `ggExtra` moves from `Imports` to
    `Suggests`.

  - All functions are more *robust* in the sense that when statistical analysis
    fails, they will return only the plots with no subtitles/captions. This
    helps avoid difficult-to-diagnose edge case failures when the primary
    functions are used in `grouped_` functions (e.g., #559). The `ggpiestats`
    and `ggbarstats` functions always behaved this way, but the rest of the
    functions now also mimic this behavior.

MINOR CHANGES

  - The `ggcoefstats` labels do not contain degrees of freedom when they are not
    available instead of displaying `Inf`.

# ggstatsplot 0.7.1

MAJOR CHANGES

  - Based on feedback from the users, the argument `title.prefix` is now
    removed. It led to redundant title prefixes across different facets of the
    plot. Given that `grouped_` functions require users to set `grouping.var`,
    it is fair to assume what variable the levels in the title correspond to.

MINOR CHANGES

  - Adapts to changes made in `statsExpressions 1.0.0`.

  - `sample.size.label` argument is retired for `ggbetweenstats`,
    `ggwithinstats`, and `ggbarstats`. I do not think it is ever a good idea to
    not do this. If the users wish to not display sample sizes, they can easily
    do this using `scale_*` functions from `ggplot2`.

  - In `ggpiestats` and `ggbarstats`, parametric proportion tests are now turned
    off when `type = "bayes"`.

# ggstatsplot 0.7.0

BREAKING CHANGES

  - `combine_plots` has been completely revised to rely not on `patchwork`, but
    on `patchwork`, to combine a list of `ggplot` together. This was done to
    have a leaner syntax. With this revision, its vestigial twin `combine_plots`
    is no longer needed and has been removed. This should not break any of the
    existing instances of `grouped_` functions, although it will lead to changed
    graphical layouts. The only instance in which this change will lead to a
    breakage is when you specified `labels` argument. So, if you used
    `plotgrid.args = list(labels = "auto")`, you will now have to replace it
    with `plotgrid.args = list(tag_level = "keep")`. You can also use
    `annotation.args` (e.g., `annotation.args = list(tag_levels = "a")` to
    customize labels (this will create labels with pattern `a`, `b`, `c`, etc.).
    Another instance of breakage is if you had used `combine_plots` function and
    provided individual plots to `...` instead as a `list`.

  - To avoid confusion among users, the default trimming level for all functions
    is now changed from `tr = 0.1` to `tr = 0.2` (which is what `WRS2` defaults
    to).

MAJOR CHANGES

  - All robust tests in this package were based on trimmed means, except for
    correlation test. This has been changed: the robust correlation measure is
    now Winsorized correlation, which is based on trimming. Therefore, the
    `beta` argument has been replaced by `tr` argument. This should result only
    in minor changes in correlation coefficient estimates.

  - Using `annotate` instead of `geom_label` had significantly slowed down
    `gghistostats` and `ggdotplotstats` functions. This has been fixed.

  - Removes the vestigial `notch` and `notchwidth` arguments for
    `ggbetweenstats` and `ggwithinstats`.

  - All Bayesian expression templates are now explicit about the type of
    estimate being displayed.

  - For `gghistostats` and `ggdotplotstats`, the centrality measure labels used
    to be attached to the vertical line, but this occluded the underlying data.
    Now this label is instead shown on the top `x`-axis. Note that this means
    that if you make any further changes to the resulting plot using the
    `ggplot2::scale_x_continuous` function, this label will likely disappear.
    The `centrality.k` argument is retired in favor of `k`.

NEW FEATURES

  - More models supported in `ggcoefstats`: `crr`, `eglm`, `elm`, `varest`.

  - `ggbetweenstats`, `ggwithinstats`, `gghistostats`, `ggdotplotstats` gain
    argument `centrality.type` that can be used to specify which centrality
    parameter is to be displayed. So one can have `type = "robust"` and still
    show median as centrality parameter by choosing `centrality.type =
    "nonparametric"`.

# ggstatsplot 0.6.8

MAJOR CHANGES

  - `gghistostats` removes `bar.measure` argument. The function now defaults to
    showing the `count` information on the `x`-axis and the `proportion`
    information on the duplicated `x`-axis.

  - `ggscatterstats` removes `method` and `method.args` arguments. It will no
    longer be possible to use this function to visualize data for when the model
    is not linear. It also retires `margins` argument.

  - For `ggbetweenstats` and `ggwithinstats` functions, the arguments of type
    `mean.*` have all been replaced by `centrality.*`. This is because now these
    functions decide which central tendency measure to show depending on the
    `type` argument (**mean** for parametric, **median** for non-parametric,
    **trimmed mean** for robust, and **MAP estimator** for Bayes).

  - Similarly, `gghistostats` and `ggdotplotstats` functions also decide which
    central tendency measure to show depending on the `type` argument (**mean**
    for parametric, **median** for non-parametric, **trimmed mean** for robust,
    and **MAP estimator** for Bayes). Therefore, `centrality.parameter` argument
    has been removed. If you want to turn off displaying centrality measure, set
    `centrality.plotting = FALSE`.

  - `gghistostats` and `ggdotplotstats` functions remove the functionality to
    display a vertical line corresponding to `test.value`. This feature was
    turned off by default in prior releases. Accordingly, all related arguments
    from these two functions have been removed.

  - `ggscatterstats` defaults to `densigram` as the marginal distribution
    visualization.

  - `ggbetweenstats` and `ggwithinstats` now display the centrality tendency
    measure in such a way that the label doesn't occlude any of the raw data
    points (#429).

  - `mean.ci` argument is retired for `ggbetweenstats` and `ggwithinstats`.
    Future `{ggstatsplot}` releases will be providing different centrality
    measures depending on the `type` argument and it is not guaranteed that all
    of them will have CIs available. So, for the sake of consistency, this
    argument is just going to be retired.

MINOR CHANGES

  - `ggcorrmat` uses pretty formatting to display sample size information.

  - `ggcoefstats` now also displays degrees of freedom for chi-squared tests.

  - Expects minor changes in some of the effect sizes and their confidence
    intervals due to changes in `{statsExpressions}`.

NEW FEATURES

  - More models supported in `ggcoefstats`: `fixest`, `ivFixed`, `ivprobit`,
    `riskRegression`.

  - `ggcorrmat` supports partial correlations.

# ggstatsplot 0.6.6

BREAKING CHANGES

  - `ggcoefstats` no longer supports `exponentiate` argument. If it is
    specified, the user will have to themselves adjust the scales
    appropriately.

  - `ggcorrmat` defaults have changed significantly:

    1. As a matter of good practice, the *p*-values are adjusted by default for
       multiple comparisons.

    2. The default matrix is upper type, and not the full matrix, which features
       many redundant comparisons and self-correlations diagonally.

    3. Default text size for legend has been increased to 15 and background grid
       has been removed.

BUG FIXES

  - In the prior release, when the GitHub version of `BayesFactor` wasn't
    present, `ggwithinstats` just outright failed to run for ANOVA designs. This
    has been fixed.

  - Setting `mean.path = FALSE` in `ggwithinstats` produced incorrect colors for
    points (#470). This bug was introduced in `0.6.5` and is now fixed.

  - If user had set `options(scipen = 999)` in their session, the *p*-value
    formatting for `ggpiestats` and `ggcoefstats` looked super-ugly (#478). This
    has been fixed.

MAJOR CHANGES

  - Drops `broomExtra` from dependencies. All regression modeling-related
    analysis now relies on `easystats` ecosystem.

  - `ggpiestats` and `ggbarstats` don't support returning dataframes. See FAQ
    vignette on how to get these dataframes:
    <https://indrajeetpatil.github.io/ggstatsplot/articles/web_only/faq.html#faq-1>

  - `ggpiestats` and `ggbarstats` were not supposed to support returning Bayes
    Factor for paired contingency table analysis, which is not supported in
    `BayesFactor` itself.

  - `ggcoefstats` defaults to displaying the intercept term. Also, when the
    degrees of freedom are not available for `t`-statistic, they are displayed
    to be `Inf`, in keeping with `easystats` conventions.

  - Instead of showing significance of *p*-values with APA's asterisks
    conventions, `ggbarstats` now instead shows the actual *p*-values from
    one-sample proportion tests.

NEW FEATURES

  - More models supported in `ggcoefstats`: `Glm`.

# ggstatsplot 0.6.5

BREAKING CHANGES

  - `ggpiestats` and `ggbarstats` no longer have the vestigial arguments `main`
    and `condition`, which are superseded by `x` and `y`, respectively.

MAJOR CHANGES

  - For consistency and to reduce confusion, all Bayes Factor (irrespective of
    whether in the subtitle or caption) are always in favor of null over
    alternative (`BF01`).

  - Retires centrality parameter tagging functionality of `ggscatterstats`.
    Although it was not the default, when turned on, it definitely created a
    cluttered plot.

# ggstatsplot 0.6.1

MAJOR CHANGES

  - `ggbetweenstats` and `ggwithinstats` functions now default to
    `pairwise.comparisons = TRUE`.

MINOR CHANGES

  - Plot borders are now removed from the default theme.

  - Small *p*-values (< 0.001) are now displayed in scientific notation.

BREAKING CHANGES

  - `pairwiseComparisons` re-exports are deprecated.

# ggstatsplot 0.6.0

NEW FEATURES

  - More models supported in `ggcoefstats`: `BFBayesFactor`, `betamfx`, `crq`,
    `coxph.penal`, `geeglm`, `glht`, `glmm`, `lm_robust`, `lqm`, `lqmm`,
    `manova`, `maov`, `margins`, `negbinmfx`, `logitmfx`, `logitsf`, `margins`,
    `poissonmfx`, `betaor`, `negbinirr`, `logitor`, `metafor`, `metaplus`,
    `orm`, `poissonirr`, `semLm`, `semLme`, `vgam`.

  - `ggpiestats` gains `label.repel` argument to cover contexts in which the
    labels might overlap. Setting it to `TRUE` will minimize such an overlap.

  - `ggbetweenstats` and `ggwithinstats` gain `ggsignif.args` argument to make
    it easy to change aesthetics of the pairwise comparison geom.

  - The subtitle and caption for Bayes Factor tests now also provide information
    about posterior estimates, when relevant.

MAJOR CHANGES

  - Removed unused `intent_morality` dataset.

  - `ggcoefstats` retires `caption.summary` argument. So, by default, the
    caption is going to contain as much information as it can and the users can
    then choose to modify the default caption using `ggplot2` functions.

MINOR CHANGES

  - The argument `method` for `ggcorrmat` has been renamed to `matrix.method`,
    since it was confusing whether this method referred to correlation method.

  - For both `ggpiestats` and `ggbarstats`, the count labels no longer include `
    n = ` in them as this was confusing since all labels had ` n = ` in them
    with no further explanation about how this `n` differed from `n` in the
    proportion test.

  - No longer relies on `groupedstats` package.

# ggstatsplot 0.5.0

BREAKING CHANGES

  - The `pairwise.annotation` argument for `ggbetweenstats` and `ggwithinstats`
    is deprecated. This was done because-

    1. Different fields have different schema for what significance levels
       asterisks represent.

    2. The *p*-value labels also contain information about whether they are
       adjusted for multiple comparisons.

  - The `normality_message` and `bartlett_message` helper functions have been
    removed. This is because model assumption checks don't really fall under the
    purview of this package. There are excellent visualization tools out there
    for model assumption checks (`ggResidpanel`, `performance`, `DHARMa`,
    `olsrr`, etc.), which should be preferred over unhelpful messages with only
    *p*-values that these functions were printing. For what it's worth, the
    functions where these messages were displayed (`ggbetweenstats` or
    `ggwithinstats`) feature visualizations rich enough and defaults sensible
    enough that most of the time one can either assess these assumptions from
    the plots or need not worry about them.

MAJOR CHANGES

  - `ggcoefstats` has been refactored to reflect that
    `broomExtra::tidy_parameters` now defaults to `parameters` package instead
    of `broom`. It also loses the following vestigial arguments:
    `p.adjust.method` and `coefficient.type`.

  - Reverts aligning title and subtitle with the plot and not the axes, since it
    looked pretty ugly (esp., `ggcoefstats`) and was causing problems for
    labels.

  - `factor.levels` (for `ggpiestats`) and `labels.legend` (for `ggbarstats`)
    are deprecated. If users would like to changes the names for factor levels,
    this should be done outside of `{ggstatsplot}`.

  - The non-parametric post hoc test for between-subjects design has been
    changed from Dwass-Steel-Crichtlow-Fligner test to Dunn test.

NEW FEATURES

  - More models supported in `ggcoefstats`: `bayesGARCH`, `clm2`, `clmm2`,
    `mcmc.list`, `robmixglm`.

# ggstatsplot 0.4.0

BREAKING CHANGES

  - `ggcorrmat` no longer returns matrices of correlation coefficients or other
    details. It now returns either a plot or a data frame and this can data frame
    can then be used to create matrices.

  - `ggbarstats` loses `x.axis.orientation` argument. This argument was supposed
    to help avoid overlapping *x*-axis label, but now `ggplot2 3.3.0` has a
    better way to handle this:
    <https://www.tidyverse.org/blog/2020/03/ggplot2-3-3-0/#rewrite-of-axis-code>

NEW FEATURES

  - More models supported in `ggcoefstats`: `bayesx`, `BBmm`, `brmultinom`,
    `lmerModLmerTest`, `lrm`.

  - Specifying `output = "proptest"` for `ggpiestats` and `ggbarstats` functions
    will now return a data frame containing results from proportion test.

  - `ggbetweenstats` and `ggwithinstats` will display pairwise comparisons even
    if `results.subtitle` is set to `FALSE`.

  - `ggcorrmat` supports computing Bayes Factors for Pearson's *r* correlation.

  - `ggbetweenstats` and `ggwithinstats` now support pairwise comparisons for
    Bayes Factor test.

MAJOR CHANGES

  - For changes related to subtitle details, see changes made in new version of
    `statsExpressions 4.0.0`:
    <https://CRAN.R-project.org/package=statsExpressions/news/news.html>

  - `ggbetweenstats` and `ggwithinstats` no longer print dataframes containing
    results from pairwise comparisons tests because this is too cluttering for
    the user's console. The users are now instead advised to either extract this
    data frame using `ggplot2::ggplot_build()` function or use the
    `pairwiseComparisons::pairwise_comparisons()` function used in the
    background by `{ggstatsplot}` to carry out this analysis.

  - Due to changes in one of the downstream dependencies, `{ggstatsplot}` now
    expects the minimum R version to be `3.6.0`.

MINOR CHANGES

  - `ggcorrmat` now internally relies on `correlation` for correlation
    analyses.

  - `ggbarstats` no longer displays `"percent"` for Y-axis label as this was
    redundant information.

  - Continuing the argument cleanup that began in `0.3.0`, `ggcoefstats` gains
    `point.args` argument instead of individuals `point.*` arguments.

  - The subtitles are more explicit about the details of the test. For the same
    reason `stat.title` argument from all relevant functions is retired since
    this argument was supposed to be for entering some additional details about
    the test. Additionally, the plot titles and subtitles for some of the plots
    are aligned with the plot.

  - `ggcorrmat` legend, in case of missing values, shows mode - instead of
    median - for the distribution of sample pairs.

  - The following vestigial arguments are retired:

      - `caption.default` in `ggcorrmat`

      - `k.caption.summary` in `ggcoefstats`

# ggstatsplot 0.3.1

This is a hotfix release to correct some of the failing tests and other minor
breakages resulting from the new release of `ggplot2 3.3.0`.

MAJOR CHANGES

  - `ggpiestats` loses `sample.size.label` argument since this information is
    included in the goodness of fit test results itself. So setting
    `proportion.test` to `FALSE` will suppress this information.

# ggstatsplot 0.3.0

BREAKING CHANGES

To give users more flexibility in terms of modifying the aesthetic defaults for
**all** `geoms` included in the `{ggstatsplot}` plots (each plot typically has
multiple geoms), the package now uses a new form of syntax. Previously, each
`geom` had a separate argument to specify each aesthetic (e.g., `geom_point`
would get arguments like `point.size`, `point.color`, etc.), which resulted in
functions with a massive number of arguments and was unsustainable in the long
run. Instead, `{ggstatsplot}` functions now expect a list of such arguments for
the respective geom (e.g., `geom_point` will have `point.args` argument where a
list of arguments `list(size = 5, color = "darkgreen", alpha = 0.8)` can be
supplied).

  - All `grouped_` functions have been refactored to reduce the number of
    arguments. These functions now internally use the new `combine_plots`
    instead of `combine_plots`. The additional arguments to primary functions
    can be provided through `...`. These changes will not necessarily break the
    existing code but will lead to some minor graphical changes (e.g., if you
    were providing `labels` argument explicitly, it will be ignored).

  - All functions lose the `return` argument, which was supposed to be
    alternative to enter `output`. But this was just leading to more confusion
    on the user's part. The biggest user-visible impact this is going to have is
    that `ggcorrmat` will no longer be backward-compatible. The older scripts
    will still work but if the `return` argument was anything except `"plot"`,
    it will just be ignored.

  - `ggcorrmat` no longer has `corr.method` argument. To be consistent with rest
    of the functions in this package, the type of statistics should be specified
    using `type` argument. Additional, it gains a new argument
    `ggcorrplot.args`, which can be used to pass additional arguments to the
    underlying plotting function (`ggcorrplot::ggcorrplot`).

  - Both `gghistostats` and `ggdotplotstats` now use the following arguments to
    modify `geom`s corresponding to the lines and labels:
    `test.value.line.args`, `test.value.label.args`, `centrality.line.args`,
    `centrality.label.args`. This helps avoid specifying millions of arguments.

  - Removes the vestigial `ggplot_converter` function.

  - `ggpiestats` and `ggbarstats` remove the following vestigial arguments:
    `facet.wrap.name`, `bias.correct`, `bar.outline.color`. The `bar.proptest`
    and `facet.proptest` arguments were difficult to remember and confusing and
    are replaced by a common `proportion.test` argument. Additionally, the
    following arguments have all been removed and replaced by `label` argument:
    `slice.label`, `bar.label`, `data.label`. These plethora of options was a
    headache to remember.

  - `gghistostats` loses the following arguments: `fill.gradient`, `low.color`,
    `high.color`. It made no sense to add a color gradient to this plot when the
    Y-axis already displayed the information about what the bar represented.

  - `ggscatterstats` loses the following arguments: `palette` and `package`.
    Since this function requires only two colors, it didn't make much sense to
    use color palettes to specify this. They can be instead specified using
    `xfill` and `yfill`. You can always use `paletteer::paletteer_d` to get a
    vector of color values and then provide values of your choosing to `xfill`
    and `yfill`.

  - Removes sorting options in `ggbetweenstats` and `ggwithinstats` functions.
    This is something the users can easily do before entering the data in these
    functions.

MAJOR CHANGES

  - `ggcorrmat` was never supposed to work with Kendall's correlation
    coefficient but it accidentally did. This is no longer the case.

  - `{ggstatsplot}` now has a logo, thanks to Sarah! :)

  - The default `theme_ggstatsplot` changes slightly. The biggest change is that
    the title and the subtitle for plots are now aligned to the left of the
    plot. This change also forced the legend for `ggpiestats` to be displayed on
    the right side of the plot rather than at the bottom.

MINOR CHANGES

  - More models supported in `ggcoefstats`: `BBreg`, `bcplm`, `bife`, `cglm`,
    `crch`, `DirichReg`, `LORgee`, `zcpglm`, `zeroinfl`.

  - Following functions are now re-exported from `ipmisc`: `bartlett_message`,
    `normality_message`. A few other internal data wrangling functions now
    reside in `ipmisc`.

# ggstatsplot 0.2.0

BREAKING CHANGES

  - To have a more manageable length of function arguments, additional aesthetic
    specifications for any given geom can be provided via a dedicated `*.args`
    argument. For example, all aesthetic arguments for `geom_vline` can be
    provided via `vline.args`, for `geom_errorbarh` via `errorbar.args`, etc.

  - `{ggstatsplot}` continues with its conscious uncoupling that started in
    `0.1.0` release: The following functions have now been moved to
    `{statsExpressions}` package: `subtitle_meta_parametric` and
    `bf_meta_message` and follow a more logical nomenclature. For the same
    reason, `lm_effsize_ci` function is also no longer exported and lives in the
    `groupedstats` package.

MAJOR CHANGES

  - The summary caption no longer displays log-likelihood value because it tends
    to be not available for a number of regression model objects and so the
    caption was unnecessarily being skipped.

  - Supports robust and Bayes Factors for random-effects meta-analysis.

MINOR CHANGES

  - New dataset included: `bugs_wide`

  - More models supported in `ggcoefstats`: `cgam`, `cgamm`, `coxme`, `cpglm`,
    `cpglmm`, `complmrob`, `feis`, `flexsurvreg`, `glmx`, `hurdle`, `iv_robust`,
    `mixor`, `rqss`, `truncreg`, `vgam`.

  - Removed vestigial arguments from `ggcorrmat` (e.g., `exact`, `continuity`,
    etc.) and `ggpiestats` (`bf.prior`, `simulate.p.value`, `B`, etc.).

# ggstatsplot 0.1.4

BUG FIXES

  - `ggbetweenstats` and `ggwithinstats` no longer produce error with variables
    with pattern `mean` (#336).

MAJOR CHANGES

  - `pairwise_p` has been reintroduced as a number of users found it useful to
    call the function from `{ggstatsplot}` itself rather than using
    `pairwiseComparisons` package.

MINOR CHANGES

  - `ggbetweenstats` and `ggwithinstats` use `[` instead of `(` to display
    confidence intervals. Additionally, $$\mu$$ denoted sample mean, but was
    confused with population mean by some users. So these functions instead
    display $$\hat{\mu}$$.

  - More models supported in `ggcoefstats`: `bmlm`, `coeftest`

  - Adapts to the new syntax provided in `paletteer` package.

# ggstatsplot 0.1.3

MAJOR CHANGES

  - To avoid excessive arguments to function, most arguments relevant for
    `ggrepel` in `ggcoefstats` function have been removed. The users can instead
    provide all such arguments in a list to `stats.labels.args` argument.

BUG FIXES

  - `ggbetweenstats` and `ggwithinstats` no longer produce incorrect label if
    the data frame already contains a variable named `n` (#317) or variables with
    pattern `mean` (#322).

  - `ggbetweenstats` and `ggwithinstats` mean labels respect `k` argument
    (#331).

MINOR

  - `ggcoefstats` now uses `parameters::p_value` instead of `sjstats::p_value`,
    as requested by the maintainer of that package. This might lead to
    differences in *p*-values for `lmer` models.

  - More models supported in `ggcoefstats`: `blavaan`, `bracl`, `brglm2`,
    `glmc`, `lavaan`, `nlreg`, `slm`, `wbgee`.

  - `ggcoefstats` gains `only.significant` argument to only display display
    stats labels for significant effects. This can be helpful when a large
    number of regression coefficients are to be displayed in a single plot.

# ggstatsplot 0.1.2

MINOR

  - Minor code refactoring that gets rid of the following dependencies:
    `magrittr`, `ellipsis`, `purrrlyr`.

MAJOR

  - The *p*-value label now specifies whether the *p*-value displayed in
    `ggbetweenstats` and `ggwithinstats` pairwise comparisons were adjusted or
    not for multiple comparisons.

# ggstatsplot 0.1.1

ANNOUNCEMENTS

`{ggstatsplot}` is undergoing *conscious uncoupling* whereby all the statistical
processing functions that make stats subtitles are being moved to a new package
called `{statsExpressions}`. This new package will act as a backend that handles
all things statistical processing. This **will not** affect the end users of
`{ggstatsplot}` unless you have been using the helper functions.

Additionally, multiple pairwise comparison tests are being moved to an
independent package called `pairwiseComparisons`.

This uncoupling is designed to achieve two things:

  - Make the code base of more manageable size in `{ggstatsplot}`, which will
    make package development a bit easier.

  - Make the workflow more customizable since now you can prepare your own plots
    and then use `{statsExpressions}` to display results in the plot rather than
    relying on `{ggstatsplot}` default plots which are heavily opinionated and
    not appealing to everyone.

BREAKING CHANGES

  - All helper functions `subtitle_*` and `bf_*` have been moved to the new
    `{statsExpressions}` package.

  - To be consistent with all the other `subtitle_` and `bf_` functions,
    `subtitle_contingency_tab` and `bf_contingency_tab` now use the arguments
    `x` and `y` instead of `main` and `condition`.

MAJOR CHANGES

  - Major refactoring to reduce the codesize and to rely fully on `rlang`.

  - There was confusion about what did the red point in `ggbetweenstats` and
    `ggbetweenstats` plots represents. Now the label also contains $\mu$ to
    highlight that what is being displayed is a mean value.

  - To be consistent with the rest of the functions, `ggpiestats` and
    `ggbarstats` now uses the following aliases for arguments: `x` for `main`
    and `y` for `condition`. This change is backward-compatible and should not
    pose any problems for scripts that used `main` and `condition` arguments in
    these functions.

  - Most subtitle expressions now report details about the design. In case of
    between-subjects design, this will be $n\_{obs}$, while in case of repeated
    measures design, this will be $n\_{pairs}$.

  - `pairwise.annotation` now defaults to `"p.value"` rather than `"asterisk"`
    for `ggbetweenstats` and `ggwithinstats` (and their `grouped_` variants)
    functions. This was done because the asterisk conventions are not consistent
    across various scientific disciplines.

MINOR CHANGES

  - New dataset included: `bugs_long`, for repeated measures designs with `NA`s
    present in the data.

  - `{ggstatsplot}` now uses `rcompanion` to compute Spearman's *rho* and
    Kendall's *W*. Therefore, `DescTools` is removed from dependencies.

  - `ggcoefstats` supports following objects: `bglmerMod`, `blmerMod`, `lme`,
    `mclogit`, `mmclogit`, `tobit`, `wblm`.

  - `ggcoefstats` now respects `conf.int`. It internally always defaulted to
    `conf.int = TRUE` in `broom::tidy` irrespective of what was specified by the
    user.

  - It was painfully confusing for a lot of users what exactly the asterisks in
    each facet of `ggpiestats` signified. So instead now `ggpiestats` displays
    more detailed results from a goodness of fit (gof) test. No such change is
    made for `ggbarstats` because there is no space to include more details
    above the bar.

  - Removed `conf.method` and `conf.type` arguments for `ggcoefstats`. Also,
    `p.kr` argument removed because `ggcoefstats` will begin to rely on
    `parameters` instead of `sjstats` package to compute *p*-values for some
    regression models.

# ggstatsplot 0.0.12

BUG FIXES

  - Bayes Factor in `ggwithinstats` caption, displayed by default, was
    incorrect. This has been fixed. This stemmed from a line of code which
    should have been `paired = TRUE`, but was instead `paired = FALSE`.

MAJOR CHANGES

  - The effect size measure for Kruskal-Wallis test has been changed from the
    more obscure H-based eta-squared statistic to more common and interpretable
    epsilon-squared.

MINOR CHANGES

  - `ggcoefstats` defaults to `bf.message = TRUE` to be consistent with the rest
    of the functions in the package.

  - `ggcoefstats` supports the following class of objects: `epi.2by2`, `negbin`,
    `emmGrid`, `lmrob`, `glmrob`, `glmmPQL`, `data.table`.

  - `bf_ttest` is introduced as a general function. The previously exported
    `bf_one_sample_ttest` and `bf_two_sample_ttest` become its aliases.

  - `bf_meta_message` syntax changes to adapt to updates made to `metaBMA`
    package (thanks to #259).

BREAKING CHANGES

  - The vestigial arguments `axis.text.x.margin.t`, `axis.text.x.margin.r`,
    `axis.text.x.margin.b`, `axis.text.x.margin.l` for `ggcorrmat` have been
    removed. The margins can be adjusted using `ggplot2::margin()`.

  - `gghistostats` no longer allows `data` argument to be `NULL`. This is to
    make this function's syntax consistent with rest of the functions in this
    package (none of which allow `data` to be `NULL`). This also removes
    confusion that arose for some users when `data` couldn't be `NULL` for its
    `grouped_` cousin (`grouped_gghistostats`).

  - `outlier_df` function is no longer exported since it was always meant to be
    an internal function and was accidently exported during initial release and
    was retained for a while for backward compatibility.

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
    various effect sizes. `{ggstatsplot}` therefore gains `DescTools` as a
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
    contingency table. It now instead displays Cohen's *G* as effect size, which
    generalizes to any kind of design.

MINOR CHANGES

  - The internal function `outlier_df` to add a column specifying outlier status
    of any given data point is now exported.

  - `{ggstatsplot}` previously relied on an internal function `chisq_v_ci` to
    compute confidence intervals for Cramer's *V* using bootstrapping but it was
    pretty slow. It now instead relies on `rcompanion` package to compute
    confidence intervals for *V*. `{ggstatsplot}`, therefore, gains a new
    dependency.

  - `subtitle_mann_nonparametric` and `subtitle_t_onesample` now computes effect
    size *r* and its confidence intervals as $$Z/\sqrt{N}$$ (with the help of
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
    `{ggstatsplot}` now relies on a new (#159) internal function
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
    data frame with regression estimates and standard error is available from
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
    `TRUE`, the specified caption will be added on top of the
    `caption.summary`.

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

  - `{ggstatsplot}` now depends on `R 3.5.0`. This is because some of its
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

  - `subtitle_t_robust` function can now handle dependent samples and gains
    `paired` argument.

  - A number of tidyverse operators are now reexported by `{ggstatsplot}`:
    `%>%`, `%<>%`, `%$%`.

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
    variables from the provided data frame will be used for computing the
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
    default for `{ggstatsplot}`).

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
    This helps avoid situations where people had large number of groups (> 12)
    and there were not enough colors in any of the `RColorBrewer` palettes.

  - `ggbetweenstats` gains `bf.message` argument to display bayes factors in
    favor of the null (currently works only for parametric *t*-test).

  - `gghistostats` function no longer has `line.labeller.y` argument; this
    position is automatically determined now.

BREAKING CHANGES

  - `legend.title.margin` function has been deprecated since `ggplot2 3.0.0` has
    improved on the margin issues from previous versions. All functions that
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
    for this function it specified the display of the visualization matrix. This
    will make the syntax more consistent across functions.

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
    produces. Thanks to Chuck Powell's PR (#43).

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
    The `theme_mprl` function will still be around and will **not** be
    deprecated, so feel free to use either or both of them since they are
    identical.

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

  - For `lmer` models, *p*-values are now computed using `sjstats::p_value`.
    This removes `lmerTest` package from dependencies.

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
    `{ggstatsplot}` no longer imports `MASS` and `sfsmisc`.

  - The `data` argument is no longer `NULL` for all functions, except
    `gghistostats`. In other words, the user **must** provide a data frame from
    which variables or formulas should be selected.

  - All subtitles containing results now also show sample size information
    (*n*). To adjust for the inflated length of the subtitle, the default
    subtitle text size has been changed from `12` to `11`.

MINOR CHANGES

  - Switched back to Shapiro-Wilk test of normality to remove `nortest` from
    imports.

  - `ggbetweenstats` and `ggpiestats` now display sample sizes for each level of
    the groping factor by default. This behavior can be turned off by setting
    `sample.size.label` to `FALSE`.

  - Three new datasets added: `Titanic_full`, `movies_wide`, `movies_long`.

  - Added confidence interval for effect size for robust ANOVA.

  - The 95% CI for Cramer'V computed using `boot::boot`. Therefore, the package
    no longer imports `DescTools`.

  - To be consistent across correlations covered, all correlations now show
    estimates for correlation coefficients, confidence intervals for the
    estimate, and *p*-values. Therefore, *t*-values and regression coefficients
    are no longer displayed for Pearson's *r*.

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

  - To be internally consistent, all functions in `{ggstatsplot}` use the
    spelling `color`, rather than `colour` in some functions, while `color` in
    others.

  - Removed the redundant argument `binwidth.adjust` from `gghistostats`
    function. This argument was relevant for the first avatar of this function,
    but is no longer playing any role.

  - To be internally consistent, the argument `lab_col` and `lab_size` in
    `ggcorrmat` have been changed to `lab.col` and `lab.size`, respectively.

MINOR CHANGES

  - Added a new argument to `ggstatsplot.theme` function to control if
    `ggstatsplot::theme_mprl` is to be overlaid on top of the selected `ggtheme`
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
    `centrality.para`. This was due to possible confusion about interpretation
    of these lines; they show central tendency measures and not intercept for
    the linear model. Thus the change.

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

