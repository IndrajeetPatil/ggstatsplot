# ggstatsplot 0.0.2.9000

NEW FEATURES

  - The `ggbetweenstats` function can now show notched box plots. Two new
    arguments `notch` and `notchwidth` control its behavior. The defaults are
    still standard box plots.
  - Removed warnings that were appearing when `outlier.label` argument was of
    `character` type.
  - The default color palette used for all plots is colorblind friendly.

BREAKING FEATURES
  - To be internally consistent, all functions in `ggstatsplot` use the spelling
    `color`, rather than `colour` in some functions, while `color` in others.
  - Removed the redundant argument `binwidth.adjust` from `gghistostats`
    function. This argument was relevant for the first avatar of this fucntion,
    but is no longer playing any role.

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
