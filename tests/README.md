Tests and Coverage
================
20 December, 2018 05:30:57

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr)
package.

| Object                                                                               | Coverage (%) |
| :----------------------------------------------------------------------------------- | :----------: |
| ggstatsplot                                                                          |    84.17     |
| [R/grouped\_ggbetweenstats.R](../R/grouped_ggbetweenstats.R)                         |     0.00     |
| [R/grouped\_gghistostats.R](../R/grouped_gghistostats.R)                             |     0.00     |
| [R/grouped\_ggpiestats.R](../R/grouped_ggpiestats.R)                                 |     0.00     |
| [R/grouped\_ggscatterstats.R](../R/grouped_ggscatterstats.R)                         |     0.00     |
| [R/combine\_plots.R](../R/combine_plots.R)                                           |    37.50     |
| [R/grouped\_ggcorrmat.R](../R/grouped_ggcorrmat.R)                                   |    45.45     |
| [R/helpers\_messages.R](../R/helpers_messages.R)                                     |    86.36     |
| [R/helpers\_effsize\_ci.R](../R/helpers_effsize_ci.R)                                |    93.96     |
| [R/ggcoefstats.R](../R/ggcoefstats.R)                                                |    94.09     |
| [R/helpers\_bf\_tests.R](../R/helpers_bf_tests.R)                                    |    94.44     |
| [R/switch\_functions.R](../R/switch_functions.R)                                     |    94.79     |
| [R/helpers\_ggpiestats\_subtitles.R](../R/helpers_ggpiestats_subtitles.R)            |    95.14     |
| [R/helpers\_ggcoefstats.R](../R/helpers_ggcoefstats.R)                               |    95.95     |
| [R/ggbetweenstats.R](../R/ggbetweenstats.R)                                          |    96.84     |
| [R/helpers\_ggbetween\_anova\_subtitles.R](../R/helpers_ggbetween_anova_subtitles.R) |    96.85     |
| [R/ggpiestats.R](../R/ggpiestats.R)                                                  |    97.85     |
| [R/helpers\_pairwise\_comparison.R](../R/helpers_pairwise_comparison.R)              |    98.20     |
| [R/ggcorrmat.R](../R/ggcorrmat.R)                                                    |    99.11     |
| [R/gghistostats.R](../R/gghistostats.R)                                              |    99.47     |
| [R/ggdotplotstats.R](../R/ggdotplotstats.R)                                          |    100.00    |
| [R/ggscatterstats.R](../R/ggscatterstats.R)                                          |    100.00    |
| [R/grouped\_ggdotplotstats.R](../R/grouped_ggdotplotstats.R)                         |    100.00    |
| [R/helpers\_ggbetween\_t\_subtitles.R](../R/helpers_ggbetween_t_subtitles.R)         |    100.00    |
| [R/helpers\_ggbetweenstats.R](../R/helpers_ggbetweenstats.R)                         |    100.00    |
| [R/helpers\_ggcorrmat.R](../R/helpers_ggcorrmat.R)                                   |    100.00    |
| [R/helpers\_gghistostats.R](../R/helpers_gghistostats.R)                             |    100.00    |
| [R/helpers\_gghistostats\_subtitles.R](../R/helpers_gghistostats_subtitles.R)        |    100.00    |
| [R/helpers\_ggscatterstats\_subtitles.R](../R/helpers_ggscatterstats_subtitles.R)    |    100.00    |
| [R/subtitle\_maker\_templates.R](../R/subtitle_maker_templates.R)                    |    100.00    |
| [R/theme\_ggstatsplot.R](../R/theme_ggstatsplot.R)                                   |    100.00    |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat)
package.

| file                                                                                          |   n |  time | error | failed | skipped | warning | icon |
| :-------------------------------------------------------------------------------------------- | --: | ----: | ----: | -----: | ------: | ------: | :--- |
| [test\_argument\_count.R](testthat/test_argument_count.R)                                     |   1 |  0.01 |     0 |      0 |       1 |       0 | \+   |
| [test\_chisq\_v\_ci.R](testthat/test_chisq_v_ci.R)                                            |   1 |  0.00 |     0 |      0 |       1 |       0 | \+   |
| [test\_combine\_plots.R](testthat/test_combine_plots.R)                                       |   1 |  0.00 |     0 |      0 |       1 |       0 | \+   |
| [test\_cor\_test\_ci.R](testthat/test_cor_test_ci.R)                                          |   1 |  0.00 |     0 |      0 |       1 |       0 | \+   |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R)                                      |  59 | 13.48 |     0 |      0 |       0 |       1 | \-   |
| [test\_ggcoefstats.R](testthat/test_ggcoefstats.R)                                            | 128 | 23.43 |     0 |      0 |       1 |       0 | \+   |
| [test\_ggcoefstats\_label\_maker.R](testthat/test_ggcoefstats_label_maker.R)                  |   3 |  2.25 |     0 |      0 |       0 |       0 |      |
| [test\_ggcorrmat.R](testthat/test_ggcorrmat.R)                                                |  55 |  0.61 |     0 |      0 |       0 |       0 |      |
| [test\_ggdotplotstats.R](testthat/test_ggdotplotstats.R)                                      |  29 |  0.64 |     0 |      0 |       0 |       0 |      |
| [test\_gghistostats.R](testthat/test_gghistostats.R)                                          |  76 |  1.86 |     0 |      0 |       0 |       0 |      |
| [test\_ggpiestats.R](testthat/test_ggpiestats.R)                                              |  37 | 12.56 |     0 |      0 |       0 |       0 |      |
| [test\_ggscatterstats.R](testthat/test_ggscatterstats.R)                                      |  41 |  3.25 |     0 |      0 |       0 |       0 |      |
| [test\_ggsignif\_position\_calculator.R](testthat/test_ggsignif_position_calculator.R)        |   1 |  0.00 |     0 |      0 |       1 |       0 | \+   |
| [test\_grouped\_ggbetweenstats.R](testthat/test_grouped_ggbetweenstats.R)                     |   1 |  0.00 |     0 |      0 |       1 |       0 | \+   |
| [test\_grouped\_ggcorrmat.R](testthat/test_grouped_ggcorrmat.R)                               |   5 |  0.10 |     0 |      0 |       1 |       0 | \+   |
| [test\_grouped\_ggdotplotstats.R](testthat/test_grouped_ggdotplotstats.R)                     |   2 |  2.55 |     0 |      0 |       0 |       0 |      |
| [test\_grouped\_gghistostats.R](testthat/test_grouped_gghistostats.R)                         |   1 |  0.00 |     0 |      0 |       1 |       0 | \+   |
| [test\_grouped\_ggpiestats.R](testthat/test_grouped_ggpiestats.R)                             |   1 |  0.00 |     0 |      0 |       1 |       0 | \+   |
| [test\_grouped\_ggscatterstats.R](testthat/test_grouped_ggscatterstats.R)                     |   1 |  0.00 |     0 |      0 |       1 |       0 | \+   |
| [test\_helper\_messages.R](testthat/test_helper_messages.R)                                   |   6 |  0.01 |     0 |      0 |       6 |       0 | \+   |
| [test\_helpers\_bf\_tests.R](testthat/test_helpers_bf_tests.R)                                |  25 |  0.42 |     0 |      0 |       0 |       0 |      |
| [test\_histo\_labeller.R](testthat/test_histo_labeller.R)                                     |   8 |  0.12 |     0 |      0 |       1 |       0 | \+   |
| [test\_kw\_eta\_h\_ci.R](testthat/test_kw_eta_h_ci.R)                                         |   1 |  0.00 |     0 |      0 |       1 |       0 | \+   |
| [test\_lm\_effsize\_ci.R](testthat/test_lm_effsize_ci.R)                                      |   1 |  0.00 |     0 |      0 |       1 |       0 | \+   |
| [test\_long\_to\_wide\_converter.R](testthat/test_long_to_wide_converter.R)                   |   4 |  0.10 |     0 |      0 |       0 |       0 |      |
| [test\_mean\_labeller.R](testthat/test_mean_labeller.R)                                       |   6 |  0.40 |     0 |      0 |       0 |       0 |      |
| [test\_numdf\_summary.R](testthat/test_numdf_summary.R)                                       |   6 |  0.06 |     0 |      0 |       0 |       0 |      |
| [test\_pairwise\_ggsignif.R](testthat/test_pairwise_ggsignif.R)                               |  59 | 85.94 |     0 |      0 |       0 |       0 |      |
| [test\_pairwise\_p.R](testthat/test_pairwise_p.R)                                             |  18 | 11.56 |     0 |      0 |       2 |       0 | \+   |
| [test\_pairwise\_p\_caption.R](testthat/test_pairwise_p_caption.R)                            |   3 |  0.00 |     0 |      0 |       0 |       0 |      |
| [test\_robcor\_ci.R](testthat/test_robcor_ci.R)                                               |   8 |  0.53 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_anova\_bayes.R](testthat/test_subtitle_anova_bayes.R)                        |   1 |  5.26 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_anova\_parametric.R](testthat/test_subtitle_anova_parametric.R)              |   6 |  1.33 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_anova\_robust.R](testthat/test_subtitle_anova_robust.R)                      |   2 |  3.68 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_contingency\_tab.R](testthat/test_subtitle_contingency_tab.R)                |   2 |  2.39 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_contingency\_tab\_gof.R](testthat/test_subtitle_contingency_tab_gof.R)       |   4 |  0.15 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_contingency\_tab\_paired.R](testthat/test_subtitle_contingency_tab_paired.R) |   2 |  0.10 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_friedman\_nonparametric.R](testthat/test_subtitle_friedman_nonparametric.R)  |   1 |  0.05 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_ggscatterstats.R](testthat/test_subtitle_ggscatterstats.R)                   |   3 |  0.51 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_kw\_nonparametric.R](testthat/test_subtitle_kw_nonparametric.R)              |   2 |  1.41 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_mann\_nonparametric.R](testthat/test_subtitle_mann_nonparametric.R)          |   1 |  0.10 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_mann\_paired.R](testthat/test_subtitle_mann_paired.R)                        |   2 |  0.11 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_meta.R](testthat/test_subtitle_meta.R)                                       |   1 |  0.00 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_t\_bayes.R](testthat/test_subtitle_t_bayes.R)                                |   1 |  0.25 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_t\_bayes\_paired.R](testthat/test_subtitle_t_bayes_paired.R)                 |   1 |  0.25 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_t\_onesample.R](testthat/test_subtitle_t_onesample.R)                        |   7 |  0.88 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_t\_parametric.R](testthat/test_subtitle_t_parametric.R)                      |   2 |  0.21 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_t\_parametric\_paired.R](testthat/test_subtitle_t_parametric_paired.R)       |   2 |  0.04 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_t\_robust.R](testthat/test_subtitle_t_robust.R)                              |   3 |  2.70 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_templates.R](testthat/test_subtitle_templates.R)                             |   3 |  0.02 |     0 |      0 |       0 |       0 |      |
| [test\_switch\_statements.R](testthat/test_switch_statements.R)                               |   6 |  0.00 |     0 |      0 |       2 |       0 | \+   |
| [test\_t1way\_ci.R](testthat/test_t1way_ci.R)                                                 |   1 |  0.00 |     0 |      0 |       1 |       0 | \+   |
| [test\_theme\_ggstatsplot.R](testthat/test_theme_ggstatsplot.R)                               |  11 |  0.06 |     0 |      0 |       0 |       0 |      |
| [test\_yuend\_ci\_paired.R](testthat/test_yuend_ci_paired.R)                                  |   1 |  0.00 |     0 |      0 |       1 |       0 | \+   |

<details open>

<summary> Show Detailed Test Results
</summary>

| file                                                                                               | context                            | test                                                                  | status  |  n |  time | icon |
| :------------------------------------------------------------------------------------------------- | :--------------------------------- | :-------------------------------------------------------------------- | :------ | -: | ----: | :--- |
| [test\_argument\_count.R](testthat/test_argument_count.R#L6)                                       | argument\_count                    | argument\_count is correct                                            | SKIPPED |  1 |  0.01 | \+   |
| [test\_chisq\_v\_ci.R](testthat/test_chisq_v_ci.R#L6)                                              | chisq\_v\_ci                       | chisq\_v\_ci works                                                    | SKIPPED |  1 |  0.00 | \+   |
| [test\_combine\_plots.R](testthat/test_combine_plots.R#L6)                                         | combine\_plots                     | checking if combining plots works                                     | SKIPPED |  1 |  0.00 | \+   |
| [test\_cor\_test\_ci.R](testthat/test_cor_test_ci.R#L6)                                            | cor\_test\_ci                      | cor\_test\_ci works                                                   | SKIPPED |  1 |  0.00 | \+   |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R#L9_L16)                                    | ggbetweenstats                     | error when x and outlier.label are same                               | PASS    |  1 |  0.00 |      |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R#L26_L37)                                   | ggbetweenstats                     | outlier.labeling works across vector types                            | WARNING |  4 |  2.92 | \-   |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R#L116)                                      | ggbetweenstats                     | checking labels and data from plot                                    | PASS    | 21 |  2.14 |      |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R#L228)                                      | ggbetweenstats                     | checking mean labels are working                                      | PASS    |  3 |  0.35 |      |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R#L303)                                      | ggbetweenstats                     | subtitles with bayesian tests work                                    | PASS    |  2 |  6.92 |      |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R#L339)                                      | ggbetweenstats                     | subtitle works with equal variance assumption                         | PASS    |  1 |  0.72 |      |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R#L390)                                      | ggbetweenstats                     | checking if plot.type argument works                                  | PASS    | 27 |  0.43 |      |
| [test\_ggcoefstats.R](testthat/test_ggcoefstats.R#L35)                                             | ggcoefstats                        | ggcoefstats with lm model                                             | PASS    | 11 |  0.17 |      |
| [test\_ggcoefstats.R](testthat/test_ggcoefstats.R#L109)                                            | ggcoefstats                        | ggcoefstats with glmer model                                          | PASS    | 12 |  0.69 |      |
| [test\_ggcoefstats.R](testthat/test_ggcoefstats.R#L172_L175)                                       | ggcoefstats                        | ggcoefstats with partial variants of effect size for f-statistic      | PASS    | 40 |  6.51 |      |
| [test\_ggcoefstats.R](testthat/test_ggcoefstats.R#L374_L381)                                       | ggcoefstats                        | ggcoefstats with non-partial variants of effect size for f-statistic  | PASS    |  2 |  6.84 |      |
| [test\_ggcoefstats.R](testthat/test_ggcoefstats.R#L468)                                            | ggcoefstats                        | check merMod output                                                   | PASS    | 11 |  7.02 |      |
| [test\_ggcoefstats.R](testthat/test_ggcoefstats.R#L545)                                            | ggcoefstats                        | check glm output                                                      | PASS    | 10 |  0.31 |      |
| [test\_ggcoefstats.R](testthat/test_ggcoefstats.R#L584)                                            | ggcoefstats                        | check lmRob output                                                    | PASS    |  4 |  0.16 |      |
| [test\_ggcoefstats.R](testthat/test_ggcoefstats.R#L629)                                            | ggcoefstats                        | check quantreg output                                                 | PASS    |  6 |  0.28 |      |
| [test\_ggcoefstats.R](testthat/test_ggcoefstats.R#L643)                                            | ggcoefstats                        | check clm models                                                      | SKIPPED |  1 |  0.02 | \+   |
| [test\_ggcoefstats.R](testthat/test_ggcoefstats.R#L757)                                            | ggcoefstats                        | ggcoefstats works with data frames                                    | PASS    | 24 |  0.65 |      |
| [test\_ggcoefstats.R](testthat/test_ggcoefstats.R#L912)                                            | ggcoefstats                        | check computing confidence intervals                                  | PASS    |  4 |  0.22 |      |
| [test\_ggcoefstats.R](testthat/test_ggcoefstats.R#L942)                                            | ggcoefstats                        | check if glance works                                                 | PASS    |  2 |  0.55 |      |
| [test\_ggcoefstats.R](testthat/test_ggcoefstats.R#L953_L958)                                       | ggcoefstats                        | unsupported model objects                                             | PASS    |  1 |  0.01 |      |
| [test\_ggcoefstats\_label\_maker.R](testthat/test_ggcoefstats_label_maker.R#L36_L45)               | ggcoefstats\_label\_maker          | glmRob works                                                          | PASS    |  2 |  0.18 |      |
| [test\_ggcoefstats\_label\_maker.R](testthat/test_ggcoefstats_label_maker.R#L81_L89)               | ggcoefstats\_label\_maker          | glmmTMB works                                                         | PASS    |  1 |  2.07 |      |
| [test\_ggcorrmat.R](testthat/test_ggcorrmat.R#L38)                                                 | ggcorrmat                          | checking ggcorrmat - without NAs - pearson’s r                        | PASS    | 16 |  0.13 |      |
| [test\_ggcorrmat.R](testthat/test_ggcorrmat.R#L130)                                                | ggcorrmat                          | checking ggcorrmat - with NAs - robust r                              | PASS    | 14 |  0.15 |      |
| [test\_ggcorrmat.R](testthat/test_ggcorrmat.R#L211)                                                | ggcorrmat                          | checking ggcorrmat - with NAs - spearman’s rho                        | PASS    | 11 |  0.08 |      |
| [test\_ggcorrmat.R](testthat/test_ggcorrmat.R#L241_L244)                                           | ggcorrmat                          | checking sample sizes                                                 | PASS    |  5 |  0.03 |      |
| [test\_ggcorrmat.R](testthat/test_ggcorrmat.R#L268)                                                | ggcorrmat                          | checking p-values                                                     | PASS    |  2 |  0.00 |      |
| [test\_ggcorrmat.R](testthat/test_ggcorrmat.R#L292_L303)                                           | ggcorrmat                          | checking confidence intervals                                         | PASS    |  3 |  0.03 |      |
| [test\_ggcorrmat.R](testthat/test_ggcorrmat.R#L360_L363)                                           | ggcorrmat                          | checking messages                                                     | PASS    |  4 |  0.19 |      |
| [test\_ggdotplotstats.R](testthat/test_ggdotplotstats.R#L64_L69)                                   | ggdotplotstats                     | ggdotplotstats works as expected                                      | PASS    | 21 |  0.44 |      |
| [test\_ggdotplotstats.R](testthat/test_ggdotplotstats.R#L178_L181)                                 | ggdotplotstats                     | ggdotplotstats works with summarized data                             | PASS    |  8 |  0.20 |      |
| [test\_gghistostats.R](testthat/test_gghistostats.R#L42)                                           | gghistostats                       | checking gghistostats plot and parametric stats - data with NAs       | PASS    | 22 |  0.49 |      |
| [test\_gghistostats.R](testthat/test_gghistostats.R#L162)                                          | gghistostats                       | checking gghistostats and non-parametric stats - data without NAs     | PASS    | 27 |  0.42 |      |
| [test\_gghistostats.R](testthat/test_gghistostats.R#L268)                                          | gghistostats                       | checking robust stats and proportions                                 | PASS    | 10 |  0.44 |      |
| [test\_gghistostats.R](testthat/test_gghistostats.R#L349)                                          | gghistostats                       | checking bayes stats and density                                      | PASS    | 12 |  0.39 |      |
| [test\_gghistostats.R](testthat/test_gghistostats.R#L398)                                          | gghistostats                       | checking with default binwidth                                        | PASS    |  5 |  0.12 |      |
| [test\_ggpiestats.R](testthat/test_ggpiestats.R#L41)                                               | ggpiestats                         | checking one sample proportion test                                   | PASS    | 11 |  0.22 |      |
| [test\_ggpiestats.R](testthat/test_ggpiestats.R#L120)                                              | ggpiestats                         | checking labels with contingency tab                                  | PASS    | 15 |  6.12 |      |
| [test\_ggpiestats.R](testthat/test_ggpiestats.R#L209)                                              | ggpiestats                         | checking labels with counts                                           | PASS    |  8 |  5.83 |      |
| [test\_ggpiestats.R](testthat/test_ggpiestats.R#L261)                                              | ggpiestats                         | checking labels with contingency tab (paired)                         | PASS    |  1 |  0.25 |      |
| [test\_ggpiestats.R](testthat/test_ggpiestats.R#L285)                                              | ggpiestats                         | checking if functions work without enough data                        | PASS    |  2 |  0.14 |      |
| [test\_ggscatterstats.R](testthat/test_ggscatterstats.R#L45)                                       | ggscatterstats                     | checking ggscatterstats - without NAs - pearson’s r                   | PASS    | 16 |  0.19 |      |
| [test\_ggscatterstats.R](testthat/test_ggscatterstats.R#L122)                                      | ggscatterstats                     | checking ggscatterstats - without NAs - spearman’s rho                | PASS    |  1 |  0.51 |      |
| [test\_ggscatterstats.R](testthat/test_ggscatterstats.R#L160)                                      | ggscatterstats                     | checking ggscatterstats - without NAs - percentage bend               | PASS    |  3 |  0.42 |      |
| [test\_ggscatterstats.R](testthat/test_ggscatterstats.R#L201_L204)                                 | ggscatterstats                     | checking median display                                               | PASS    |  8 |  0.21 |      |
| [test\_ggscatterstats.R](testthat/test_ggscatterstats.R#L267)                                      | ggscatterstats                     | bayes factor plus class of object                                     | PASS    |  6 |  0.75 |      |
| [test\_ggscatterstats.R](testthat/test_ggscatterstats.R#L300_L312)                                 | ggscatterstats                     | checking ggscatterstats with different kinds of inputs to labeling    | PASS    |  4 |  0.46 |      |
| [test\_ggscatterstats.R](testthat/test_ggscatterstats.R#L376_L379)                                 | ggscatterstats                     | with marginals                                                        | PASS    |  1 |  0.65 |      |
| [test\_ggscatterstats.R](testthat/test_ggscatterstats.R#L413_L416)                                 | ggscatterstats                     | class of object                                                       | PASS    |  2 |  0.06 |      |
| [test\_ggsignif\_position\_calculator.R](testthat/test_ggsignif_position_calculator.R#L9)          | ggsignif\_position\_calculator     | y coordinates for ggsignif are accurate                               | SKIPPED |  1 |  0.00 | \+   |
| [test\_grouped\_ggbetweenstats.R](testthat/test_grouped_ggbetweenstats.R#L9)                       | grouped\_ggbetweenstats            | grouping.var works across vector types                                | SKIPPED |  1 |  0.00 | \+   |
| [test\_grouped\_ggcorrmat.R](testthat/test_grouped_ggcorrmat.R#L8)                                 | grouped\_ggcorrmat                 | grouped\_ggcorrmat plots work                                         | SKIPPED |  1 |  0.02 | \+   |
| [test\_grouped\_ggcorrmat.R](testthat/test_grouped_ggcorrmat.R#L91)                                | grouped\_ggcorrmat                 | grouped\_ggcorrmat stats work                                         | PASS    |  4 |  0.08 |      |
| [test\_grouped\_ggdotplotstats.R](testthat/test_grouped_ggdotplotstats.R#L46)                      | grouped\_ggdotplotstats            | grouped\_ggdotplotstats works                                         | PASS    |  2 |  2.55 |      |
| [test\_grouped\_gghistostats.R](testthat/test_grouped_gghistostats.R#L6)                           | grouped\_gghistostats              | grouped\_gghistostats works                                           | SKIPPED |  1 |  0.00 | \+   |
| [test\_grouped\_ggpiestats.R](testthat/test_grouped_ggpiestats.R#L6)                               | grouped\_ggpiestats                | grouped\_ggpiestats works                                             | SKIPPED |  1 |  0.00 | \+   |
| [test\_grouped\_ggscatterstats.R](testthat/test_grouped_ggscatterstats.R#L6)                       | grouped\_ggscatterstats            | grouped\_ggscatterstats works                                         | SKIPPED |  1 |  0.00 | \+   |
| [test\_helper\_messages.R](testthat/test_helper_messages.R#L9)                                     | helper\_messages                   | grouped\_message is working                                           | SKIPPED |  1 |  0.01 | \+   |
| [test\_helper\_messages.R](testthat/test_helper_messages.R#L24)                                    | helper\_messages                   | effsize\_ci\_message is working                                       | SKIPPED |  1 |  0.00 | \+   |
| [test\_helper\_messages.R](testthat/test_helper_messages.R#L46)                                    | helper\_messages                   | ggcorrmat\_matrix\_message is working                                 | SKIPPED |  1 |  0.00 | \+   |
| [test\_helper\_messages.R](testthat/test_helper_messages.R#L61)                                    | helper\_messages                   | palette\_message is working                                           | SKIPPED |  1 |  0.00 | \+   |
| [test\_helper\_messages.R](testthat/test_helper_messages.R#L80)                                    | helper\_messages                   | normality\_message is working                                         | SKIPPED |  1 |  0.00 | \+   |
| [test\_helper\_messages.R](testthat/test_helper_messages.R#L109)                                   | helper\_messages                   | bartlett\_message is working                                          | SKIPPED |  1 |  0.00 | \+   |
| [test\_helpers\_bf\_tests.R](testthat/test_helpers_bf_tests.R#L21)                                 | helpers\_bf\_tests                 | bayes factor plus posterior checks (correlation)                      | PASS    | 11 |  0.09 |      |
| [test\_helpers\_bf\_tests.R](testthat/test_helpers_bf_tests.R#L76)                                 | helpers\_bf\_tests                 | bayes factor plus posterior checks (paired t-test)                    | PASS    | 13 |  0.31 |      |
| [test\_helpers\_bf\_tests.R](testthat/test_helpers_bf_tests.R#L115_L131)                           | helpers\_bf\_tests                 | bayes factor caption maker check                                      | PASS    |  1 |  0.02 |      |
| [test\_histo\_labeller.R](testthat/test_histo_labeller.R#L8)                                       | Helpers gghistostats               | y coordinate for labeller works                                       | SKIPPED |  1 |  0.00 | \+   |
| [test\_histo\_labeller.R](testthat/test_histo_labeller.R#L45)                                      | Helpers gghistostats               | checking if labeling works                                            | PASS    |  7 |  0.12 |      |
| [test\_kw\_eta\_h\_ci.R](testthat/test_kw_eta_h_ci.R#L6)                                           | kw\_eta\_h\_ci                     | confidence interval for effect size for Kruskal-Wallis test           | SKIPPED |  1 |  0.00 | \+   |
| [test\_lm\_effsize\_ci.R](testthat/test_lm_effsize_ci.R#L6)                                        | lm\_effsize\_ci                    | lm\_effsize\_ci works                                                 | SKIPPED |  1 |  0.00 | \+   |
| [test\_long\_to\_wide\_converter.R](testthat/test_long_to_wide_converter.R#L31)                    | long\_to\_wide\_converter          | long\_to\_wide\_converter works                                       | PASS    |  4 |  0.10 |      |
| [test\_mean\_labeller.R](testthat/test_mean_labeller.R#L30_L33)                                    | mean\_labeller                     | mean\_labeller works                                                  | PASS    |  6 |  0.40 |      |
| [test\_numdf\_summary.R](testthat/test_numdf_summary.R#L15)                                        | numdf\_summary                     | checking numdf\_summary - with NAs                                    | PASS    |  3 |  0.03 |      |
| [test\_numdf\_summary.R](testthat/test_numdf_summary.R#L33)                                        | numdf\_summary                     | checking numdf\_summary - without NAs                                 | PASS    |  3 |  0.03 |      |
| [test\_pairwise\_ggsignif.R](testthat/test_pairwise_ggsignif.R#L25)                                | pairwise\_p with ggsignif          | check comparison significant displays - adjusted                      | PASS    |  2 |  2.24 |      |
| [test\_pairwise\_ggsignif.R](testthat/test_pairwise_ggsignif.R#L73)                                | pairwise\_p with ggsignif          | check non-significant comparison displays - no adjustment             | PASS    | 14 |  1.75 |      |
| [test\_pairwise\_ggsignif.R](testthat/test_pairwise_ggsignif.R#L151)                               | pairwise\_p with ggsignif          | check mixed comparison displays - adjusted                            | PASS    | 18 | 79.40 |      |
| [test\_pairwise\_ggsignif.R](testthat/test_pairwise_ggsignif.R#L267)                               | pairwise\_p with ggsignif          | check robust test display - adjusted                                  | PASS    | 12 |  2.10 |      |
| [test\_pairwise\_ggsignif.R](testthat/test_pairwise_ggsignif.R#L347)                               | pairwise\_p with ggsignif          | check student’s t test display - adjusted                             | PASS    | 13 |  0.45 |      |
| [test\_pairwise\_p.R](testthat/test_pairwise_p.R#L8)                                               | pairwise\_p                        | `pairwise_p()` works for between-subjects design                      | SKIPPED |  1 |  0.00 | \+   |
| [test\_pairwise\_p.R](testthat/test_pairwise_p.R#L139)                                             | pairwise\_p                        | `pairwise_p()` works for within-subjects design                       | SKIPPED |  1 |  0.00 | \+   |
| [test\_pairwise\_p.R](testthat/test_pairwise_p.R#L290)                                             | pairwise\_p                        | `pairwise_p()` messages are correct for between-subjects              | PASS    |  9 | 11.30 |      |
| [test\_pairwise\_p.R](testthat/test_pairwise_p.R#L376)                                             | pairwise\_p                        | `pairwise_p()` messages are correct for within-subjects               | PASS    |  7 |  0.26 |      |
| [test\_pairwise\_p\_caption.R](testthat/test_pairwise_p_caption.R#L32_L43)                         | pairwise\_p\_caption               | `pairwise_p_caption()` works                                          | PASS    |  3 |  0.00 |      |
| [test\_robcor\_ci.R](testthat/test_robcor_ci.R#L40)                                                | robcor\_ci                         | robcor\_ci works                                                      | PASS    |  8 |  0.53 |      |
| [test\_subtitle\_anova\_bayes.R](testthat/test_subtitle_anova_bayes.R#L51)                         | subtitle\_anova\_bayes             | subtitle\_anova\_bayes works                                          | PASS    |  1 |  5.26 |      |
| [test\_subtitle\_anova\_parametric.R](testthat/test_subtitle_anova_parametric.R#L57)               | subtitle\_anova\_parametric        | parametric anova subtitles work (without NAs)                         | PASS    |  1 |  0.05 |      |
| [test\_subtitle\_anova\_parametric.R](testthat/test_subtitle_anova_parametric.R#L128)              | subtitle\_anova\_parametric        | parametric anova subtitles work (with NAs)                            | PASS    |  1 |  0.94 |      |
| [test\_subtitle\_anova\_parametric.R](testthat/test_subtitle_anova_parametric.R#L185)              | subtitle\_anova\_parametric        | parametric anova subtitles with partial omega-squared                 | PASS    |  1 |  0.12 |      |
| [test\_subtitle\_anova\_parametric.R](testthat/test_subtitle_anova_parametric.R#L243_L246)         | subtitle\_anova\_parametric        | parametric anova subtitles with partial eta-squared and data with NAs | PASS    |  1 |  0.05 |      |
| [test\_subtitle\_anova\_parametric.R](testthat/test_subtitle_anova_parametric.R#L356)              | subtitle\_anova\_parametric        | parametric anova subtitles with partial eta-squared and data with NAs | PASS    |  2 |  0.17 |      |
| [test\_subtitle\_anova\_robust.R](testthat/test_subtitle_anova_robust.R#L56)                       | subtitle\_anova\_robust            | subtitle\_anova\_robust works - conf.type = norm                      | PASS    |  1 |  1.75 |      |
| [test\_subtitle\_anova\_robust.R](testthat/test_subtitle_anova_robust.R#L114)                      | subtitle\_anova\_robust            | subtitle\_anova\_robust works - conf.type = perc                      | PASS    |  1 |  1.93 |      |
| [test\_subtitle\_contingency\_tab.R](testthat/test_subtitle_contingency_tab.R#L57)                 | subtitle\_contingency\_tab         | subtitle\_contingency\_tab works - data without NAs                   | PASS    |  1 |  0.75 |      |
| [test\_subtitle\_contingency\_tab.R](testthat/test_subtitle_contingency_tab.R#L129)                | subtitle\_contingency\_tab         | subtitle\_contingency\_tab works - data with NAs                      | PASS    |  1 |  1.64 |      |
| [test\_subtitle\_contingency\_tab\_gof.R](testthat/test_subtitle_contingency_tab_gof.R#L43)        | subtitle\_contingency\_tab\_gof    | Goodness of Fit subtitle\_contingency\_tab works without counts       | PASS    |  1 |  0.03 |      |
| [test\_subtitle\_contingency\_tab\_gof.R](testthat/test_subtitle_contingency_tab_gof.R#L88)        | subtitle\_contingency\_tab\_gof    | Goodness of Fit subtitle\_contingency\_tab works with counts          | PASS    |  1 |  0.04 |      |
| [test\_subtitle\_contingency\_tab\_gof.R](testthat/test_subtitle_contingency_tab_gof.R#L123)       | subtitle\_contingency\_tab\_gof    | works                                                                 | PASS    |  2 |  0.08 |      |
| [test\_subtitle\_contingency\_tab\_paired.R](testthat/test_subtitle_contingency_tab_paired.R#L78)  | subtitle\_contingency\_tab\_paired | paired subtitle\_contingency\_tab works - counts data without NAs     | PASS    |  1 |  0.05 |      |
| [test\_subtitle\_contingency\_tab\_paired.R](testthat/test_subtitle_contingency_tab_paired.R#L177) | subtitle\_contingency\_tab\_paired | paired subtitle\_contingency\_tab works - with NAs                    | PASS    |  1 |  0.05 |      |
| [test\_subtitle\_friedman\_nonparametric.R](testthat/test_subtitle_friedman_nonparametric.R#L51)   | subtitle\_friedman\_nonparametric  | subtitle\_friedman\_nonparametric works                               | PASS    |  1 |  0.05 |      |
| [test\_subtitle\_ggscatterstats.R](testthat/test_subtitle_ggscatterstats.R#L48)                    | subtitle\_ggscatterstats           | subtitle\_ggscatterstats works - nonparametric                        | PASS    |  1 |  0.47 |      |
| [test\_subtitle\_ggscatterstats.R](testthat/test_subtitle_ggscatterstats.R#L97)                    | subtitle\_ggscatterstats           | subtitle\_ggscatterstats works - parametric                           | PASS    |  1 |  0.00 |      |
| [test\_subtitle\_ggscatterstats.R](testthat/test_subtitle_ggscatterstats.R#L144)                   | subtitle\_ggscatterstats           | subtitle\_ggscatterstats works - robust                               | PASS    |  1 |  0.04 |      |
| [test\_subtitle\_kw\_nonparametric.R](testthat/test_subtitle_kw_nonparametric.R#L53)               | subtitle\_kw\_nonparametric        | subtitle\_kw\_nonparametric works - data without NAs                  | PASS    |  1 |  0.88 |      |
| [test\_subtitle\_kw\_nonparametric.R](testthat/test_subtitle_kw_nonparametric.R#L110)              | subtitle\_kw\_nonparametric        | subtitle\_kw\_nonparametric works - data with NAs                     | PASS    |  1 |  0.53 |      |
| [test\_subtitle\_mann\_nonparametric.R](testthat/test_subtitle_mann_nonparametric.R#L46)           | subtitle\_mann\_nonparametric      | subtitle\_mann\_nonparametric works                                   | PASS    |  1 |  0.10 |      |
| [test\_subtitle\_mann\_paired.R](testthat/test_subtitle_mann_paired.R#L217)                        | subtitle\_mann\_paired             | subtitle\_mann\_paired works                                          | PASS    |  2 |  0.11 |      |
| [test\_subtitle\_meta.R](testthat/test_subtitle_meta.R#L56)                                        | subtitle\_meta\_ggcoefstats        | subtitle\_meta\_ggcoefstats works                                     | PASS    |  1 |  0.00 |      |
| [test\_subtitle\_t\_bayes.R](testthat/test_subtitle_t_bayes.R#L47)                                 | subtitle\_t\_bayes                 | subtitle\_t\_bayes works                                              | PASS    |  1 |  0.25 |      |
| [test\_subtitle\_t\_bayes\_paired.R](testthat/test_subtitle_t_bayes_paired.R#L218)                 | subtitle\_t\_bayes\_paired         | subtitle\_t\_bayes\_paired works                                      | PASS    |  1 |  0.25 |      |
| [test\_subtitle\_t\_onesample.R](testthat/test_subtitle_t_onesample.R#L56)                         | subtitle\_t\_onesample             | subtitle\_t\_onesample parametric works                               | PASS    |  1 |  0.27 |      |
| [test\_subtitle\_t\_onesample.R](testthat/test_subtitle_t_onesample.R#L104)                        | subtitle\_t\_onesample             | subtitle\_t\_onesample non-parametric works                           | PASS    |  1 |  0.06 |      |
| [test\_subtitle\_t\_onesample.R](testthat/test_subtitle_t_onesample.R#L153)                        | subtitle\_t\_onesample             | subtitle\_t\_onesample robust works                                   | PASS    |  1 |  0.11 |      |
| [test\_subtitle\_t\_onesample.R](testthat/test_subtitle_t_onesample.R#L203)                        | subtitle\_t\_onesample             | subtitle\_t\_onesample bayes factor works                             | PASS    |  1 |  0.12 |      |
| [test\_subtitle\_t\_onesample.R](testthat/test_subtitle_t_onesample.R#L244_L247)                   | subtitle\_t\_onesample             | subtitle\_t\_onesample works                                          | PASS    |  3 |  0.32 |      |
| [test\_subtitle\_t\_parametric.R](testthat/test_subtitle_t_parametric.R#L63)                       | subtitle\_t\_parametric            | parametric t-test works (between-subjects without NAs)                | PASS    |  1 |  0.01 |      |
| [test\_subtitle\_t\_parametric.R](testthat/test_subtitle_t_parametric.R#L116)                      | subtitle\_t\_parametric            | parametric t-test works (between-subjects with NAs)                   | PASS    |  1 |  0.20 |      |
| [test\_subtitle\_t\_parametric\_paired.R](testthat/test_subtitle_t_parametric_paired.R#L226)       | subtitle\_t\_parametric\_paired    | subtitle\_t\_parametric\_paired works (Hedge’s g)                     | PASS    |  1 |  0.02 |      |
| [test\_subtitle\_t\_parametric\_paired.R](testthat/test_subtitle_t_parametric_paired.R#L284)       | subtitle\_t\_parametric\_paired    | subtitle\_t\_parametric\_paired works (Cohen’s d)                     | PASS    |  1 |  0.02 |      |
| [test\_subtitle\_t\_robust.R](testthat/test_subtitle_t_robust.R#L57)                               | subtitle\_t\_robust                | subtitle\_t\_robust - within-subjects                                 | PASS    |  2 |  2.33 |      |
| [test\_subtitle\_t\_robust.R](testthat/test_subtitle_t_robust.R#L132)                              | subtitle\_t\_robust                | subtitle\_t\_robust - between-subjects                                | PASS    |  1 |  0.37 |      |
| [test\_subtitle\_templates.R](testthat/test_subtitle_templates.R#L58)                              | subtitle\_templates                | checking if subtitle template works with a single parameter           | PASS    |  1 |  0.00 |      |
| [test\_subtitle\_templates.R](testthat/test_subtitle_templates.R#L120)                             | subtitle\_templates                | checking if subtitle template works with two parameters               | PASS    |  2 |  0.02 |      |
| [test\_switch\_statements.R](testthat/test_switch_statements.R#L8)                                 | switch statements                  | switch for p adjustment works                                         | PASS    |  4 |  0.00 |      |
| [test\_switch\_statements.R](testthat/test_switch_statements.R#L23)                                | switch statements                  | switch for effct size type works                                      | SKIPPED |  1 |  0.00 | \+   |
| [test\_switch\_statements.R](testthat/test_switch_statements.R#L47)                                | switch statements                  | switch for stats type works                                           | SKIPPED |  1 |  0.00 | \+   |
| [test\_t1way\_ci.R](testthat/test_t1way_ci.R#L6)                                                   | t1way\_ci                          | t1way\_ci works                                                       | SKIPPED |  1 |  0.00 | \+   |
| [test\_theme\_ggstatsplot.R](testthat/test_theme_ggstatsplot.R#L23)                                | theme\_ggstatsplot                 | `theme_ggstatsplot()` works                                           | PASS    |  6 |  0.03 |      |
| [test\_theme\_ggstatsplot.R](testthat/test_theme_ggstatsplot.R#L58)                                | theme\_ggstatsplot                 | `theme_pie()` works                                                   | PASS    |  5 |  0.03 |      |
| [test\_yuend\_ci\_paired.R](testthat/test_yuend_ci_paired.R#L6)                                    | test\_yuend\_ci\_paired            | Yuen’s test on trimmed means for dependent samples works              | SKIPPED |  1 |  0.00 | \+   |

| Failed | Warning | Skipped |
| :----- | :------ | :------ |
| \!     | \-      | \+      |

</details>

<details>

<summary> Session Info </summary>

| Field    | Value                                              |
| :------- | :------------------------------------------------- |
| Version  | R Under development (unstable) (2018-11-30 r75724) |
| Platform | x86\_64-w64-mingw32/x64 (64-bit)                   |
| Running  | Windows \>= 8 x64 (build 9200)                     |
| Language | English\_United States                             |
| Timezone | Asia/Calcutta                                      |

| Package  | Version |
| :------- | :------ |
| testthat | 2.0.1   |
| covr     | 3.2.1   |
| covrpage | 0.0.67  |

</details>

<!--- Final Status : skipped/warning --->
