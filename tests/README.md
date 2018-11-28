Tests and Coverage
================
28 November, 2018 10:50:47

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr)
package.

| Object                                                                               | Coverage (%) |
| :----------------------------------------------------------------------------------- | :----------: |
| ggstatsplot                                                                          |    60.33     |
| [R/ggcoefstats.R](../R/ggcoefstats.R)                                                |     0.00     |
| [R/ggdotplotstats.R](../R/ggdotplotstats.R)                                          |     0.00     |
| [R/helpers\_ggcoefstats.R](../R/helpers_ggcoefstats.R)                               |     0.00     |
| [R/helpers\_pairwise\_comparison.R](../R/helpers_pairwise_comparison.R)              |     0.00     |
| [R/set\_cwd.R](../R/set_cwd.R)                                                       |     0.00     |
| [R/helpers\_messages.R](../R/helpers_messages.R)                                     |     5.00     |
| [R/combine\_plots.R](../R/combine_plots.R)                                           |    10.94     |
| [R/helpers\_gghistostats.R](../R/helpers_gghistostats.R)                             |    37.80     |
| [R/ggscatterstats.R](../R/ggscatterstats.R)                                          |    53.87     |
| [R/helpers\_gghistostats\_subtitles.R](../R/helpers_gghistostats_subtitles.R)        |    55.31     |
| [R/gghistostats.R](../R/gghistostats.R)                                              |    56.38     |
| [R/grouped\_ggbetweenstats.R](../R/grouped_ggbetweenstats.R)                         |    57.56     |
| [R/helpers\_ggbetween\_anova\_subtitles.R](../R/helpers_ggbetween_anova_subtitles.R) |    69.81     |
| [R/helpers\_bf\_tests.R](../R/helpers_bf_tests.R)                                    |    71.98     |
| [R/helpers\_effsize\_ci.R](../R/helpers_effsize_ci.R)                                |    72.83     |
| [R/grouped\_ggcorrmat.R](../R/grouped_ggcorrmat.R)                                   |    73.21     |
| [R/helpers\_ggscatterstats\_subtitles.R](../R/helpers_ggscatterstats_subtitles.R)    |    73.61     |
| [R/grouped\_ggscatterstats.R](../R/grouped_ggscatterstats.R)                         |    73.68     |
| [R/ggbetweenstats.R](../R/ggbetweenstats.R)                                          |    73.77     |
| [R/ggcorrmat.R](../R/ggcorrmat.R)                                                    |    74.46     |
| [R/helpers\_ggbetween\_t\_subtitles.R](../R/helpers_ggbetween_t_subtitles.R)         |    82.73     |
| [R/helpers\_ggpiestats\_subtitles.R](../R/helpers_ggpiestats_subtitles.R)            |    85.49     |
| [R/ggpiestats.R](../R/ggpiestats.R)                                                  |    91.05     |
| [R/switch\_functions.R](../R/switch_functions.R)                                     |    94.55     |
| [R/theme\_ggstatsplot.R](../R/theme_ggstatsplot.R)                                   |    98.37     |
| [R/grouped\_gghistostats.R](../R/grouped_gghistostats.R)                             |    98.97     |
| [R/grouped\_ggpiestats.R](../R/grouped_ggpiestats.R)                                 |    99.58     |
| [R/helpers\_ggbetweenstats.R](../R/helpers_ggbetweenstats.R)                         |    100.00    |
| [R/helpers\_ggcorrmat.R](../R/helpers_ggcorrmat.R)                                   |    100.00    |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat)
package.

| file                                                                                          |  n |  time | error | failed | skipped | warning |
| :-------------------------------------------------------------------------------------------- | -: | ----: | ----: | -----: | ------: | ------: |
| [test\_argument\_count.R](testthat/test_argument_count.R)                                     |  1 |  0.02 |     0 |      0 |       0 |       0 |
| [test\_chisq\_v\_ci.R](testthat/test_chisq_v_ci.R)                                            | 10 |  2.31 |     0 |      0 |       0 |       0 |
| [test\_cor\_test\_ci.R](testthat/test_cor_test_ci.R)                                          | 12 |  0.50 |     0 |      0 |       0 |       0 |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R)                                      | 25 |  6.42 |     0 |      0 |       0 |       0 |
| [test\_ggcorrmat.R](testthat/test_ggcorrmat.R)                                                | 46 |  0.22 |     0 |      0 |       0 |       0 |
| [test\_ggpiestats.R](testthat/test_ggpiestats.R)                                              | 38 | 12.85 |     0 |      0 |       0 |       0 |
| [test\_grouped\_ggbetweenstats.R](testthat/test_grouped_ggbetweenstats.R)                     |  3 |  2.51 |     0 |      0 |       0 |       0 |
| [test\_grouped\_ggcorrmat.R](testthat/test_grouped_ggcorrmat.R)                               |  4 |  1.86 |     0 |      0 |       0 |       0 |
| [test\_grouped\_gghistostats.R](testthat/test_grouped_gghistostats.R)                         |  3 |  2.32 |     0 |      0 |       0 |       0 |
| [test\_grouped\_ggpiestats.R](testthat/test_grouped_ggpiestats.R)                             |  8 | 29.99 |     0 |      0 |       0 |       0 |
| [test\_grouped\_ggscatterstats.R](testthat/test_grouped_ggscatterstats.R)                     |  3 |  1.09 |     0 |      0 |       0 |       0 |
| [test\_helpers\_ggbetweenstats.R](testthat/test_helpers_ggbetweenstats.R)                     |  6 |  0.28 |     0 |      0 |       0 |       0 |
| [test\_lm\_effsize\_ci.R](testthat/test_lm_effsize_ci.R)                                      |  9 |  3.66 |     0 |      0 |       0 |       0 |
| [test\_numdf\_summary.R](testthat/test_numdf_summary.R)                                       |  6 |  0.01 |     0 |      0 |       0 |       0 |
| [test\_robcor\_ci.R](testthat/test_robcor_ci.R)                                               |  8 |  0.15 |     0 |      0 |       0 |       0 |
| [test\_subtitle\_anova\_bayes.R](testthat/test_subtitle_anova_bayes.R)                        |  4 |  0.03 |     0 |      0 |       0 |       0 |
| [test\_subtitle\_anova\_parametric.R](testthat/test_subtitle_anova_parametric.R)              |  7 |  0.73 |     0 |      0 |       0 |       0 |
| [test\_subtitle\_anova\_robust.R](testthat/test_subtitle_anova_robust.R)                      |  4 |  0.98 |     0 |      0 |       0 |       0 |
| [test\_subtitle\_contingency\_tab.R](testthat/test_subtitle_contingency_tab.R)                |  5 |  0.94 |     0 |      0 |       0 |       0 |
| [test\_subtitle\_contingency\_tab\_gof.R](testthat/test_subtitle_contingency_tab_gof.R)       |  3 |  0.03 |     0 |      0 |       0 |       0 |
| [test\_subtitle\_contingency\_tab\_paired.R](testthat/test_subtitle_contingency_tab_paired.R) |  3 |  0.05 |     0 |      0 |       0 |       0 |
| [test\_subtitle\_ggscatterstats.R](testthat/test_subtitle_ggscatterstats.R)                   |  3 |  0.49 |     0 |      0 |       0 |       0 |
| [test\_subtitle\_kw\_nonparametric.R](testthat/test_subtitle_kw_nonparametric.R)              |  4 |  1.20 |     0 |      0 |       0 |       0 |
| [test\_subtitle\_mann\_nonparametric.R](testthat/test_subtitle_mann_nonparametric.R)          |  4 |  0.19 |     0 |      0 |       0 |       0 |
| [test\_subtitle\_mann\_paired.R](testthat/test_subtitle_mann_paired.R)                        |  4 |  0.03 |     0 |      0 |       0 |       0 |
| [test\_subtitle\_t\_bayes.R](testthat/test_subtitle_t_bayes.R)                                |  4 |  0.12 |     0 |      0 |       0 |       0 |
| [test\_subtitle\_t\_bayes\_paired.R](testthat/test_subtitle_t_bayes_paired.R)                 |  4 |  0.11 |     0 |      0 |       0 |       0 |
| [test\_subtitle\_t\_onesample.R](testthat/test_subtitle_t_onesample.R)                        |  4 |  0.30 |     0 |      0 |       0 |       0 |
| [test\_subtitle\_t\_parametric.R](testthat/test_subtitle_t_parametric.R)                      |  5 |  0.24 |     0 |      0 |       0 |       0 |
| [test\_subtitle\_t\_parametric\_paired.R](testthat/test_subtitle_t_parametric_paired.R)       |  4 |  0.01 |     0 |      0 |       0 |       0 |
| [test\_t1way\_ci.R](testthat/test_t1way_ci.R)                                                 |  5 |  4.92 |     0 |      0 |       0 |       0 |

<details closed>

<summary> Show Detailed Test Results
</summary>

| file                                                                                                  | context                            | test                                                   | status |  n |  time |
| :---------------------------------------------------------------------------------------------------- | :--------------------------------- | :----------------------------------------------------- | :----- | -: | ----: |
| [test\_argument\_count.R](testthat/test_argument_count.R#L57_L60)                                     | argument\_count                    | argument\_count is correct                             | PASS   |  1 |  0.02 |
| [test\_chisq\_v\_ci.R](testthat/test_chisq_v_ci.R#L46)                                                | chisq\_v\_ci                       | chisq\_v\_ci works                                     | PASS   | 10 |  2.31 |
| [test\_cor\_test\_ci.R](testthat/test_cor_test_ci.R#L45)                                              | cor\_test\_ci                      | cor\_test\_ci works                                    | PASS   | 12 |  0.50 |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R#L9_L16)                                       | ggbetweenstats                     | error when x and outlier.label are same                | PASS   |  1 |  0.02 |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R#L26_L36)                                      | ggbetweenstats                     | outlier.labeling works across vector types             | PASS   |  3 |  3.50 |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R#L114)                                         | ggbetweenstats                     | checking labels and data from plot                     | PASS   | 18 |  2.67 |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R#L190)                                         | ggbetweenstats                     | checking mean labels are working                       | PASS   |  3 |  0.23 |
| [test\_ggcorrmat.R](testthat/test_ggcorrmat.R#L33)                                                    | ggcorrmat                          | checking ggcorrmat - without NAs - pearson’s r         | PASS   | 17 |  0.03 |
| [test\_ggcorrmat.R](testthat/test_ggcorrmat.R#L93)                                                    | ggcorrmat                          | checking ggcorrmat - with NAs - robust r               | PASS   | 17 |  0.14 |
| [test\_ggcorrmat.R](testthat/test_ggcorrmat.R#L145)                                                   | ggcorrmat                          | checking ggcorrmat - with NAs - spearman’s rho         | PASS   | 12 |  0.05 |
| [test\_ggpiestats.R](testthat/test_ggpiestats.R#L37)                                                  | ggpiestats                         | checking one sample proportion test                    | PASS   | 11 |  0.08 |
| [test\_ggpiestats.R](testthat/test_ggpiestats.R#L86)                                                  | ggpiestats                         | checking labels with contingency tab                   | PASS   | 11 |  4.03 |
| [test\_ggpiestats.R](testthat/test_ggpiestats.R#L142)                                                 | ggpiestats                         | checking labels with counts                            | PASS   | 15 |  8.53 |
| [test\_ggpiestats.R](testthat/test_ggpiestats.R#L202)                                                 | ggpiestats                         | checking labels with contingency tab (paired)          | PASS   |  1 |  0.21 |
| [test\_grouped\_ggbetweenstats.R](testthat/test_grouped_ggbetweenstats.R#L12_L32)                     | grouped\_ggbetweenstats            | grouping.var works across vector types                 | PASS   |  3 |  2.51 |
| [test\_grouped\_ggcorrmat.R](testthat/test_grouped_ggcorrmat.R#L18_L27)                               | grouped\_ggcorrmat                 | grouped\_ggcorrmat works                               | PASS   |  4 |  1.86 |
| [test\_grouped\_gghistostats.R](testthat/test_grouped_gghistostats.R#L9_L19)                          | grouped\_gghistostats              | grouped\_gghistostats works                            | PASS   |  3 |  2.32 |
| [test\_grouped\_ggpiestats.R](testthat/test_grouped_ggpiestats.R#L12_L21)                             | grouped\_ggpiestats                | grouped\_ggpiestats works                              | PASS   |  8 | 29.99 |
| [test\_grouped\_ggscatterstats.R](testthat/test_grouped_ggscatterstats.R#L9_L17)                      | grouped\_ggscatterstats            | grouped\_ggscatterstats works                          | PASS   |  3 |  1.09 |
| [test\_helpers\_ggbetweenstats.R](testthat/test_helpers_ggbetweenstats.R#L30_L33)                     | helpers\_ggbetweenstats            | mean\_labeller works                                   | PASS   |  6 |  0.28 |
| [test\_lm\_effsize\_ci.R](testthat/test_lm_effsize_ci.R#L67)                                          | lm\_effsize\_ci                    | lm\_effsize\_ci works                                  | PASS   |  9 |  3.66 |
| [test\_numdf\_summary.R](testthat/test_numdf_summary.R#L15)                                           | numdf\_summary                     | checking numdf\_summary - with NAs                     | PASS   |  3 |  0.01 |
| [test\_numdf\_summary.R](testthat/test_numdf_summary.R#L33)                                           | numdf\_summary                     | checking numdf\_summary - without NAs                  | PASS   |  3 |  0.00 |
| [test\_robcor\_ci.R](testthat/test_robcor_ci.R#L39)                                                   | robcor\_ci                         | robcor\_ci works                                       | PASS   |  8 |  0.15 |
| [test\_subtitle\_anova\_bayes.R](testthat/test_subtitle_anova_bayes.R#L51_L54)                        | subtitle\_anova\_bayes             | subtitle\_anova\_bayes works                           | PASS   |  4 |  0.03 |
| [test\_subtitle\_anova\_parametric.R](testthat/test_subtitle_anova_parametric.R#L54_L57)              | subtitle\_anova\_parametric        | parametric anova subtitles work (without NAs)          | PASS   |  3 |  0.01 |
| [test\_subtitle\_anova\_parametric.R](testthat/test_subtitle_anova_parametric.R#L105)                 | subtitle\_anova\_parametric        | parametric anova subtitles work (with NAs)             | PASS   |  4 |  0.72 |
| [test\_subtitle\_anova\_robust.R](testthat/test_subtitle_anova_robust.R#L53_L56)                      | subtitle\_anova\_robust            | subtitle\_anova\_robust works                          | PASS   |  4 |  0.98 |
| [test\_subtitle\_contingency\_tab.R](testthat/test_subtitle_contingency_tab.R#L57_L60)                | subtitle\_contingency\_tab         | subtitle\_contingency\_tab works                       | PASS   |  5 |  0.94 |
| [test\_subtitle\_contingency\_tab\_gof.R](testthat/test_subtitle_contingency_tab_gof.R#L41_L44)       | subtitle\_contingency\_tab\_gof    | Goodness of Fit subtitle\_contingency\_tab works       | PASS   |  3 |  0.03 |
| [test\_subtitle\_contingency\_tab\_paired.R](testthat/test_subtitle_contingency_tab_paired.R#L76_L79) | subtitle\_contingency\_tab\_paired | Paired subtitle\_contingency\_tab works                | PASS   |  3 |  0.05 |
| [test\_subtitle\_ggscatterstats.R](testthat/test_subtitle_ggscatterstats.R#L46)                       | subtitle\_ggscatterstats           | subtitle\_ggscatterstats works                         | PASS   |  3 |  0.49 |
| [test\_subtitle\_kw\_nonparametric.R](testthat/test_subtitle_kw_nonparametric.R#L51_L54)              | subtitle\_kw\_nonparametric        | subtitle\_kw\_nonparametric works                      | PASS   |  4 |  1.20 |
| [test\_subtitle\_mann\_nonparametric.R](testthat/test_subtitle_mann_nonparametric.R#L46_L49)          | subtitle\_mann\_nonparametric      | subtitle\_mann\_nonparametric works                    | PASS   |  4 |  0.19 |
| [test\_subtitle\_mann\_paired.R](testthat/test_subtitle_mann_paired.R#L217_L220)                      | subtitle\_mann\_paired             | subtitle\_mann\_paired works                           | PASS   |  4 |  0.03 |
| [test\_subtitle\_t\_bayes.R](testthat/test_subtitle_t_bayes.R#L47)                                    | subtitle\_t\_bayes                 | subtitle\_t\_bayes works                               | PASS   |  4 |  0.12 |
| [test\_subtitle\_t\_bayes\_paired.R](testthat/test_subtitle_t_bayes_paired.R#L218_L221)               | subtitle\_t\_bayes\_paired         | subtitle\_t\_bayes\_paired works                       | PASS   |  4 |  0.11 |
| [test\_subtitle\_t\_onesample.R](testthat/test_subtitle_t_onesample.R#L53_L56)                        | subtitle\_t\_onesample             | subtitle\_t\_onesample works                           | PASS   |  4 |  0.30 |
| [test\_subtitle\_t\_parametric.R](testthat/test_subtitle_t_parametric.R#L63_L66)                      | subtitle\_t\_parametric            | parametric t-test works (between-subjects without NAs) | PASS   |  4 |  0.02 |
| [test\_subtitle\_t\_parametric.R](testthat/test_subtitle_t_parametric.R#L133)                         | subtitle\_t\_parametric            | parametric t-test works (between-subjects with NAs)    | PASS   |  1 |  0.22 |
| [test\_subtitle\_t\_parametric\_paired.R](testthat/test_subtitle_t_parametric_paired.R#L223_L226)     | subtitle\_t\_parametric\_paired    | subtitle\_t\_parametric\_paired works                  | PASS   |  4 |  0.01 |
| [test\_t1way\_ci.R](testthat/test_t1way_ci.R#L56)                                                     | t1way\_ci                          | t1way\_ci works                                        | PASS   |  5 |  4.92 |

</details>

<details>

<summary> Session Info </summary>

| Field    | Value                                              |
| :------- | :------------------------------------------------- |
| Version  | R Under development (unstable) (2018-10-20 r75474) |
| Platform | x86\_64-w64-mingw32/x64 (64-bit)                   |
| Running  | Windows \>= 8 x64 (build 9200)                     |
| Language | English\_United States                             |
| Timezone | America/New\_York                                  |

| Package  | Version |
| :------- | :------ |
| testthat | 2.0.1   |
| covr     | 3.2.1   |
| covrpage | 0.0.66  |

</details>

<!--- Final Status : pass --->
