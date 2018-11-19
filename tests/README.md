Tests and Coverage
================
19 November, 2018 17:37:10

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr)
package.

| Object                                                                               | Coverage (%) |
| :----------------------------------------------------------------------------------- | :----------: |
| ggstatsplot                                                                          |    57.85     |
| [R/ggcoefstats.R](../R/ggcoefstats.R)                                                |     0.00     |
| [R/ggdotplotstats.R](../R/ggdotplotstats.R)                                          |     0.00     |
| [R/helpers\_ggcoefstats.R](../R/helpers_ggcoefstats.R)                               |     0.00     |
| [R/helpers\_ggcorrmat.R](../R/helpers_ggcorrmat.R)                                   |     0.00     |
| [R/helpers\_pairwise\_comparison.R](../R/helpers_pairwise_comparison.R)              |     0.00     |
| [R/set\_cwd.R](../R/set_cwd.R)                                                       |     0.00     |
| [R/helpers\_messages.R](../R/helpers_messages.R)                                     |     4.04     |
| [R/combine\_plots.R](../R/combine_plots.R)                                           |    10.94     |
| [R/gghistostats.R](../R/gghistostats.R)                                              |    46.59     |
| [R/ggscatterstats.R](../R/ggscatterstats.R)                                          |    54.06     |
| [R/helpers\_gghistostats\_subtitles.R](../R/helpers_gghistostats_subtitles.R)        |    54.55     |
| [R/helpers\_bf\_tests.R](../R/helpers_bf_tests.R)                                    |    54.95     |
| [R/grouped\_ggbetweenstats.R](../R/grouped_ggbetweenstats.R)                         |    57.74     |
| [R/ggcorrmat.R](../R/ggcorrmat.R)                                                    |    62.72     |
| [R/helpers\_ggbetween\_anova\_subtitles.R](../R/helpers_ggbetween_anova_subtitles.R) |    69.86     |
| [R/helpers\_ggbetween\_t\_subtitles.R](../R/helpers_ggbetween_t_subtitles.R)         |    71.32     |
| [R/ggbetweenstats.R](../R/ggbetweenstats.R)                                          |    72.60     |
| [R/helpers\_effsize\_ci.R](../R/helpers_effsize_ci.R)                                |    72.83     |
| [R/grouped\_ggcorrmat.R](../R/grouped_ggcorrmat.R)                                   |    72.95     |
| [R/helpers\_ggscatterstats\_subtitles.R](../R/helpers_ggscatterstats_subtitles.R)    |    73.48     |
| [R/grouped\_ggscatterstats.R](../R/grouped_ggscatterstats.R)                         |    73.78     |
| [R/helpers\_ggbetweenstats.R](../R/helpers_ggbetweenstats.R)                         |    83.16     |
| [R/helpers\_ggpiestats\_subtitles.R](../R/helpers_ggpiestats_subtitles.R)            |    83.97     |
| [R/ggpiestats.R](../R/ggpiestats.R)                                                  |    87.31     |
| [R/switch\_functions.R](../R/switch_functions.R)                                     |    92.73     |
| [R/theme\_ggstatsplot.R](../R/theme_ggstatsplot.R)                                   |    98.37     |
| [R/grouped\_gghistostats.R](../R/grouped_gghistostats.R)                             |    98.96     |
| [R/grouped\_ggpiestats.R](../R/grouped_ggpiestats.R)                                 |    99.57     |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat)
package.

| file                                                                                          |  n |  time | error | failed | skipped | warning | icon |
| :-------------------------------------------------------------------------------------------- | -: | ----: | ----: | -----: | ------: | ------: | :--- |
| [test\_anova\_subtitles.R](testthat/test_anova_subtitles.R)                                   |  4 |  0.71 |     0 |      0 |       0 |       0 |      |
| [test\_argument\_count.R](testthat/test_argument_count.R)                                     |  1 |  0.04 |     0 |      0 |       0 |       0 |      |
| [test\_chisq\_v\_ci.R](testthat/test_chisq_v_ci.R)                                            | 10 |  2.29 |     0 |      0 |       0 |       0 |      |
| [test\_cor\_test\_ci.R](testthat/test_cor_test_ci.R)                                          | 12 |  0.49 |     0 |      0 |       0 |       0 |      |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R)                                      |  5 |  3.42 |     0 |      0 |       1 |       0 | \+   |
| [test\_grouped\_ggbetweenstats.R](testthat/test_grouped_ggbetweenstats.R)                     |  3 |  2.48 |     0 |      0 |       0 |       0 |      |
| [test\_grouped\_ggcorrmat.R](testthat/test_grouped_ggcorrmat.R)                               |  4 |  2.07 |     0 |      0 |       0 |       0 |      |
| [test\_grouped\_gghistostats.R](testthat/test_grouped_gghistostats.R)                         |  3 |  2.16 |     0 |      0 |       0 |       0 |      |
| [test\_grouped\_ggpiestats.R](testthat/test_grouped_ggpiestats.R)                             |  8 | 25.97 |     0 |      0 |       0 |       0 |      |
| [test\_grouped\_ggscatterstats.R](testthat/test_grouped_ggscatterstats.R)                     |  3 |  0.96 |     0 |      0 |       0 |       0 |      |
| [test\_lm\_effsize\_ci.R](testthat/test_lm_effsize_ci.R)                                      |  9 |  3.07 |     0 |      0 |       0 |       0 |      |
| [test\_robcor\_ci.R](testthat/test_robcor_ci.R)                                               |  8 |  0.21 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_anova\_bayes.R](testthat/test_subtitle_anova_bayes.R)                        |  4 |  0.14 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_anova\_parametric.R](testthat/test_subtitle_anova_parametric.R)              |  3 |  0.03 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_anova\_robust.R](testthat/test_subtitle_anova_robust.R)                      |  4 |  1.20 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_contingency\_tab.R](testthat/test_subtitle_contingency_tab.R)                |  5 |  1.05 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_contingency\_tab\_gof.R](testthat/test_subtitle_contingency_tab_gof.R)       |  3 |  0.04 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_contingency\_tab\_paired.R](testthat/test_subtitle_contingency_tab_paired.R) |  3 |  0.08 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_ggscatterstats.R](testthat/test_subtitle_ggscatterstats.R)                   |  3 |  0.57 |     0 |      0 |       0 |       0 |      |
| [test\_subtitle\_kw\_nonparametric.R](testthat/test_subtitle_kw_nonparametric.R)              |  4 |  1.25 |     0 |      0 |       0 |       0 |      |
| [test\_t\_test\_subtitles.R](testthat/test_t_test_subtitles.R)                                |  1 |  0.37 |     0 |      0 |       0 |       0 |      |
| [test\_t1way\_ci.R](testthat/test_t1way_ci.R)                                                 |  5 |  4.33 |     0 |      0 |       0 |       0 |      |

<details open>

<summary> Show Detailed Test Results
</summary>

| file                                                                                                  | context                            | test                                             | status  |  n |  time | icon |
| :---------------------------------------------------------------------------------------------------- | :--------------------------------- | :----------------------------------------------- | :------ | -: | ----: | :--- |
| [test\_anova\_subtitles.R](testthat/test_anova_subtitles.R#L32_L35)                                   | subtitle\_anova\_parametric        | parametric anova subtitles work                  | PASS    |  4 |  0.71 |      |
| [test\_argument\_count.R](testthat/test_argument_count.R#L56_L59)                                     | argument\_count                    | argument\_count is correct                       | PASS    |  1 |  0.04 |      |
| [test\_chisq\_v\_ci.R](testthat/test_chisq_v_ci.R#L46_L50)                                            | chisq\_v\_ci                       | chisq\_v\_ci works                               | PASS    | 10 |  2.29 |      |
| [test\_cor\_test\_ci.R](testthat/test_cor_test_ci.R#L45_L49)                                          | cor\_test\_ci                      | cor\_test\_ci works                              | PASS    | 12 |  0.49 |      |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R#L9_L16)                                       | ggbetweenstats                     | error when x and outlier.label are same          | PASS    |  1 |  0.00 |      |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R#L26_L36)                                      | ggbetweenstats                     | outlier.labeling works across vector types       | PASS    |  3 |  2.67 |      |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R#L79_L95)                                      | ggbetweenstats                     | ggbetweenstats works                             | SKIPPED |  1 |  0.75 | \+   |
| [test\_grouped\_ggbetweenstats.R](testthat/test_grouped_ggbetweenstats.R#L12_L32)                     | grouped\_ggbetweenstats            | grouping.var works across vector types           | PASS    |  3 |  2.48 |      |
| [test\_grouped\_ggcorrmat.R](testthat/test_grouped_ggcorrmat.R#L18_L27)                               | grouped\_ggcorrmat                 | grouped\_ggcorrmat works                         | PASS    |  4 |  2.07 |      |
| [test\_grouped\_gghistostats.R](testthat/test_grouped_gghistostats.R#L9_L19)                          | grouped\_gghistostats              | grouped\_gghistostats works                      | PASS    |  3 |  2.16 |      |
| [test\_grouped\_ggpiestats.R](testthat/test_grouped_ggpiestats.R#L13_L21)                             | grouped\_ggpiestats                | grouped\_ggpiestats works                        | PASS    |  8 | 25.97 |      |
| [test\_grouped\_ggscatterstats.R](testthat/test_grouped_ggscatterstats.R#L9_L17)                      | grouped\_ggscatterstats            | grouped\_ggscatterstats works                    | PASS    |  3 |  0.96 |      |
| [test\_lm\_effsize\_ci.R](testthat/test_lm_effsize_ci.R#L67_L71)                                      | lm\_effsize\_ci                    | lm\_effsize\_ci works                            | PASS    |  9 |  3.07 |      |
| [test\_robcor\_ci.R](testthat/test_robcor_ci.R#L39_L43)                                               | robcor\_ci                         | robcor\_ci works                                 | PASS    |  8 |  0.21 |      |
| [test\_subtitle\_anova\_bayes.R](testthat/test_subtitle_anova_bayes.R#L51_L54)                        | subtitle\_anova\_bayes             | subtitle\_anova\_bayes works                     | PASS    |  4 |  0.14 |      |
| [test\_subtitle\_anova\_parametric.R](testthat/test_subtitle_anova_parametric.R#L53_L56)              | helpers\_ggbetween\_subtitles      | helpers\_ggbetween\_subtitles works              | PASS    |  3 |  0.03 |      |
| [test\_subtitle\_anova\_robust.R](testthat/test_subtitle_anova_robust.R#L52_L55)                      | subtitle\_anova\_robust            | subtitle\_anova\_robust works                    | PASS    |  4 |  1.20 |      |
| [test\_subtitle\_contingency\_tab.R](testthat/test_subtitle_contingency_tab.R#L56_L59)                | subtitle\_contingency\_tab         | subtitle\_contingency\_tab works                 | PASS    |  5 |  1.05 |      |
| [test\_subtitle\_contingency\_tab\_gof.R](testthat/test_subtitle_contingency_tab_gof.R#L41_L44)       | subtitle\_contingency\_tab\_gof    | Goodness of Fit subtitle\_contingency\_tab works | PASS    |  3 |  0.04 |      |
| [test\_subtitle\_contingency\_tab\_paired.R](testthat/test_subtitle_contingency_tab_paired.R#L75_L78) | subtitle\_contingency\_tab\_paired | Paired subtitle\_contingency\_tab works          | PASS    |  3 |  0.08 |      |
| [test\_subtitle\_ggscatterstats.R](testthat/test_subtitle_ggscatterstats.R#L46)                       | subtitle\_ggscatterstats           | subtitle\_ggscatterstats works                   | PASS    |  3 |  0.57 |      |
| [test\_subtitle\_kw\_nonparametric.R](testthat/test_subtitle_kw_nonparametric.R#L50_L53)              | subtitle\_kw\_nonparametric        | subtitle\_kw\_nonparametric works                | PASS    |  4 |  1.25 |      |
| [test\_t\_test\_subtitles.R](testthat/test_t_test_subtitles.R#L43_L47)                                | t\_test\_subtitles                 | t-test subtitles work                            | PASS    |  1 |  0.37 |      |
| [test\_t1way\_ci.R](testthat/test_t1way_ci.R#L57)                                                     | t1way\_ci                          | t1way\_ci works                                  | PASS    |  5 |  4.33 |      |

| Failed | Warning | Skipped |
| :----- | :------ | :------ |
| \!     | \-      | \+      |

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

<!--- Final Status : skipped/warning --->
