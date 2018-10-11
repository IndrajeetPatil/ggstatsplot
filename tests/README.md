Tests and Coverage
================
11 October, 2018 01:51:49

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

    ## - Tests Failed
    ##   Coverage statistics are approximations of the non-failing tests.
    ##   Use with caution
    ## 
    ##  For further investigation check in testthat summary tables.

| Object                                                                            | Coverage (%) |
| :-------------------------------------------------------------------------------- | :----------: |
| ggstatsplot                                                                       |    20.62     |
| [R/combine\_plots.R](../R/combine_plots.R)                                        |     0.00     |
| [R/ggcoefstats.R](../R/ggcoefstats.R)                                             |     0.00     |
| [R/ggcorrmat.R](../R/ggcorrmat.R)                                                 |     0.00     |
| [R/gghistostats.R](../R/gghistostats.R)                                           |     0.00     |
| [R/ggpiestats.R](../R/ggpiestats.R)                                               |     0.00     |
| [R/grouped\_ggbetweenstats.R](../R/grouped_ggbetweenstats.R)                      |     0.00     |
| [R/grouped\_ggcorrmat.R](../R/grouped_ggcorrmat.R)                                |     0.00     |
| [R/grouped\_gghistostats.R](../R/grouped_gghistostats.R)                          |     0.00     |
| [R/grouped\_ggpiestats.R](../R/grouped_ggpiestats.R)                              |     0.00     |
| [R/helpers\_ggcoefstats.R](../R/helpers_ggcoefstats.R)                            |     0.00     |
| [R/helpers\_ggcorrmat.R](../R/helpers_ggcorrmat.R)                                |     0.00     |
| [R/helpers\_gghistostats\_subtitles.R](../R/helpers_gghistostats_subtitles.R)     |     0.00     |
| [R/helpers\_ggpiestats\_subtitles.R](../R/helpers_ggpiestats_subtitles.R)         |     0.00     |
| [R/helpers\_ggscatterstats.R](../R/helpers_ggscatterstats.R)                      |     0.00     |
| [R/helpers\_messages.R](../R/helpers_messages.R)                                  |     0.00     |
| [R/set\_cwd.R](../R/set_cwd.R)                                                    |     0.00     |
| [R/helpers\_ggbetween\_subtitles.R](../R/helpers_ggbetween_subtitles.R)           |    23.32     |
| [R/helpers\_ggscatterstats\_subtitles.R](../R/helpers_ggscatterstats_subtitles.R) |    25.45     |
| [R/theme\_ggstatsplot.R](../R/theme_ggstatsplot.R)                                |    29.27     |
| [R/grouped\_ggscatterstats.R](../R/grouped_ggscatterstats.R)                      |    45.21     |
| [R/ggscatterstats.R](../R/ggscatterstats.R)                                       |    51.97     |
| [R/ggbetweenstats.R](../R/ggbetweenstats.R)                                       |    55.89     |
| [R/switch\_functions.R](../R/switch_functions.R)                                  |    59.09     |
| [R/helpers\_ggbetweenstats.R](../R/helpers_ggbetweenstats.R)                      |    60.94     |
| [R/helpers\_effsize\_ci.R](../R/helpers_effsize_ci.R)                             |    80.17     |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat)
package.

| icon | file                                                                      |  n | time | error | failed | skipped | warning |
| :--- | :------------------------------------------------------------------------ | -: | ---: | ----: | -----: | ------: | ------: |
| \-   | [test\_anova\_subtitles.R](testthat/test_anova_subtitles.R)               |  5 | 0.88 |     0 |      0 |       0 |       1 |
|      | [test\_argument\_count.R](testthat/test_argument_count.R)                 |  1 | 0.03 |     0 |      0 |       0 |       0 |
|      | [test\_chisq\_v\_ci.R](testthat/test_chisq_v_ci.R)                        | 10 | 2.48 |     0 |      0 |       0 |       0 |
|      | [test\_cor\_test\_ci.R](testthat/test_cor_test_ci.R)                      | 12 | 0.42 |     0 |      0 |       0 |       0 |
| \+   | [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R)                  |  1 | 2.17 |     0 |      0 |       1 |       0 |
|      | [test\_grouped\_ggscatterstats.R](testthat/test_grouped_ggscatterstats.R) |  1 | 0.14 |     0 |      0 |       0 |       0 |
|      | [test\_lm\_effsize\_ci.R](testthat/test_lm_effsize_ci.R)                  |  9 | 3.44 |     0 |      0 |       0 |       0 |
|      | [test\_robcor\_ci.R](testthat/test_robcor_ci.R)                           |  8 | 0.19 |     0 |      0 |       0 |       0 |
|      | [test\_t\_test\_subtitles.R](testthat/test_t_test_subtitles.R)            |  1 | 0.39 |     0 |      0 |       0 |       0 |
|      | [test\_t1way\_ci.R](testthat/test_t1way_ci.R)                             |  5 | 3.70 |     0 |      0 |       0 |       0 |

<details open>

<summary> Show Detailed Test Results
</summary>

| icon | file                                                                             | context                 | test                          | status  |  n | time |
| :--- | :------------------------------------------------------------------------------- | :---------------------- | :---------------------------- | :------ | -: | ---: |
| \-   | [test\_anova\_subtitles.R](testthat/test_anova_subtitles.R#L18_L24)              | anova\_subtitles        | anova subtitles work          | WARNING |  5 | 0.88 |
|      | [test\_argument\_count.R](testthat/test_argument_count.R#L54_L55)                | argument\_count         | argument\_count is correct    | PASS    |  1 | 0.03 |
|      | [test\_chisq\_v\_ci.R](testthat/test_chisq_v_ci.R#L45_L49)                       | chisq\_v\_ci            | chisq\_v\_ci works            | PASS    | 10 | 2.48 |
|      | [test\_cor\_test\_ci.R](testthat/test_cor_test_ci.R#L45_L49)                     | cor\_test\_ci           | cor\_test\_ci works           | PASS    | 12 | 0.42 |
| \+   | [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R#L17_L20)                 | ggbetweenstats          | ggbetweenstats works          | SKIPPED |  1 | 2.17 |
|      | [test\_grouped\_ggscatterstats.R](testthat/test_grouped_ggscatterstats.R#L9_L17) | grouped\_ggscatterstats | grouped\_ggscatterstats works | PASS    |  1 | 0.14 |
|      | [test\_lm\_effsize\_ci.R](testthat/test_lm_effsize_ci.R#L67_L71)                 | lm\_effsize\_ci         | lm\_effsize\_ci works         | PASS    |  9 | 3.44 |
|      | [test\_robcor\_ci.R](testthat/test_robcor_ci.R#L39_L43)                          | robcor\_ci              | robcor\_ci works              | PASS    |  8 | 0.19 |
|      | [test\_t\_test\_subtitles.R](testthat/test_t_test_subtitles.R#L43_L47)           | t\_test\_subtitles      | t-test subtitles work         | PASS    |  1 | 0.39 |
|      | [test\_t1way\_ci.R](testthat/test_t1way_ci.R#L58)                                | t1way\_ci               | t1way\_ci works               | PASS    |  5 | 3.70 |

| Failed | Warning | Skipped |
| :----- | :------ | :------ |
| \!     | \-      | \+      |

</details>

<details>

<summary> Session Info </summary>

| Field    | Value                            |
| :------- | :------------------------------- |
| Version  | R version 3.5.1 (2018-07-02)     |
| Platform | x86\_64-w64-mingw32/x64 (64-bit) |
| Running  | Windows \>= 8 x64 (build 9200)   |
| Language | English\_United States           |
| Timezone | America/New\_York                |

| Package  | Version    |
| :------- | :--------- |
| testthat | 2.0.0      |
| covr     | 3.2.0.9000 |
| covrpage | 0.0.60     |

</details>

<!--- Final Status : skipped/warning --->
