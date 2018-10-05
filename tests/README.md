Tests and Coverage
================
05 October, 2018 13:26:52

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr)
package.

| Object                                                                            | Coverage (%) |
| :-------------------------------------------------------------------------------- | :----------: |
| ggstatsplot                                                                       |    20.32     |
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
| [R/helpers\_messages.R](../R/helpers_messages.R)                                  |     0.00     |
| [R/helpers\_stats.R](../R/helpers_stats.R)                                        |     0.00     |
| [R/helpers\_ggbetween\_subtitles.R](../R/helpers_ggbetween_subtitles.R)           |    23.17     |
| [R/theme\_ggstatsplot.R](../R/theme_ggstatsplot.R)                                |    29.27     |
| [R/helpers\_ggscatterstats\_subtitles.R](../R/helpers_ggscatterstats_subtitles.R) |    30.77     |
| [R/ggbetweenstats.R](../R/ggbetweenstats.R)                                       |    41.34     |
| [R/grouped\_ggscatterstats.R](../R/grouped_ggscatterstats.R)                      |    45.54     |
| [R/ggscatterstats.R](../R/ggscatterstats.R)                                       |    53.56     |
| [R/helpers\_effsize\_ci.R](../R/helpers_effsize_ci.R)                             |    80.78     |
| [R/specify\_decimal\_p.R](../R/specify_decimal_p.R)                               |    100.00    |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat)
package.

|                                 | file                                                                      |  n | time | error | failed | skipped | warning |
| ------------------------------- | :------------------------------------------------------------------------ | -: | ---: | ----: | -----: | ------: | ------: |
| test\_anova\_subtitles.R        | [test\_anova\_subtitles.R](testthat/test_anova_subtitles.R)               |  4 | 0.85 |     0 |      0 |       0 |       0 |
| test\_chisq\_v\_ci.R            | [test\_chisq\_v\_ci.R](testthat/test_chisq_v_ci.R)                        | 10 | 2.42 |     0 |      0 |       0 |       0 |
| test\_cor\_tets\_ci.R           | [test\_cor\_tets\_ci.R](testthat/test_cor_tets_ci.R)                      | 12 | 0.84 |     0 |      0 |       0 |       0 |
| test\_ggbetweenstats.R          | [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R)                  |  1 | 2.44 |     0 |      0 |       1 |       0 |
| test\_grouped\_ggscatterstats.R | [test\_grouped\_ggscatterstats.R](testthat/test_grouped_ggscatterstats.R) |  1 | 0.18 |     0 |      0 |       0 |       0 |
| test\_helpers\_effsize\_ci.R    | [test\_helpers\_effsize\_ci.R](testthat/test_helpers_effsize_ci.R)        |  5 | 6.20 |     0 |      0 |       0 |       0 |
| test\_lm\_effsize\_ci.R         | [test\_lm\_effsize\_ci.R](testthat/test_lm_effsize_ci.R)                  |  9 | 4.58 |     0 |      0 |       0 |       0 |
| test\_robcor\_ci.R              | [test\_robcor\_ci.R](testthat/test_robcor_ci.R)                           |  8 | 0.28 |     0 |      0 |       0 |       0 |
| test\_specify\_decimal\_p.R     | [test\_specify\_decimal\_p.R](testthat/test_specify_decimal_p.R)          |  4 | 0.00 |     0 |      0 |       0 |       0 |
| test\_t\_test\_subtitles.R      | [test\_t\_test\_subtitles.R](testthat/test_t_test_subtitles.R)            |  1 | 0.36 |     0 |      0 |       0 |       0 |

<details open>

<summary> Show Detailed Test Results
</summary>

| file                                                                             | context                 | test                          | status  |  n | time |
| :------------------------------------------------------------------------------- | :---------------------- | :---------------------------- | :------ | -: | ---: |
| [test\_anova\_subtitles.R](testthat/test_anova_subtitles.R#L30_L33)              | anova\_subtitles        | anova subtitles work          | PASS    |  4 | 0.85 |
| [test\_chisq\_v\_ci.R](testthat/test_chisq_v_ci.R#L43_L47)                       | chisq\_v\_ci            | chisq\_v\_ci works            | PASS    | 10 | 2.42 |
| [test\_cor\_tets\_ci.R](testthat/test_cor_tets_ci.R#L43_L47)                     | cor\_tets\_ci           | cor\_tets\_ci works           | PASS    | 12 | 0.84 |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R#L17_L20)                 | ggbetweenstats          | ggbetweenstats works          | SKIPPED |  1 | 2.44 |
| [test\_grouped\_ggscatterstats.R](testthat/test_grouped_ggscatterstats.R#L7_L15) | grouped\_ggscatterstats | grouped\_ggscatterstats works | PASS    |  1 | 0.18 |
| [test\_helpers\_effsize\_ci.R](testthat/test_helpers_effsize_ci.R#L56)           | helpers\_effsize\_ci    | ci stuff works                | PASS    |  5 | 6.20 |
| [test\_lm\_effsize\_ci.R](testthat/test_lm_effsize_ci.R#L65_L69)                 | lm\_effsize\_ci         | lm\_effsize\_ci works         | PASS    |  9 | 4.58 |
| [test\_robcor\_ci.R](testthat/test_robcor_ci.R#L37_L41)                          | robcor\_ci              | robcor\_ci works              | PASS    |  8 | 0.28 |
| [test\_specify\_decimal\_p.R](testthat/test_specify_decimal_p.R#L17)             | Specify decimals        | specify\_decimal\_p works     | PASS    |  4 | 0.00 |
| [test\_t\_test\_subtitles.R](testthat/test_t_test_subtitles.R#L41_L45)           | t\_test\_subtitles      | t-test subtitles work         | PASS    |  1 | 0.36 |

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
| covrpage | 0.0.59     |

</details>

<!--- Final Status : skipped/warning --->
