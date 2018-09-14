Tests and Coverage
================
13 September, 2018 17:45:25

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr)
package.

| Object                                                                            | Coverage (%) |
| :-------------------------------------------------------------------------------- | :----------: |
| ggstatsplot                                                                       |     7.15     |
| [R/combine\_plots.R](../R/combine_plots.R)                                        |     0.00     |
| [R/ggcoefstats.R](../R/ggcoefstats.R)                                             |     0.00     |
| [R/ggcorrmat.R](../R/ggcorrmat.R)                                                 |     0.00     |
| [R/gghistostats.R](../R/gghistostats.R)                                           |     0.00     |
| [R/ggpiestats.R](../R/ggpiestats.R)                                               |     0.00     |
| [R/ggscatterstats.R](../R/ggscatterstats.R)                                       |     0.00     |
| [R/grouped\_ggbetweenstats.R](../R/grouped_ggbetweenstats.R)                      |     0.00     |
| [R/grouped\_ggcorrmat.R](../R/grouped_ggcorrmat.R)                                |     0.00     |
| [R/grouped\_gghistostats.R](../R/grouped_gghistostats.R)                          |     0.00     |
| [R/grouped\_ggpiestats.R](../R/grouped_ggpiestats.R)                              |     0.00     |
| [R/grouped\_ggscatterstats.R](../R/grouped_ggscatterstats.R)                      |     0.00     |
| [R/helpers\_effsize\_ci.R](../R/helpers_effsize_ci.R)                             |     0.00     |
| [R/helpers\_gghistostats\_subtitles.R](../R/helpers_gghistostats_subtitles.R)     |     0.00     |
| [R/helpers\_ggpiestats\_subtitles.R](../R/helpers_ggpiestats_subtitles.R)         |     0.00     |
| [R/helpers\_ggscatterstats\_subtitles.R](../R/helpers_ggscatterstats_subtitles.R) |     0.00     |
| [R/helpers\_labeller.R](../R/helpers_labeller.R)                                  |     0.00     |
| [R/helpers\_messages.R](../R/helpers_messages.R)                                  |     0.00     |
| [R/helpers\_stats.R](../R/helpers_stats.R)                                        |     0.00     |
| [R/helpers\_ggbetween\_subtitles.R](../R/helpers_ggbetween_subtitles.R)           |    20.30     |
| [R/theme\_ggstatsplot.R](../R/theme_ggstatsplot.R)                                |    29.51     |
| [R/ggbetweenstats.R](../R/ggbetweenstats.R)                                       |    40.36     |
| [R/specify\_decimal\_p.R](../R/specify_decimal_p.R)                               |    100.00    |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat)
package.

|                          | file                                                        | n | time | error | failed | skipped | warning |
| ------------------------ | :---------------------------------------------------------- | -: | ---: | ----: | -----: | ------: | ------: |
| test\_anova\_subtitles.R | [test\_anova\_subtitles.R](testthat/test_anova_subtitles.R) | 4 | 3.77 |     0 |      0 |       0 |       0 |
| test\_ggbetweenstats.R   | [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R)    | 1 | 2.78 |     0 |      0 |       1 |       0 |

<details open>

<summary> Show Detailed Test Results
</summary>

| file                                                             | context          | test                              | status  | n | time |
| :--------------------------------------------------------------- | :--------------- | :-------------------------------- | :------ | -: | ---: |
| [test\_anova\_subtitles.R](testthat/test_anova_subtitles.R#L36)  | anova\_subtitles | anova subtitles work              | PASS    | 4 | 3.77 |
| [test\_ggbetweenstats.R](testthat/test_ggbetweenstats.R#L17_L20) | ggbetweenstats   | ggbetweenstats working correctly? | SKIPPED | 1 | 2.78 |

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

| Package  | Version |
| :------- | :------ |
| testthat | 2.0.0   |
| covr     | 3.1.0   |
| covrpage | 0.0.55  |

</details>

<!--- Final Status : skipped/warning --->
