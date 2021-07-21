# grouped_ggcorrmat stats work

    Code
      grouped_ggcorrmat(data = dplyr::select(ggplot2::msleep, dplyr::matches(
        "sleep|awake|vore")), grouping.var = vore, type = "r", output = "data", tr = 0.2)
    Output
      # A tibble: 24 x 12
         vore  parameter1  parameter2  estimate conf.level conf.low conf.high
         <chr> <chr>       <chr>          <dbl>      <dbl>    <dbl>     <dbl>
       1 carni sleep_total sleep_rem      0.948       0.95    0.790     0.988
       2 carni sleep_total sleep_cycle    0.632       0.95   -0.565     0.972
       3 carni sleep_total awake         -1.00        0.95   -1.00     -1.00 
       4 carni sleep_rem   sleep_cycle    0.333       0.95   -0.778     0.939
       5 carni sleep_rem   awake         -0.948       0.95   -0.988    -0.790
       6 carni sleep_cycle awake         -0.632       0.95   -0.972     0.565
       7 herbi sleep_total sleep_rem      0.900       0.95    0.780     0.956
       8 herbi sleep_total sleep_cycle   -0.677       0.95   -0.901    -0.169
       9 herbi sleep_total awake         -1           0.95   -1        -1    
      10 herbi sleep_rem   sleep_cycle   -0.343       0.95   -0.766     0.287
         statistic df.error  p.value method                         n.obs
             <dbl>    <int>    <dbl> <chr>                          <int>
       1     8.43         8 1.50e- 4 Winsorized Pearson correlation    10
       2     1.41         3 7.57e- 1 Winsorized Pearson correlation     5
       3  -962.          17 6.36e-41 Winsorized Pearson correlation    19
       4     0.612        3 7.57e- 1 Winsorized Pearson correlation     5
       5    -8.43         8 1.50e- 4 Winsorized Pearson correlation    10
       6    -1.41         3 7.57e- 1 Winsorized Pearson correlation     5
       7     9.69        22 1.07e- 8 Winsorized Pearson correlation    24
       8    -2.91        10 4.68e- 2 Winsorized Pearson correlation    12
       9  -Inf           30 0        Winsorized Pearson correlation    32
      10    -1.16        10 2.74e- 1 Winsorized Pearson correlation    12
      # ... with 14 more rows

