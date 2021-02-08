# checking ggcorrmat - with NAs - spearman's rho

    Code
      suppressWarnings(purrr::pmap_dfr(.l = list(data = list(ggplot2::msleep), type = list(
        "p", "p", "np", "np", "r", "r", "bf", "bayes"), output = list("dataframe"),
      partial = list(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)), .f = ggcorrmat))
    Output
      # A tibble: 120 x 17
         parameter1 parameter2 estimate conf.level conf.low conf.high statistic
         <chr>      <chr>         <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
       1 sleep_tot~ sleep_rem   0.314         0.95  -0.0520     0.606    1.75  
       2 sleep_tot~ sleep_cyc~ -0.0225        0.95  -0.380      0.341   -0.119 
       3 sleep_tot~ awake      -1             0.95  -1         -1     -Inf     
       4 sleep_tot~ brainwt    -0.0970        0.95  -0.442      0.273   -0.516 
       5 sleep_tot~ bodywt     -0.179         0.95  -0.506      0.194   -0.961 
       6 sleep_rem  sleep_cyc~ -0.0766        0.95  -0.425      0.292   -0.407 
       7 sleep_rem  awake       0.0560        0.95  -0.311      0.408    0.297 
       8 sleep_rem  brainwt     0.0857        0.95  -0.283      0.433    0.455 
       9 sleep_rem  bodywt     -0.0341        0.95  -0.390      0.330   -0.181 
      10 sleep_cyc~ awake      -0.00479       0.95  -0.364      0.356   -0.0253
      # ... with 110 more rows, and 10 more variables: df.error <int>, p.value <dbl>,
      #   method <chr>, n.obs <int>, pd <dbl>, rope.percentage <dbl>,
      #   prior.distribution <chr>, prior.location <dbl>, prior.scale <dbl>,
      #   bayes.factor <dbl>

