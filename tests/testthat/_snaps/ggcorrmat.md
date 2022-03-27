# ggcorrmat warnings are as expected

    Mismatch between number of variables and names.

# checking all dataframe outputs

    Code
      suppressWarnings(purrr::pmap(.l = list(data = list(dplyr::select(ggplot2::msleep,
      brainwt, sleep_rem, bodywt)), type = list("p", "p", "np", "np", "r", "r", "bf",
        "bayes"), output = list("dataframe"), partial = list(TRUE, FALSE, TRUE, FALSE,
        TRUE, FALSE, TRUE, FALSE)), .f = ggcorrmat))
    Output
      [[1]]
      # A tibble: 3 x 11
        parameter1 parameter2 estimate conf.level conf.low conf.high statistic
        <chr>      <chr>         <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 brainwt    sleep_rem   -0.0961       0.95   -0.370     0.193    -0.655
      2 brainwt    bodywt       0.485        0.95    0.233     0.676     3.76 
      3 sleep_rem  bodywt      -0.108        0.95   -0.381     0.182    -0.737
        df.error p.value method              n.obs
           <int>   <dbl> <chr>               <int>
      1       46 0.929   Pearson correlation    48
      2       46 0.00144 Pearson correlation    48
      3       46 0.929   Pearson correlation    48
      
      [[2]]
      # A tibble: 3 x 11
        parameter1 parameter2 estimate conf.level conf.low conf.high statistic
        <chr>      <chr>         <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 brainwt    sleep_rem    -0.221       0.95   -0.476    0.0670     -1.54
      2 brainwt    bodywt        0.934       0.95    0.889    0.961      19.2 
      3 sleep_rem  bodywt       -0.328       0.95   -0.535   -0.0826     -2.66
        df.error  p.value method              n.obs
           <int>    <dbl> <chr>               <int>
      1       46 1.31e- 1 Pearson correlation    48
      2       54 2.75e-25 Pearson correlation    56
      3       59 1.99e- 2 Pearson correlation    61
      
      [[3]]
      # A tibble: 3 x 10
        parameter1 parameter2 estimate conf.level conf.low conf.high statistic
        <chr>      <chr>         <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 brainwt    sleep_rem    -0.271       0.95   -0.522    0.0230     23414
      2 brainwt    bodywt        0.785       0.95    0.640    0.876       3962
      3 sleep_rem  bodywt        0.154       0.95   -0.145    0.427      15588
         p.value method               n.obs
           <dbl> <chr>                <int>
      1 1.25e- 1 Spearman correlation    48
      2 1.20e-10 Spearman correlation    48
      3 2.96e- 1 Spearman correlation    48
      
      [[4]]
      # A tibble: 3 x 10
        parameter1 parameter2 estimate conf.level conf.low conf.high statistic
        <chr>      <chr>         <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 brainwt    sleep_rem    -0.414       0.95   -0.630    -0.139    26050.
      2 brainwt    bodywt        0.957       0.95    0.927     0.975     1254.
      3 sleep_rem  bodywt       -0.452       0.95   -0.636    -0.218    54904.
         p.value method               n.obs
           <dbl> <chr>                <int>
      1 3.45e- 3 Spearman correlation    48
      2 2.91e-30 Spearman correlation    56
      3 5.16e- 4 Spearman correlation    61
      
      [[5]]
      # A tibble: 3 x 11
        parameter1 parameter2 estimate conf.level conf.low conf.high statistic
        <chr>      <chr>         <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 brainwt    sleep_rem    -0.290       0.95   -0.531  -0.00694     -2.06
      2 brainwt    bodywt        0.681       0.95    0.493   0.809        6.32
      3 sleep_rem  bodywt        0.183       0.95   -0.107   0.444        1.26
        df.error     p.value method                         n.obs
           <int>       <dbl> <chr>                          <int>
      1       46 0.0904      Winsorized Pearson correlation    48
      2       46 0.000000292 Winsorized Pearson correlation    48
      3       46 0.213       Winsorized Pearson correlation    48
      
      [[6]]
      # A tibble: 3 x 11
        parameter1 parameter2 estimate conf.level conf.low conf.high statistic
        <chr>      <chr>         <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 brainwt    sleep_rem    -0.412       0.95   -0.623    -0.145     -3.06
      2 brainwt    bodywt        0.910       0.95    0.851     0.947     16.2 
      3 sleep_rem  bodywt       -0.375       0.95   -0.572    -0.136     -3.10
        df.error  p.value method                         n.obs
           <int>    <dbl> <chr>                          <int>
      1       46 5.86e- 3 Winsorized Pearson correlation    48
      2       54 7.22e-22 Winsorized Pearson correlation    56
      3       59 5.86e- 3 Winsorized Pearson correlation    61
      
      [[7]]
      # A tibble: 3 x 14
        parameter1 parameter2 estimate conf.level conf.low conf.high    pd
        <chr>      <chr>         <dbl>      <dbl>    <dbl>     <dbl> <dbl>
      1 brainwt    sleep_rem   -0.0911       0.95   -0.373     0.171 0.740
      2 brainwt    bodywt       0.461        0.95    0.228     0.663 1    
      3 sleep_rem  bodywt      -0.0959       0.95   -0.368     0.172 0.756
        rope.percentage prior.distribution prior.location prior.scale bayes.factor
                  <dbl> <chr>                       <dbl>       <dbl>        <dbl>
      1         0.430   beta                         1.41        1.41        0.269
      2         0.00425 beta                         1.41        1.41       73.6  
      3         0.434   beta                         1.41        1.41        0.283
        method                       n.obs
        <chr>                        <int>
      1 Bayesian Pearson correlation    48
      2 Bayesian Pearson correlation    48
      3 Bayesian Pearson correlation    48
      
      [[8]]
      # A tibble: 3 x 14
        parameter1 parameter2 estimate conf.level conf.low conf.high    pd
        <chr>      <chr>         <dbl>      <dbl>    <dbl>     <dbl> <dbl>
      1 brainwt    sleep_rem    -0.205       0.95   -0.458    0.0639 0.928
      2 brainwt    bodywt        0.926       0.95    0.883    0.960  1    
      3 sleep_rem  bodywt       -0.310       0.95   -0.537   -0.0972 0.990
        rope.percentage prior.distribution prior.location prior.scale bayes.factor
                  <dbl> <chr>                       <dbl>       <dbl>        <dbl>
      1          0.212  beta                         1.41        1.41     6.54e- 1
      2          0      beta                         1.41        1.41     1.58e+22
      3          0.0365 beta                         1.41        1.41     4.80e+ 0
        method                       n.obs
        <chr>                        <int>
      1 Bayesian Pearson correlation    48
      2 Bayesian Pearson correlation    56
      3 Bayesian Pearson correlation    61
      

