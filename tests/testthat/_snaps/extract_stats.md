# checking if extract_stats works

    Code
      extract_stats(p1)
    Output
      $subtitle_data
      # A tibble: 1 x 16
        term  group mean.group1 mean.group2 statistic df.error p.value
        <chr> <chr>       <dbl>       <dbl>     <dbl>    <dbl>   <dbl>
      1 mpg   am           17.1        24.4     -3.77     18.3 0.00137
        method                  estimate conf.level conf.low conf.high effectsize
        <chr>                      <dbl>      <dbl>    <dbl>     <dbl> <chr>     
      1 Welch Two Sample t-test    -1.35       0.95    -2.17    -0.512 Hedges' g 
        conf.method conf.distribution expression
        <chr>       <chr>             <list>    
      1 ncp         t                 <language>
      
      $caption_data
      # A tibble: 2 x 13
        term       estimate conf.level conf.low conf.high    pd rope.percentage
        <chr>         <dbl>      <dbl>    <dbl>     <dbl> <dbl>           <dbl>
      1 Difference     6.44       0.95     2.68    10.0   0.999               0
      2 Cohens_d      -1.30       0.95    -2.11    -0.519 0.999               0
        prior.distribution prior.location prior.scale  bf10 method          expression
        <chr>                       <dbl>       <dbl> <dbl> <chr>           <list>    
      1 cauchy                          0       0.707  86.6 Bayesian t-test <language>
      2 cauchy                          0       0.707  86.6 Bayesian t-test <language>
      

---

    Code
      extract_stats(p2)
    Output
      $subtitle_data
      # A tibble: 1 x 13
        parameter1 parameter2 estimate conf.level conf.low conf.high statistic
        <chr>      <chr>         <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 wt         mpg          -0.864       0.95   -0.932    -0.738     -9.41
        df.error  p.value method                         n.obs
           <int>    <dbl> <chr>                          <int>
      1       30 1.84e-10 Winsorized Pearson correlation    32
        effectsize                     expression
        <chr>                          <list>    
      1 Winsorized Pearson correlation <language>
      
      $caption_data
      NULL
      

---

    Code
      extract_stats(p3)
    Output
      $subtitle_data
      NULL
      
      $caption_data
      NULL
      

