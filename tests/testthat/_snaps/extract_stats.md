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
      
      $pairwise_comparisons_data
      NULL
      
      $descriptive_data
      NULL
      
      $one_sample_data
      NULL
      

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
      
      $pairwise_comparisons_data
      NULL
      
      $descriptive_data
      NULL
      
      $one_sample_data
      NULL
      

---

    Code
      extract_stats(p3)
    Output
      $subtitle_data
      NULL
      
      $caption_data
      NULL
      
      $pairwise_comparisons_data
      NULL
      
      $descriptive_data
      NULL
      
      $one_sample_data
      NULL
      

---

    Code
      extract_stats(p4)$pairwise_comparisons_data
    Output
      # A tibble: 3 x 11
        group1 group2 statistic   p.value alternative method            distribution
        <chr>  <chr>      <dbl>     <dbl> <chr>       <chr>             <chr>       
      1 4      6          -6.67 0.00110   two.sided   Games-Howell test q           
      2 4      8         -10.7  0.0000140 two.sided   Games-Howell test q           
      3 6      8          -7.48 0.000257  two.sided   Games-Howell test q           
        p.adjustment test.details      p.value.adjustment
        <chr>        <chr>             <chr>             
      1 none         Games-Howell test Holm              
      2 none         Games-Howell test Holm              
      3 none         Games-Howell test Holm              
        label                                     
        <chr>                                     
      1 list(~italic(p)[Holm-corrected]==0.001)   
      2 list(~italic(p)[Holm-corrected]==1.4e-05) 
      3 list(~italic(p)[Holm-corrected]==2.57e-04)

---

    Code
      extract_stats(p5)
    Output
      $subtitle_data
      # A tibble: 1 x 12
        statistic    df p.value method                                   estimate
            <dbl> <dbl>   <dbl> <chr>                                       <dbl>
      1      2.31     2   0.315 Chi-squared test for given probabilities   0.0644
        conf.level conf.low conf.high effectsize        conf.method conf.distribution
             <dbl>    <dbl>     <dbl> <chr>             <chr>       <chr>            
      1       0.95        0     0.226 Cramer's V (adj.) ncp         chisq            
        expression
        <list>    
      1 <language>
      
      $caption_data
      # A tibble: 1 x 3
          bf10 prior.scale expression
         <dbl>       <dbl> <list>    
      1 0.0603           1 <language>
      
      $pairwise_comparisons_data
      NULL
      
      $descriptive_data
      # A tibble: 3 x 4
        cyl   counts  perc .label
        <fct>  <int> <dbl> <chr> 
      1 8         14  43.8 44%   
      2 6          7  21.9 22%   
      3 4         11  34.4 34%   
      
      $one_sample_data
      NULL
      

---

    Code
      extract_stats(p6)
    Output
      $subtitle_data
      # A tibble: 1 x 12
        statistic    df p.value method                     estimate conf.level
            <dbl> <int>   <dbl> <chr>                         <dbl>      <dbl>
      1      8.74     2  0.0126 Pearson's Chi-squared test    0.464       0.95
        conf.low conf.high effectsize        conf.method conf.distribution expression
           <dbl>     <dbl> <chr>             <chr>       <chr>             <list>    
      1        0     0.777 Cramer's V (adj.) ncp         chisq             <language>
      
      $caption_data
      # A tibble: 8 x 13
        term      estimate conf.level conf.low conf.high    pd rope.percentage
        <chr>        <dbl>      <dbl>    <dbl>     <dbl> <dbl>           <dbl>
      1 cell[1,1]    3.15        0.95    0.703     6.56      1               0
      2 cell[2,1]    4.02        0.95    1.19      7.58      1               0
      3 cell[3,1]   10.9         0.95    6.54     15.8       1               0
      4 cell[1,2]    7.48        0.95    3.59     11.9       1               0
      5 cell[2,2]    3.16        0.95    0.889     6.66      1               0
      6 cell[3,2]    2.28        0.95    0.371     5.25      1               0
      7 Ratio       NA          NA      NA        NA        NA              NA
      8 Cramers_v    0.476       0.95    0.224     0.718     1               0
        prior.distribution      prior.location prior.scale  bf10
        <chr>                            <dbl>       <dbl> <dbl>
      1 independent multinomial              0           1  15.1
      2 independent multinomial              0           1  15.1
      3 independent multinomial              0           1  15.1
      4 independent multinomial              0           1  15.1
      5 independent multinomial              0           1  15.1
      6 independent multinomial              0           1  15.1
      7 independent multinomial              0           1  15.1
      8 independent multinomial              0           1  15.1
        method                              expression
        <chr>                               <list>    
      1 Bayesian contingency table analysis <language>
      2 Bayesian contingency table analysis <language>
      3 Bayesian contingency table analysis <language>
      4 Bayesian contingency table analysis <language>
      5 Bayesian contingency table analysis <language>
      6 Bayesian contingency table analysis <language>
      7 Bayesian contingency table analysis <language>
      8 Bayesian contingency table analysis <language>
      
      $pairwise_comparisons_data
      NULL
      
      $descriptive_data
      # A tibble: 6 x 5
        am    cyl   counts  perc .label
        <fct> <fct>  <int> <dbl> <chr> 
      1 0     8         12  63.2 63%   
      2 1     8          2  15.4 15%   
      3 0     6          4  21.1 21%   
      4 1     6          3  23.1 23%   
      5 0     4          3  15.8 16%   
      6 1     4          8  61.5 62%   
      
      $one_sample_data
      # A tibble: 2 x 10
        am    counts  perc N        statistic    df p.value
        <fct>  <int> <dbl> <chr>        <dbl> <dbl>   <dbl>
      1 1         13  40.6 (n = 13)      4.77     2  0.0921
      2 0         19  59.4 (n = 19)      7.68     2  0.0214
        method                                  
        <chr>                                   
      1 Chi-squared test for given probabilities
      2 Chi-squared test for given probabilities
        .label                                                            
        <chr>                                                             
      1 list(~chi['gof']^2~(2)==4.77, ~italic(p)=='0.092', ~italic(n)==13)
      2 list(~chi['gof']^2~(2)==7.68, ~italic(p)=='0.021', ~italic(n)==19)
        .p.label                 
        <chr>                    
      1 list(~italic(p)=='0.092')
      2 list(~italic(p)=='0.021')
      

