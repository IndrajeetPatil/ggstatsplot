# `pairwise_comparisons()` works for between-subjects design

    Code
      list(df1, df2, df3, df4, df5)
    Output
      [[1]]
      # A tibble: 6 x 6
        group1  group2  p.value test.details     p.value.adjustment
        <chr>   <chr>     <dbl> <chr>            <chr>             
      1 carni   herbi     1     Student's t-test Bonferroni        
      2 carni   insecti   1     Student's t-test Bonferroni        
      3 carni   omni      1     Student's t-test Bonferroni        
      4 herbi   insecti   1     Student's t-test Bonferroni        
      5 herbi   omni      0.979 Student's t-test Bonferroni        
      6 insecti omni      1     Student's t-test Bonferroni        
        label                                        
        <chr>                                        
      1 list(~italic(p)[Bonferroni-corrected]==1.000)
      2 list(~italic(p)[Bonferroni-corrected]==1.000)
      3 list(~italic(p)[Bonferroni-corrected]==1.000)
      4 list(~italic(p)[Bonferroni-corrected]==1.000)
      5 list(~italic(p)[Bonferroni-corrected]==0.979)
      6 list(~italic(p)[Bonferroni-corrected]==1.000)
      
      [[2]]
      # A tibble: 6 x 11
        group1  group2  statistic p.value alternative method            distribution
        <chr>   <chr>       <dbl>   <dbl> <chr>       <chr>             <chr>       
      1 carni   herbi        2.17       1 two.sided   Games-Howell test q           
      2 carni   insecti     -2.17       1 two.sided   Games-Howell test q           
      3 carni   omni         1.10       1 two.sided   Games-Howell test q           
      4 herbi   insecti     -2.41       1 two.sided   Games-Howell test q           
      5 herbi   omni        -1.87       1 two.sided   Games-Howell test q           
      6 insecti omni         2.19       1 two.sided   Games-Howell test q           
        p.adjustment test.details      p.value.adjustment
        <chr>        <chr>             <chr>             
      1 none         Games-Howell test Bonferroni        
      2 none         Games-Howell test Bonferroni        
      3 none         Games-Howell test Bonferroni        
      4 none         Games-Howell test Bonferroni        
      5 none         Games-Howell test Bonferroni        
      6 none         Games-Howell test Bonferroni        
        label                                        
        <chr>                                        
      1 list(~italic(p)[Bonferroni-corrected]==1.000)
      2 list(~italic(p)[Bonferroni-corrected]==1.000)
      3 list(~italic(p)[Bonferroni-corrected]==1.000)
      4 list(~italic(p)[Bonferroni-corrected]==1.000)
      5 list(~italic(p)[Bonferroni-corrected]==1.000)
      6 list(~italic(p)[Bonferroni-corrected]==1.000)
      
      [[3]]
      # A tibble: 6 x 11
        group1  group2  statistic p.value alternative method               
        <chr>   <chr>       <dbl>   <dbl> <chr>       <chr>                
      1 carni   herbi       0.582  0.561  two.sided   Dunn's all-pairs test
      2 carni   insecti     1.88   0.0595 two.sided   Dunn's all-pairs test
      3 carni   omni        1.14   0.254  two.sided   Dunn's all-pairs test
      4 herbi   insecti     1.63   0.102  two.sided   Dunn's all-pairs test
      5 herbi   omni        0.717  0.474  two.sided   Dunn's all-pairs test
      6 insecti omni        1.14   0.254  two.sided   Dunn's all-pairs test
        distribution p.adjustment test.details p.value.adjustment
        <chr>        <chr>        <chr>        <chr>             
      1 z            none         Dunn test    None              
      2 z            none         Dunn test    None              
      3 z            none         Dunn test    None              
      4 z            none         Dunn test    None              
      5 z            none         Dunn test    None              
      6 z            none         Dunn test    None              
        label                               
        <chr>                               
      1 list(~italic(p)[uncorrected]==0.561)
      2 list(~italic(p)[uncorrected]==0.060)
      3 list(~italic(p)[uncorrected]==0.254)
      4 list(~italic(p)[uncorrected]==0.102)
      5 list(~italic(p)[uncorrected]==0.474)
      6 list(~italic(p)[uncorrected]==0.254)
      
      [[4]]
      # A tibble: 6 x 10
        group1  group2  estimate conf.level conf.low conf.high p.value
        <chr>   <chr>      <dbl>      <dbl>    <dbl>     <dbl>   <dbl>
      1 carni   herbi   -0.0323        0.95  -0.248     0.184    0.790
      2 carni   insecti  0.0451        0.95  -0.0484    0.139    0.552
      3 carni   omni     0.00520       0.95  -0.114     0.124    0.898
      4 herbi   insecti  0.0774        0.95  -0.133     0.288    0.552
      5 herbi   omni     0.0375        0.95  -0.182     0.257    0.790
      6 insecti omni    -0.0399        0.95  -0.142     0.0625   0.552
        test.details              p.value.adjustment
        <chr>                     <chr>             
      1 Yuen's trimmed means test FDR               
      2 Yuen's trimmed means test FDR               
      3 Yuen's trimmed means test FDR               
      4 Yuen's trimmed means test FDR               
      5 Yuen's trimmed means test FDR               
      6 Yuen's trimmed means test FDR               
        label                                 
        <chr>                                 
      1 list(~italic(p)[FDR-corrected]==0.790)
      2 list(~italic(p)[FDR-corrected]==0.552)
      3 list(~italic(p)[FDR-corrected]==0.898)
      4 list(~italic(p)[FDR-corrected]==0.552)
      5 list(~italic(p)[FDR-corrected]==0.790)
      6 list(~italic(p)[FDR-corrected]==0.552)
      
      [[5]]
      # A tibble: 3 x 6
        group1 group2 p.value test.details     p.value.adjustment
        <chr>  <chr>    <dbl> <chr>            <chr>             
      1 PG     PG-13  0.316   Student's t-test Holm              
      2 PG     R      0.00283 Student's t-test Holm              
      3 PG-13  R      0.00310 Student's t-test Holm              
        label                                  
        <chr>                                  
      1 list(~italic(p)[Holm-corrected]==0.316)
      2 list(~italic(p)[Holm-corrected]==0.003)
      3 list(~italic(p)[Holm-corrected]==0.003)
      

