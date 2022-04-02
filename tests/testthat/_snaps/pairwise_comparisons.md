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
      1 list(~italic(p)[Bonferroni-corrected]==1.00)
      2 list(~italic(p)[Bonferroni-corrected]==1.00)
      3 list(~italic(p)[Bonferroni-corrected]==1.00)
      4 list(~italic(p)[Bonferroni-corrected]==1.00)
      5 list(~italic(p)[Bonferroni-corrected]==0.98)
      6 list(~italic(p)[Bonferroni-corrected]==1.00)
      
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
      1 list(~italic(p)[Bonferroni-corrected]==1.00)
      2 list(~italic(p)[Bonferroni-corrected]==1.00)
      3 list(~italic(p)[Bonferroni-corrected]==1.00)
      4 list(~italic(p)[Bonferroni-corrected]==1.00)
      5 list(~italic(p)[Bonferroni-corrected]==1.00)
      6 list(~italic(p)[Bonferroni-corrected]==1.00)
      
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
      1 list(~italic(p)[uncorrected]==0.56)
      2 list(~italic(p)[uncorrected]==0.06)
      3 list(~italic(p)[uncorrected]==0.25)
      4 list(~italic(p)[uncorrected]==0.10)
      5 list(~italic(p)[uncorrected]==0.47)
      6 list(~italic(p)[uncorrected]==0.25)
      
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
      1 list(~italic(p)[FDR-corrected]==0.79)
      2 list(~italic(p)[FDR-corrected]==0.55)
      3 list(~italic(p)[FDR-corrected]==0.90)
      4 list(~italic(p)[FDR-corrected]==0.55)
      5 list(~italic(p)[FDR-corrected]==0.79)
      6 list(~italic(p)[FDR-corrected]==0.55)
      
      [[5]]
      # A tibble: 3 x 6
        group1 group2 p.value test.details     p.value.adjustment
        <chr>  <chr>    <dbl> <chr>            <chr>             
      1 PG     PG-13  0.316   Student's t-test Holm              
      2 PG     R      0.00283 Student's t-test Holm              
      3 PG-13  R      0.00310 Student's t-test Holm              
        label                                     
        <chr>                                     
      1 list(~italic(p)[Holm-corrected]==0.32)    
      2 list(~italic(p)[Holm-corrected]==2.83e-03)
      3 list(~italic(p)[Holm-corrected]==3.10e-03)
      

# dropped levels are not included

    Code
      df2$label
    Output
      [1] "list(~italic(p)[uncorrected]==0.87)"

# data without NAs

    Code
      df$label
    Output
      [1] "list(~italic(p)[FDR-corrected]==1.32e-15)"
      [2] "list(~italic(p)[FDR-corrected]==6.64e-32)"
      [3] "list(~italic(p)[FDR-corrected]==2.77e-09)"

# `pairwise_comparisons()` works for within-subjects design - NAs

    Code
      list(df1, df2, df3)
    Output
      [[1]]
      # A tibble: 6 x 6
        group1 group2  p.value test.details     p.value.adjustment
        <chr>  <chr>     <dbl> <chr>            <chr>             
      1 HDHF   HDLF   3.18e- 3 Student's t-test Bonferroni        
      2 HDHF   LDHF   4.21e- 1 Student's t-test Bonferroni        
      3 HDHF   LDLF   3.95e-12 Student's t-test Bonferroni        
      4 HDLF   LDHF   3.37e- 1 Student's t-test Bonferroni        
      5 HDLF   LDLF   7.94e- 3 Student's t-test Bonferroni        
      6 LDHF   LDLF   1.33e- 8 Student's t-test Bonferroni        
        label                                            
        <chr>                                            
      1 list(~italic(p)[Bonferroni-corrected]==0.003)    
      2 list(~italic(p)[Bonferroni-corrected]==0.421)    
      3 list(~italic(p)[Bonferroni-corrected]==3.950e-12)
      4 list(~italic(p)[Bonferroni-corrected]==0.337)    
      5 list(~italic(p)[Bonferroni-corrected]==0.008)    
      6 list(~italic(p)[Bonferroni-corrected]==1.331e-08)
      
      [[2]]
      # A tibble: 6 x 11
        group1 group2 statistic  p.value alternative
        <chr>  <chr>      <dbl>    <dbl> <chr>      
      1 HDHF   HDLF        4.78 1.44e- 5 two.sided  
      2 HDHF   LDHF        2.44 4.47e- 2 two.sided  
      3 HDHF   LDLF        8.01 5.45e-13 two.sided  
      4 HDLF   LDHF        2.34 4.96e- 2 two.sided  
      5 HDLF   LDLF        3.23 5.05e- 3 two.sided  
      6 LDHF   LDLF        5.57 4.64e- 7 two.sided  
        method                                                                
        <chr>                                                                 
      1 Durbin's all-pairs test for a two-way balanced incomplete block design
      2 Durbin's all-pairs test for a two-way balanced incomplete block design
      3 Durbin's all-pairs test for a two-way balanced incomplete block design
      4 Durbin's all-pairs test for a two-way balanced incomplete block design
      5 Durbin's all-pairs test for a two-way balanced incomplete block design
      6 Durbin's all-pairs test for a two-way balanced incomplete block design
        distribution p.adjustment test.details        p.value.adjustment
        <chr>        <chr>        <chr>               <chr>             
      1 t            none         Durbin-Conover test BY                
      2 t            none         Durbin-Conover test BY                
      3 t            none         Durbin-Conover test BY                
      4 t            none         Durbin-Conover test BY                
      5 t            none         Durbin-Conover test BY                
      6 t            none         Durbin-Conover test BY                
        label                                    
        <chr>                                    
      1 list(~italic(p)[BY-corrected]==1.436e-05)
      2 list(~italic(p)[BY-corrected]==0.045)    
      3 list(~italic(p)[BY-corrected]==5.447e-13)
      4 list(~italic(p)[BY-corrected]==0.050)    
      5 list(~italic(p)[BY-corrected]==0.005)    
      6 list(~italic(p)[BY-corrected]==4.635e-07)
      
      [[3]]
      # A tibble: 6 x 11
        group1 group2 estimate conf.level conf.low conf.high     p.value  p.crit
        <chr>  <chr>     <dbl>      <dbl>    <dbl>     <dbl>       <dbl>   <dbl>
      1 HDHF   HDLF      1.03        0.95   0.140      1.92  0.00999     0.0127 
      2 HDHF   LDHF      0.454       0.95  -0.104      1.01  0.0520      0.025  
      3 HDHF   LDLF      1.95        0.95   1.09       2.82  0.000000564 0.00851
      4 HDLF   LDHF     -0.676       0.95  -1.61       0.256 0.0520      0.05   
      5 HDLF   LDLF      0.889       0.95   0.0244     1.75  0.0203      0.0169 
      6 LDHF   LDLF      1.35        0.95   0.560      2.14  0.000102    0.0102 
        test.details              p.value.adjustment
        <chr>                     <chr>             
      1 Yuen's trimmed means test Hommel            
      2 Yuen's trimmed means test Hommel            
      3 Yuen's trimmed means test Hommel            
      4 Yuen's trimmed means test Hommel            
      5 Yuen's trimmed means test Hommel            
      6 Yuen's trimmed means test Hommel            
        label                                        
        <chr>                                        
      1 list(~italic(p)[Hommel-corrected]==0.010)    
      2 list(~italic(p)[Hommel-corrected]==0.052)    
      3 list(~italic(p)[Hommel-corrected]==5.642e-07)
      4 list(~italic(p)[Hommel-corrected]==0.052)    
      5 list(~italic(p)[Hommel-corrected]==0.020)    
      6 list(~italic(p)[Hommel-corrected]==1.017e-04)
      

# `pairwise_comparisons()` works for within-subjects design - without NAs

    Code
      list(df1, df2, df3)
    Output
      [[1]]
      # A tibble: 3 x 6
        group1 group2  p.value test.details     p.value.adjustment
        <chr>  <chr>     <dbl> <chr>            <chr>             
      1 Wine A Wine B 0.732    Student's t-test None              
      2 Wine A Wine C 0.0142   Student's t-test None              
      3 Wine B Wine C 0.000675 Student's t-test None              
        label                                   
        <chr>                                   
      1 list(~italic(p)[uncorrected]==0.732)    
      2 list(~italic(p)[uncorrected]==0.014)    
      3 list(~italic(p)[uncorrected]==6.754e-04)
      
      [[2]]
      # A tibble: 3 x 11
        group1 group2 statistic  p.value alternative
        <chr>  <chr>      <dbl>    <dbl> <chr>      
      1 Wine A Wine B      1.05 0.301    two.sided  
      2 Wine A Wine C      3.66 0.000691 two.sided  
      3 Wine B Wine C      2.62 0.0123   two.sided  
        method                                                                
        <chr>                                                                 
      1 Durbin's all-pairs test for a two-way balanced incomplete block design
      2 Durbin's all-pairs test for a two-way balanced incomplete block design
      3 Durbin's all-pairs test for a two-way balanced incomplete block design
        distribution p.adjustment test.details        p.value.adjustment
        <chr>        <chr>        <chr>               <chr>             
      1 t            none         Durbin-Conover test None              
      2 t            none         Durbin-Conover test None              
      3 t            none         Durbin-Conover test None              
        label                                   
        <chr>                                   
      1 list(~italic(p)[uncorrected]==0.301)    
      2 list(~italic(p)[uncorrected]==6.915e-04)
      3 list(~italic(p)[uncorrected]==0.012)    
      
      [[3]]
      # A tibble: 3 x 11
        group1 group2 estimate conf.level conf.low conf.high p.value p.crit
        <chr>  <chr>     <dbl>      <dbl>    <dbl>     <dbl>   <dbl>  <dbl>
      1 Wine A Wine B   0.0214       0.95 -0.0216     0.0645 0.195   0.05  
      2 Wine A Wine C   0.114        0.95  0.0215     0.207  0.00492 0.0169
      3 Wine B Wine C   0.0821       0.95  0.00891    0.155  0.00878 0.025 
        test.details              p.value.adjustment
        <chr>                     <chr>             
      1 Yuen's trimmed means test None              
      2 Yuen's trimmed means test None              
      3 Yuen's trimmed means test None              
        label                               
        <chr>                               
      1 list(~italic(p)[uncorrected]==0.195)
      2 list(~italic(p)[uncorrected]==0.005)
      3 list(~italic(p)[uncorrected]==0.009)
      

# additional arguments are passed to underlying methods

    Code
      list(df1, df2, df3, df4)
    Output
      [[1]]
      # A tibble: 6 x 6
        group1 group2  p.value test.details     p.value.adjustment
        <chr>  <chr>     <dbl> <chr>            <chr>             
      1 HDHF   HDLF   2.65e- 4 Student's t-test None              
      2 HDHF   LDHF   3.51e- 2 Student's t-test None              
      3 HDHF   LDLF   3.29e-13 Student's t-test None              
      4 HDLF   LDHF   9.72e- 1 Student's t-test None              
      5 HDLF   LDLF   6.62e- 4 Student's t-test None              
      6 LDHF   LDLF   1.11e- 9 Student's t-test None              
        label                                  
        <chr>                                  
      1 list(~italic(p)[uncorrected]==2.65e-04)
      2 list(~italic(p)[uncorrected]==0.04)    
      3 list(~italic(p)[uncorrected]==3.29e-13)
      4 list(~italic(p)[uncorrected]==0.97)    
      5 list(~italic(p)[uncorrected]==6.62e-04)
      6 list(~italic(p)[uncorrected]==1.11e-09)
      
      [[2]]
      # A tibble: 6 x 6
        group1 group2 p.value test.details     p.value.adjustment
        <chr>  <chr>    <dbl> <chr>            <chr>             
      1 HDHF   HDLF    1.00   Student's t-test None              
      2 HDHF   LDHF    0.965  Student's t-test None              
      3 HDHF   LDLF    1.00   Student's t-test None              
      4 HDLF   LDHF    0.0281 Student's t-test None              
      5 HDLF   LDLF    0.999  Student's t-test None              
      6 LDHF   LDLF    1.00   Student's t-test None              
        label                              
        <chr>                              
      1 list(~italic(p)[uncorrected]==1.00)
      2 list(~italic(p)[uncorrected]==0.96)
      3 list(~italic(p)[uncorrected]==1.00)
      4 list(~italic(p)[uncorrected]==0.03)
      5 list(~italic(p)[uncorrected]==1.00)
      6 list(~italic(p)[uncorrected]==1.00)
      
      [[3]]
      # A tibble: 3 x 6
        group1 group2 p.value test.details     p.value.adjustment
        <chr>  <chr>    <dbl> <chr>            <chr>             
      1 4      6        0.995 Student's t-test None              
      2 4      8        1.00  Student's t-test None              
      3 6      8        0.997 Student's t-test None              
        label                              
        <chr>                              
      1 list(~italic(p)[uncorrected]==0.99)
      2 list(~italic(p)[uncorrected]==1.00)
      3 list(~italic(p)[uncorrected]==1.00)
      
      [[4]]
      # A tibble: 3 x 6
        group1 group2     p.value test.details     p.value.adjustment
        <chr>  <chr>        <dbl> <chr>            <chr>             
      1 4      6      0.00532     Student's t-test None              
      2 4      8      0.000000103 Student's t-test None              
      3 6      8      0.00258     Student's t-test None              
        label                                  
        <chr>                                  
      1 list(~italic(p)[uncorrected]==5.32e-03)
      2 list(~italic(p)[uncorrected]==1.03e-07)
      3 list(~italic(p)[uncorrected]==2.58e-03)
      

