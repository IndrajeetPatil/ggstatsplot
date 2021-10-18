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
      3 list(~italic(p)[Bonferroni-corrected]==3.95e-12)
      4 list(~italic(p)[Bonferroni-corrected]==0.337)   
      5 list(~italic(p)[Bonferroni-corrected]==0.008)   
      6 list(~italic(p)[Bonferroni-corrected]==1.33e-08)
      
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
      1 list(~italic(p)[BY-corrected]==1.44e-05)
      2 list(~italic(p)[BY-corrected]==0.045)   
      3 list(~italic(p)[BY-corrected]==5.45e-13)
      4 list(~italic(p)[BY-corrected]==0.050)   
      5 list(~italic(p)[BY-corrected]==0.005)   
      6 list(~italic(p)[BY-corrected]==4.64e-07)
      
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
      3 list(~italic(p)[Hommel-corrected]==5.64e-07)
      4 list(~italic(p)[Hommel-corrected]==0.052)   
      5 list(~italic(p)[Hommel-corrected]==0.020)   
      6 list(~italic(p)[Hommel-corrected]==1.02e-04)
      

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
      3 list(~italic(p)[uncorrected]==6.75e-04)
      
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
      2 list(~italic(p)[uncorrected]==6.91e-04)
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
      

