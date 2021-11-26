# `pairwise_comparisons()` - test additional arguments

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
      

