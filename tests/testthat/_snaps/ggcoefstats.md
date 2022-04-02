# data frame outputs as expected

    Code
      list(tidy_df, glance_df)
    Output
      [[1]]
      # A tibble: 3 x 10
        term              estimate std.error conf.level conf.low conf.high statistic
        <fct>                <dbl>     <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 (Intercept)          5.01     0.0728       0.95    4.86       5.15     68.8 
      2 Speciesversicolor    0.930    0.103        0.95    0.727      1.13      9.03
      3 Speciesvirginica     1.58     0.103        0.95    1.38       1.79     15.4 
        df.error   p.value
           <int>     <dbl>
      1      147 1.13e-113
      2      147 8.77e- 16
      3      147 2.21e- 32
        label                                                                         
        <glue>                                                                        
      1 list(widehat(italic(beta))=='5.01', italic(t)('147')=='68.76', italic(p)=='1.~
      2 list(widehat(italic(beta))=='0.93', italic(t)('147')=='9.03', italic(p)=='8.7~
      3 list(widehat(italic(beta))=='1.58', italic(t)('147')=='15.37', italic(p)=='2.~
      
      [[2]]
      # A tibble: 1 x 6
          AIC   BIC    R2 R2_adjusted  RMSE Sigma
        <dbl> <dbl> <dbl>       <dbl> <dbl> <dbl>
      1  231.  243. 0.619       0.614 0.510 0.515
      

# edge cases

    Elements in `term` column must be unique.

