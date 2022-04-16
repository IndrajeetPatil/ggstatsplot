# ggcoefstats doesn't work if no estimate column found

    The tidy dataframe *must* contain 'estimate' column.

# data frame outputs as expected

    Code
      list(tidy_df, glance_df)
    Output
      [[1]]
      # A tibble: 2 x 10
        term        estimate std.error conf.level conf.low conf.high statistic
        <fct>          <dbl>     <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 (Intercept)    6.05     0.309        0.95    5.42      6.68      19.6 
      2 mpg           -0.141    0.0147       0.95   -0.171    -0.111     -9.56
        df.error  p.value
           <int>    <dbl>
      1       30 1.20e-18
      2       30 1.29e-10
        expression                                                                    
        <glue>                                                                        
      1 list(widehat(italic(beta))=='6.05', italic(t)('30')=='19.59', italic(p)=='1.2~
      2 list(widehat(italic(beta))=='-0.14', italic(t)('30')=='-9.56', italic(p)=='1.~
      
      [[2]]
      # A tibble: 1 x 6
          AIC   BIC    R2 R2_adjusted  RMSE Sigma
        <dbl> <dbl> <dbl>       <dbl> <dbl> <dbl>
      1  49.7  54.1 0.753       0.745 0.479 0.494
      

# edge cases

    Elements in `term` column must be unique.

