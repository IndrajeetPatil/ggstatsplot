# ggcoefstats doesn't work if no estimate column found

    The tidy data frame *must* contain 'estimate' column.

# data frame outputs as expected

    Code
      list(tidy_df, select(glance_df, AIC, BIC))
    Output
      [[1]]
      # A tibble: 2 x 11
        term        estimate std.error conf.level conf.low conf.high statistic
        <fct>          <dbl>     <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
      1 (Intercept)    6.05     0.309        0.95    5.42      6.68      19.6 
      2 mpg           -0.141    0.0147       0.95   -0.171    -0.111     -9.56
        df.error  p.value conf.method expression
           <int>    <dbl> <chr>       <list>    
      1       30 1.20e-18 Wald        <language>
      2       30 1.29e-10 Wald        <language>
      
      [[2]]
      # A tibble: 1 x 2
          AIC   BIC
        <dbl> <dbl>
      1  49.7  54.1
      

# edge cases

    Elements in `term` column must be unique.

