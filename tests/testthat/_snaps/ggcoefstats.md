# ggcoefstats with glm with z

    Code
      pb$data
    Output
      [[1]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[2]]
                 x       xmin       xmax y PANEL group ymin ymax colour size linetype
      1 -0.7800447 -1.1521325 -0.4124066 1     1     1    1    1  black  0.5        1
      2  2.2940067  2.0985524  2.4932922 2     1     2    2    2  black  0.5        1
      3 -0.5564393 -0.9299989 -0.1804338 3     1     3    3    3  black  0.5        1
        height alpha
      1      0    NA
      2      0    NA
      3      0    NA
      
      [[3]]
                 x y PANEL group shape colour size fill alpha stroke
      1 -0.7800447 1     1     1    19   blue    3   NA    NA    0.5
      2  2.2940067 2     1     2    19   blue    3   NA    NA    0.5
      3 -0.5564393 3     1     3    19   blue    3   NA    NA    0.5
      
      [[4]]
                 x y
      1 -0.7800447 1
      2  2.2940067 2
      3 -0.5564393 3
                                                                                    label
      1   list(~widehat(italic(beta))=='-0.78', ~italic(z)=='-3.47', ~italic(p)=='0.001')
      2 list(~widehat(italic(beta))=='2.29', ~italic(z)=='19.13', ~italic(p)=='1.54e-81')
      3   list(~widehat(italic(beta))=='-0.56', ~italic(z)=='-2.44', ~italic(p)=='0.014')
        PANEL group    colour  fill size angle alpha family fontface lineheight hjust
      1     1     1 #1B9E77FF white    3     0    NA               1        1.2   0.5
      2     1     2 #D95F02FF white    3     0    NA               1        1.2   0.5
      3     1     3 #7570B3FF white    3     0    NA               1        1.2   0.5
        vjust point.size segment.linetype segment.size segment.curvature
      1   0.5          1                1          0.5                 0
      2   0.5          1                1          0.5                 0
      3   0.5          1                1          0.5                 0
        segment.angle segment.ncp segment.shape segment.square segment.squareShape
      1            90           1           0.5           TRUE                   1
      2            90           1           0.5           TRUE                   1
      3            90           1           0.5           TRUE                   1
        segment.inflect segment.debug
      1           FALSE         FALSE
      2           FALSE         FALSE
      3           FALSE         FALSE
      

# ggcoefstats with coxph.panel model

    Code
      pb$data
    Output
      [[1]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[2]]
                  x         xmin       xmax y PANEL group ymin ymax colour size
      1  0.01703351 -0.001062183  0.0351292 1     1     1    1    1  black  0.5
      2 -0.51166834 -0.840312344 -0.1830243 2     1     2    2    2  black  0.5
        linetype height alpha
      1        1      0    NA
      2        1      0    NA
      
      [[3]]
                  x y PANEL group shape colour size fill alpha stroke
      1  0.01703351 1     1     1    19   blue    3   NA    NA    0.5
      2 -0.51166834 2     1     2    19   blue    3   NA    NA    0.5
      
      [[4]]
                  x y
      1  0.01703351 1
      2 -0.51166834 2
                                                                                            label
      1  list(~widehat(italic(beta))=='0.02', ~italic(chi)^2~('225')==3.40', ~italic(p)=='0.065')
      2 list(~widehat(italic(beta))=='-0.51', ~italic(chi)^2~('225')==9.31', ~italic(p)=='0.002')
        PANEL group    colour  fill size angle alpha family fontface lineheight hjust
      1     1     1 #1B9E77FF white    3     0    NA               1        1.2   0.5
      2     1     2 #D95F02FF white    3     0    NA               1        1.2   0.5
        vjust point.size segment.linetype segment.size segment.curvature
      1   0.5          1                1          0.5                 0
      2   0.5          1                1          0.5                 0
        segment.angle segment.ncp segment.shape segment.square segment.squareShape
      1            90           1           0.5           TRUE                   1
      2            90           1           0.5           TRUE                   1
        segment.inflect segment.debug
      1           FALSE         FALSE
      2           FALSE         FALSE
      

# ggcoefstats with lm model

    Code
      pb$data
    Output
      [[1]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[2]]
                  x        xmin        xmax y PANEL group ymin ymax colour size
      1 -0.15565484 -0.22929853 -0.08201116 1     1     1    1    1  black  0.5
      2 -1.80872181 -3.71964130  0.10219768 2     1     2    2    2  black  0.5
      3  0.06471454 -0.02784951  0.15727859 3     1     3    3    3  black  0.5
        linetype height alpha
      1        1      0    NA
      2        1      0    NA
      3        1      0    NA
      
      [[3]]
                  x y PANEL group shape colour size fill alpha stroke
      1 -0.15565484 1     1     1    19   blue    3   NA    NA    0.5
      2 -1.80872181 2     1     2    19   blue    3   NA    NA    0.5
      3  0.06471454 3     1     3    19   blue    3   NA    NA    0.5
      
      [[4]]
                  x y
      1 -0.15565484 1
      2 -1.80872181 2
      3  0.06471454 3
                                                                                             label
      1 list(~widehat(italic(beta))=='-0.156', ~italic(t)('28')=='-5.840', ~italic(p)=='2.81e-06')
      2    list(~widehat(italic(beta))=='-1.809', ~italic(t)('28')=='-2.615', ~italic(p)=='0.014')
      3                                                                                       <NA>
        PANEL group    colour  fill size angle alpha family fontface lineheight hjust
      1     1     1 #1B9E77FF white    3     0    NA               1        1.2   0.5
      2     1     2 #D95F02FF white    3     0    NA               1        1.2   0.5
      3     1     3 #7570B3FF white    3     0    NA               1        1.2   0.5
        vjust point.size segment.linetype segment.size segment.curvature
      1   0.5          1                1          0.5                 0
      2   0.5          1                1          0.5                 0
      3   0.5          1                1          0.5                 0
        segment.angle segment.ncp segment.shape segment.square segment.squareShape
      1            90           1           0.5           TRUE                   1
      2            90           1           0.5           TRUE                   1
      3            90           1           0.5           TRUE                   1
        segment.inflect segment.debug
      1           FALSE         FALSE
      2           FALSE         FALSE
      3           FALSE         FALSE
      

# ggcoefstats with partial variants of effect size for f-statistic

    Code
      list(tidy_df1, p$labels)
    Output
      [[1]]
      # A tibble: 3 x 12
        term    sumsq    df meansq statistic  p.value estimate conf.low conf.high
        <fct>   <dbl> <dbl>  <dbl>     <dbl>    <dbl>    <dbl>    <dbl>     <dbl>
      1 mpg    22.3       1 22.3      119.   1.38e-11    0.809   0.663      0.880
      2 am      1.37      1  1.37       7.30 1.16e- 2    0.207   0.0118     0.448
      3 mpg:am  0.701     1  0.701      3.73 6.36e- 2    0.118   0          0.357
        df.error estimate.type      
           <dbl> <chr>              
      1       28 partial eta-squared
      2       28 partial eta-squared
      3       28 partial eta-squared
        label                                                                         
        <chr>                                                                         
      1 "list(~italic(F)('1'*\",\"*'28')=='118.89', ~italic(p)=='1.38e-11', ~widehat(~
      2 "list(~italic(F)('1'*\",\"*'28')=='7.30', ~italic(p)=='0.012', ~widehat(itali~
      3 "list(~italic(F)('1'*\",\"*'28')=='3.73', ~italic(p)=='0.064', ~widehat(itali~
      
      [[2]]
      [[2]]$x
      [1] "partial eta-squared"
      
      [[2]]$y
      [1] "effect"
      
      [[2]]$title
      NULL
      
      [[2]]$subtitle
      NULL
      
      [[2]]$caption
      atop(displaystyle(NULL), expr = paste("AIC = ", "43", ", BIC = ", 
          "50"))
      
      [[2]]$xintercept
      [1] "xintercept"
      
      [[2]]$xmin
      [1] "conf.low"
      
      [[2]]$xmax
      [1] "conf.high"
      
      [[2]]$label
      [1] "label"
      
      

---

    Code
      list(tidy_df2, p$labels)
    Output
      [[1]]
      # A tibble: 3 x 12
        term         sumsq    df meansq statistic  p.value estimate conf.low conf.high
        <fct>        <dbl> <dbl>  <dbl>     <dbl>    <dbl>    <dbl>    <dbl>     <dbl>
      1 vore         19.6      3   6.54      7.39 0.000584   0.308    0.0458     0.498
      2 brainwt       1.80     1   1.80      2.03 0.163      0.0235   0          0.192
      3 vore:brainwt 10.7      3   3.55      4.01 0.0148     0.174    0          0.364
        df.error estimate.type        
           <dbl> <chr>                
      1       35 partial omega-squared
      2       35 partial omega-squared
      3       35 partial omega-squared
        label                                                                         
        <chr>                                                                         
      1 "list(~italic(F)('3'*\",\"*'35')=='7.388', ~italic(p)=='0.001', ~widehat(ital~
      2 "list(~italic(F)('1'*\",\"*'35')=='2.034', ~italic(p)=='0.163', ~widehat(ital~
      3 "list(~italic(F)('3'*\",\"*'35')=='4.012', ~italic(p)=='0.015', ~widehat(ital~
      
      [[2]]
      [[2]]$x
      [1] "partial omega-squared"
      
      [[2]]$y
      [1] "term"
      
      [[2]]$title
      [1] "mammalian sleep"
      
      [[2]]$subtitle
      [1] "Source: `ggplot2` package"
      
      [[2]]$caption
      atop(displaystyle(paste(italic("Note"), ": From `tidyverse`")), 
          expr = paste("AIC = ", "126", ", BIC = ", "142"))
      
      [[2]]$xintercept
      [1] "xintercept"
      
      [[2]]$xmin
      [1] "conf.low"
      
      [[2]]$xmax
      [1] "conf.high"
      
      [[2]]$label
      [1] "label"
      
      

# check tidy output

    Code
      list(tidy_df1, tidy_df2)
    Output
      [[1]]
      # A tibble: 7 x 13
        group  term    sumsq    df  meansq statistic p.value estimate conf.low
        <chr>  <fct>   <dbl> <dbl>   <dbl>     <dbl>   <dbl>    <dbl>    <dbl>
      1 block  N:P:K  37.0       1  37.0      0.483  0.525    0.108     0     
      2 Within N     189.        1 189.      12.3    0.00437  0.505     0.0844
      3 Within P       8.40      1   8.40     0.544  0.475    0.0434    0     
      4 Within K      95.2       1  95.2      6.17   0.0288   0.339     0     
      5 Within N:P    21.3       1  21.3      1.38   0.263    0.103     0     
      6 Within N:K    33.1       1  33.1      2.15   0.169    0.152     0     
      7 Within P:K     0.482     1   0.482    0.0312 0.863    0.00259   0     
        conf.high df.error estimate.type      
            <dbl>    <dbl> <chr>              
      1     0.639        4 partial eta-squared
      2     0.741       12 partial eta-squared
      3     0.378       12 partial eta-squared
      4     0.642       12 partial eta-squared
      5     0.455       12 partial eta-squared
      6     0.502       12 partial eta-squared
      7     0.223       12 partial eta-squared
        label                                                                         
        <chr>                                                                         
      1 "list(~italic(F)('1'*\",\"*'4')=='0.48', ~italic(p)=='0.525', ~widehat(italic~
      2 "list(~italic(F)('1'*\",\"*'12')=='12.26', ~italic(p)=='0.004', ~widehat(ital~
      3 "list(~italic(F)('1'*\",\"*'12')=='0.54', ~italic(p)=='0.475', ~widehat(itali~
      4 "list(~italic(F)('1'*\",\"*'12')=='6.17', ~italic(p)=='0.029', ~widehat(itali~
      5 "list(~italic(F)('1'*\",\"*'12')=='1.38', ~italic(p)=='0.263', ~widehat(itali~
      6 "list(~italic(F)('1'*\",\"*'12')=='2.15', ~italic(p)=='0.169', ~widehat(itali~
      7 "list(~italic(F)('1'*\",\"*'12')=='0.03', ~italic(p)=='0.863', ~widehat(itali~
      
      [[2]]
      # A tibble: 7 x 12
        term    sumsq    df  meansq statistic p.value estimate conf.low conf.high
        <fct>   <dbl> <dbl>   <dbl>     <dbl>   <dbl>    <dbl>    <dbl>     <dbl>
      1 N     189.        1 189.       6.16    0.0245 0.278    0.000467     0.568
      2 P       8.40      1   8.40     0.273   0.608  0.0168   0            0.274
      3 K      95.2       1  95.2      3.10    0.0975 0.162    0            0.473
      4 N:P    21.3       1  21.3      0.693   0.418  0.0415   0            0.328
      5 N:K    33.1       1  33.1      1.08    0.314  0.0631   0            0.362
      6 P:K     0.482     1   0.482    0.0157  0.902  0.000979 0            0.147
      7 N:P:K  37.0       1  37.0      1.20    0.289  0.0700   0            0.372
        df.error estimate.type      
           <dbl> <chr>              
      1       16 partial eta-squared
      2       16 partial eta-squared
      3       16 partial eta-squared
      4       16 partial eta-squared
      5       16 partial eta-squared
      6       16 partial eta-squared
      7       16 partial eta-squared
        label                                                                         
        <chr>                                                                         
      1 "list(~italic(F)('1'*\",\"*'16')=='6.16', ~italic(p)=='0.025', ~widehat(itali~
      2 "list(~italic(F)('1'*\",\"*'16')=='0.27', ~italic(p)=='0.608', ~widehat(itali~
      3 "list(~italic(F)('1'*\",\"*'16')=='3.10', ~italic(p)=='0.097', ~widehat(itali~
      4 "list(~italic(F)('1'*\",\"*'16')=='0.69', ~italic(p)=='0.418', ~widehat(itali~
      5 "list(~italic(F)('1'*\",\"*'16')=='1.08', ~italic(p)=='0.314', ~widehat(itali~
      6 "list(~italic(F)('1'*\",\"*'16')=='0.02', ~italic(p)=='0.902', ~widehat(itali~
      7 "list(~italic(F)('1'*\",\"*'16')=='1.20', ~italic(p)=='0.289', ~widehat(itali~
      

# duplicated terms

    Code
      pb$data
    Output
      [[1]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[2]]
                x       xmin      xmax y PANEL group ymin ymax colour size linetype
      1 29.322072 29.0912441 29.552899 1     1     1    1    1  black  0.5        1
      2  1.124451  0.6227402  1.626161 2     1     2    2    2  black  0.5        1
      3 29.954761 29.7319808 30.177540 3     1     3    3    3  black  0.5        1
      4  1.182257  0.8641108  1.500404 4     1     4    4    4  black  0.5        1
      5 30.628379 30.4324684 30.824290 5     1     5    5    5  black  0.5        1
      6  1.251657  0.8884855  1.614829 6     1     6    6    6  black  0.5        1
        height alpha
      1      0    NA
      2      0    NA
      3      0    NA
      4      0    NA
      5      0    NA
      6      0    NA
      
      [[3]]
                x y PANEL group shape colour size fill alpha stroke
      1 29.322072 1     1     1    19   blue    3   NA    NA    0.5
      2  1.124451 2     1     2    19   blue    3   NA    NA    0.5
      3 29.954761 3     1     3    19   blue    3   NA    NA    0.5
      4  1.182257 4     1     4    19   blue    3   NA    NA    0.5
      5 30.628379 5     1     5    19   blue    3   NA    NA    0.5
      6  1.251657 6     1     6    19   blue    3   NA    NA    0.5
      
      [[4]]
                x y
      1 29.322072 1
      2  1.124451 2
      3 29.954761 3
      4  1.182257 4
      5 30.628379 5
      6  1.251657 6
                                                                                      label
      1 list(~widehat(italic(beta))=='29.32', ~italic(t)=='249.58', ~italic(p)=='9.84e-78')
      2    list(~widehat(italic(beta))=='1.12', ~italic(t)=='4.40', ~italic(p)=='5.77e-05')
      3 list(~widehat(italic(beta))=='29.95', ~italic(t)=='264.18', ~italic(p)=='6.08e-79')
      4    list(~widehat(italic(beta))=='1.18', ~italic(t)=='7.30', ~italic(p)=='2.27e-09')
      5 list(~widehat(italic(beta))=='30.63', ~italic(t)=='307.16', ~italic(p)=='3.78e-82')
      6     list(~widehat(italic(beta))=='1.25', ~italic(t)=='6.77', ~italic(p)=='1.5e-08')
        PANEL group    colour  fill size angle alpha family fontface lineheight hjust
      1     1     1 #1B9E77FF white    3     0    NA               1        1.2   0.5
      2     1     2 #D95F02FF white    3     0    NA               1        1.2   0.5
      3     1     3 #7570B3FF white    3     0    NA               1        1.2   0.5
      4     1     4 #E7298AFF white    3     0    NA               1        1.2   0.5
      5     1     5 #66A61EFF white    3     0    NA               1        1.2   0.5
      6     1     6 #E6AB02FF white    3     0    NA               1        1.2   0.5
        vjust point.size segment.linetype segment.size segment.curvature
      1   0.5          1                1          0.5                 0
      2   0.5          1                1          0.5                 0
      3   0.5          1                1          0.5                 0
      4   0.5          1                1          0.5                 0
      5   0.5          1                1          0.5                 0
      6   0.5          1                1          0.5                 0
        segment.angle segment.ncp segment.shape segment.square segment.squareShape
      1            90           1           0.5           TRUE                   1
      2            90           1           0.5           TRUE                   1
      3            90           1           0.5           TRUE                   1
      4            90           1           0.5           TRUE                   1
      5            90           1           0.5           TRUE                   1
      6            90           1           0.5           TRUE                   1
        segment.inflect segment.debug
      1           FALSE         FALSE
      2           FALSE         FALSE
      3           FALSE         FALSE
      4           FALSE         FALSE
      5           FALSE         FALSE
      6           FALSE         FALSE
      

