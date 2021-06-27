# ggcoefstats with glm with z

    Code
      pb$data
    Output
      [[1]]
                 x y PANEL group shape colour size fill alpha stroke
      1 -0.7800447 1     1     1    19   blue    3   NA    NA    0.5
      2  2.2940067 2     1     2    19   blue    3   NA    NA    0.5
      3 -0.5564393 3     1     3    19   blue    3   NA    NA    0.5
      
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
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
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
      

# ggcoefstats with chi-squared statistic model

    Code
      pb$data
    Output
      [[1]]
                  x y PANEL group shape colour size fill alpha stroke
      1  0.01703351 1     1     1    19   blue    3   NA    NA    0.5
      2 -0.51166834 2     1     2    19   blue    3   NA    NA    0.5
      
      [[2]]
                  x         xmin       xmax y PANEL group ymin ymax colour size
      1  0.01703351 -0.001062183  0.0351292 1     1     1    1    1  black  0.5
      2 -0.51166834 -0.840312344 -0.1830243 2     1     2    2    2  black  0.5
        linetype height alpha
      1        1      0    NA
      2        1      0    NA
      
      [[3]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[4]]
                  x y
      1  0.01703351 1
      2 -0.51166834 2
                                                                                             label
      1  list(~widehat(italic(beta))=='0.02', ~italic(chi)^2~('225')=='3.40', ~italic(p)=='0.065')
      2 list(~widehat(italic(beta))=='-0.51', ~italic(chi)^2~('225')=='9.31', ~italic(p)=='0.002')
        PANEL group    colour  fill size angle alpha family fontface lineheight hjust
      1     1     1 #3182BDFF white    3     0    NA               1        1.2   0.5
      2     1     2 #E6550DFF white    3     0    NA               1        1.2   0.5
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
                  x y PANEL group shape colour size fill alpha stroke
      1 -0.15565484 1     1     1    19   blue    3   NA    NA    0.5
      2 -1.80872181 2     1     2    19   blue    3   NA    NA    0.5
      3  0.06471454 3     1     3    19   blue    3   NA    NA    0.5
      
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
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
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
      # A tibble: 3 x 14
        term   statistic    df df.error  p.value  sumsq meansq estimate conf.low
        <fct>      <dbl> <dbl>    <dbl>    <dbl>  <dbl>  <dbl>    <dbl>    <dbl>
      1 mpg       119.       1       28 1.38e-11 22.3   22.3      0.809   0.663 
      2 am          7.30     1       28 1.16e- 2  1.37   1.37     0.207   0.0118
      3 mpg:am      3.73     1       28 6.36e- 2  0.701  0.701    0.118   0     
        conf.high sum.squares.error mean.square.error effectsize         
            <dbl>             <dbl>             <dbl> <chr>              
      1     0.880              5.26              5.26 partial eta-squared
      2     0.448              5.26              5.26 partial eta-squared
      3     0.357              5.26              5.26 partial eta-squared
        label                                                                         
        <chr>                                                                         
      1 "list(~italic(F)('1'*\",\"*'28')=='118.89', ~italic(p)=='1.38e-11', ~widehat(~
      2 "list(~italic(F)('1'*\",\"*'28')=='7.30', ~italic(p)=='0.012', ~widehat(itali~
      3 "list(~italic(F)('1'*\",\"*'28')=='3.73', ~italic(p)=='0.064', ~widehat(itali~
      
      [[2]]
      [[2]]$x
      [1] "estimate"
      
      [[2]]$y
      [1] "effect"
      
      [[2]]$title
      NULL
      
      [[2]]$subtitle
      NULL
      
      [[2]]$caption
      atop(displaystyle(NULL), expr = paste("AIC = ", "43", ", BIC = ", 
          "50"))
      
      [[2]]$xmin
      [1] "conf.low"
      
      [[2]]$xmax
      [1] "conf.high"
      
      [[2]]$xintercept
      [1] "xintercept"
      
      [[2]]$label
      [1] "label"
      
      

---

    Code
      list(tidy_df2, p$labels)
    Output
      [[1]]
      # A tibble: 3 x 14
        term         statistic    df df.error  p.value sumsq meansq estimate conf.low
        <fct>            <dbl> <dbl>    <dbl>    <dbl> <dbl>  <dbl>    <dbl>    <dbl>
      1 vore              7.39     3       35 0.000584 19.6    6.54   0.308    0.0458
      2 brainwt           2.03     1       35 0.163     1.80   1.80   0.0235   0     
      3 vore:brainwt      4.01     3       35 0.0148   10.7    3.55   0.174    0     
        conf.high sum.squares.error mean.square.error effectsize           
            <dbl>             <dbl>             <dbl> <chr>                
      1     0.498              31.0              31.0 partial omega-squared
      2     0.192              31.0              31.0 partial omega-squared
      3     0.364              31.0              31.0 partial omega-squared
        label                                                                         
        <chr>                                                                         
      1 "list(~italic(F)('3'*\",\"*'35')=='7.388', ~italic(p)=='0.001', ~widehat(ital~
      2 "list(~italic(F)('1'*\",\"*'35')=='2.034', ~italic(p)=='0.163', ~widehat(ital~
      3 "list(~italic(F)('3'*\",\"*'35')=='4.012', ~italic(p)=='0.015', ~widehat(ital~
      
      [[2]]
      [[2]]$x
      [1] "estimate"
      
      [[2]]$y
      [1] "term"
      
      [[2]]$title
      [1] "mammalian sleep"
      
      [[2]]$subtitle
      [1] "Source: `ggplot2` package"
      
      [[2]]$caption
      atop(displaystyle(paste(italic("Note"), ": From `tidyverse`")), 
          expr = paste("AIC = ", "126", ", BIC = ", "142"))
      
      [[2]]$xmin
      [1] "conf.low"
      
      [[2]]$xmax
      [1] "conf.high"
      
      [[2]]$xintercept
      [1] "xintercept"
      
      [[2]]$label
      [1] "label"
      
      

# check tidy output

    Code
      list(tidy_df1, tidy_df2)
    Output
      [[1]]
      # A tibble: 7 x 15
        term  statistic    df df.error p.value group    sumsq  meansq estimate
        <fct>     <dbl> <dbl>    <dbl>   <dbl> <chr>    <dbl>   <dbl>    <dbl>
      1 N       12.3        1       12 0.00437 Within 189.    189.     0.505  
      2 P        0.544      1       12 0.475   Within   8.40    8.40   0.0434 
      3 K        6.17       1       12 0.0288  Within  95.2    95.2    0.339  
      4 N:P      1.38       1       12 0.263   Within  21.3    21.3    0.103  
      5 N:K      2.15       1       12 0.169   Within  33.1    33.1    0.152  
      6 P:K      0.0312     1       12 0.863   Within   0.482   0.482  0.00259
      7 N:P:K    0.483      1        4 0.525   block   37.0    37.0    0.108  
        conf.low conf.high sum.squares.error mean.square.error effectsize         
           <dbl>     <dbl>             <dbl>             <dbl> <chr>              
      1   0.0844     0.741              185.              185. partial eta-squared
      2   0          0.378              185.              185. partial eta-squared
      3   0          0.642              185.              185. partial eta-squared
      4   0          0.455              185.              185. partial eta-squared
      5   0          0.502              185.              185. partial eta-squared
      6   0          0.223              185.              185. partial eta-squared
      7   0          0.639              306.              306. partial eta-squared
        label                                                                         
        <chr>                                                                         
      1 "list(~italic(F)('1'*\",\"*'12')=='12.26', ~italic(p)=='0.004', ~widehat(ital~
      2 "list(~italic(F)('1'*\",\"*'12')=='0.54', ~italic(p)=='0.475', ~widehat(itali~
      3 "list(~italic(F)('1'*\",\"*'12')=='6.17', ~italic(p)=='0.029', ~widehat(itali~
      4 "list(~italic(F)('1'*\",\"*'12')=='1.38', ~italic(p)=='0.263', ~widehat(itali~
      5 "list(~italic(F)('1'*\",\"*'12')=='2.15', ~italic(p)=='0.169', ~widehat(itali~
      6 "list(~italic(F)('1'*\",\"*'12')=='0.03', ~italic(p)=='0.863', ~widehat(itali~
      7 "list(~italic(F)('1'*\",\"*'4')=='0.48', ~italic(p)=='0.525', ~widehat(italic~
      
      [[2]]
      # A tibble: 7 x 14
        term  statistic    df df.error p.value   sumsq  meansq estimate conf.low
        <fct>     <dbl> <dbl>    <dbl>   <dbl>   <dbl>   <dbl>    <dbl>    <dbl>
      1 N        6.16       1       16  0.0245 189.    189.    0.278    0.000467
      2 P        0.273      1       16  0.608    8.40    8.40  0.0168   0       
      3 K        3.10       1       16  0.0975  95.2    95.2   0.162    0       
      4 N:P      0.693      1       16  0.418   21.3    21.3   0.0415   0       
      5 N:K      1.08       1       16  0.314   33.1    33.1   0.0631   0       
      6 P:K      0.0157     1       16  0.902    0.482   0.482 0.000979 0       
      7 N:P:K    1.20       1       16  0.289   37.0    37.0   0.0700   0       
        conf.high sum.squares.error mean.square.error effectsize         
            <dbl>             <dbl>             <dbl> <chr>              
      1     0.568              492.              492. partial eta-squared
      2     0.274              492.              492. partial eta-squared
      3     0.473              492.              492. partial eta-squared
      4     0.328              492.              492. partial eta-squared
      5     0.362              492.              492. partial eta-squared
      6     0.147              492.              492. partial eta-squared
      7     0.372              492.              492. partial eta-squared
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
                x y PANEL group shape colour size fill alpha stroke
      1 29.322072 1     1     1    19   blue    3   NA    NA    0.5
      2  1.124451 2     1     2    19   blue    3   NA    NA    0.5
      3 29.954761 3     1     3    19   blue    3   NA    NA    0.5
      4  1.182257 4     1     4    19   blue    3   NA    NA    0.5
      5 30.628379 5     1     5    19   blue    3   NA    NA    0.5
      6  1.251657 6     1     6    19   blue    3   NA    NA    0.5
      
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
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[4]]
                x y
      1 29.322072 1
      2  1.124451 2
      3 29.954761 3
      4  1.182257 4
      5 30.628379 5
      6  1.251657 6
                                                                                      label
      1 list(~widehat(italic(beta))=='29.32', ~italic(z)=='249.58', ~italic(p)=='9.84e-78')
      2    list(~widehat(italic(beta))=='1.12', ~italic(z)=='4.40', ~italic(p)=='5.77e-05')
      3 list(~widehat(italic(beta))=='29.95', ~italic(z)=='264.18', ~italic(p)=='6.08e-79')
      4    list(~widehat(italic(beta))=='1.18', ~italic(z)=='7.30', ~italic(p)=='2.27e-09')
      5 list(~widehat(italic(beta))=='30.63', ~italic(z)=='307.16', ~italic(p)=='3.78e-82')
      6     list(~widehat(italic(beta))=='1.25', ~italic(z)=='6.77', ~italic(p)=='1.5e-08')
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
      

