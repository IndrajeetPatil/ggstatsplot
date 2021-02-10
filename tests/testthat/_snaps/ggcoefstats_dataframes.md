# ggcoefstats works with data frames

    Code
      list(pb1$data, pb2$data, pb3$data, pb4$data, pb5$data, pb6$data, pb7$data)
    Output
      [[1]]
      [[1]][[1]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[1]][[2]]
             x   xmin  xmax y PANEL group ymin ymax colour size linetype height alpha
      1 0.0665 -0.778 0.911 1     1     1    1    1  black  0.5        1      0    NA
      2 0.5420 -0.280 1.360 2     1     2    2    2  black  0.5        1      0    NA
      3 0.0450  0.030 0.650 3     1     3    3    3  black  0.5        1      0    NA
      
      [[1]][[3]]
             x y PANEL group shape colour size fill alpha stroke
      1 0.0665 1     1     1    19   blue    3   NA    NA    0.5
      2 0.5420 2     1     2    19   blue    3   NA    NA    0.5
      3 0.0450 3     1     3    19   blue    3   NA    NA    0.5
      
      [[1]][[4]]
             x y
      1 0.0665 1
      2 0.5420 2
      3 0.0450 3
                                                                                label
      1  list(~widehat(italic(beta))==0.07, ~italic(t)(5)==0.16, ~italic(p)=='0.875')
      2 list(~widehat(italic(beta))==0.54, ~italic(t)(10)==1.33, ~italic(p)=='0.191')
      3 list(~widehat(italic(beta))==0.04, ~italic(t)(12)==1.24, ~italic(p)=='0.001')
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
      
      
      [[2]]
      [[2]][[1]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[2]][[2]]
             x   xmin  xmax y PANEL group ymin ymax colour size linetype height alpha
      1 0.0665 -0.778 0.911 2     1     2    2    2  black  0.5        1      0    NA
      2 0.5420 -0.280 1.360 1     1     1    1    1  black  0.5        1      0    NA
      3 0.0450  0.030 0.650 3     1     3    3    3  black  0.5        1      0    NA
      
      [[2]][[3]]
             x y PANEL group shape colour size fill alpha stroke
      1 0.0665 2     1     2    19   blue    3   NA    NA    0.5
      2 0.5420 1     1     1    19   blue    3   NA    NA    0.5
      3 0.0450 3     1     3    19   blue    3   NA    NA    0.5
      
      [[2]][[4]]
             x y
      1 0.0665 2
      2 0.5420 1
      3 0.0450 3
                                                                            label
      1 list(~widehat(italic(beta))==0.07, ~italic(z)==0.16, ~italic(p)=='0.875')
      2 list(~widehat(italic(beta))==0.54, ~italic(z)==1.33, ~italic(p)=='0.191')
      3 list(~widehat(italic(beta))==0.04, ~italic(z)==1.24, ~italic(p)=='0.001')
        PANEL group    colour  fill size angle alpha family fontface lineheight hjust
      1     1     2 #1B9E77FF white    3     0    NA               1        1.2   0.5
      2     1     1 #D95F02FF white    3     0    NA               1        1.2   0.5
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
      
      
      [[3]]
      [[3]][[1]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[3]][[2]]
             x   xmin  xmax y PANEL group ymin ymax colour size linetype height alpha
      1 0.0665 -0.778 0.911 1     1     1    1    1  black  0.5        1      0    NA
      2 0.5420 -0.280 1.360 2     1     2    2    2  black  0.5        1      0    NA
      3 0.0450  0.030 0.650 3     1     3    3    3  black  0.5        1      0    NA
      
      [[3]][[3]]
             x y PANEL group shape colour size fill alpha stroke
      1 0.0665 1     1     1    19   blue    3   NA    NA    0.5
      2 0.5420 2     1     2    19   blue    3   NA    NA    0.5
      3 0.0450 3     1     3    19   blue    3   NA    NA    0.5
      
      
      [[4]]
      [[4]][[1]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[4]][[2]]
        y      x   xmin  xmax PANEL group ymin ymax colour size linetype height alpha
      1 1 0.0665 -0.778 0.911     1     1    1    1  black  0.5        1      0    NA
      2 2 0.5420 -0.280 1.360     1     2    2    2  black  0.5        1      0    NA
      3 3 0.0450  0.030 0.650     1     3    3    3  black  0.5        1      0    NA
      
      [[4]][[3]]
        y      x PANEL group shape colour size fill alpha stroke
      1 1 0.0665     1     1    19   blue    3   NA    NA    0.5
      2 2 0.5420     1     2    19   blue    3   NA    NA    0.5
      3 3 0.0450     1     3    19   blue    3   NA    NA    0.5
      
      
      [[5]]
      [[5]][[1]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[5]][[2]]
             x   xmin  xmax y PANEL group ymin ymax colour size linetype height alpha
      1 0.0665 -0.778 0.911 1     1     1    1    1  black  0.5        1      0    NA
      2 0.5420 -0.280 1.360 2     1     2    2    2  black  0.5        1      0    NA
      3 0.0450  0.030 0.650 3     1     3    3    3  black  0.5        1      0    NA
      
      [[5]][[3]]
             x y PANEL group shape colour size fill alpha stroke
      1 0.0665 1     1     1    19   blue    3   NA    NA    0.5
      2 0.5420 2     1     2    19   blue    3   NA    NA    0.5
      3 0.0450 3     1     3    19   blue    3   NA    NA    0.5
      
      [[5]][[4]]
             x y
      1 0.0665 1
      2 0.5420 2
      3 0.0450 3
                                                                                 label
      1 list(~widehat(italic(beta))==0.07, ~italic(t)(Inf)==0.16, ~italic(p)=='0.875')
      2 list(~widehat(italic(beta))==0.54, ~italic(t)(Inf)==1.33, ~italic(p)=='0.191')
      3 list(~widehat(italic(beta))==0.04, ~italic(t)(Inf)==1.24, ~italic(p)=='0.001')
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
      
      
      [[6]]
      [[6]][[1]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[6]][[2]]
             x   xmin  xmax y PANEL group ymin ymax colour size linetype height alpha
      1 0.0665 -0.778 0.911 1     1     1    1    1  black  0.5        1      0    NA
      2 0.5420 -0.280 1.360 2     1     2    2    2  black  0.5        1      0    NA
      3 0.0450  0.030 0.650 3     1     3    3    3  black  0.5        1      0    NA
      
      [[6]][[3]]
             x y PANEL group shape colour size fill alpha stroke
      1 0.0665 1     1     1    19   blue    3   NA    NA    0.5
      2 0.5420 2     1     2    19   blue    3   NA    NA    0.5
      3 0.0450 3     1     3    19   blue    3   NA    NA    0.5
      
      [[6]][[4]]
             x y
      1 0.0665 1
      2 0.5420 2
      3 0.0450 3
                                                                                  label
      1  list(~widehat(italic(beta))==0.066, ~italic(t)(5)==0.158, ~italic(p)=='0.875')
      2 list(~widehat(italic(beta))==0.542, ~italic(t)(10)==1.330, ~italic(p)=='0.191')
      3 list(~widehat(italic(beta))==0.045, ~italic(t)(12)==1.240, ~italic(p)=='0.001')
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
      
      
      [[7]]
      [[7]][[1]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[7]][[2]]
             x   xmin  xmax y PANEL group ymin ymax colour size linetype height alpha
      1 0.0665 -0.778 0.911 1     1     1    1    1  black  0.5        1      0    NA
      2 0.5420 -0.280 1.360 2     1     2    2    2  black  0.5        1      0    NA
      3 0.0450  0.030 0.650 3     1     3    3    3  black  0.5        1      0    NA
      
      [[7]][[3]]
             x y PANEL group shape colour size fill alpha stroke
      1 0.0665 1     1     1    19   blue    3   NA    NA    0.5
      2 0.5420 2     1     2    19   blue    3   NA    NA    0.5
      3 0.0450 3     1     3    19   blue    3   NA    NA    0.5
      
      [[7]][[4]]
             x y
      1 0.0665 1
      2 0.5420 2
      3 0.0450 3
                                                                                  label
      1  list(~widehat(italic(beta))==0.066, ~italic(t)(5)==0.158, ~italic(p)=='0.875')
      2 list(~widehat(italic(beta))==0.542, ~italic(t)(10)==1.330, ~italic(p)=='0.191')
      3 list(~widehat(italic(beta))==0.045, ~italic(t)(12)==1.240, ~italic(p)=='0.001')
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
      
      

