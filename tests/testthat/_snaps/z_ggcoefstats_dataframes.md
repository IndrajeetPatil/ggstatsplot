# ggcoefstats works with data frames

    Code
      list(pb1$data, pb2$data, pb3$data, pb4$data, pb5$data, pb6$data, pb7$data)
    Output
      [[1]]
      [[1]][[1]]
             x y PANEL group shape colour size fill alpha stroke
      1 0.0665 1     1     1    19   blue    3   NA    NA    0.5
      2 0.5420 2     1     2    19   blue    3   NA    NA    0.5
      3 0.0450 3     1     3    19   blue    3   NA    NA    0.5
      
      [[1]][[2]]
             x   xmin  xmax y PANEL group ymin ymax colour size linetype height alpha
      1 0.0665 -0.778 0.911 1     1     1    1    1  black  0.5        1      0    NA
      2 0.5420 -0.280 1.360 2     1     2    2    2  black  0.5        1      0    NA
      3 0.0450  0.030 0.650 3     1     3    3    3  black  0.5        1      0    NA
      
      [[1]][[3]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[1]][[4]]
             x y
      1 0.0665 1
      2 0.5420 2
      3 0.0450 3
                                                                                      label
      1  list(~widehat(italic(beta))=='0.07', ~italic(t)('5')=='0.16', ~italic(p)=='0.875')
      2 list(~widehat(italic(beta))=='0.54', ~italic(t)('10')=='1.33', ~italic(p)=='0.191')
      3 list(~widehat(italic(beta))=='0.04', ~italic(t)('12')=='1.24', ~italic(p)=='0.001')
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
             x y PANEL group shape colour size fill alpha stroke
      1 0.0665 2     1     2    19   blue    3   NA    NA    0.5
      2 0.5420 1     1     1    19   blue    3   NA    NA    0.5
      3 0.0450 3     1     3    19   blue    3   NA    NA    0.5
      
      [[2]][[2]]
             x   xmin  xmax y PANEL group ymin ymax colour size linetype height alpha
      1 0.0665 -0.778 0.911 2     1     2    2    2  black  0.5        1      0    NA
      2 0.5420 -0.280 1.360 1     1     1    1    1  black  0.5        1      0    NA
      3 0.0450  0.030 0.650 3     1     3    3    3  black  0.5        1      0    NA
      
      [[2]][[3]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[2]][[4]]
             x y
      1 0.0665 2
      2 0.5420 1
      3 0.0450 3
                                                                                label
      1 list(~widehat(italic(beta))=='0.07', ~italic(z)=='0.16', ~italic(p)=='0.875')
      2 list(~widehat(italic(beta))=='0.54', ~italic(z)=='1.33', ~italic(p)=='0.191')
      3 list(~widehat(italic(beta))=='0.04', ~italic(z)=='1.24', ~italic(p)=='0.001')
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
             x y PANEL group shape colour size fill alpha stroke
      1 0.0665 1     1     1    19   blue    3   NA    NA    0.5
      2 0.5420 2     1     2    19   blue    3   NA    NA    0.5
      3 0.0450 3     1     3    19   blue    3   NA    NA    0.5
      
      [[3]][[2]]
             x   xmin  xmax y PANEL group ymin ymax colour size linetype height alpha
      1 0.0665 -0.778 0.911 1     1     1    1    1  black  0.5        1      0    NA
      2 0.5420 -0.280 1.360 2     1     2    2    2  black  0.5        1      0    NA
      3 0.0450  0.030 0.650 3     1     3    3    3  black  0.5        1      0    NA
      
      [[3]][[3]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      
      [[4]]
      [[4]][[1]]
        y      x PANEL group shape colour size fill alpha stroke
      1 1 0.0665     1     1    19   blue    3   NA    NA    0.5
      2 2 0.5420     1     2    19   blue    3   NA    NA    0.5
      3 3 0.0450     1     3    19   blue    3   NA    NA    0.5
      
      [[4]][[2]]
        y      x   xmin  xmax PANEL group ymin ymax colour size linetype height alpha
      1 1 0.0665 -0.778 0.911     1     1    1    1  black  0.5        1      0    NA
      2 2 0.5420 -0.280 1.360     1     2    2    2  black  0.5        1      0    NA
      3 3 0.0450  0.030 0.650     1     3    3    3  black  0.5        1      0    NA
      
      [[4]][[3]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      
      [[5]]
      [[5]][[1]]
             x y PANEL group shape colour size fill alpha stroke
      1 0.0665 1     1     1    19   blue    3   NA    NA    0.5
      2 0.5420 2     1     2    19   blue    3   NA    NA    0.5
      3 0.0450 3     1     3    19   blue    3   NA    NA    0.5
      
      [[5]][[2]]
             x   xmin  xmax y PANEL group ymin ymax colour size linetype height alpha
      1 0.0665 -0.778 0.911 1     1     1    1    1  black  0.5        1      0    NA
      2 0.5420 -0.280 1.360 2     1     2    2    2  black  0.5        1      0    NA
      3 0.0450  0.030 0.650 3     1     3    3    3  black  0.5        1      0    NA
      
      [[5]][[3]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[5]][[4]]
             x y
      1 0.0665 1
      2 0.5420 2
      3 0.0450 3
                                                                                label
      1 list(~widehat(italic(beta))=='0.07', ~italic(t)=='0.16', ~italic(p)=='0.875')
      2 list(~widehat(italic(beta))=='0.54', ~italic(t)=='1.33', ~italic(p)=='0.191')
      3 list(~widehat(italic(beta))=='0.04', ~italic(t)=='1.24', ~italic(p)=='0.001')
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
             x y PANEL group shape colour size fill alpha stroke
      1 0.0665 1     1     1    19   blue    3   NA    NA    0.5
      2 0.5420 2     1     2    19   blue    3   NA    NA    0.5
      3 0.0450 3     1     3    19   blue    3   NA    NA    0.5
      
      [[6]][[2]]
             x   xmin  xmax y PANEL group ymin ymax colour size linetype height alpha
      1 0.0665 -0.778 0.911 1     1     1    1    1  black  0.5        1      0    NA
      2 0.5420 -0.280 1.360 2     1     2    2    2  black  0.5        1      0    NA
      3 0.0450  0.030 0.650 3     1     3    3    3  black  0.5        1      0    NA
      
      [[6]][[3]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[6]][[4]]
             x y
      1 0.0665 1
      2 0.5420 2
      3 0.0450 3
                                                                                      label
      1  list(~widehat(italic(beta))=='0.07', ~italic(t)('5')=='0.16', ~italic(p)=='0.875')
      2 list(~widehat(italic(beta))=='0.54', ~italic(t)('10')=='1.33', ~italic(p)=='0.191')
      3 list(~widehat(italic(beta))=='0.04', ~italic(t)('12')=='1.24', ~italic(p)=='0.001')
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
             x y PANEL group shape colour size fill alpha stroke
      1 0.0665 1     1     1    19   blue    3   NA    NA    0.5
      2 0.5420 2     1     2    19   blue    3   NA    NA    0.5
      3 0.0450 3     1     3    19   blue    3   NA    NA    0.5
      
      [[7]][[2]]
             x   xmin  xmax y PANEL group ymin ymax colour size linetype height alpha
      1 0.0665 -0.778 0.911 1     1     1    1    1  black  0.5        1      0    NA
      2 0.5420 -0.280 1.360 2     1     2    2    2  black  0.5        1      0    NA
      3 0.0450  0.030 0.650 3     1     3    3    3  black  0.5        1      0    NA
      
      [[7]][[3]]
        xintercept PANEL group colour size linetype alpha
      1          0     1    -1  black    1   dashed    NA
      
      [[7]][[4]]
             x y
      1 0.0665 1
      2 0.5420 2
      3 0.0450 3
                                                                                      label
      1  list(~widehat(italic(beta))=='0.07', ~italic(t)('5')=='0.16', ~italic(p)=='0.875')
      2 list(~widehat(italic(beta))=='0.54', ~italic(t)('10')=='1.33', ~italic(p)=='0.191')
      3 list(~widehat(italic(beta))=='0.04', ~italic(t)('12')=='1.24', ~italic(p)=='0.001')
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
      
      

---

    Code
      list(pb1$plot$labels, pb2$plot$labels, pb3$plot$labels, pb4$plot$labels, pb5$
        plot$labels)
    Output
      [[1]]
      [[1]]$x
      [1] "estimate"
      
      [[1]]$y
      [1] "term"
      
      [[1]]$title
      NULL
      
      [[1]]$subtitle
      NULL
      
      [[1]]$caption
      NULL
      
      [[1]]$xmin
      [1] "conf.low"
      
      [[1]]$xmax
      [1] "conf.high"
      
      [[1]]$xintercept
      [1] "xintercept"
      
      [[1]]$label
      [1] "label"
      
      [[1]]$alt
      [1] ""
      
      
      [[2]]
      [[2]]$x
      [1] "estimate"
      
      [[2]]$y
      [1] "term"
      
      [[2]]$title
      NULL
      
      [[2]]$subtitle
      NULL
      
      [[2]]$caption
      NULL
      
      [[2]]$xmin
      [1] "conf.low"
      
      [[2]]$xmax
      [1] "conf.high"
      
      [[2]]$xintercept
      [1] "xintercept"
      
      [[2]]$label
      [1] "label"
      
      [[2]]$alt
      [1] ""
      
      
      [[3]]
      [[3]]$x
      [1] "estimate"
      
      [[3]]$y
      [1] "term"
      
      [[3]]$title
      NULL
      
      [[3]]$subtitle
      NULL
      
      [[3]]$caption
      NULL
      
      [[3]]$xmin
      [1] "conf.low"
      
      [[3]]$xmax
      [1] "conf.high"
      
      [[3]]$xintercept
      [1] "xintercept"
      
      [[3]]$alt
      [1] ""
      
      
      [[4]]
      [[4]]$x
      [1] "location"
      
      [[4]]$y
      NULL
      
      [[4]]$title
      NULL
      
      [[4]]$subtitle
      NULL
      
      [[4]]$caption
      NULL
      
      [[4]]$xmin
      [1] "conf.low"
      
      [[4]]$xmax
      [1] "conf.high"
      
      [[4]]$xintercept
      [1] "xintercept"
      
      [[4]]$alt
      [1] ""
      
      
      [[5]]
      [[5]]$x
      [1] "estimate"
      
      [[5]]$y
      [1] "term"
      
      [[5]]$title
      NULL
      
      [[5]]$subtitle
      NULL
      
      [[5]]$caption
      NULL
      
      [[5]]$xmin
      [1] "conf.low"
      
      [[5]]$xmax
      [1] "conf.high"
      
      [[5]]$xintercept
      [1] "xintercept"
      
      [[5]]$label
      [1] "label"
      
      [[5]]$alt
      [1] ""
      
      

