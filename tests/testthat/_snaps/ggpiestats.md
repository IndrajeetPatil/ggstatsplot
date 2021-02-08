# checking one sample proportion test

    Code
      pb$data
    Output
      [[1]]
             fill         y x PANEL group flipped_aes      ymin      ymax xmin xmax
      1 #1B9E77FF 1.0000000 1     1     1       FALSE 0.7368421 1.0000000  0.5  1.5
      2 #D95F02FF 0.7368421 1     1     2       FALSE 0.6710526 0.7368421  0.5  1.5
      3 #7570B3FF 0.6710526 1     1     3       FALSE 0.2500000 0.6710526  0.5  1.5
      4 #E7298AFF 0.2500000 1     1     4       FALSE 0.0000000 0.2500000  0.5  1.5
        colour size linetype alpha
      1  black  0.5        1    NA
      2  black  0.5        1    NA
      3  black  0.5        1    NA
      4  black  0.5        1    NA
      
      [[2]]
                y x        label group PANEL      ymax xmin xmax      ymin colour
      1 0.8684211 1 20\n(26.32%)     1     1 1.0000000    1    1 0.7368421  black
      2 0.7039474 1   5\n(6.58%)     2     1 0.7368421    1    1 0.6710526  black
      3 0.4605263 1 32\n(42.11%)     3     1 0.6710526    1    1 0.2500000  black
      4 0.1250000 1    19\n(25%)     4     1 0.2500000    1    1 0.0000000  black
         fill size angle hjust vjust alpha family fontface lineheight
      1 white 3.88     0   0.5   0.5     1               1        1.2
      2 white 3.88     0   0.5   0.5     1               1        1.2
      3 white 3.88     0   0.5   0.5     1               1        1.2
      4 white 3.88     0   0.5   0.5     1               1        1.2
      

# checking labels with contingency tab

    Code
      list(pb$data, pb1$data)
    Output
      [[1]]
      [[1]][[1]]
             fill         y x PANEL group flipped_aes      ymin      ymax xmin xmax
      1 #9A8822FF 1.0000000 1     1     1       FALSE 0.2727273 1.0000000  0.5  1.5
      2 #F5CDB4FF 0.2727273 1     1     2       FALSE 0.0000000 0.2727273  0.5  1.5
      3 #9A8822FF 1.0000000 1     2     1       FALSE 0.5714286 1.0000000  0.5  1.5
      4 #F5CDB4FF 0.5714286 1     2     2       FALSE 0.0000000 0.5714286  0.5  1.5
      5 #9A8822FF 1.0000000 1     3     1       FALSE 0.8571429 1.0000000  0.5  1.5
      6 #F5CDB4FF 0.8571429 1     3     2       FALSE 0.0000000 0.8571429  0.5  1.5
        colour size linetype alpha
      1  black  0.5        1    NA
      2  black  0.5        1    NA
      3  black  0.5        1    NA
      4  black  0.5        1    NA
      5  black  0.5        1    NA
      6  black  0.5        1    NA
      
      [[1]][[2]]
                y x label group PANEL      ymax xmin xmax      ymin colour  fill size
      1 0.6363636 1     8     1     1 1.0000000    1    1 0.2727273  black white 3.88
      2 0.1363636 1     3     2     1 0.2727273    1    1 0.0000000  black white 3.88
      3 0.7857143 1     3     1     2 1.0000000    1    1 0.5714286  black white 3.88
      4 0.2857143 1     4     2     2 0.5714286    1    1 0.0000000  black white 3.88
      5 0.9285714 1     2     1     3 1.0000000    1    1 0.8571429  black white 3.88
      6 0.4285714 1    12     2     3 0.8571429    1    1 0.0000000  black white 3.88
        angle hjust vjust alpha family fontface lineheight
      1     0   0.5   0.5     1               1        1.2
      2     0   0.5   0.5     1               1        1.2
      3     0   0.5   0.5     1               1        1.2
      4     0   0.5   0.5     1               1        1.2
      5     0   0.5   0.5     1               1        1.2
      6     0   0.5   0.5     1               1        1.2
      
      [[1]][[3]]
        y    x                                                              label
      1 1 1.65 list(~chi['gof']^2~(1)==2.27, ~italic(p)=='0.132', ~italic(n)==11)
      2 1 1.65  list(~chi['gof']^2~(1)==0.14, ~italic(p)=='0.705', ~italic(n)==7)
      3 1 1.65 list(~chi['gof']^2~(1)==7.14, ~italic(p)=='0.008', ~italic(n)==14)
        PANEL group ymax xmin xmax ymin colour size angle hjust vjust alpha family
      1     1    -1    1 1.65 1.65    0  black  2.8     0   0.5   0.5    NA       
      2     2    -1    1 1.65 1.65    0  black  2.8     0   0.5   0.5    NA       
      3     3    -1    1 1.65 1.65    0  black  2.8     0   0.5   0.5    NA       
        fontface lineheight
      1        1        1.2
      2        1        1.2
      3        1        1.2
      
      
      [[2]]
      [[2]][[1]]
             fill         y x PANEL group flipped_aes      ymin      ymax xmin xmax
      1 #1B9E77FF 1.0000000 1     1     1       FALSE 0.3684211 1.0000000  0.5  1.5
      2 #D95F02FF 0.3684211 1     1     2       FALSE 0.1578947 0.3684211  0.5  1.5
      3 #7570B3FF 0.1578947 1     1     3       FALSE 0.0000000 0.1578947  0.5  1.5
        colour size linetype alpha
      1  black  0.5        1    NA
      2  black  0.5        1    NA
      3  black  0.5        1    NA
      
      [[2]][[2]]
                 y x label group PANEL      ymax xmin xmax      ymin colour  fill
      1 0.68421053 1   63%     1     1 1.0000000    1    1 0.3684211  black white
      2 0.26315789 1   21%     2     1 0.3684211    1    1 0.1578947  black white
      3 0.07894737 1   16%     3     1 0.1578947    1    1 0.0000000  black white
        size angle hjust vjust alpha family fontface lineheight
      1 3.88     0   0.5   0.5     1               1        1.2
      2 3.88     0   0.5   0.5     1               1        1.2
      3 3.88     0   0.5   0.5     1               1        1.2
      
      

# repelling labels

    Code
      pb$data
    Output
      [[1]]
             fill         y x PANEL group flipped_aes      ymin      ymax xmin xmax
      1 #1B9E77FF 1.0000000 1     1     1       FALSE 0.8479021 1.0000000  0.5  1.5
      2 #D95F02FF 0.8479021 1     1     2       FALSE 0.8128739 0.8479021  0.5  1.5
      3 #7570B3FF 0.8128739 1     1     3       FALSE 0.6511922 0.8128739  0.5  1.5
      4 #E7298AFF 0.6511922 1     1     4       FALSE 0.0000000 0.6511922  0.5  1.5
      5 #1B9E77FF 1.0000000 1     2     1       FALSE 0.9917011 1.0000000  0.5  1.5
      6 #D95F02FF 0.9917011 1     2     2       FALSE 0.9722386 0.9917011  0.5  1.5
      7 #7570B3FF 0.9722386 1     2     3       FALSE 0.8895863 0.9722386  0.5  1.5
      8 #E7298AFF 0.8895863 1     2     4       FALSE 0.0000000 0.8895863  0.5  1.5
        colour size linetype alpha
      1  black  0.5        1    NA
      2  black  0.5        1    NA
      3  black  0.5        1    NA
      4  black  0.5        1    NA
      5  black  0.5        1    NA
      6  black  0.5        1    NA
      7  black  0.5        1    NA
      8  black  0.5        1    NA
      
      [[2]]
                y x label group PANEL      ymax xmin xmax      ymin colour  fill size
      1 0.9239510 1   15%     1     1 1.0000000    1    1 0.8479021  black white 3.88
      2 0.8303880 1    4%     2     1 0.8479021    1    1 0.8128739  black white 3.88
      3 0.7320330 1   16%     3     1 0.8128739    1    1 0.6511922  black white 3.88
      4 0.3255961 1   65%     4     1 0.6511922    1    1 0.0000000  black white 3.88
      5 0.9958505 1    1%     1     2 1.0000000    1    1 0.9917011  black white 3.88
      6 0.9819698 1    2%     2     2 0.9917011    1    1 0.9722386  black white 3.88
      7 0.9309125 1    8%     3     2 0.9722386    1    1 0.8895863  black white 3.88
      8 0.4447932 1   89%     4     2 0.8895863    1    1 0.0000000  black white 3.88
        angle alpha family fontface lineheight hjust vjust point.size
      1     0     1               1        1.2   0.5   0.5          1
      2     0     1               1        1.2   0.5   0.5          1
      3     0     1               1        1.2   0.5   0.5          1
      4     0     1               1        1.2   0.5   0.5          1
      5     0     1               1        1.2   0.5   0.5          1
      6     0     1               1        1.2   0.5   0.5          1
      7     0     1               1        1.2   0.5   0.5          1
      8     0     1               1        1.2   0.5   0.5          1
        segment.linetype segment.size segment.curvature segment.angle segment.ncp
      1                1          0.5                 0            90           1
      2                1          0.5                 0            90           1
      3                1          0.5                 0            90           1
      4                1          0.5                 0            90           1
      5                1          0.5                 0            90           1
      6                1          0.5                 0            90           1
      7                1          0.5                 0            90           1
      8                1          0.5                 0            90           1
        segment.shape segment.square segment.squareShape segment.inflect
      1           0.5           TRUE                   1           FALSE
      2           0.5           TRUE                   1           FALSE
      3           0.5           TRUE                   1           FALSE
      4           0.5           TRUE                   1           FALSE
      5           0.5           TRUE                   1           FALSE
      6           0.5           TRUE                   1           FALSE
      7           0.5           TRUE                   1           FALSE
      8           0.5           TRUE                   1           FALSE
        segment.debug
      1         FALSE
      2         FALSE
      3         FALSE
      4         FALSE
      5         FALSE
      6         FALSE
      7         FALSE
      8         FALSE
      

