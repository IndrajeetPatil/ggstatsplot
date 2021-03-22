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
      1   list(~widehat(italic(beta))==-0.78, ~italic(z)==-3.47, ~italic(p)=='0.001')
      2 list(~widehat(italic(beta))==2.29, ~italic(z)==19.13, ~italic(p)=='1.54e-81')
      3   list(~widehat(italic(beta))==-0.56, ~italic(z)==-2.44, ~italic(p)=='0.014')
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
      1  list(~widehat(italic(beta))==0.02, ~italic(chi)^2~(225)==3.40, ~italic(p)=='0.065')
      2 list(~widehat(italic(beta))==-0.51, ~italic(chi)^2~(225)==9.31, ~italic(p)=='0.002')
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
      1 list(~widehat(italic(beta))==-0.156, ~italic(t)==-5.840, ~italic(p)=='2.81e-06')
      2    list(~widehat(italic(beta))==-1.809, ~italic(t)==-2.615, ~italic(p)=='0.014')
      3                                                                             <NA>
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
      1 list(~widehat(italic(beta))==29.32, ~italic(t)==249.58, ~italic(p)=='9.84e-78')
      2    list(~widehat(italic(beta))==1.12, ~italic(t)==4.40, ~italic(p)=='5.77e-05')
      3 list(~widehat(italic(beta))==29.95, ~italic(t)==264.18, ~italic(p)=='6.08e-79')
      4    list(~widehat(italic(beta))==1.18, ~italic(t)==7.30, ~italic(p)=='2.27e-09')
      5 list(~widehat(italic(beta))==30.63, ~italic(t)==307.16, ~italic(p)=='3.78e-82')
      6     list(~widehat(italic(beta))==1.25, ~italic(t)==6.77, ~italic(p)=='1.5e-08')
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
      

