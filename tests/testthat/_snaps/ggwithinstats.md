# basic plotting works - two groups

    Code
      list(pb1$data[[1]], pb1$data[[2]], pb1$data[[4]], pb1$data[[5]], pb1$data[[6]],
      pb1$data[[7]])
    Output
      [[1]]
            colour x    y group PANEL shape size fill alpha stroke
      1  #D95F02FF 2  9.0     1     1    19    3   NA   0.5    0.5
      2  #D95F02FF 2 10.0     2     1    19    3   NA   0.5    0.5
      3  #D95F02FF 2 10.0     3     1    19    3   NA   0.5    0.5
      4  #D95F02FF 2  6.0     4     1    19    3   NA   0.5    0.5
      5  #D95F02FF 2  5.5     5     1    19    3   NA   0.5    0.5
      6  #D95F02FF 2  7.5     6     1    19    3   NA   0.5    0.5
      7  #D95F02FF 2 10.0     7     1    19    3   NA   0.5    0.5
      8  #D95F02FF 2  9.0     8     1    19    3   NA   0.5    0.5
      9  #D95F02FF 2  6.0     9     1    19    3   NA   0.5    0.5
      10 #D95F02FF 2  0.0    10     1    19    3   NA   0.5    0.5
      11 #D95F02FF 2  8.5    11     1    19    3   NA   0.5    0.5
      12 #D95F02FF 2  6.5    12     1    19    3   NA   0.5    0.5
      13 #D95F02FF 2  4.0    13     1    19    3   NA   0.5    0.5
      14 #D95F02FF 2  6.0    14     1    19    3   NA   0.5    0.5
      15 #D95F02FF 2  8.5    15     1    19    3   NA   0.5    0.5
      16 #D95F02FF 2 10.0    16     1    19    3   NA   0.5    0.5
      17 #D95F02FF 2  7.5    17     1    19    3   NA   0.5    0.5
      18 #D95F02FF 2 10.0    18     1    19    3   NA   0.5    0.5
      19 #D95F02FF 2  8.5    19     1    19    3   NA   0.5    0.5
      20 #D95F02FF 2  5.0    20     1    19    3   NA   0.5    0.5
      21 #D95F02FF 2  4.5    21     1    19    3   NA   0.5    0.5
      22 #D95F02FF 2  9.0    22     1    19    3   NA   0.5    0.5
      23 #D95F02FF 2  4.0    23     1    19    3   NA   0.5    0.5
      24 #D95F02FF 2  4.5    24     1    19    3   NA   0.5    0.5
      25 #D95F02FF 2  3.5    25     1    19    3   NA   0.5    0.5
      26 #D95F02FF 2 10.0    26     1    19    3   NA   0.5    0.5
      27 #D95F02FF 2  8.0    27     1    19    3   NA   0.5    0.5
      28 #1B9E77FF 1 10.0     1     1    19    3   NA   0.5    0.5
      29 #1B9E77FF 1 10.0     2     1    19    3   NA   0.5    0.5
      30 #1B9E77FF 1 10.0     3     1    19    3   NA   0.5    0.5
      31 #1B9E77FF 1  9.0     4     1    19    3   NA   0.5    0.5
      32 #1B9E77FF 1  8.5     5     1    19    3   NA   0.5    0.5
      33 #1B9E77FF 1  3.0     6     1    19    3   NA   0.5    0.5
      34 #1B9E77FF 1 10.0     7     1    19    3   NA   0.5    0.5
      35 #1B9E77FF 1 10.0     8     1    19    3   NA   0.5    0.5
      36 #1B9E77FF 1 10.0     9     1    19    3   NA   0.5    0.5
      37 #1B9E77FF 1  0.0    10     1    19    3   NA   0.5    0.5
      38 #1B9E77FF 1 10.0    11     1    19    3   NA   0.5    0.5
      39 #1B9E77FF 1  8.5    12     1    19    3   NA   0.5    0.5
      40 #1B9E77FF 1  8.5    13     1    19    3   NA   0.5    0.5
      41 #1B9E77FF 1  7.0    14     1    19    3   NA   0.5    0.5
      42 #1B9E77FF 1  9.0    15     1    19    3   NA   0.5    0.5
      43 #1B9E77FF 1 10.0    16     1    19    3   NA   0.5    0.5
      44 #1B9E77FF 1 10.0    17     1    19    3   NA   0.5    0.5
      45 #1B9E77FF 1 10.0    18     1    19    3   NA   0.5    0.5
      46 #1B9E77FF 1  9.5    19     1    19    3   NA   0.5    0.5
      47 #1B9E77FF 1  9.5    20     1    19    3   NA   0.5    0.5
      48 #1B9E77FF 1  7.5    21     1    19    3   NA   0.5    0.5
      49 #1B9E77FF 1 10.0    22     1    19    3   NA   0.5    0.5
      50 #1B9E77FF 1  1.5    23     1    19    3   NA   0.5    0.5
      51 #1B9E77FF 1  5.5    24     1    19    3   NA   0.5    0.5
      52 #1B9E77FF 1 10.0    25     1    19    3   NA   0.5    0.5
      53 #1B9E77FF 1 10.0    26     1    19    3   NA   0.5    0.5
      54 #1B9E77FF 1  8.0    27     1    19    3   NA   0.5    0.5
      
      [[2]]
        ymin lower middle upper ymax           outliers notchupper notchlower x
      1    7  8.25    9.5    10   10 3.0, 0.0, 1.5, 5.5  10.032124   8.967876 1
      2    0  5.25    7.5     9   10                      8.640267   6.359733 2
        flipped_aes PANEL group ymin_final ymax_final xmin xmax xid newx new_width
      1       FALSE     1     1          0         10  0.9  1.1   1    1       0.2
      2       FALSE     1     2          0         10  1.9  2.1   2    2       0.2
        weight colour  fill size alpha shape linetype
      1      1 grey20 white  0.5   0.5    19    solid
      2      1 grey20 white  0.5   0.5    19    solid
      
      [[3]]
         x    y group PANEL colour size linetype alpha
      1  2  9.0     1     1    red  0.5        1    NA
      2  2 10.0     2     1    red  0.5        1    NA
      3  2 10.0     3     1    red  0.5        1    NA
      4  2  6.0     4     1    red  0.5        1    NA
      5  2  5.5     5     1    red  0.5        1    NA
      6  2  7.5     6     1    red  0.5        1    NA
      7  2 10.0     7     1    red  0.5        1    NA
      8  2  9.0     8     1    red  0.5        1    NA
      9  2  6.0     9     1    red  0.5        1    NA
      10 2  0.0    10     1    red  0.5        1    NA
      11 2  8.5    11     1    red  0.5        1    NA
      12 2  6.5    12     1    red  0.5        1    NA
      13 2  4.0    13     1    red  0.5        1    NA
      14 2  6.0    14     1    red  0.5        1    NA
      15 2  8.5    15     1    red  0.5        1    NA
      16 2 10.0    16     1    red  0.5        1    NA
      17 2  7.5    17     1    red  0.5        1    NA
      18 2 10.0    18     1    red  0.5        1    NA
      19 2  8.5    19     1    red  0.5        1    NA
      20 2  5.0    20     1    red  0.5        1    NA
      21 2  4.5    21     1    red  0.5        1    NA
      22 2  9.0    22     1    red  0.5        1    NA
      23 2  4.0    23     1    red  0.5        1    NA
      24 2  4.5    24     1    red  0.5        1    NA
      25 2  3.5    25     1    red  0.5        1    NA
      26 2 10.0    26     1    red  0.5        1    NA
      27 2  8.0    27     1    red  0.5        1    NA
      28 1 10.0     1     1    red  0.5        1    NA
      29 1 10.0     2     1    red  0.5        1    NA
      30 1 10.0     3     1    red  0.5        1    NA
      31 1  9.0     4     1    red  0.5        1    NA
      32 1  8.5     5     1    red  0.5        1    NA
      33 1  3.0     6     1    red  0.5        1    NA
      34 1 10.0     7     1    red  0.5        1    NA
      35 1 10.0     8     1    red  0.5        1    NA
      36 1 10.0     9     1    red  0.5        1    NA
      37 1  0.0    10     1    red  0.5        1    NA
      38 1 10.0    11     1    red  0.5        1    NA
      39 1  8.5    12     1    red  0.5        1    NA
      40 1  8.5    13     1    red  0.5        1    NA
      41 1  7.0    14     1    red  0.5        1    NA
      42 1  9.0    15     1    red  0.5        1    NA
      43 1 10.0    16     1    red  0.5        1    NA
      44 1 10.0    17     1    red  0.5        1    NA
      45 1 10.0    18     1    red  0.5        1    NA
      46 1  9.5    19     1    red  0.5        1    NA
      47 1  9.5    20     1    red  0.5        1    NA
      48 1  7.5    21     1    red  0.5        1    NA
      49 1 10.0    22     1    red  0.5        1    NA
      50 1  1.5    23     1    red  0.5        1    NA
      51 1  5.5    24     1    red  0.5        1    NA
      52 1 10.0    25     1    red  0.5        1    NA
      53 1 10.0    26     1    red  0.5        1    NA
      54 1  8.0    27     1    red  0.5        1    NA
      
      [[4]]
        x   y         label PANEL group colour  fill size angle alpha family fontface
      1 1 3.0        Europe     1     1  black white    3     0    NA               1
      2 1 0.0 North America     1     1  black white    3     0    NA               1
      3 1 1.5        Europe     1     1  black white    3     0    NA               1
      4 1 5.5        Europe     1     1  black white    3     0    NA               1
        lineheight hjust vjust point.size segment.linetype segment.size
      1        1.2   0.5   0.5          1                1          0.5
      2        1.2   0.5   0.5          1                1          0.5
      3        1.2   0.5   0.5          1                1          0.5
      4        1.2   0.5   0.5          1                1          0.5
        segment.curvature segment.angle segment.ncp segment.shape segment.square
      1                 0            90           1           0.5           TRUE
      2                 0            90           1           0.5           TRUE
      3                 0            90           1           0.5           TRUE
      4                 0            90           1           0.5           TRUE
        segment.squareShape segment.inflect segment.debug
      1                   1           FALSE         FALSE
      2                   1           FALSE         FALSE
      3                   1           FALSE         FALSE
      4                   1           FALSE         FALSE
      
      [[5]]
        x        y group PANEL colour size linetype alpha
      1 1 8.333333     1     1   blue    2        1   0.8
      2 2 7.074074     1     1   blue    2        1   0.8
      
      [[6]]
        x        y PANEL group shape    colour size fill alpha stroke
      1 1 8.333333     1     1    19 darkgreen    3   NA   0.5    0.5
      2 2 7.074074     1     2    19 darkgreen    3   NA   0.5    0.5
      

---

    Code
      within(pb1$plot$labels, rm(subtitle, caption))
    Output
      $x
      [1] "condition"
      
      $y
      [1] "desire"
      
      $colour
      [1] "condition"
      
      $title
      [1] "bugs dataset"
      
      $group
      [1] ".rowid"
      
      $label
      [1] "outlier.label"
      
      $alt
      [1] ""
      

# basic plotting works - more than two groups

    Code
      list(pb1$data[[1]], pb1$data[[2]], pb1$data[[4]], pb1$data[[5]], pb1$data[[6]],
      pb1$data[[7]])
    Output
      [[1]]
            colour x    y group PANEL shape size fill alpha stroke
      1  #1B9E77FF 1 5.40     1     1    19    3   NA   0.5    0.5
      2  #D95F02FF 2 5.50     1     1    19    3   NA   0.5    0.5
      3  #7570B3FF 3 5.55     1     1    19    3   NA   0.5    0.5
      4  #1B9E77FF 1 5.85     2     1    19    3   NA   0.5    0.5
      5  #D95F02FF 2 5.70     2     1    19    3   NA   0.5    0.5
      6  #7570B3FF 3 5.75     2     1    19    3   NA   0.5    0.5
      7  #1B9E77FF 1 5.20     3     1    19    3   NA   0.5    0.5
      8  #D95F02FF 2 5.60     3     1    19    3   NA   0.5    0.5
      9  #7570B3FF 3 5.50     3     1    19    3   NA   0.5    0.5
      10 #1B9E77FF 1 5.55     4     1    19    3   NA   0.5    0.5
      11 #D95F02FF 2 5.50     4     1    19    3   NA   0.5    0.5
      12 #7570B3FF 3 5.40     4     1    19    3   NA   0.5    0.5
      13 #1B9E77FF 1 5.90     5     1    19    3   NA   0.5    0.5
      14 #D95F02FF 2 5.85     5     1    19    3   NA   0.5    0.5
      15 #7570B3FF 3 5.70     5     1    19    3   NA   0.5    0.5
      16 #1B9E77FF 1 5.45     6     1    19    3   NA   0.5    0.5
      17 #D95F02FF 2 5.55     6     1    19    3   NA   0.5    0.5
      18 #7570B3FF 3 5.60     6     1    19    3   NA   0.5    0.5
      19 #1B9E77FF 1 5.40     7     1    19    3   NA   0.5    0.5
      20 #D95F02FF 2 5.40     7     1    19    3   NA   0.5    0.5
      21 #7570B3FF 3 5.35     7     1    19    3   NA   0.5    0.5
      22 #1B9E77FF 1 5.45     8     1    19    3   NA   0.5    0.5
      23 #D95F02FF 2 5.50     8     1    19    3   NA   0.5    0.5
      24 #7570B3FF 3 5.35     8     1    19    3   NA   0.5    0.5
      25 #1B9E77FF 1 5.25     9     1    19    3   NA   0.5    0.5
      26 #D95F02FF 2 5.15     9     1    19    3   NA   0.5    0.5
      27 #7570B3FF 3 5.00     9     1    19    3   NA   0.5    0.5
      28 #1B9E77FF 1 5.85    10     1    19    3   NA   0.5    0.5
      29 #D95F02FF 2 5.80    10     1    19    3   NA   0.5    0.5
      30 #7570B3FF 3 5.70    10     1    19    3   NA   0.5    0.5
      31 #1B9E77FF 1 5.25    11     1    19    3   NA   0.5    0.5
      32 #D95F02FF 2 5.20    11     1    19    3   NA   0.5    0.5
      33 #7570B3FF 3 5.10    11     1    19    3   NA   0.5    0.5
      34 #1B9E77FF 1 5.65    12     1    19    3   NA   0.5    0.5
      35 #D95F02FF 2 5.55    12     1    19    3   NA   0.5    0.5
      36 #7570B3FF 3 5.45    12     1    19    3   NA   0.5    0.5
      37 #1B9E77FF 1 5.60    13     1    19    3   NA   0.5    0.5
      38 #D95F02FF 2 5.35    13     1    19    3   NA   0.5    0.5
      39 #7570B3FF 3 5.45    13     1    19    3   NA   0.5    0.5
      40 #1B9E77FF 1 5.05    14     1    19    3   NA   0.5    0.5
      41 #D95F02FF 2 5.00    14     1    19    3   NA   0.5    0.5
      42 #7570B3FF 3 4.95    14     1    19    3   NA   0.5    0.5
      43 #1B9E77FF 1 5.50    15     1    19    3   NA   0.5    0.5
      44 #D95F02FF 2 5.50    15     1    19    3   NA   0.5    0.5
      45 #7570B3FF 3 5.40    15     1    19    3   NA   0.5    0.5
      46 #1B9E77FF 1 5.45    16     1    19    3   NA   0.5    0.5
      47 #D95F02FF 2 5.55    16     1    19    3   NA   0.5    0.5
      48 #7570B3FF 3 5.50    16     1    19    3   NA   0.5    0.5
      49 #1B9E77FF 1 5.55    17     1    19    3   NA   0.5    0.5
      50 #D95F02FF 2 5.55    17     1    19    3   NA   0.5    0.5
      51 #7570B3FF 3 5.35    17     1    19    3   NA   0.5    0.5
      52 #1B9E77FF 1 5.45    18     1    19    3   NA   0.5    0.5
      53 #D95F02FF 2 5.50    18     1    19    3   NA   0.5    0.5
      54 #7570B3FF 3 5.55    18     1    19    3   NA   0.5    0.5
      55 #1B9E77FF 1 5.50    19     1    19    3   NA   0.5    0.5
      56 #D95F02FF 2 5.45    19     1    19    3   NA   0.5    0.5
      57 #7570B3FF 3 5.25    19     1    19    3   NA   0.5    0.5
      58 #1B9E77FF 1 5.65    20     1    19    3   NA   0.5    0.5
      59 #D95F02FF 2 5.60    20     1    19    3   NA   0.5    0.5
      60 #7570B3FF 3 5.40    20     1    19    3   NA   0.5    0.5
      61 #1B9E77FF 1 5.70    21     1    19    3   NA   0.5    0.5
      62 #D95F02FF 2 5.65    21     1    19    3   NA   0.5    0.5
      63 #7570B3FF 3 5.55    21     1    19    3   NA   0.5    0.5
      64 #1B9E77FF 1 6.30    22     1    19    3   NA   0.5    0.5
      65 #D95F02FF 2 6.30    22     1    19    3   NA   0.5    0.5
      66 #7570B3FF 3 6.25    22     1    19    3   NA   0.5    0.5
      
      [[2]]
        ymin  lower middle upper ymax                     outliers notchupper
      1 5.20 5.4125  5.500  5.65 5.90                   5.05, 6.30   5.580004
      2 5.35 5.4625  5.525  5.60 5.80 5.85, 5.15, 5.20, 5.00, 6.30   5.571318
      3 5.10 5.3500  5.450  5.55 5.75             5.00, 4.95, 6.25   5.517371
        notchlower x flipped_aes PANEL group ymin_final ymax_final xmin xmax xid newx
      1   5.419996 1       FALSE     1     1       5.05       6.30  0.9  1.1   1    1
      2   5.478682 2       FALSE     1     2       5.00       6.30  1.9  2.1   2    2
      3   5.382629 3       FALSE     1     3       4.95       6.25  2.9  3.1   3    3
        new_width weight colour  fill size alpha shape linetype
      1       0.2      1 grey20 white  0.5   0.5    19    solid
      2       0.2      1 grey20 white  0.5   0.5    19    solid
      3       0.2      1 grey20 white  0.5   0.5    19    solid
      
      [[3]]
        x    y label PANEL group colour  fill size angle alpha family fontface
      1 2 5.00  5.00     1     2  black white    3     0    NA               1
      2 1 6.30  6.30     1     1  black white    3     0    NA               1
      3 2 6.30  6.30     1     2  black white    3     0    NA               1
      4 3 6.25  6.25     1     3  black white    3     0    NA               1
        lineheight hjust vjust point.size segment.linetype segment.size
      1        1.2   0.5   0.5          1                1          0.5
      2        1.2   0.5   0.5          1                1          0.5
      3        1.2   0.5   0.5          1                1          0.5
      4        1.2   0.5   0.5          1                1          0.5
        segment.curvature segment.angle segment.ncp segment.shape segment.square
      1                 0            90           1           0.5           TRUE
      2                 0            90           1           0.5           TRUE
      3                 0            90           1           0.5           TRUE
      4                 0            90           1           0.5           TRUE
        segment.squareShape segment.inflect segment.debug
      1                   1           FALSE         FALSE
      2                   1           FALSE         FALSE
      3                   1           FALSE         FALSE
      4                   1           FALSE         FALSE
      
      [[4]]
        x        y group PANEL colour size linetype alpha
      1 1 5.543182     1     1    red    1        1   0.5
      2 2 5.534091     1     1    red    1        1   0.5
      3 3 5.459091     1     1    red    1        1   0.5
      
      [[5]]
        x        y PANEL group shape  colour size fill alpha stroke
      1 1 5.543182     1     1    19 darkred    5   NA    NA    0.5
      2 2 5.534091     1     2    19 darkred    5   NA    NA    0.5
      3 3 5.459091     1     3    19 darkred    5   NA    NA    0.5
      
      [[6]]
        x        y                              label PANEL group nudge_x  nudge_y
      1 1 5.543182 list(~widehat(mu)[mean]=='5.5432')     1     1     1.4 5.543182
      2 2 5.534091 list(~widehat(mu)[mean]=='5.5341')     1     2     2.4 5.534091
      3 3 5.459091 list(~widehat(mu)[mean]=='5.4591')     1     3     3.4 5.459091
        colour  fill size angle alpha family fontface lineheight hjust vjust
      1  black white    3     0    NA               1        1.2   0.5   0.5
      2  black white    3     0    NA               1        1.2   0.5   0.5
      3  black white    3     0    NA               1        1.2   0.5   0.5
        point.size segment.linetype segment.size segment.curvature segment.angle
      1          1                4          0.5                 0            90
      2          1                4          0.5                 0            90
      3          1                4          0.5                 0            90
        segment.ncp segment.shape segment.square segment.squareShape segment.inflect
      1           1           0.5           TRUE                   1           FALSE
      2           1           0.5           TRUE                   1           FALSE
      3           1           0.5           TRUE                   1           FALSE
        segment.debug
      1         FALSE
      2         FALSE
      3         FALSE
      

---

    Code
      within(pb1$plot$labels, rm(subtitle, caption))
    Output
      $x
      [1] "Wine"
      
      $y
      [1] "Taste"
      
      $colour
      [1] "Wine"
      
      $title
      [1] "wine tasting data"
      
      $group
      [1] ".rowid"
      
      $label
      [1] "outlier.label"
      
      $alt
      [1] ""
      

# checking subtitle outputs - without NAs

    Code
      within(pb1$plot$labels, rm(subtitle))
    Output
      $x
      [1] "condition"
      
      $y
      [1] "value"
      
      $colour
      [1] "condition"
      
      $title
      NULL
      
      $caption
      atop(displaystyle(NULL), expr = paste("Pairwise test: ", bold("Durbin-Conover test"), 
          "; Comparisons shown: ", bold("only significant")))
      
      $group
      [1] ".rowid"
      
      $label
      [1] "label"
      
      $alt
      [1] ""
      

---

    Code
      within(pb2$plot$labels, rm(subtitle))
    Output
      $x
      [1] "condition"
      
      $y
      [1] "value"
      
      $colour
      [1] "condition"
      
      $title
      NULL
      
      $caption
      atop(displaystyle(NULL), expr = paste("Pairwise test: ", bold("Yuen's trimmed means test"), 
          "; Comparisons shown: ", bold("only non-significant")))
      
      $group
      [1] ".rowid"
      
      $label
      [1] "label"
      
      $alt
      [1] ""
      

---

    Code
      within(pb3$plot$labels, rm(subtitle))
    Output
      $x
      [1] "modality"
      
      $y
      [1] "score"
      
      $colour
      [1] "modality"
      
      $title
      NULL
      
      $caption
      NULL
      
      $group
      [1] ".rowid"
      
      $label
      [1] "label"
      
      $alt
      [1] ""
      

---

    Code
      within(pb4$plot$labels, rm(subtitle))
    Output
      $x
      [1] "modality"
      
      $y
      [1] "score"
      
      $colour
      [1] "modality"
      
      $title
      NULL
      
      $caption
      NULL
      
      $group
      [1] ".rowid"
      
      $label
      [1] "label"
      
      $alt
      [1] ""
      

# ggplot component addition works

    Code
      pb$plot$labels
    Output
      $y
      [1] "Taste rating"
      
      $x
      [1] "Wine"
      
      $colour
      [1] "Wine"
      
      $title
      NULL
      
      $subtitle
      NULL
      
      $caption
      atop(displaystyle(NULL), expr = paste("Pairwise test: ", bold("Student's t-test"), 
          "; Comparisons shown: ", bold("only significant")))
      
      $group
      [1] ".rowid"
      
      $label
      [1] "label"
      
      $alt
      [1] ""
      

