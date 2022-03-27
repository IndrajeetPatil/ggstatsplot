# checking ggcorrmat - without NAs - pearson's r

    Code
      pb$data
    Output
      [[1]]
            fill x y PANEL group xmin xmax ymin ymax colour size linetype alpha width
      1  #7570B3 1 1     1     1  0.5  1.5  0.5  1.5   gray  0.1        1    NA    NA
      2  #CC6A19 2 1     1     5  1.5  2.5  0.5  1.5   gray  0.1        1    NA    NA
      3  #8B6E9F 3 1     1     9  2.5  3.5  0.5  1.5   gray  0.1        1    NA    NA
      4  #936D97 4 1     1    13  3.5  4.5  0.5  1.5   gray  0.1        1    NA    NA
      5  #CC6A19 1 2     1     2  0.5  1.5  1.5  2.5   gray  0.1        1    NA    NA
      6  #7570B3 2 2     1     6  1.5  2.5  1.5  2.5   gray  0.1        1    NA    NA
      7  #A7813E 3 2     1    10  2.5  3.5  1.5  2.5   gray  0.1        1    NA    NA
      8  #AF7D37 4 2     1    14  3.5  4.5  1.5  2.5   gray  0.1        1    NA    NA
      9  #8B6E9F 1 3     1     3  0.5  1.5  2.5  3.5   gray  0.1        1    NA    NA
      10 #A7813E 2 3     1     7  1.5  2.5  2.5  3.5   gray  0.1        1    NA    NA
      11 #7570B3 3 3     1    11  2.5  3.5  2.5  3.5   gray  0.1        1    NA    NA
      12 #7C6FAD 4 3     1    15  3.5  4.5  2.5  3.5   gray  0.1        1    NA    NA
      13 #936D97 1 4     1     4  0.5  1.5  3.5  4.5   gray  0.1        1    NA    NA
      14 #AF7D37 2 4     1     8  1.5  2.5  3.5  4.5   gray  0.1        1    NA    NA
      15 #7C6FAD 3 4     1    12  2.5  3.5  3.5  4.5   gray  0.1        1    NA    NA
      16 #7570B3 4 4     1    16  3.5  4.5  3.5  4.5   gray  0.1        1    NA    NA
         height
      1      NA
      2      NA
      3      NA
      4      NA
      5      NA
      6      NA
      7      NA
      8      NA
      9      NA
      10     NA
      11     NA
      12     NA
      13     NA
      14     NA
      15     NA
      16     NA
      
      [[2]]
            fill x y PANEL group colour size angle hjust vjust alpha family fontface
      1  #7570B3 1 1     1     1  white    4     0   0.5   0.5    NA               1
      2  #CC6A19 2 1     1     5  white    4     0   0.5   0.5    NA               1
      3  #8B6E9F 3 1     1     9  white    4     0   0.5   0.5    NA               1
      4  #936D97 4 1     1    13  white    4     0   0.5   0.5    NA               1
      5  #CC6A19 1 2     1     2  white    4     0   0.5   0.5    NA               1
      6  #7570B3 2 2     1     6  white    4     0   0.5   0.5    NA               1
      7  #A7813E 3 2     1    10  white    4     0   0.5   0.5    NA               1
      8  #AF7D37 4 2     1    14  white    4     0   0.5   0.5    NA               1
      9  #8B6E9F 1 3     1     3  white    4     0   0.5   0.5    NA               1
      10 #A7813E 2 3     1     7  white    4     0   0.5   0.5    NA               1
      11 #7570B3 3 3     1    11  white    4     0   0.5   0.5    NA               1
      12 #7C6FAD 4 3     1    15  white    4     0   0.5   0.5    NA               1
      13 #936D97 1 4     1     4  white    4     0   0.5   0.5    NA               1
      14 #AF7D37 2 4     1     8  white    4     0   0.5   0.5    NA               1
      15 #7C6FAD 3 4     1    12  white    4     0   0.5   0.5    NA               1
      16 #7570B3 4 4     1    16  white    4     0   0.5   0.5    NA               1
         lineheight   label
      1         1.2  1.0000
      2         1.2 -0.1176
      3         1.2  0.8718
      4         1.2  0.8179
      5         1.2 -0.1176
      6         1.2  1.0000
      7         1.2 -0.4284
      8         1.2 -0.3661
      9         1.2  0.8718
      10        1.2 -0.4284
      11        1.2  1.0000
      12        1.2  0.9629
      13        1.2  0.8179
      14        1.2 -0.3661
      15        1.2  0.9629
      16        1.2  1.0000
      
      [[3]]
           fill x y PANEL group shape colour size alpha stroke
      1 #D0622D 2 1     1     2 cross  white    5    NA    0.5
      2 #D0622D 1 2     1     1 cross  white    5    NA    0.5
      

---

    Code
      list(p$labels, pb$plot$plot_env$legend.title)
    Output
      [[1]]
      [[1]]$xlab
      NULL
      
      [[1]]$ylab
      NULL
      
      [[1]]$title
      [1] "Iris dataset"
      
      [[1]]$subtitle
      [1] "By Edgar Anderson"
      
      [[1]]$caption
      atop(displaystyle(NULL), expr = paste(bold("X"), " = non-significant at ", 
          italic("p"), " < ", 0.001, " (Adjustment: ", "FDR", ")"))
      
      [[1]]$fill
      [1] "value"
      
      [[1]]$x
      [1] "Var1"
      
      [[1]]$y
      [1] "Var2"
      
      
      [[2]]
      atop(atop(scriptstyle(bold("sample sizes:")), italic(n) ~ "=" ~ 
          "150"), atop(scriptstyle(bold("correlation:")), "Pearson"))
      

# checking ggcorrmat - without NAs - robust r

    Code
      pb$data
    Output
      [[1]]
            fill x y PANEL group xmin xmax ymin ymax colour size linetype alpha width
      1  #009E73 1 1     1     1  0.5  1.5  0.5  1.5  black  0.1        1    NA    NA
      2  #009E73 1 2     1     2  0.5  1.5  1.5  2.5  black  0.1        1    NA    NA
      3  #009E73 2 2     1     8  1.5  2.5  1.5  2.5  black  0.1        1    NA    NA
      4  #BBDFCF 1 3     1     3  0.5  1.5  2.5  3.5  black  0.1        1    NA    NA
      5  #BBDFCF 2 3     1     9  1.5  2.5  2.5  3.5  black  0.1        1    NA    NA
      6  #BBDFCF 3 3     1    14  2.5  3.5  2.5  3.5  black  0.1        1    NA    NA
      7  #E6F4ED 1 4     1     4  0.5  1.5  3.5  4.5  black  0.1        1    NA    NA
      8  #E6F4ED 2 4     1    10  1.5  2.5  3.5  4.5  black  0.1        1    NA    NA
      9  #E6F4ED 3 4     1    15  2.5  3.5  3.5  4.5  black  0.1        1    NA    NA
      10 #DEF0E7 4 4     1    19  3.5  4.5  3.5  4.5  black  0.1        1    NA    NA
      11 #F1F8F5 1 5     1     5  0.5  1.5  4.5  5.5  black  0.1        1    NA    NA
      12 #F1F8F5 2 5     1    11  1.5  2.5  4.5  5.5  black  0.1        1    NA    NA
      13 #F1F8F5 3 5     1    16  2.5  3.5  4.5  5.5  black  0.1        1    NA    NA
      14 #FED6A3 4 5     1    20  3.5  4.5  4.5  5.5  black  0.1        1    NA    NA
      15 #FFFDFA 5 5     1    23  4.5  5.5  4.5  5.5  black  0.1        1    NA    NA
      16 #FFEBD2 1 6     1     6  0.5  1.5  5.5  6.5  black  0.1        1    NA    NA
      17 #FFEBD2 2 6     1    12  1.5  2.5  5.5  6.5  black  0.1        1    NA    NA
      18 #FFEBD2 3 6     1    17  2.5  3.5  5.5  6.5  black  0.1        1    NA    NA
      19 #F7FBF9 4 6     1    21  3.5  4.5  5.5  6.5  black  0.1        1    NA    NA
      20 #FAC882 5 6     1    24  4.5  5.5  5.5  6.5  black  0.1        1    NA    NA
      21 #FCCF91 6 6     1    26  5.5  6.5  5.5  6.5  black  0.1        1    NA    NA
      22 #FED5A1 1 7     1     7  0.5  1.5  6.5  7.5  black  0.1        1    NA    NA
      23 #FED5A1 2 7     1    13  1.5  2.5  6.5  7.5  black  0.1        1    NA    NA
      24 #FED5A1 3 7     1    18  2.5  3.5  6.5  7.5  black  0.1        1    NA    NA
      25 #60B794 4 7     1    22  3.5  4.5  6.5  7.5  black  0.1        1    NA    NA
      26 #FFDBAE 5 7     1    25  4.5  5.5  6.5  7.5  black  0.1        1    NA    NA
      27 #EDF6F2 6 7     1    27  5.5  6.5  6.5  7.5  black  0.1        1    NA    NA
      28 #FFE7C9 7 7     1    28  6.5  7.5  6.5  7.5  black  0.1        1    NA    NA
         height
      1      NA
      2      NA
      3      NA
      4      NA
      5      NA
      6      NA
      7      NA
      8      NA
      9      NA
      10     NA
      11     NA
      12     NA
      13     NA
      14     NA
      15     NA
      16     NA
      17     NA
      18     NA
      19     NA
      20     NA
      21     NA
      22     NA
      23     NA
      24     NA
      25     NA
      26     NA
      27     NA
      28     NA
      
      [[2]]
            fill x y PANEL group colour size angle hjust vjust alpha family fontface
      1  #009E73 1 1     1     1  black    4     0   0.5   0.5    NA               1
      2  #009E73 1 2     1     2  black    4     0   0.5   0.5    NA               1
      3  #009E73 2 2     1     8  black    4     0   0.5   0.5    NA               1
      4  #BBDFCF 1 3     1     3  black    4     0   0.5   0.5    NA               1
      5  #BBDFCF 2 3     1     9  black    4     0   0.5   0.5    NA               1
      6  #BBDFCF 3 3     1    14  black    4     0   0.5   0.5    NA               1
      7  #E6F4ED 1 4     1     4  black    4     0   0.5   0.5    NA               1
      8  #E6F4ED 2 4     1    10  black    4     0   0.5   0.5    NA               1
      9  #E6F4ED 3 4     1    15  black    4     0   0.5   0.5    NA               1
      10 #DEF0E7 4 4     1    19  black    4     0   0.5   0.5    NA               1
      11 #F1F8F5 1 5     1     5  black    4     0   0.5   0.5    NA               1
      12 #F1F8F5 2 5     1    11  black    4     0   0.5   0.5    NA               1
      13 #F1F8F5 3 5     1    16  black    4     0   0.5   0.5    NA               1
      14 #FED6A3 4 5     1    20  black    4     0   0.5   0.5    NA               1
      15 #FFFDFA 5 5     1    23  black    4     0   0.5   0.5    NA               1
      16 #FFEBD2 1 6     1     6  black    4     0   0.5   0.5    NA               1
      17 #FFEBD2 2 6     1    12  black    4     0   0.5   0.5    NA               1
      18 #FFEBD2 3 6     1    17  black    4     0   0.5   0.5    NA               1
      19 #F7FBF9 4 6     1    21  black    4     0   0.5   0.5    NA               1
      20 #FAC882 5 6     1    24  black    4     0   0.5   0.5    NA               1
      21 #FCCF91 6 6     1    26  black    4     0   0.5   0.5    NA               1
      22 #FED5A1 1 7     1     7  black    4     0   0.5   0.5    NA               1
      23 #FED5A1 2 7     1    13  black    4     0   0.5   0.5    NA               1
      24 #FED5A1 3 7     1    18  black    4     0   0.5   0.5    NA               1
      25 #60B794 4 7     1    22  black    4     0   0.5   0.5    NA               1
      26 #FFDBAE 5 7     1    25  black    4     0   0.5   0.5    NA               1
      27 #EDF6F2 6 7     1    27  black    4     0   0.5   0.5    NA               1
      28 #FFE7C9 7 7     1    28  black    4     0   0.5   0.5    NA               1
         lineheight label
      1         1.2  1.00
      2         1.2  1.00
      3         1.2  1.00
      4         1.2  0.33
      5         1.2  0.33
      6         1.2  0.33
      7         1.2  0.12
      8         1.2  0.12
      9         1.2  0.12
      10        1.2  0.16
      11        1.2  0.07
      12        1.2  0.07
      13        1.2  0.07
      14        1.2 -0.41
      15        1.2 -0.02
      16        1.2 -0.20
      17        1.2 -0.20
      18        1.2 -0.20
      19        1.2  0.04
      20        1.2 -0.56
      21        1.2 -0.49
      22        1.2 -0.42
      23        1.2 -0.42
      24        1.2 -0.42
      25        1.2  0.75
      26        1.2 -0.36
      27        1.2  0.09
      28        1.2 -0.24
      
      [[3]]
            fill x y PANEL group shape colour size alpha stroke
      1  #009E73 1 3     1     1 cross  black   14    NA    0.5
      2  #009E73 2 3     1     6 cross  black   14    NA    0.5
      3  #009E73 3 3     1    11 cross  black   14    NA    0.5
      4  #009E73 1 4     1     2 cross  black   14    NA    0.5
      5  #009E73 2 4     1     7 cross  black   14    NA    0.5
      6  #009E73 3 4     1    12 cross  black   14    NA    0.5
      7  #009E73 4 4     1    16 cross  black   14    NA    0.5
      8  #009E73 1 5     1     3 cross  black   14    NA    0.5
      9  #009E73 2 5     1     8 cross  black   14    NA    0.5
      10 #009E73 3 5     1    13 cross  black   14    NA    0.5
      11 #009E73 4 5     1    17 cross  black   14    NA    0.5
      12 #009E73 5 5     1    20 cross  black   14    NA    0.5
      13 #009E73 1 6     1     4 cross  black   14    NA    0.5
      14 #009E73 2 6     1     9 cross  black   14    NA    0.5
      15 #009E73 3 6     1    14 cross  black   14    NA    0.5
      16 #009E73 4 6     1    18 cross  black   14    NA    0.5
      17 #009E73 5 6     1    21 cross  black   14    NA    0.5
      18 #009E73 6 6     1    23 cross  black   14    NA    0.5
      19 #009E73 1 7     1     5 cross  black   14    NA    0.5
      20 #009E73 2 7     1    10 cross  black   14    NA    0.5
      21 #009E73 3 7     1    15 cross  black   14    NA    0.5
      22 #D6ECE2 4 7     1    19 cross  black   14    NA    0.5
      23 #009E73 5 7     1    22 cross  black   14    NA    0.5
      24 #009E73 6 7     1    24 cross  black   14    NA    0.5
      25 #009E73 7 7     1    25 cross  black   14    NA    0.5
      

---

    Code
      list(p$labels, pb$plot$plot_env$legend.title)
    Output
      [[1]]
      [[1]]$xlab
      NULL
      
      [[1]]$ylab
      NULL
      
      [[1]]$title
      NULL
      
      [[1]]$subtitle
      NULL
      
      [[1]]$caption
      atop(displaystyle(NULL), expr = paste(bold("X"), " = non-significant at ", 
          italic("p"), " < ", 0.05, " (Adjustment: ", "Holm", ")"))
      
      [[1]]$fill
      [1] "value"
      
      [[1]]$x
      [1] "Var1"
      
      [[1]]$y
      [1] "Var2"
      
      
      [[2]]
      atop(atop(scriptstyle(bold("sample sizes:")), italic(n) ~ "=" ~ 
          "11"), atop(scriptstyle(bold("correlation (partial):")), 
          "Winsorized Pearson"))
      

# checking ggcorrmat - with NAs - robust r - partial

    Code
      pb$data
    Output
      [[1]]
            fill x y PANEL group xmin xmax ymin ymax colour size linetype alpha width
      1  #B9DECD 1 1     1     1  0.5  1.5  0.5  1.5  black  0.1        1    NA    NA
      2  #FFFDFA 1 2     1     2  0.5  1.5  1.5  2.5  black  0.1        1    NA    NA
      3  #FFF7ED 2 2     1     6  1.5  2.5  1.5  2.5  black  0.1        1    NA    NA
      4  #E69F00 1 3     1     3  0.5  1.5  2.5  3.5  black  0.1        1    NA    NA
      5  #CAE6D9 2 3     1     7  1.5  2.5  2.5  3.5  black  0.1        1    NA    NA
      6  #EFF7F3 3 3     1    10  2.5  3.5  2.5  3.5  black  0.1        1    NA    NA
      7  #D0E9DD 1 4     1     4  0.5  1.5  3.5  4.5  black  0.1        1    NA    NA
      8  #F3F9F6 2 4     1     8  1.5  2.5  3.5  4.5  black  0.1        1    NA    NA
      9  #77C0A2 3 4     1    11  2.5  3.5  3.5  4.5  black  0.1        1    NA    NA
      10 #C6E4D6 4 4     1    13  3.5  4.5  3.5  4.5  black  0.1        1    NA    NA
      11 #E2F2EA 1 5     1     5  0.5  1.5  4.5  5.5  black  0.1        1    NA    NA
      12 #EAF5F0 2 5     1     9  1.5  2.5  4.5  5.5  black  0.1        1    NA    NA
      13 #FFFDFA 3 5     1    12  2.5  3.5  4.5  5.5  black  0.1        1    NA    NA
      14 #FFF5E8 4 5     1    14  3.5  4.5  4.5  5.5  black  0.1        1    NA    NA
      15 #B1DBC8 5 5     1    15  4.5  5.5  4.5  5.5  black  0.1        1    NA    NA
         height
      1      NA
      2      NA
      3      NA
      4      NA
      5      NA
      6      NA
      7      NA
      8      NA
      9      NA
      10     NA
      11     NA
      12     NA
      13     NA
      14     NA
      15     NA
      
      [[2]]
            fill x y PANEL group colour size angle hjust vjust alpha family fontface
      1  #B9DECD 1 1     1     1  black    4     0   0.5   0.5    NA               1
      2  #FFFDFA 1 2     1     2  black    4     0   0.5   0.5    NA               1
      3  #FFF7ED 2 2     1     6  black    4     0   0.5   0.5    NA               1
      4  #E69F00 1 3     1     3  black    4     0   0.5   0.5    NA               1
      5  #CAE6D9 2 3     1     7  black    4     0   0.5   0.5    NA               1
      6  #EFF7F3 3 3     1    10  black    4     0   0.5   0.5    NA               1
      7  #D0E9DD 1 4     1     4  black    4     0   0.5   0.5    NA               1
      8  #F3F9F6 2 4     1     8  black    4     0   0.5   0.5    NA               1
      9  #77C0A2 3 4     1    11  black    4     0   0.5   0.5    NA               1
      10 #C6E4D6 4 4     1    13  black    4     0   0.5   0.5    NA               1
      11 #E2F2EA 1 5     1     5  black    4     0   0.5   0.5    NA               1
      12 #EAF5F0 2 5     1     9  black    4     0   0.5   0.5    NA               1
      13 #FFFDFA 3 5     1    12  black    4     0   0.5   0.5    NA               1
      14 #FFF5E8 4 5     1    14  black    4     0   0.5   0.5    NA               1
      15 #B1DBC8 5 5     1    15  black    4     0   0.5   0.5    NA               1
         lineheight label
      1         1.2  0.34
      2         1.2 -0.02
      3         1.2 -0.08
      4         1.2 -1.00
      5         1.2  0.26
      6         1.2  0.08
      7         1.2  0.23
      8         1.2  0.06
      9         1.2  0.65
      10        1.2  0.28
      11        1.2  0.14
      12        1.2  0.10
      13        1.2 -0.02
      14        1.2 -0.10
      15        1.2  0.38
      
      [[3]]
            fill x y PANEL group shape colour size alpha stroke
      1  #70BD9D 1 1     1     1 cross  black   14    NA    0.5
      2  #36A880 1 2     1     2 cross  black   14    NA    0.5
      3  #36A880 2 2     1     5 cross  black   14    NA    0.5
      4  #36A880 2 3     1     6 cross  black   14    NA    0.5
      5  #36A880 3 3     1     9 cross  black   14    NA    0.5
      6  #36A880 1 4     1     3 cross  black   14    NA    0.5
      7  #36A880 2 4     1     7 cross  black   14    NA    0.5
      8  #36A880 4 4     1    11 cross  black   14    NA    0.5
      9  #36A880 1 5     1     4 cross  black   14    NA    0.5
      10 #36A880 2 5     1     8 cross  black   14    NA    0.5
      11 #36A880 3 5     1    10 cross  black   14    NA    0.5
      12 #36A880 4 5     1    12 cross  black   14    NA    0.5
      13 #9CD1B9 5 5     1    13 cross  black   14    NA    0.5
      

---

    Code
      list(p$labels, pb$plot$plot_env$legend.title)
    Output
      [[1]]
      [[1]]$caption
      NULL
      
      [[1]]$xlab
      NULL
      
      [[1]]$ylab
      NULL
      
      [[1]]$title
      NULL
      
      [[1]]$subtitle
      NULL
      
      [[1]]$fill
      [1] "value"
      
      [[1]]$x
      [1] "Var1"
      
      [[1]]$y
      [1] "Var2"
      
      
      [[2]]
      atop(atop(scriptstyle(bold("sample sizes:")), italic(n) ~ "=" ~ 
          "30"), atop(scriptstyle(bold("correlation (partial):")), 
          "Winsorized Pearson"))
      

# checking ggcorrmat - with NAs - spearman's rho

    Code
      pb$data
    Output
      [[1]]
            fill x y PANEL group xmin xmax ymin ymax colour size linetype alpha width
      1  #0B775E 1 1     1     1  0.5  1.5  0.5  1.5  black  0.1        1    NA    NA
      2  #57896B 2 1     1     5  1.5  2.5  0.5  1.5  black  0.1        1    NA    NA
      3  #E6BE81 3 1     1     9  2.5  3.5  0.5  1.5  black  0.1        1    NA    NA
      4  #E1BD6D 4 1     1    13  3.5  4.5  0.5  1.5  black  0.1        1    NA    NA
      5  #57896B 1 2     1     2  0.5  1.5  1.5  2.5  black  0.1        1    NA    NA
      6  #0B775E 2 2     1     6  1.5  2.5  1.5  2.5  black  0.1        1    NA    NA
      7  #E7BE87 3 2     1    10  2.5  3.5  1.5  2.5  black  0.1        1    NA    NA
      8  #E3BD77 4 2     1    14  3.5  4.5  1.5  2.5  black  0.1        1    NA    NA
      9  #E6BE81 1 3     1     3  0.5  1.5  2.5  3.5  black  0.1        1    NA    NA
      10 #E7BE87 2 3     1     7  1.5  2.5  2.5  3.5  black  0.1        1    NA    NA
      11 #0B775E 3 3     1    11  2.5  3.5  2.5  3.5  black  0.1        1    NA    NA
      12 #8E9C79 4 3     1    15  3.5  4.5  2.5  3.5  black  0.1        1    NA    NA
      13 #E1BD6D 1 4     1     4  0.5  1.5  3.5  4.5  black  0.1        1    NA    NA
      14 #E3BD77 2 4     1     8  1.5  2.5  3.5  4.5  black  0.1        1    NA    NA
      15 #8E9C79 3 4     1    12  2.5  3.5  3.5  4.5  black  0.1        1    NA    NA
      16 #0B775E 4 4     1    16  3.5  4.5  3.5  4.5  black  0.1        1    NA    NA
         height
      1      NA
      2      NA
      3      NA
      4      NA
      5      NA
      6      NA
      7      NA
      8      NA
      9      NA
      10     NA
      11     NA
      12     NA
      13     NA
      14     NA
      15     NA
      16     NA
      
      [[2]]
            fill x y PANEL group colour size angle hjust vjust alpha family fontface
      1  #0B775E 1 1     1     1  black    4     0   0.5   0.5    NA               1
      2  #57896B 2 1     1     5  black    4     0   0.5   0.5    NA               1
      3  #E6BE81 3 1     1     9  black    4     0   0.5   0.5    NA               1
      4  #E1BD6D 4 1     1    13  black    4     0   0.5   0.5    NA               1
      5  #57896B 1 2     1     2  black    4     0   0.5   0.5    NA               1
      6  #0B775E 2 2     1     6  black    4     0   0.5   0.5    NA               1
      7  #E7BE87 3 2     1    10  black    4     0   0.5   0.5    NA               1
      8  #E3BD77 4 2     1    14  black    4     0   0.5   0.5    NA               1
      9  #E6BE81 1 3     1     3  black    4     0   0.5   0.5    NA               1
      10 #E7BE87 2 3     1     7  black    4     0   0.5   0.5    NA               1
      11 #0B775E 3 3     1    11  black    4     0   0.5   0.5    NA               1
      12 #8E9C79 4 3     1    15  black    4     0   0.5   0.5    NA               1
      13 #E1BD6D 1 4     1     4  black    4     0   0.5   0.5    NA               1
      14 #E3BD77 2 4     1     8  black    4     0   0.5   0.5    NA               1
      15 #8E9C79 3 4     1    12  black    4     0   0.5   0.5    NA               1
      16 #0B775E 4 4     1    16  black    4     0   0.5   0.5    NA               1
         lineheight label
      1         1.2  1.00
      2         1.2  0.76
      3         1.2 -0.49
      4         1.2 -1.00
      5         1.2  0.76
      6         1.2  1.00
      7         1.2 -0.33
      8         1.2 -0.76
      9         1.2 -0.49
      10        1.2 -0.33
      11        1.2  1.00
      12        1.2  0.49
      13        1.2 -1.00
      14        1.2 -0.76
      15        1.2  0.49
      16        1.2  1.00
      
      [[3]]
           fill x y PANEL group shape colour size alpha stroke
      1 #DEBA91 3 2     1     2 cross  black   14    NA    0.5
      2 #DEBA91 2 3     1     1 cross  black   14    NA    0.5
      

---

    Code
      list(p$labels, pb$plot$plot_env$legend.title)
    Output
      [[1]]
      [[1]]$xlab
      NULL
      
      [[1]]$ylab
      NULL
      
      [[1]]$title
      NULL
      
      [[1]]$subtitle
      NULL
      
      [[1]]$caption
      atop(displaystyle(NULL), expr = paste(bold("X"), " = non-significant at ", 
          italic("p"), " < ", 0.01, " (Adjustment: ", "Hommel", ")"))
      
      [[1]]$fill
      [1] "value"
      
      [[1]]$x
      [1] "Var1"
      
      [[1]]$y
      [1] "Var2"
      
      
      [[2]]
      atop(atop(atop(scriptstyle(bold("sample sizes:")), italic(n)[min] ~ 
          "=" ~ "32"), atop(italic(n)[mode] ~ "=" ~ "32", italic(n)[max] ~ 
          "=" ~ "83")), atop(scriptstyle(bold("correlation:")), "Spearman"))
      

# checking Bayesian pearson (with NA)

    Code
      list(p$labels, pb$plot$plot_env$legend.title)
    Output
      [[1]]
      [[1]]$xlab
      NULL
      
      [[1]]$ylab
      NULL
      
      [[1]]$title
      NULL
      
      [[1]]$subtitle
      NULL
      
      [[1]]$caption
      NULL
      
      [[1]]$fill
      [1] "value"
      
      [[1]]$x
      [1] "Var1"
      
      [[1]]$y
      [1] "Var2"
      
      
      [[2]]
      atop(atop(atop(scriptstyle(bold("sample sizes:")), italic(n)[min] ~ 
          "=" ~ "56"), atop(italic(n)[mode] ~ "=" ~ "56", italic(n)[max] ~ 
          "=" ~ "56")), atop(scriptstyle(bold("correlation:")), "Bayesian Pearson"))
      

