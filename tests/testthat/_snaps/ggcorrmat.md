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
      1 #D0622D 2 1     1     2 cross  white   14    NA    0.5
      2 #D0622D 1 2     1     1 cross  white   14    NA    0.5
      

# checking ggcorrmat - without NAs - robust r

    Code
      pb$data
    Output
      [[1]]
            fill x y PANEL group xmin xmax ymin ymax colour size linetype alpha width
      1  #009E73 1 1     1     1  0.5  1.5  0.5  1.5  black  0.1        1    NA    NA
      2  #009E73 1 2     1     2  0.5  1.5  1.5  2.5  black  0.1        1    NA    NA
      3  #009E73 2 2     1     8  1.5  2.5  1.5  2.5  black  0.1        1    NA    NA
      4  #FFFEFD 1 3     1     3  0.5  1.5  2.5  3.5  black  0.1        1    NA    NA
      5  #FFFEFD 2 3     1     9  1.5  2.5  2.5  3.5  black  0.1        1    NA    NA
      6  #FFFEFD 3 3     1    14  2.5  3.5  2.5  3.5  black  0.1        1    NA    NA
      7  #AFDAC6 1 4     1     4  0.5  1.5  3.5  4.5  black  0.1        1    NA    NA
      8  #AFDAC6 2 4     1    10  1.5  2.5  3.5  4.5  black  0.1        1    NA    NA
      9  #AFDAC6 3 4     1    15  2.5  3.5  3.5  4.5  black  0.1        1    NA    NA
      10 #D8EDE3 4 4     1    19  3.5  4.5  3.5  4.5  black  0.1        1    NA    NA
      11 #FFE2BD 1 5     1     5  0.5  1.5  4.5  5.5  black  0.1        1    NA    NA
      12 #FFE2BD 2 5     1    11  1.5  2.5  4.5  5.5  black  0.1        1    NA    NA
      13 #FFE2BD 3 5     1    16  2.5  3.5  4.5  5.5  black  0.1        1    NA    NA
      14 #FDD298 4 5     1    20  3.5  4.5  4.5  5.5  black  0.1        1    NA    NA
      15 #EDF6F2 5 5     1    23  4.5  5.5  4.5  5.5  black  0.1        1    NA    NA
      16 #FFFDFA 1 6     1     6  0.5  1.5  5.5  6.5  black  0.1        1    NA    NA
      17 #FFFDFA 2 6     1    12  1.5  2.5  5.5  6.5  black  0.1        1    NA    NA
      18 #FFFDFA 3 6     1    17  2.5  3.5  5.5  6.5  black  0.1        1    NA    NA
      19 #FFFDFA 4 6     1    21  3.5  4.5  5.5  6.5  black  0.1        1    NA    NA
      20 #FACA86 5 6     1    24  4.5  5.5  5.5  6.5  black  0.1        1    NA    NA
      21 #FFF0DD 6 6     1    26  5.5  6.5  5.5  6.5  black  0.1        1    NA    NA
      22 #DCEFE6 1 7     1     7  0.5  1.5  6.5  7.5  black  0.1        1    NA    NA
      23 #DCEFE6 2 7     1    13  1.5  2.5  6.5  7.5  black  0.1        1    NA    NA
      24 #DCEFE6 3 7     1    18  2.5  3.5  6.5  7.5  black  0.1        1    NA    NA
      25 #60B794 4 7     1    22  3.5  4.5  6.5  7.5  black  0.1        1    NA    NA
      26 #FED9A9 5 7     1    25  4.5  5.5  6.5  7.5  black  0.1        1    NA    NA
      27 #D2EADF 6 7     1    27  5.5  6.5  6.5  7.5  black  0.1        1    NA    NA
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
      4  #FFFEFD 1 3     1     3  black    4     0   0.5   0.5    NA               1
      5  #FFFEFD 2 3     1     9  black    4     0   0.5   0.5    NA               1
      6  #FFFEFD 3 3     1    14  black    4     0   0.5   0.5    NA               1
      7  #AFDAC6 1 4     1     4  black    4     0   0.5   0.5    NA               1
      8  #AFDAC6 2 4     1    10  black    4     0   0.5   0.5    NA               1
      9  #AFDAC6 3 4     1    15  black    4     0   0.5   0.5    NA               1
      10 #D8EDE3 4 4     1    19  black    4     0   0.5   0.5    NA               1
      11 #FFE2BD 1 5     1     5  black    4     0   0.5   0.5    NA               1
      12 #FFE2BD 2 5     1    11  black    4     0   0.5   0.5    NA               1
      13 #FFE2BD 3 5     1    16  black    4     0   0.5   0.5    NA               1
      14 #FDD298 4 5     1    20  black    4     0   0.5   0.5    NA               1
      15 #EDF6F2 5 5     1    23  black    4     0   0.5   0.5    NA               1
      16 #FFFDFA 1 6     1     6  black    4     0   0.5   0.5    NA               1
      17 #FFFDFA 2 6     1    12  black    4     0   0.5   0.5    NA               1
      18 #FFFDFA 3 6     1    17  black    4     0   0.5   0.5    NA               1
      19 #FFFDFA 4 6     1    21  black    4     0   0.5   0.5    NA               1
      20 #FACA86 5 6     1    24  black    4     0   0.5   0.5    NA               1
      21 #FFF0DD 6 6     1    26  black    4     0   0.5   0.5    NA               1
      22 #DCEFE6 1 7     1     7  black    4     0   0.5   0.5    NA               1
      23 #DCEFE6 2 7     1    13  black    4     0   0.5   0.5    NA               1
      24 #DCEFE6 3 7     1    18  black    4     0   0.5   0.5    NA               1
      25 #60B794 4 7     1    22  black    4     0   0.5   0.5    NA               1
      26 #FED9A9 5 7     1    25  black    4     0   0.5   0.5    NA               1
      27 #D2EADF 6 7     1    27  black    4     0   0.5   0.5    NA               1
      28 #FFE7C9 7 7     1    28  black    4     0   0.5   0.5    NA               1
         lineheight label
      1         1.2  1.00
      2         1.2  1.00
      3         1.2  1.00
      4         1.2 -0.01
      5         1.2 -0.01
      6         1.2 -0.01
      7         1.2  0.39
      8         1.2  0.39
      9         1.2  0.39
      10        1.2  0.19
      11        1.2 -0.29
      12        1.2 -0.29
      13        1.2 -0.29
      14        1.2 -0.46
      15        1.2  0.09
      16        1.2 -0.02
      17        1.2 -0.02
      18        1.2 -0.02
      19        1.2 -0.02
      20        1.2 -0.54
      21        1.2 -0.15
      22        1.2  0.17
      23        1.2  0.17
      24        1.2  0.17
      25        1.2  0.75
      26        1.2 -0.38
      27        1.2  0.22
      28        1.2 -0.24
      
      [[3]]
            fill x y PANEL group shape colour size alpha stroke
      1  #009E73 1 3     1     1 cross  black    5    NA    0.5
      2  #009E73 2 3     1     6 cross  black    5    NA    0.5
      3  #009E73 3 3     1    11 cross  black    5    NA    0.5
      4  #009E73 1 4     1     2 cross  black    5    NA    0.5
      5  #009E73 2 4     1     7 cross  black    5    NA    0.5
      6  #009E73 3 4     1    12 cross  black    5    NA    0.5
      7  #009E73 4 4     1    16 cross  black    5    NA    0.5
      8  #009E73 1 5     1     3 cross  black    5    NA    0.5
      9  #009E73 2 5     1     8 cross  black    5    NA    0.5
      10 #009E73 3 5     1    13 cross  black    5    NA    0.5
      11 #009E73 4 5     1    17 cross  black    5    NA    0.5
      12 #009E73 5 5     1    20 cross  black    5    NA    0.5
      13 #009E73 1 6     1     4 cross  black    5    NA    0.5
      14 #009E73 2 6     1     9 cross  black    5    NA    0.5
      15 #009E73 3 6     1    14 cross  black    5    NA    0.5
      16 #009E73 4 6     1    18 cross  black    5    NA    0.5
      17 #009E73 5 6     1    21 cross  black    5    NA    0.5
      18 #009E73 6 6     1    23 cross  black    5    NA    0.5
      19 #009E73 1 7     1     5 cross  black    5    NA    0.5
      20 #009E73 2 7     1    10 cross  black    5    NA    0.5
      21 #009E73 3 7     1    15 cross  black    5    NA    0.5
      22 #D8EDE3 4 7     1    19 cross  black    5    NA    0.5
      23 #009E73 5 7     1    22 cross  black    5    NA    0.5
      24 #009E73 6 7     1    24 cross  black    5    NA    0.5
      25 #009E73 7 7     1    25 cross  black    5    NA    0.5
      

# checking ggcorrmat - with NAs - robust r - partial

    Code
      pb$data
    Output
      [[1]]
            fill x y PANEL group xmin xmax ymin ymax colour size linetype alpha width
      1  #D0E9DD 1 1     1     1  0.5  1.5  0.5  1.5  black  0.1        1    NA    NA
      2  #FFFEFD 1 2     1     2  0.5  1.5  1.5  2.5  black  0.1        1    NA    NA
      3  #FFF9F1 2 2     1     6  1.5  2.5  1.5  2.5  black  0.1        1    NA    NA
      4  #E69F00 1 3     1     3  0.5  1.5  2.5  3.5  black  0.1        1    NA    NA
      5  #DEF0E7 2 3     1     7  1.5  2.5  2.5  3.5  black  0.1        1    NA    NA
      6  #FFFEFD 3 3     1    10  2.5  3.5  2.5  3.5  black  0.1        1    NA    NA
      7  #DCEFE6 1 4     1     4  0.5  1.5  3.5  4.5  black  0.1        1    NA    NA
      8  #F5FAF8 2 4     1     8  1.5  2.5  3.5  4.5  black  0.1        1    NA    NA
      9  #71BE9E 3 4     1    11  2.5  3.5  3.5  4.5  black  0.1        1    NA    NA
      10 #BFE1D2 4 4     1    13  3.5  4.5  3.5  4.5  black  0.1        1    NA    NA
      11 #C3E3D5 1 5     1     5  0.5  1.5  4.5  5.5  black  0.1        1    NA    NA
      12 #F5FAF8 2 5     1     9  1.5  2.5  4.5  5.5  black  0.1        1    NA    NA
      13 #FFFDFA 3 5     1    12  2.5  3.5  4.5  5.5  black  0.1        1    NA    NA
      14 #FFE7C9 4 5     1    14  3.5  4.5  4.5  5.5  black  0.1        1    NA    NA
      15 #A7D6C1 5 5     1    15  4.5  5.5  4.5  5.5  black  0.1        1    NA    NA
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
      1  #D0E9DD 1 1     1     1  black    4     0   0.5   0.5    NA               1
      2  #FFFEFD 1 2     1     2  black    4     0   0.5   0.5    NA               1
      3  #FFF9F1 2 2     1     6  black    4     0   0.5   0.5    NA               1
      4  #E69F00 1 3     1     3  black    4     0   0.5   0.5    NA               1
      5  #DEF0E7 2 3     1     7  black    4     0   0.5   0.5    NA               1
      6  #FFFEFD 3 3     1    10  black    4     0   0.5   0.5    NA               1
      7  #DCEFE6 1 4     1     4  black    4     0   0.5   0.5    NA               1
      8  #F5FAF8 2 4     1     8  black    4     0   0.5   0.5    NA               1
      9  #71BE9E 3 4     1    11  black    4     0   0.5   0.5    NA               1
      10 #BFE1D2 4 4     1    13  black    4     0   0.5   0.5    NA               1
      11 #C3E3D5 1 5     1     5  black    4     0   0.5   0.5    NA               1
      12 #F5FAF8 2 5     1     9  black    4     0   0.5   0.5    NA               1
      13 #FFFDFA 3 5     1    12  black    4     0   0.5   0.5    NA               1
      14 #FFE7C9 4 5     1    14  black    4     0   0.5   0.5    NA               1
      15 #A7D6C1 5 5     1    15  black    4     0   0.5   0.5    NA               1
         lineheight label
      1         1.2  0.23
      2         1.2 -0.01
      3         1.2 -0.06
      4         1.2 -1.00
      5         1.2  0.16
      6         1.2 -0.01
      7         1.2  0.17
      8         1.2  0.05
      9         1.2  0.68
      10        1.2  0.31
      11        1.2  0.29
      12        1.2  0.05
      13        1.2 -0.02
      14        1.2 -0.24
      15        1.2  0.43
      
      [[3]]
            fill x y PANEL group shape colour size alpha stroke
      1  #1FA278 1 1     1     1 cross  black    5    NA    0.5
      2  #1FA278 1 2     1     2 cross  black    5    NA    0.5
      3  #1FA278 2 2     1     5 cross  black    5    NA    0.5
      4  #1FA278 2 3     1     6 cross  black    5    NA    0.5
      5  #1FA278 3 3     1     9 cross  black    5    NA    0.5
      6  #1FA278 1 4     1     3 cross  black    5    NA    0.5
      7  #1FA278 2 4     1     7 cross  black    5    NA    0.5
      8  #1FA278 4 4     1    11 cross  black    5    NA    0.5
      9  #1FA278 1 5     1     4 cross  black    5    NA    0.5
      10 #1FA278 2 5     1     8 cross  black    5    NA    0.5
      11 #1FA278 3 5     1    10 cross  black    5    NA    0.5
      12 #1FA278 4 5     1    12 cross  black    5    NA    0.5
      13 #D2EADF 5 5     1    13 cross  black    5    NA    0.5
      

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
      1 #DEBA91 3 2     1     2 cross  black    5    NA    0.5
      2 #DEBA91 2 3     1     1 cross  black    5    NA    0.5
      

# checking Bayesian pearson (with NA)

    Code
      pb$data
    Output
      [[1]]
            fill x y PANEL group xmin xmax ymin ymax colour size linetype alpha width
      1  #65B997 1 1     1     1  0.5  1.5  0.5  1.5  black  0.1        1    NA    NA
      2  #FDD49E 1 2     1     2  0.5  1.5  1.5  2.5  black  0.1        1    NA    NA
      3  #FFE0B9 2 2     1     6  1.5  2.5  1.5  2.5  black  0.1        1    NA    NA
      4  #E69F00 1 3     1     3  0.5  1.5  2.5  3.5  black  0.1        1    NA    NA
      5  #F4B85B 2 3     1     7  1.5  2.5  2.5  3.5  black  0.1        1    NA    NA
      6  #A4D5BF 3 3     1    10  2.5  3.5  2.5  3.5  black  0.1        1    NA    NA
      7  #FFDDB2 1 4     1     4  0.5  1.5  3.5  4.5  black  0.1        1    NA    NA
      8  #FFEBD2 2 4     1     8  1.5  2.5  3.5  4.5  black  0.1        1    NA    NA
      9  #4FB08B 3 4     1    11  2.5  3.5  3.5  4.5  black  0.1        1    NA    NA
      10 #B9DECD 4 4     1    13  3.5  4.5  3.5  4.5  black  0.1        1    NA    NA
      11 #FFE1BB 1 5     1     5  0.5  1.5  4.5  5.5  black  0.1        1    NA    NA
      12 #FFDFB7 2 5     1     9  1.5  2.5  4.5  5.5  black  0.1        1    NA    NA
      13 #AFDAC6 3 5     1    12  2.5  3.5  4.5  5.5  black  0.1        1    NA    NA
      14 #C1E2D3 4 5     1    14  3.5  4.5  4.5  5.5  black  0.1        1    NA    NA
      15 #2DA57C 5 5     1    15  4.5  5.5  4.5  5.5  black  0.1        1    NA    NA
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
      1  #65B997 1 1     1     1  black    4     0   0.5   0.5    NA               1
      2  #FDD49E 1 2     1     2  black    4     0   0.5   0.5    NA               1
      3  #FFE0B9 2 2     1     6  black    4     0   0.5   0.5    NA               1
      4  #E69F00 1 3     1     3  black    4     0   0.5   0.5    NA               1
      5  #F4B85B 2 3     1     7  black    4     0   0.5   0.5    NA               1
      6  #A4D5BF 3 3     1    10  black    4     0   0.5   0.5    NA               1
      7  #FFDDB2 1 4     1     4  black    4     0   0.5   0.5    NA               1
      8  #FFEBD2 2 4     1     8  black    4     0   0.5   0.5    NA               1
      9  #4FB08B 3 4     1    11  black    4     0   0.5   0.5    NA               1
      10 #B9DECD 4 4     1    13  black    4     0   0.5   0.5    NA               1
      11 #FFE1BB 1 5     1     5  black    4     0   0.5   0.5    NA               1
      12 #FFDFB7 2 5     1     9  black    4     0   0.5   0.5    NA               1
      13 #AFDAC6 3 5     1    12  black    4     0   0.5   0.5    NA               1
      14 #C1E2D3 4 5     1    14  black    4     0   0.5   0.5    NA               1
      15 #2DA57C 5 5     1    15  black    4     0   0.5   0.5    NA               1
         lineheight label
      1         1.2  0.73
      2         1.2 -0.43
      3         1.2 -0.31
      4         1.2 -1.00
      5         1.2 -0.73
      6         1.2  0.44
      7         1.2 -0.34
      8         1.2 -0.20
      9         1.2  0.82
      10        1.2  0.34
      11        1.2 -0.30
      12        1.2 -0.32
      13        1.2  0.39
      14        1.2  0.30
      15        1.2  0.93
      
      [[3]]
      data frame with 0 columns and 0 rows
      

# checking all dataframe outputs

    Code
      suppressWarnings(purrr::pmap_dfr(.l = list(data = list(ggplot2::msleep), type = list(
        "p", "p", "np", "np", "r", "r", "bf", "bayes"), output = list("dataframe"),
      partial = list(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)), .f = ggcorrmat))
    Output
      # A tibble: 120 x 17
         parameter1 parameter2 estimate conf.level conf.low conf.high statistic
         <chr>      <chr>         <dbl>      <dbl>    <dbl>     <dbl>     <dbl>
       1 sleep_tot~ sleep_rem   0.314         0.95  -0.0520     0.606    1.75  
       2 sleep_tot~ sleep_cyc~ -0.0225        0.95  -0.380      0.341   -0.119 
       3 sleep_tot~ awake      -1             0.95  -1         -1     -Inf     
       4 sleep_tot~ brainwt    -0.0970        0.95  -0.442      0.273   -0.516 
       5 sleep_tot~ bodywt     -0.179         0.95  -0.506      0.194   -0.961 
       6 sleep_rem  sleep_cyc~ -0.0766        0.95  -0.425      0.292   -0.407 
       7 sleep_rem  awake       0.0560        0.95  -0.311      0.408    0.297 
       8 sleep_rem  brainwt     0.0857        0.95  -0.283      0.433    0.455 
       9 sleep_rem  bodywt     -0.0341        0.95  -0.390      0.330   -0.181 
      10 sleep_cyc~ awake      -0.00479       0.95  -0.364      0.356   -0.0253
      # ... with 110 more rows, and 10 more variables: df.error <int>, p.value <dbl>,
      #   method <chr>, n.obs <int>, pd <dbl>, rope.percentage <dbl>,
      #   prior.distribution <chr>, prior.location <dbl>, prior.scale <dbl>,
      #   bayes.factor <dbl>

