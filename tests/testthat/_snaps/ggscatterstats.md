# checking ggscatterstats - without NAs - pearson's r

    Code
      list(pb$data[[1]], head(pb$data[[2]]), pb$data[[3]])
    Output
      [[1]]
           x         y PANEL group shape colour size fill alpha stroke
      1 14.9 0.1333333     1    -1    19  black    3   NA   0.4      0
      2  9.1 0.1500000     1    -1    19  black    3   NA   0.4      0
      3 17.4 0.3833333     1    -1    19  black    3   NA   0.4      0
      4 18.0 0.3333333     1    -1    19  black    3   NA   0.4      0
      5 19.7 0.1166667     1    -1    19  black    3   NA   0.4      0
      6 10.1 0.2833333     1    -1    19  black    3   NA   0.4      0
      7 13.0 0.1833333     1    -1    19  black    3   NA   0.4      0
      8  8.4 0.1666667     1    -1    19  black    3   NA   0.4      0
      9 13.8 0.2166667     1    -1    19  black    3   NA   0.4      0
      
      [[2]]
               x         y       ymin      ymax         se flipped_aes PANEL group
      1 8.400000 0.1868825 0.05397985 0.3197852 0.05620456       FALSE     1    -1
      2 8.543038 0.1877171 0.05714189 0.3182922 0.05522027       FALSE     1    -1
      3 8.686076 0.1885516 0.06028225 0.3168210 0.05424514       FALSE     1    -1
      4 8.829114 0.1893862 0.06339976 0.3153726 0.05327968       FALSE     1    -1
      5 8.972152 0.1902207 0.06649314 0.3139483 0.05232442       FALSE     1    -1
      6 9.115190 0.1910553 0.06956104 0.3125495 0.05137994       FALSE     1    -1
        colour   fill size linetype weight alpha
      1   blue grey60  1.5        1      1   0.4
      2   blue grey60  1.5        1      1   0.4
      3   blue grey60  1.5        1      1   0.4
      4   blue grey60  1.5        1      1   0.4
      5   blue grey60  1.5        1      1   0.4
      6   blue grey60  1.5        1      1   0.4
      
      [[3]]
           x         y                  label PANEL group colour  fill size angle
      1 17.4 0.3833333   Long-nosed armadillo     1    -1  black white    3     0
      2 18.0 0.3333333 North American Opossum     1    -1  black white    3     0
        alpha family fontface lineheight hjust vjust point.size segment.linetype
      1    NA               1        1.2   0.5   0.5          1                1
      2    NA               1        1.2   0.5   0.5          1                1
        segment.size segment.curvature segment.angle segment.ncp segment.shape
      1          0.5                 0            90           1           0.5
      2          0.5                 0            90           1           0.5
        segment.square segment.squareShape segment.inflect segment.debug
      1           TRUE                   1           FALSE         FALSE
      2           TRUE                   1           FALSE         FALSE
      

---

    Code
      within(pb$plot$labels, rm(subtitle, caption))
    Output
      $x
      [1] "sleep (total)"
      
      $y
      [1] "sleep cycle"
      
      $title
      [1] "Mammalian sleep"
      
      $label
      [1] "name"
      
      $alt
      [1] ""
      

# checking ggscatterstats - without NAs - spearman's rho

    Code
      pb$data[[1]]
    Output
           x         y PANEL group shape colour size fill alpha stroke
      1 14.9 0.1333333     1    -1    19  black    3   NA   0.4      0
      2  9.1 0.1500000     1    -1    19  black    3   NA   0.4      0
      3 17.4 0.3833333     1    -1    19  black    3   NA   0.4      0
      4 18.0 0.3333333     1    -1    19  black    3   NA   0.4      0
      5 19.7 0.1166667     1    -1    19  black    3   NA   0.4      0
      6 10.1 0.2833333     1    -1    19  black    3   NA   0.4      0
      7 13.0 0.1833333     1    -1    19  black    3   NA   0.4      0
      8  8.4 0.1666667     1    -1    19  black    3   NA   0.4      0
      9 13.8 0.2166667     1    -1    19  black    3   NA   0.4      0

---

    Code
      within(pb$plot$labels, rm(subtitle))
    Output
      $x
      [1] "sleep_total"
      
      $y
      [1] "sleep_cycle"
      
      $title
      NULL
      
      $caption
      NULL
      
      $alt
      [1] ""
      

# checking ggscatterstats - without NAs - winsorized Pearson

    Code
      pb$data[[1]]
    Output
           x         y PANEL group shape colour size fill alpha stroke
      1 14.9 0.1333333     1    -1    19    red    5   NA    NA      0
      2  9.1 0.1500000     1    -1    19    red    5   NA    NA      0
      3 17.4 0.3833333     1    -1    19    red    5   NA    NA      0
      4 18.0 0.3333333     1    -1    19    red    5   NA    NA      0
      5 19.7 0.1166667     1    -1    19    red    5   NA    NA      0
      6 10.1 0.2833333     1    -1    19    red    5   NA    NA      0
      7 13.0 0.1833333     1    -1    19    red    5   NA    NA      0
      8  8.4 0.1666667     1    -1    19    red    5   NA    NA      0
      9 13.8 0.2166667     1    -1    19    red    5   NA    NA      0

---

    Code
      within(pb$plot$labels, rm(subtitle))
    Output
      $x
      [1] "sleep_total"
      
      $y
      [1] "sleep_cycle"
      
      $title
      NULL
      
      $caption
      NULL
      
      $alt
      [1] ""
      

# aesthetic modifications work

    Code
      list(pb$data[[1]], head(pb$data[[2]]), pb$data[[3]])
    Output
      [[1]]
                y    x PANEL group shape colour size fill alpha stroke
      1 0.1333333 14.9     1    -1    19  black    3   NA   0.4      0
      2 0.1500000  9.1     1    -1    19  black    3   NA   0.4      0
      3 0.3833333 17.4     1    -1    19  black    3   NA   0.4      0
      4 0.3333333 18.0     1    -1    19  black    3   NA   0.4      0
      5 0.1166667 19.7     1    -1    19  black    3   NA   0.4      0
      6 0.2833333 10.1     1    -1    19  black    3   NA   0.4      0
      7 0.1833333 13.0     1    -1    19  black    3   NA   0.4      0
      8 0.1666667  8.4     1    -1    19  black    3   NA   0.4      0
      9 0.2166667 13.8     1    -1    19  black    3   NA   0.4      0
      
      [[2]]
               x         y       ymin      ymax         se flipped_aes PANEL group
      1 8.400000 0.1868825 0.05397985 0.3197852 0.05620456       FALSE     1    -1
      2 8.543038 0.1877171 0.05714189 0.3182922 0.05522027       FALSE     1    -1
      3 8.686076 0.1885516 0.06028225 0.3168210 0.05424514       FALSE     1    -1
      4 8.829114 0.1893862 0.06339976 0.3153726 0.05327968       FALSE     1    -1
      5 8.972152 0.1902207 0.06649314 0.3139483 0.05232442       FALSE     1    -1
      6 9.115190 0.1910553 0.06956104 0.3125495 0.05137994       FALSE     1    -1
        colour   fill size linetype weight alpha
      1   blue grey60  1.5        1      1   0.4
      2   blue grey60  1.5        1      1   0.4
      3   blue grey60  1.5        1      1   0.4
      4   blue grey60  1.5        1      1   0.4
      5   blue grey60  1.5        1      1   0.4
      6   blue grey60  1.5        1      1   0.4
      
      [[3]]
                y    x           label PANEL group colour  fill size angle alpha
      1 0.3833333 17.4       Cingulata     1    -1   blue white    4     0   0.5
      2 0.3333333 18.0 Didelphimorphia     1    -1   blue white    4     0   0.5
      3 0.1166667 19.7      Chiroptera     1    -1   blue white    4     0   0.5
        family fontface lineheight hjust vjust point.size segment.linetype
      1               1        1.2   0.5   0.5          1                1
      2               1        1.2   0.5   0.5          1                1
      3               1        1.2   0.5   0.5          1                1
        segment.size segment.curvature segment.angle segment.ncp segment.shape
      1          0.5                 0            90           1           0.5
      2          0.5                 0            90           1           0.5
      3          0.5                 0            90           1           0.5
        segment.square segment.squareShape segment.inflect segment.debug
      1           TRUE                   1           FALSE         FALSE
      2           TRUE                   1           FALSE         FALSE
      3           TRUE                   1           FALSE         FALSE
      

---

    Code
      pb$plot$labels
    Output
      $x
      [1] "sleep_total"
      
      $y
      [1] "sleep_cycle"
      
      $title
      NULL
      
      $subtitle
      NULL
      
      $caption
      NULL
      
      $label
      [1] "order"
      
      $alt
      [1] ""
      

