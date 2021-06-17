# checking labels with counts

    Code
      pb$data
    Output
      [[1]]
             fill         y x PANEL group flipped_aes      ymin      ymax xmin xmax
      1 #1B9E77FF 1.0000000 1     1     1       FALSE 0.9154362 1.0000000 0.55 1.45
      2 #1B9E77FF 1.0000000 2     1     3       FALSE 0.5161744 1.0000000 1.55 2.45
      3 #D95F02FF 0.9154362 1     1     2       FALSE 0.0000000 0.9154362 0.55 1.45
      4 #D95F02FF 0.5161744 2     1     4       FALSE 0.0000000 0.5161744 1.55 2.45
        colour size linetype alpha
      1  black  0.5        1    NA
      2  black  0.5        1    NA
      3  black  0.5        1    NA
      4  black  0.5        1    NA
      
      [[2]]
                y x  label group PANEL      ymax xmin xmax      ymin colour  fill
      1 0.9577181 1  8.46%     1     1 1.0000000    1    1 0.9154362  black white
      2 0.7580872 2 48.38%     1     1 1.0000000    2    2 0.5161744  black white
      3 0.4577181 1 91.54%     2     1 0.9154362    1    1 0.0000000  black white
      4 0.2580872 2 51.62%     2     1 0.5161744    2    2 0.0000000  black white
        size angle hjust vjust alpha family fontface lineheight
      1 3.88     0   0.5   0.5     1               1        1.2
      2 3.88     0   0.5   0.5     1               1        1.2
      3 3.88     0   0.5   0.5     1               1        1.2
      4 3.88     0   0.5   0.5     1               1        1.2
      
      [[3]]
           y x                         label PANEL group colour size angle hjust
      1 1.05 2     list(~italic(p)=='0.388')     1     2  black  2.8     0   0.5
      2 1.05 1 list(~italic(p)=='1.08e-225')     1     1  black  2.8     0   0.5
        vjust alpha family fontface lineheight
      1   0.5    NA               1        1.2
      2   0.5    NA               1        1.2
      
      [[4]]
            y x       label PANEL group colour size angle hjust vjust alpha family
      1 -0.05 2   (n = 711)     1     2  black    4     0   0.5   0.5    NA       
      2 -0.05 1 (n = 1,490)     1     1  black    4     0   0.5   0.5    NA       
        fontface lineheight
      1        1        1.2
      2        1        1.2
      

---

    Code
      within(pb$plot$labels, rm(subtitle))
    Output
      $x
      [1] "Passenger sex"
      
      $y
      [1] "proportion"
      
      $title
      NULL
      
      $caption
      NULL
      
      $fill
      [1] "Sex"
      
      $label
      [1] ".label"
      
      $group
      [1] "Sex"
      
      $alt
      [1] ""
      

# aesthetic modifications

    Code
      list(pb$data, pb1$data)
    Output
      [[1]]
      [[1]][[1]]
             fill          y x PANEL group flipped_aes       ymin       ymax xmin
      1 #9A8822FF 1.00000000 1     1     1       FALSE 0.09090909 1.00000000 0.55
      2 #9A8822FF 1.00000000 2     1     3       FALSE 0.42857143 1.00000000 1.55
      3 #F5CDB4FF 0.09090909 1     1     2       FALSE 0.00000000 0.09090909 0.55
      4 #F5CDB4FF 0.42857143 2     1     4       FALSE 0.00000000 0.42857143 1.55
      5 #F5CDB4FF 1.00000000 3     1     5       FALSE 0.00000000 1.00000000 2.55
        xmax colour size linetype alpha
      1 1.45  black  0.5        1    NA
      2 2.45  black  0.5        1    NA
      3 1.45  black  0.5        1    NA
      4 2.45  black  0.5        1    NA
      5 3.45  black  0.5        1    NA
      
      [[1]][[2]]
                 y x      label group PANEL       ymax xmin xmax       ymin colour
      1 0.54545455 1  10\n(91%)     1     1 1.00000000    1    1 0.09090909  black
      2 0.71428571 2   4\n(57%)     1     1 1.00000000    2    2 0.42857143  black
      3 0.04545455 1    1\n(9%)     2     1 0.09090909    1    1 0.00000000  black
      4 0.21428571 2   3\n(43%)     2     1 0.42857143    2    2 0.00000000  black
      5 0.50000000 3 14\n(100%)     2     1 1.00000000    3    3 0.00000000  black
         fill size angle hjust vjust alpha family fontface lineheight
      1 white 3.88     0   0.5   0.5     1               1        1.2
      2 white 3.88     0   0.5   0.5     1               1        1.2
      3 white 3.88     0   0.5   0.5     1               1        1.2
      4 white 3.88     0   0.5   0.5     1               1        1.2
      5 white 3.88     0   0.5   0.5     1               1        1.2
      
      [[1]][[3]]
           y x                        label PANEL group colour size angle hjust vjust
      1 1.05 3 list(~italic(p)=='1.83e-04')     1     3  black  2.8     0   0.5   0.5
      2 1.05 2    list(~italic(p)=='0.705')     1     2  black  2.8     0   0.5   0.5
      3 1.05 1    list(~italic(p)=='0.007')     1     1  black  2.8     0   0.5   0.5
        alpha family fontface lineheight
      1    NA               1        1.2
      2    NA               1        1.2
      3    NA               1        1.2
      
      [[1]][[4]]
            y x    label PANEL group colour size angle hjust vjust alpha family
      1 -0.05 3 (n = 14)     1     3  black    4     0   0.5   0.5    NA       
      2 -0.05 2  (n = 7)     1     2  black    4     0   0.5   0.5    NA       
      3 -0.05 1 (n = 11)     1     1  black    4     0   0.5   0.5    NA       
        fontface lineheight
      1        1        1.2
      2        1        1.2
      3        1        1.2
      
      
      [[2]]
      [[2]][[1]]
             fill          y x PANEL group flipped_aes       ymin       ymax xmin
      1 #1B9E77FF 1.00000000 1     1     1       FALSE 0.09090909 1.00000000 0.55
      2 #1B9E77FF 1.00000000 2     1     3       FALSE 0.42857143 1.00000000 1.55
      3 #D95F02FF 0.09090909 1     1     2       FALSE 0.00000000 0.09090909 0.55
      4 #D95F02FF 0.42857143 2     1     4       FALSE 0.00000000 0.42857143 1.55
      5 #D95F02FF 1.00000000 3     1     5       FALSE 0.00000000 1.00000000 2.55
        xmax colour size linetype alpha
      1 1.45  black  0.5        1    NA
      2 2.45  black  0.5        1    NA
      3 1.45  black  0.5        1    NA
      4 2.45  black  0.5        1    NA
      5 3.45  black  0.5        1    NA
      
      [[2]][[2]]
                 y x label group PANEL       ymax xmin xmax       ymin colour  fill
      1 0.54545455 1    10     1     1 1.00000000    1    1 0.09090909  black white
      2 0.71428571 2     4     1     1 1.00000000    2    2 0.42857143  black white
      3 0.04545455 1     1     2     1 0.09090909    1    1 0.00000000  black white
      4 0.21428571 2     3     2     1 0.42857143    2    2 0.00000000  black white
      5 0.50000000 3    14     2     1 1.00000000    3    3 0.00000000  black white
        size angle hjust vjust alpha family fontface lineheight
      1 3.88     0   0.5   0.5     1               1        1.2
      2 3.88     0   0.5   0.5     1               1        1.2
      3 3.88     0   0.5   0.5     1               1        1.2
      4 3.88     0   0.5   0.5     1               1        1.2
      5 3.88     0   0.5   0.5     1               1        1.2
      
      [[2]][[3]]
           y x                        label PANEL group colour size angle hjust vjust
      1 1.05 3 list(~italic(p)=='1.83e-04')     1     3  black  2.8     0   0.5   0.5
      2 1.05 2    list(~italic(p)=='0.705')     1     2  black  2.8     0   0.5   0.5
      3 1.05 1    list(~italic(p)=='0.007')     1     1  black  2.8     0   0.5   0.5
        alpha family fontface lineheight
      1    NA               1        1.2
      2    NA               1        1.2
      3    NA               1        1.2
      
      [[2]][[4]]
            y x    label PANEL group colour size angle hjust vjust alpha family
      1 -0.05 3 (n = 14)     1     3  black    4     0   0.5   0.5    NA       
      2 -0.05 2  (n = 7)     1     2  black    4     0   0.5   0.5    NA       
      3 -0.05 1 (n = 11)     1     1  black    4     0   0.5   0.5    NA       
        fontface lineheight
      1        1        1.2
      2        1        1.2
      3        1        1.2
      
      

# dropped factor levels

    Code
      pb$data
    Output
      [[1]]
             fill         y x PANEL group flipped_aes      ymin      ymax xmin xmax
      1 #1B9E77FF 1.0000000 1     1     1       FALSE 0.3684211 1.0000000 0.55 1.45
      2 #D95F02FF 0.3684211 1     1     2       FALSE 0.1578947 0.3684211 0.55 1.45
      3 #7570B3FF 0.1578947 1     1     3       FALSE 0.0000000 0.1578947 0.55 1.45
        colour size linetype alpha
      1  black  0.5        1    NA
      2  black  0.5        1    NA
      3  black  0.5        1    NA
      
      [[2]]
                 y x label group PANEL      ymax xmin xmax      ymin colour  fill
      1 0.68421053 1   63%     1     1 1.0000000    1    1 0.3684211  black white
      2 0.26315789 1   21%     2     1 0.3684211    1    1 0.1578947  black white
      3 0.07894737 1   16%     3     1 0.1578947    1    1 0.0000000  black white
        size angle hjust vjust alpha family fontface lineheight
      1 3.88     0   0.5   0.5     1               1        1.2
      2 3.88     0   0.5   0.5     1               1        1.2
      3 3.88     0   0.5   0.5     1               1        1.2
      
      [[3]]
            y x    label PANEL group colour size angle hjust vjust alpha family
      1 -0.05 1 (n = 19)     1     1  black    4     0   0.5   0.5    NA       
        fontface lineheight
      1        1        1.2
      

---

    Code
      pb$plot$labels
    Output
      $x
      [1] "am"
      
      $y
      NULL
      
      $title
      NULL
      
      $subtitle
      NULL
      
      $caption
      NULL
      
      $fill
      [1] "cyl"
      
      $label
      [1] ".label"
      
      $group
      [1] "cyl"
      
      $alt
      [1] ""
      

