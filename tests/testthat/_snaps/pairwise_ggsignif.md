# check comparison significant displays - FDR-corrected

    Code
      pb$data[[6]]
    Output
        x          y                             label PANEL group nudge_x    nudge_y
      1 1 0.07925556 list(~widehat(mu)[mean]=='0.079')     1     1     1.4 0.07925556
      2 2 0.62159750 list(~widehat(mu)[mean]=='0.622')     1     2     2.4 0.62159750
      3 3 0.02155000 list(~widehat(mu)[mean]=='0.022')     1     3     3.4 0.02155000
      4 4 0.14573118 list(~widehat(mu)[mean]=='0.146')     1     4     4.4 0.14573118
        colour  fill size angle alpha family fontface lineheight hjust vjust
      1  black white    3     0    NA               1        1.2   0.5   0.5
      2  black white    3     0    NA               1        1.2   0.5   0.5
      3  black white    3     0    NA               1        1.2   0.5   0.5
      4  black white    3     0    NA               1        1.2   0.5   0.5
        point.size segment.linetype segment.size segment.curvature segment.angle
      1          1                4          0.5                 0            90
      2          1                4          0.5                 0            90
      3          1                4          0.5                 0            90
      4          1                4          0.5                 0            90
        segment.ncp segment.shape segment.square segment.squareShape segment.inflect
      1           1           0.5           TRUE                   1           FALSE
      2           1           0.5           TRUE                   1           FALSE
      3           1           0.5           TRUE                   1           FALSE
      4           1           0.5           TRUE                   1           FALSE
        segment.debug
      1         FALSE
      2         FALSE
      3         FALSE
      4         FALSE

---

    Code
      pb$plot$labels
    Output
      $x
      [1] "vore"
      
      $y
      [1] "brainwt"
      
      $colour
      [1] "vore"
      
      $title
      NULL
      
      $subtitle
      NULL
      
      $caption
      atop(displaystyle("mammalian sleep"), expr = paste("Pairwise test: ", 
          bold("Games-Howell test"), "; Comparisons shown: ", bold("only significant")))
      
      $label
      [1] "label"
      

# check non-significant comparison displays - no adjustment

    Code
      pb$data[[6]]
    Output
        x         y                                 label PANEL group nudge_x
      1 1  8440.335  list(~widehat(mu)[mean]=='8440.335')     1     1     1.4
      2 2 11148.255 list(~widehat(mu)[mean]=='11148.255')     1     2     2.4
      3 3  9243.369  list(~widehat(mu)[mean]=='9243.369')     1     3     3.4
          nudge_y colour  fill size angle alpha family fontface lineheight hjust
      1  8440.335  black white    3     0    NA               1        1.2   0.5
      2 11148.255  black white    3     0    NA               1        1.2   0.5
      3  9243.369  black white    3     0    NA               1        1.2   0.5
        vjust point.size segment.linetype segment.size segment.curvature
      1   0.5          1                4          0.5                 0
      2   0.5          1                4          0.5                 0
      3   0.5          1                4          0.5                 0
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
      pb$plot$labels
    Output
      $x
      [1] "mpaa"
      
      $y
      [1] "votes"
      
      $colour
      [1] "mpaa"
      
      $title
      NULL
      
      $subtitle
      NULL
      
      $caption
      atop(displaystyle(NULL), expr = paste("Pairwise test: ", bold("Games-Howell test"), 
          "; Comparisons shown: ", bold("only non-significant")))
      
      $label
      [1] "label"
      

# check mixed comparison displays - FDR-corrected

    Code
      list(pb$data[[6]], pb$data[[7]])
    Output
      [[1]]
        x   y                               label PANEL group nudge_x nudge_y colour
      1 1 5.5 list(~widehat(mu)[median]=='5.500')     1     1     1.4     5.5  black
      2 2 5.5 list(~widehat(mu)[median]=='5.500')     1     2     2.4     5.5  black
      3 3 5.9 list(~widehat(mu)[median]=='5.900')     1     3     3.4     5.9  black
         fill size angle alpha family fontface lineheight hjust vjust point.size
      1 white    3     0    NA               1        1.2   0.5   0.5          1
      2 white    3     0    NA               1        1.2   0.5   0.5          1
      3 white    3     0    NA               1        1.2   0.5   0.5          1
        segment.linetype segment.size segment.curvature segment.angle segment.ncp
      1                4          0.5                 0            90           1
      2                4          0.5                 0            90           1
      3                4          0.5                 0            90           1
        segment.shape segment.square segment.squareShape segment.inflect
      1           0.5           TRUE                   1           FALSE
      2           0.5           TRUE                   1           FALSE
      3           0.5           TRUE                   1           FALSE
        segment.debug
      1         FALSE
      2         FALSE
      3         FALSE
      
      [[2]]
        x xend       y    yend                                annotation
      1 1    1  9.5170  9.5900    list(~italic(p)[FDR-corrected]==0.812)
      2 1    2  9.5900  9.5900    list(~italic(p)[FDR-corrected]==0.812)
      3 2    2  9.5900  9.5170    list(~italic(p)[FDR-corrected]==0.812)
      4 1    1 10.0645 10.1375 list(~italic(p)[FDR-corrected]==4.18e-04)
      5 1    3 10.1375 10.1375 list(~italic(p)[FDR-corrected]==4.18e-04)
      6 3    3 10.1375 10.0645 list(~italic(p)[FDR-corrected]==4.18e-04)
      7 2    2 10.6120 10.6850 list(~italic(p)[FDR-corrected]==4.18e-04)
      8 2    3 10.6850 10.6850 list(~italic(p)[FDR-corrected]==4.18e-04)
      9 3    3 10.6850 10.6120 list(~italic(p)[FDR-corrected]==4.18e-04)
                  group PANEL shape colour textsize angle hjust vjust alpha family
      1 Action-Comedy-1     1    19  black        3     0   0.5     0    NA       
      2 Action-Comedy-1     1    19  black        3     0   0.5     0    NA       
      3 Action-Comedy-1     1    19  black        3     0   0.5     0    NA       
      4 Action-RomCom-2     1    19  black        3     0   0.5     0    NA       
      5 Action-RomCom-2     1    19  black        3     0   0.5     0    NA       
      6 Action-RomCom-2     1    19  black        3     0   0.5     0    NA       
      7 Comedy-RomCom-3     1    19  black        3     0   0.5     0    NA       
      8 Comedy-RomCom-3     1    19  black        3     0   0.5     0    NA       
      9 Comedy-RomCom-3     1    19  black        3     0   0.5     0    NA       
        fontface lineheight linetype size
      1        1        1.2        1  0.5
      2        1        1.2        1  0.5
      3        1        1.2        1  0.5
      4        1        1.2        1  0.5
      5        1        1.2        1  0.5
      6        1        1.2        1  0.5
      7        1        1.2        1  0.5
      8        1        1.2        1  0.5
      9        1        1.2        1  0.5
      

---

    Code
      pb$plot$labels
    Output
      $x
      [1] "genre"
      
      $y
      [1] "rating"
      
      $colour
      [1] "genre"
      
      $title
      NULL
      
      $subtitle
      NULL
      
      $caption
      atop(displaystyle(NULL), expr = paste("Pairwise test: ", bold("Dunn test"), 
          "; Comparisons shown: ", bold("all")))
      
      $label
      [1] "label"
      

# check robust test display - FDR-corrected

    Code
      list(pb$data[[6]], pb$data[[7]])
    Output
      [[1]]
        x        y                                 label PANEL group nudge_x  nudge_y
      1 1 14.07937 list(~widehat(mu)[trimmed]=='14.079')     1     1     1.4 14.07937
      2 2 19.43750 list(~widehat(mu)[trimmed]=='19.438')     1     2     2.4 19.43750
      3 3 14.13333 list(~widehat(mu)[trimmed]=='14.133')     1     3     3.4 14.13333
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
      
      [[2]]
        x xend      y   yend                                 annotation group PANEL
      1 1    1 36.915 37.175    list(~italic(p)[Holm-corrected]==0e+00) 4-f-1     1
      2 1    2 37.175 37.175    list(~italic(p)[Holm-corrected]==0e+00) 4-f-1     1
      3 2    2 37.175 36.915    list(~italic(p)[Holm-corrected]==0e+00) 4-f-1     1
      4 2    2 38.865 39.125 list(~italic(p)[Holm-corrected]==6.09e-08) f-r-2     1
      5 2    3 39.125 39.125 list(~italic(p)[Holm-corrected]==6.09e-08) f-r-2     1
      6 3    3 39.125 38.865 list(~italic(p)[Holm-corrected]==6.09e-08) f-r-2     1
        shape colour textsize angle hjust vjust alpha family fontface lineheight
      1    19  black        3     0   0.5     0    NA               1        1.2
      2    19  black        3     0   0.5     0    NA               1        1.2
      3    19  black        3     0   0.5     0    NA               1        1.2
      4    19  black        3     0   0.5     0    NA               1        1.2
      5    19  black        3     0   0.5     0    NA               1        1.2
      6    19  black        3     0   0.5     0    NA               1        1.2
        linetype size
      1        1  0.5
      2        1  0.5
      3        1  0.5
      4        1  0.5
      5        1  0.5
      6        1  0.5
      

---

    Code
      pb$plot$labels
    Output
      $x
      [1] "drv"
      
      $y
      [1] "cty"
      
      $colour
      [1] "drv"
      
      $title
      NULL
      
      $subtitle
      NULL
      
      $caption
      atop(displaystyle(NULL), expr = paste("Pairwise test: ", bold("Yuen's trimmed means test"), 
          "; Comparisons shown: ", bold("only significant")))
      
      $label
      [1] "label"
      

# check student's t test display - FDR-corrected

    Code
      list(pb$data[[6]], pb$data[[7]])
    Output
      [[1]]
        x        y                             label PANEL group nudge_x  nudge_y
      1 1 2.285727 list(~widehat(mu)[mean]=='2.286')     1     1     1.4 2.285727
      2 2 3.117143 list(~widehat(mu)[mean]=='3.117')     1     2     2.4 3.117143
      3 3 3.999214 list(~widehat(mu)[mean]=='3.999')     1     3     3.4 3.999214
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
      
      [[2]]
        x xend        y     yend                                       annotation
      1 1    1 5.716040 5.755150    list(~italic(p)[Bonferroni-corrected]==0.032)
      2 1    2 5.755150 5.755150    list(~italic(p)[Bonferroni-corrected]==0.032)
      3 2    2 5.755150 5.716040    list(~italic(p)[Bonferroni-corrected]==0.032)
      4 1    1 6.009365 6.048475 list(~italic(p)[Bonferroni-corrected]==6.21e-07)
      5 1    3 6.048475 6.048475 list(~italic(p)[Bonferroni-corrected]==6.21e-07)
      6 3    3 6.048475 6.009365 list(~italic(p)[Bonferroni-corrected]==6.21e-07)
      7 2    2 6.302690 6.341800    list(~italic(p)[Bonferroni-corrected]==0.015)
      8 2    3 6.341800 6.341800    list(~italic(p)[Bonferroni-corrected]==0.015)
      9 3    3 6.341800 6.302690    list(~italic(p)[Bonferroni-corrected]==0.015)
        group PANEL shape colour textsize angle hjust vjust alpha family fontface
      1 4-6-1     1    19  black        3     0   0.5     0    NA               1
      2 4-6-1     1    19  black        3     0   0.5     0    NA               1
      3 4-6-1     1    19  black        3     0   0.5     0    NA               1
      4 4-8-2     1    19  black        3     0   0.5     0    NA               1
      5 4-8-2     1    19  black        3     0   0.5     0    NA               1
      6 4-8-2     1    19  black        3     0   0.5     0    NA               1
      7 6-8-3     1    19  black        3     0   0.5     0    NA               1
      8 6-8-3     1    19  black        3     0   0.5     0    NA               1
      9 6-8-3     1    19  black        3     0   0.5     0    NA               1
        lineheight linetype size
      1        1.2        1  0.5
      2        1.2        1  0.5
      3        1.2        1  0.5
      4        1.2        1  0.5
      5        1.2        1  0.5
      6        1.2        1  0.5
      7        1.2        1  0.5
      8        1.2        1  0.5
      9        1.2        1  0.5
      

---

    Code
      pb$plot$labels
    Output
      $x
      [1] "cyl"
      
      $y
      [1] "wt"
      
      $colour
      [1] "cyl"
      
      $title
      NULL
      
      $subtitle
      NULL
      
      $caption
      atop(displaystyle(NULL), expr = paste("Pairwise test: ", bold("Student's t-test"), 
          "; Comparisons shown: ", bold("all")))
      
      $label
      [1] "label"
      

