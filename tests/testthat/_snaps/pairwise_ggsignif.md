# check mcp displays - parametric - significant

    Code
      list(pb$data[[6]], pb$data[[7]])
    Output
      [[1]]
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
      
      [[2]]
         x xend        y     yend                           annotation
      1  1    1 6.083274 6.140393 list(~italic(p)[uncorrected]==0.437)
      2  1    2 6.140393 6.140393 list(~italic(p)[uncorrected]==0.437)
      3  2    2 6.140393 6.083274 list(~italic(p)[uncorrected]==0.437)
      4  1    1 6.425986 6.483105 list(~italic(p)[uncorrected]==0.452)
      5  1    3 6.483105 6.483105 list(~italic(p)[uncorrected]==0.452)
      6  3    3 6.483105 6.425986 list(~italic(p)[uncorrected]==0.452)
      7  1    1 6.768698 6.825816 list(~italic(p)[uncorrected]==0.865)
      8  1    4 6.825816 6.825816 list(~italic(p)[uncorrected]==0.865)
      9  4    4 6.825816 6.768698 list(~italic(p)[uncorrected]==0.865)
      10 2    2 7.111409 7.168528 list(~italic(p)[uncorrected]==0.348)
      11 2    3 7.168528 7.168528 list(~italic(p)[uncorrected]==0.348)
      12 3    3 7.168528 7.111409 list(~italic(p)[uncorrected]==0.348)
      13 2    2 7.454121 7.511239 list(~italic(p)[uncorrected]==0.560)
      14 2    4 7.511239 7.511239 list(~italic(p)[uncorrected]==0.560)
      15 4    4 7.511239 7.454121 list(~italic(p)[uncorrected]==0.560)
      16 3    3 7.796832 7.853951 list(~italic(p)[uncorrected]==0.433)
      17 3    4 7.853951 7.853951 list(~italic(p)[uncorrected]==0.433)
      18 4    4 7.853951 7.796832 list(~italic(p)[uncorrected]==0.433)
                   group PANEL shape colour textsize angle hjust vjust alpha family
      1    carni-herbi-1     1    19  black        3     0   0.5     0    NA       
      2    carni-herbi-1     1    19  black        3     0   0.5     0    NA       
      3    carni-herbi-1     1    19  black        3     0   0.5     0    NA       
      4  carni-insecti-2     1    19  black        3     0   0.5     0    NA       
      5  carni-insecti-2     1    19  black        3     0   0.5     0    NA       
      6  carni-insecti-2     1    19  black        3     0   0.5     0    NA       
      7     carni-omni-3     1    19  black        3     0   0.5     0    NA       
      8     carni-omni-3     1    19  black        3     0   0.5     0    NA       
      9     carni-omni-3     1    19  black        3     0   0.5     0    NA       
      10 herbi-insecti-4     1    19  black        3     0   0.5     0    NA       
      11 herbi-insecti-4     1    19  black        3     0   0.5     0    NA       
      12 herbi-insecti-4     1    19  black        3     0   0.5     0    NA       
      13    herbi-omni-5     1    19  black        3     0   0.5     0    NA       
      14    herbi-omni-5     1    19  black        3     0   0.5     0    NA       
      15    herbi-omni-5     1    19  black        3     0   0.5     0    NA       
      16  insecti-omni-6     1    19  black        3     0   0.5     0    NA       
      17  insecti-omni-6     1    19  black        3     0   0.5     0    NA       
      18  insecti-omni-6     1    19  black        3     0   0.5     0    NA       
         fontface lineheight linetype size
      1         1        1.2        1  0.5
      2         1        1.2        1  0.5
      3         1        1.2        1  0.5
      4         1        1.2        1  0.5
      5         1        1.2        1  0.5
      6         1        1.2        1  0.5
      7         1        1.2        1  0.5
      8         1        1.2        1  0.5
      9         1        1.2        1  0.5
      10        1        1.2        1  0.5
      11        1        1.2        1  0.5
      12        1        1.2        1  0.5
      13        1        1.2        1  0.5
      14        1        1.2        1  0.5
      15        1        1.2        1  0.5
      16        1        1.2        1  0.5
      17        1        1.2        1  0.5
      18        1        1.2        1  0.5
      

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
          bold("Games-Howell test"), "; Comparisons shown: ", bold("only non-significant")))
      
      $label
      [1] "label"
      

# check mcp displays - non-significant

    Code
      list(pb1$data[[6]], pb1$data[[7]], pb2$data[[6]], pb2$data[[7]])
    Output
      [[1]]
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
      
      [[2]]
        x xend        y     yend                           annotation      group
      1 1    1 167852.2 169428.2 list(~italic(p)[uncorrected]==0.139) PG-PG-13-1
      2 1    2 169428.2 169428.2 list(~italic(p)[uncorrected]==0.139) PG-PG-13-1
      3 2    2 169428.2 167852.2 list(~italic(p)[uncorrected]==0.139) PG-PG-13-1
      4 1    1 179672.2 181248.2 list(~italic(p)[uncorrected]==0.825)     PG-R-2
      5 1    3 181248.2 181248.2 list(~italic(p)[uncorrected]==0.825)     PG-R-2
      6 3    3 181248.2 179672.2 list(~italic(p)[uncorrected]==0.825)     PG-R-2
      7 2    2 191492.2 193068.2 list(~italic(p)[uncorrected]==0.079)  PG-13-R-3
      8 2    3 193068.2 193068.2 list(~italic(p)[uncorrected]==0.079)  PG-13-R-3
      9 3    3 193068.2 191492.2 list(~italic(p)[uncorrected]==0.079)  PG-13-R-3
        PANEL shape colour textsize angle hjust vjust alpha family fontface
      1     1    19  black        3     0   0.5     0    NA               1
      2     1    19  black        3     0   0.5     0    NA               1
      3     1    19  black        3     0   0.5     0    NA               1
      4     1    19  black        3     0   0.5     0    NA               1
      5     1    19  black        3     0   0.5     0    NA               1
      6     1    19  black        3     0   0.5     0    NA               1
      7     1    19  black        3     0   0.5     0    NA               1
      8     1    19  black        3     0   0.5     0    NA               1
      9     1    19  black        3     0   0.5     0    NA               1
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
      
      [[3]]
        x         y                                  label PANEL group nudge_x
      1 1  8440.335  list(~widehat(mu)[mean]=='8440.3350')     1     1     1.4
      2 2 11148.255 list(~widehat(mu)[mean]=='11148.2549')     1     2     2.4
      3 3  9243.369  list(~widehat(mu)[mean]=='9243.3687')     1     3     3.4
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
      
      [[4]]
        x xend        y     yend                            annotation      group
      1 1    1 167852.2 169428.2 list(~italic(p)[uncorrected]==0.0467) PG-PG-13-1
      2 1    2 169428.2 169428.2 list(~italic(p)[uncorrected]==0.0467) PG-PG-13-1
      3 2    2 169428.2 167852.2 list(~italic(p)[uncorrected]==0.0467) PG-PG-13-1
      4 2    2 179672.2 181248.2 list(~italic(p)[uncorrected]==0.0354)  PG-13-R-2
      5 2    3 181248.2 181248.2 list(~italic(p)[uncorrected]==0.0354)  PG-13-R-2
      6 3    3 181248.2 179672.2 list(~italic(p)[uncorrected]==0.0354)  PG-13-R-2
        PANEL shape colour textsize angle hjust vjust alpha family fontface
      1     1    19  black        3     0   0.5     0    NA               1
      2     1    19  black        3     0   0.5     0    NA               1
      3     1    19  black        3     0   0.5     0    NA               1
      4     1    19  black        3     0   0.5     0    NA               1
      5     1    19  black        3     0   0.5     0    NA               1
      6     1    19  black        3     0   0.5     0    NA               1
        lineheight linetype size
      1        1.2        1  0.5
      2        1.2        1  0.5
      3        1.2        1  0.5
      4        1.2        1  0.5
      5        1.2        1  0.5
      6        1.2        1  0.5
      

---

    Code
      list(pb1$plot$labels, pb2$plot$labels)
    Output
      [[1]]
      [[1]]$x
      [1] "mpaa"
      
      [[1]]$y
      [1] "votes"
      
      [[1]]$colour
      [1] "mpaa"
      
      [[1]]$title
      NULL
      
      [[1]]$subtitle
      NULL
      
      [[1]]$caption
      atop(displaystyle(NULL), expr = paste("Pairwise test: ", bold("Games-Howell test"), 
          "; Comparisons shown: ", bold("only non-significant")))
      
      [[1]]$label
      [1] "label"
      
      
      [[2]]
      [[2]]$x
      [1] "mpaa"
      
      [[2]]$y
      [1] "votes"
      
      [[2]]$colour
      [1] "mpaa"
      
      [[2]]$title
      NULL
      
      [[2]]$subtitle
      NULL
      
      [[2]]$caption
      atop(displaystyle(NULL), expr = paste("Pairwise test: ", bold("Student's t-test"), 
          "; Comparisons shown: ", bold("only significant")))
      
      [[2]]$label
      [1] "label"
      
      

# check mixed comparison displays - nonparametric

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
      

# check bayesian test display

    Code
      list(pb$data[[6]], pb$data[[7]])
    Output
      [[1]]
        x        y                            label PANEL group nudge_x  nudge_y
      1 1 2.093311 list(~widehat(mu)[MAP]=='2.093')     1     1     1.4 2.093311
      2 2 3.426334 list(~widehat(mu)[MAP]=='3.426')     1     2     2.4 3.426334
      3 3 3.626088 list(~widehat(mu)[MAP]=='3.626')     1     3     3.4 3.626088
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
        x xend        y     yend                      annotation group PANEL shape
      1 1    1 5.716040 5.755150 list(~log[e](BF['01'])==-2.437) 4-6-1     1    19
      2 1    2 5.755150 5.755150 list(~log[e](BF['01'])==-2.437) 4-6-1     1    19
      3 2    2 5.755150 5.716040 list(~log[e](BF['01'])==-2.437) 4-6-1     1    19
      4 1    1 6.009365 6.048475 list(~log[e](BF['01'])==-8.561) 4-8-2     1    19
      5 1    3 6.048475 6.048475 list(~log[e](BF['01'])==-8.561) 4-8-2     1    19
      6 3    3 6.048475 6.009365 list(~log[e](BF['01'])==-8.561) 4-8-2     1    19
      7 2    2 6.302690 6.341800 list(~log[e](BF['01'])==-1.679) 6-8-3     1    19
      8 2    3 6.341800 6.341800 list(~log[e](BF['01'])==-1.679) 6-8-3     1    19
      9 3    3 6.341800 6.302690 list(~log[e](BF['01'])==-1.679) 6-8-3     1    19
        colour textsize angle hjust vjust alpha family fontface lineheight linetype
      1  black        3     0   0.5     0    NA               1        1.2        1
      2  black        3     0   0.5     0    NA               1        1.2        1
      3  black        3     0   0.5     0    NA               1        1.2        1
      4  black        3     0   0.5     0    NA               1        1.2        1
      5  black        3     0   0.5     0    NA               1        1.2        1
      6  black        3     0   0.5     0    NA               1        1.2        1
      7  black        3     0   0.5     0    NA               1        1.2        1
      8  black        3     0   0.5     0    NA               1        1.2        1
      9  black        3     0   0.5     0    NA               1        1.2        1
        size
      1  0.5
      2  0.5
      3  0.5
      4  0.5
      5  0.5
      6  0.5
      7  0.5
      8  0.5
      9  0.5
      

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
      NULL
      
      $label
      [1] "label"
      

