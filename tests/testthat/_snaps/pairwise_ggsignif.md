# check mcp displays - parametric - significant

    Code
      list(pb$data[[6]], pb$data[[7]])
    Output
      [[1]]
        x          y                      label PANEL group nudge_x    nudge_y colour
      1 1 0.07925556 widehat(mu)[mean]=='0.079'     1     1     1.4 0.07925556  black
      2 2 0.62159750 widehat(mu)[mean]=='0.622'     1     2     2.4 0.62159750  black
      3 3 0.02155000 widehat(mu)[mean]=='0.022'     1     3     3.4 0.02155000  black
      4 4 0.14573118 widehat(mu)[mean]=='0.146'     1     4     4.4 0.14573118  black
         fill size angle alpha family fontface lineheight hjust vjust point.size
      1 white    3     0    NA               1        1.2   0.5   0.5          1
      2 white    3     0    NA               1        1.2   0.5   0.5          1
      3 white    3     0    NA               1        1.2   0.5   0.5          1
      4 white    3     0    NA               1        1.2   0.5   0.5          1
        segment.linetype segment.size segment.curvature segment.angle segment.ncp
      1                4          0.5                 0            90           1
      2                4          0.5                 0            90           1
      3                4          0.5                 0            90           1
      4                4          0.5                 0            90           1
        segment.shape segment.square segment.squareShape segment.inflect
      1           0.5           TRUE                   1           FALSE
      2           0.5           TRUE                   1           FALSE
      3           0.5           TRUE                   1           FALSE
      4           0.5           TRUE                   1           FALSE
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
                   group flipped_aes PANEL shape colour textsize angle hjust vjust
      1    carni-herbi-1       FALSE     1    19  black        3     0   0.5     0
      2    carni-herbi-1       FALSE     1    19  black        3     0   0.5     0
      3    carni-herbi-1       FALSE     1    19  black        3     0   0.5     0
      4  carni-insecti-2       FALSE     1    19  black        3     0   0.5     0
      5  carni-insecti-2       FALSE     1    19  black        3     0   0.5     0
      6  carni-insecti-2       FALSE     1    19  black        3     0   0.5     0
      7     carni-omni-3       FALSE     1    19  black        3     0   0.5     0
      8     carni-omni-3       FALSE     1    19  black        3     0   0.5     0
      9     carni-omni-3       FALSE     1    19  black        3     0   0.5     0
      10 herbi-insecti-4       FALSE     1    19  black        3     0   0.5     0
      11 herbi-insecti-4       FALSE     1    19  black        3     0   0.5     0
      12 herbi-insecti-4       FALSE     1    19  black        3     0   0.5     0
      13    herbi-omni-5       FALSE     1    19  black        3     0   0.5     0
      14    herbi-omni-5       FALSE     1    19  black        3     0   0.5     0
      15    herbi-omni-5       FALSE     1    19  black        3     0   0.5     0
      16  insecti-omni-6       FALSE     1    19  black        3     0   0.5     0
      17  insecti-omni-6       FALSE     1    19  black        3     0   0.5     0
      18  insecti-omni-6       FALSE     1    19  black        3     0   0.5     0
         alpha family fontface lineheight linetype size
      1     NA               1        1.2        1  0.5
      2     NA               1        1.2        1  0.5
      3     NA               1        1.2        1  0.5
      4     NA               1        1.2        1  0.5
      5     NA               1        1.2        1  0.5
      6     NA               1        1.2        1  0.5
      7     NA               1        1.2        1  0.5
      8     NA               1        1.2        1  0.5
      9     NA               1        1.2        1  0.5
      10    NA               1        1.2        1  0.5
      11    NA               1        1.2        1  0.5
      12    NA               1        1.2        1  0.5
      13    NA               1        1.2        1  0.5
      14    NA               1        1.2        1  0.5
      15    NA               1        1.2        1  0.5
      16    NA               1        1.2        1  0.5
      17    NA               1        1.2        1  0.5
      18    NA               1        1.2        1  0.5
      

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
      expression(atop(displaystyle("mammalian sleep"), list("Pairwise test:" ~ 
          bold("Games-Howell test"), "Comparisons shown:" ~ bold("only non-significant"))))
      
      $label
      [1] "expression"
      
      $alt
      [1] ""
      

# check mcp displays - non-significant

    Code
      list(pb1$data[[6]], pb1$data[[7]], pb2$data[[6]], pb2$data[[7]])
    Output
      [[1]]
        x         y                          label PANEL group nudge_x   nudge_y
      1 1  8440.335  widehat(mu)[mean]=='8440.335'     1     1     1.4  8440.335
      2 2 11148.255 widehat(mu)[mean]=='11148.255'     1     2     2.4 11148.255
      3 3  9243.369  widehat(mu)[mean]=='9243.369'     1     3     3.4  9243.369
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
        flipped_aes PANEL shape colour textsize angle hjust vjust alpha family
      1       FALSE     1    19  black        3     0   0.5     0    NA       
      2       FALSE     1    19  black        3     0   0.5     0    NA       
      3       FALSE     1    19  black        3     0   0.5     0    NA       
      4       FALSE     1    19  black        3     0   0.5     0    NA       
      5       FALSE     1    19  black        3     0   0.5     0    NA       
      6       FALSE     1    19  black        3     0   0.5     0    NA       
      7       FALSE     1    19  black        3     0   0.5     0    NA       
      8       FALSE     1    19  black        3     0   0.5     0    NA       
      9       FALSE     1    19  black        3     0   0.5     0    NA       
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
      
      [[3]]
        x         y                           label PANEL group nudge_x   nudge_y
      1 1  8440.335  widehat(mu)[mean]=='8440.3350'     1     1     1.4  8440.335
      2 2 11148.255 widehat(mu)[mean]=='11148.2549'     1     2     2.4 11148.255
      3 3  9243.369  widehat(mu)[mean]=='9243.3687'     1     3     3.4  9243.369
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
      
      [[4]]
        x xend        y     yend                            annotation      group
      1 1    1 167852.2 169428.2 list(~italic(p)[uncorrected]==0.0467) PG-PG-13-1
      2 1    2 169428.2 169428.2 list(~italic(p)[uncorrected]==0.0467) PG-PG-13-1
      3 2    2 169428.2 167852.2 list(~italic(p)[uncorrected]==0.0467) PG-PG-13-1
      4 2    2 179672.2 181248.2 list(~italic(p)[uncorrected]==0.0354)  PG-13-R-2
      5 2    3 181248.2 181248.2 list(~italic(p)[uncorrected]==0.0354)  PG-13-R-2
      6 3    3 181248.2 179672.2 list(~italic(p)[uncorrected]==0.0354)  PG-13-R-2
        flipped_aes PANEL shape colour textsize angle hjust vjust alpha family
      1       FALSE     1    19  black        3     0   0.5     0    NA       
      2       FALSE     1    19  black        3     0   0.5     0    NA       
      3       FALSE     1    19  black        3     0   0.5     0    NA       
      4       FALSE     1    19  black        3     0   0.5     0    NA       
      5       FALSE     1    19  black        3     0   0.5     0    NA       
      6       FALSE     1    19  black        3     0   0.5     0    NA       
        fontface lineheight linetype size
      1        1        1.2        1  0.5
      2        1        1.2        1  0.5
      3        1        1.2        1  0.5
      4        1        1.2        1  0.5
      5        1        1.2        1  0.5
      6        1        1.2        1  0.5
      

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
      expression(list("Pairwise test:" ~ bold("Games-Howell test"), 
          "Comparisons shown:" ~ bold("only non-significant")))
      
      [[1]]$label
      [1] "expression"
      
      [[1]]$alt
      [1] ""
      
      
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
      expression(list("Pairwise test:" ~ bold("Student's t-test"), 
          "Comparisons shown:" ~ bold("only significant")))
      
      [[2]]$label
      [1] "expression"
      
      [[2]]$alt
      [1] ""
      
      

# check mixed comparison displays - nonparametric

    Code
      list(pb$data[[6]], pb$data[[7]])
    Output
      [[1]]
        x   y                        label PANEL group nudge_x nudge_y colour  fill
      1 1 5.5 widehat(mu)[median]=='5.500'     1     1     1.4     5.5  black white
      2 2 5.5 widehat(mu)[median]=='5.500'     1     2     2.4     5.5  black white
      3 3 5.9 widehat(mu)[median]=='5.900'     1     3     3.4     5.9  black white
        size angle alpha family fontface lineheight hjust vjust point.size
      1    3     0    NA               1        1.2   0.5   0.5          1
      2    3     0    NA               1        1.2   0.5   0.5          1
      3    3     0    NA               1        1.2   0.5   0.5          1
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
        x xend       y    yend                                 annotation
      1 1    1  9.5170  9.5900     list(~italic(p)[FDR-corrected]==0.812)
      2 1    2  9.5900  9.5900     list(~italic(p)[FDR-corrected]==0.812)
      3 2    2  9.5900  9.5170     list(~italic(p)[FDR-corrected]==0.812)
      4 1    1 10.0645 10.1375 list(~italic(p)[FDR-corrected]==4.179e-04)
      5 1    3 10.1375 10.1375 list(~italic(p)[FDR-corrected]==4.179e-04)
      6 3    3 10.1375 10.0645 list(~italic(p)[FDR-corrected]==4.179e-04)
      7 2    2 10.6120 10.6850 list(~italic(p)[FDR-corrected]==4.179e-04)
      8 2    3 10.6850 10.6850 list(~italic(p)[FDR-corrected]==4.179e-04)
      9 3    3 10.6850 10.6120 list(~italic(p)[FDR-corrected]==4.179e-04)
                  group flipped_aes PANEL shape colour textsize angle hjust vjust
      1 Action-Comedy-1       FALSE     1    19  black        3     0   0.5     0
      2 Action-Comedy-1       FALSE     1    19  black        3     0   0.5     0
      3 Action-Comedy-1       FALSE     1    19  black        3     0   0.5     0
      4 Action-RomCom-2       FALSE     1    19  black        3     0   0.5     0
      5 Action-RomCom-2       FALSE     1    19  black        3     0   0.5     0
      6 Action-RomCom-2       FALSE     1    19  black        3     0   0.5     0
      7 Comedy-RomCom-3       FALSE     1    19  black        3     0   0.5     0
      8 Comedy-RomCom-3       FALSE     1    19  black        3     0   0.5     0
      9 Comedy-RomCom-3       FALSE     1    19  black        3     0   0.5     0
        alpha family fontface lineheight linetype size
      1    NA               1        1.2        1  0.5
      2    NA               1        1.2        1  0.5
      3    NA               1        1.2        1  0.5
      4    NA               1        1.2        1  0.5
      5    NA               1        1.2        1  0.5
      6    NA               1        1.2        1  0.5
      7    NA               1        1.2        1  0.5
      8    NA               1        1.2        1  0.5
      9    NA               1        1.2        1  0.5
      

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
      expression(list("Pairwise test:" ~ bold("Dunn test"), "Comparisons shown:" ~ 
          bold("all")))
      
      $label
      [1] "expression"
      
      $alt
      [1] ""
      

# check robust test display - FDR-corrected

    Code
      list(pb$data[[6]], pb$data[[7]])
    Output
      [[1]]
        x        y                          label PANEL group nudge_x  nudge_y colour
      1 1 14.07937 widehat(mu)[trimmed]=='14.079'     1     1     1.4 14.07937  black
      2 2 19.43750 widehat(mu)[trimmed]=='19.438'     1     2     2.4 19.43750  black
      3 3 14.13333 widehat(mu)[trimmed]=='14.133'     1     3     3.4 14.13333  black
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
        x xend      y   yend                                  annotation group
      1 1    1 36.915 37.175     list(~italic(p)[Holm-corrected]==0.000) 4-f-1
      2 1    2 37.175 37.175     list(~italic(p)[Holm-corrected]==0.000) 4-f-1
      3 2    2 37.175 36.915     list(~italic(p)[Holm-corrected]==0.000) 4-f-1
      4 2    2 38.865 39.125 list(~italic(p)[Holm-corrected]==3.045e-08) f-r-2
      5 2    3 39.125 39.125 list(~italic(p)[Holm-corrected]==3.045e-08) f-r-2
      6 3    3 39.125 38.865 list(~italic(p)[Holm-corrected]==3.045e-08) f-r-2
        flipped_aes PANEL shape colour textsize angle hjust vjust alpha family
      1       FALSE     1    19  black        3     0   0.5     0    NA       
      2       FALSE     1    19  black        3     0   0.5     0    NA       
      3       FALSE     1    19  black        3     0   0.5     0    NA       
      4       FALSE     1    19  black        3     0   0.5     0    NA       
      5       FALSE     1    19  black        3     0   0.5     0    NA       
      6       FALSE     1    19  black        3     0   0.5     0    NA       
        fontface lineheight linetype size
      1        1        1.2        1  0.5
      2        1        1.2        1  0.5
      3        1        1.2        1  0.5
      4        1        1.2        1  0.5
      5        1        1.2        1  0.5
      6        1        1.2        1  0.5
      

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
      expression(list("Pairwise test:" ~ bold("Yuen's trimmed means test"), 
          "Comparisons shown:" ~ bold("only significant")))
      
      $label
      [1] "expression"
      
      $alt
      [1] ""
      

# check bayesian test display

    Code
      list(pb$data[[6]], pb$data[[7]])
    Output
      [[1]]
        x        y                     label PANEL group nudge_x  nudge_y colour
      1 1 5.021408 widehat(mu)[MAP]=='5.021'     1     1     1.4 5.021408  black
      2 2 5.747801 widehat(mu)[MAP]=='5.748'     1     2     2.4 5.747801  black
      3 3 6.398534 widehat(mu)[MAP]=='6.399'     1     3     3.4 6.398534  black
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
        x xend      y   yend                       annotation                  group
      1 1    1 8.2415 8.2775 list(~log[e](BF['01'])==-33.669)    setosa-versicolor-1
      2 1    2 8.2775 8.2775 list(~log[e](BF['01'])==-33.669)    setosa-versicolor-1
      3 2    2 8.2775 8.2415 list(~log[e](BF['01'])==-33.669)    setosa-versicolor-1
      4 1    1 8.5115 8.5475 list(~log[e](BF['01'])==-56.343)     setosa-virginica-2
      5 1    3 8.5475 8.5475 list(~log[e](BF['01'])==-56.343)     setosa-virginica-2
      6 3    3 8.5475 8.5115 list(~log[e](BF['01'])==-56.343)     setosa-virginica-2
      7 2    2 8.7815 8.8175 list(~log[e](BF['01'])==-11.162) versicolor-virginica-3
      8 2    3 8.8175 8.8175 list(~log[e](BF['01'])==-11.162) versicolor-virginica-3
      9 3    3 8.8175 8.7815 list(~log[e](BF['01'])==-11.162) versicolor-virginica-3
        flipped_aes PANEL shape colour textsize angle hjust vjust alpha family
      1       FALSE     1    19  black        3     0   0.5     0    NA       
      2       FALSE     1    19  black        3     0   0.5     0    NA       
      3       FALSE     1    19  black        3     0   0.5     0    NA       
      4       FALSE     1    19  black        3     0   0.5     0    NA       
      5       FALSE     1    19  black        3     0   0.5     0    NA       
      6       FALSE     1    19  black        3     0   0.5     0    NA       
      7       FALSE     1    19  black        3     0   0.5     0    NA       
      8       FALSE     1    19  black        3     0   0.5     0    NA       
      9       FALSE     1    19  black        3     0   0.5     0    NA       
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
      [1] "Species"
      
      $y
      [1] "Sepal.Length"
      
      $colour
      [1] "Species"
      
      $title
      NULL
      
      $subtitle
      NULL
      
      $caption
      expression(list("Pairwise test:" ~ bold("Student's t-test"), 
          "Comparisons shown:" ~ bold("all")))
      
      $label
      [1] "expression"
      
      $alt
      [1] ""
      

# additional test

    Code
      length(pb1$data)
    Output
      [1] 6

---

    Code
      list(pb2$data[[6]], pb2$data[[7]])
    Output
      [[1]]
        x        y                       label PANEL group nudge_x  nudge_y colour
      1 1 116.2667 widehat(mu)[mean]=='116.27'     1     1     1.4 116.2667  black
      2 2 116.6944 widehat(mu)[mean]=='116.69'     1     2     2.4 116.6944  black
      3 3 102.3838 widehat(mu)[mean]=='102.38'     1     3     3.4 102.3838  black
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
        x xend       y    yend                                 annotation     group
      1 2    2 264.115 265.825 list(~italic(p)[Holm-corrected]==1.27e-03) PG-13-R-1
      2 2    3 265.825 265.825 list(~italic(p)[Holm-corrected]==1.27e-03) PG-13-R-1
      3 3    3 265.825 264.115 list(~italic(p)[Holm-corrected]==1.27e-03) PG-13-R-1
        flipped_aes PANEL shape colour textsize angle hjust vjust alpha family
      1       FALSE     1    19  black        3     0   0.5     0    NA       
      2       FALSE     1    19  black        3     0   0.5     0    NA       
      3       FALSE     1    19  black        3     0   0.5     0    NA       
        fontface lineheight linetype size
      1        1        1.2        1  0.5
      2        1        1.2        1  0.5
      3        1        1.2        1  0.5
      

---

    Code
      pb1$plot$labels
    Output
      $x
      [1] "mpaa"
      
      $y
      [1] "length"
      
      $colour
      [1] "mpaa"
      
      $title
      NULL
      
      $subtitle
      NULL
      
      $caption
      expression(list("Pairwise test:" ~ bold("Games-Howell test"), 
          "Comparisons shown:" ~ bold("only significant")))
      
      $label
      [1] "expression"
      
      $alt
      [1] ""
      

---

    Code
      pb2$plot$labels
    Output
      $x
      [1] "mpaa"
      
      $y
      [1] "length"
      
      $colour
      [1] "mpaa"
      
      $title
      NULL
      
      $subtitle
      NULL
      
      $caption
      expression(list("Pairwise test:" ~ bold("Games-Howell test"), 
          "Comparisons shown:" ~ bold("only significant")))
      
      $label
      [1] "expression"
      
      $alt
      [1] ""
      

