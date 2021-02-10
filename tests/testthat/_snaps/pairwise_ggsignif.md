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

# check mixed comparison displays - FDR-corrected

    Code
      pb$data[[6]]
    Output
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

# check robust test display - FDR-corrected

    Code
      pb$data[[6]]
    Output
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

# check student's t test display - FDR-corrected

    Code
      pb$data[[6]]
    Output
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

