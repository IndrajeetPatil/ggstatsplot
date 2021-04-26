# ggdotplotstats works as expected

    Code
      within(pb$plot$labels, rm(subtitle, caption))
    Output
      $x
      paste("Speed of light (", italic("c"), ")")
      
      $y
      [1] "Experimental run"
      
      $title
      [1] "Michelson-Morley experiment"
      
      $xintercept
      [1] "xintercept"
      

---

    Code
      within(pb$layout$panel_params[[1]], rm(x, y, x.sec, y.sec))
    Output
      $x.arrange
      [1] "secondary" "primary"  
      
      $x.range
      [1] 816.075 913.425
      
      $y.arrange
      [1] "primary"   "secondary"
      
      $y.range
      [1] 0.8 5.2
      

---

    Code
      pb$data
    Output
      [[1]]
        y     x PANEL group shape colour size fill alpha stroke
      1 1 820.5     1    -1    16  black    3   NA    NA    0.5
      2 2 831.5     1    -1    16  black    3   NA    NA    0.5
      3 3 845.0     1    -1    16  black    3   NA    NA    0.5
      4 4 856.0     1    -1    16  black    3   NA    NA    0.5
      5 5 909.0     1    -1    16  black    3   NA    NA    0.5
      
      [[2]]
        xintercept PANEL group colour size linetype alpha
      1      852.4     1    -1   blue    1   dashed    NA
      

# messing with factors

    Code
      pb1$data
    Output
      [[1]]
        y          x PANEL group shape colour size fill alpha stroke
      1 1 0.02155000     1    -1    16  black    3   NA    NA    0.5
      2 2 0.07925556     1    -1    16  black    3   NA    NA    0.5
      3 3 0.62159750     1    -1    16  black    3   NA    NA    0.5
      
      [[2]]
        xintercept PANEL group colour size linetype alpha
      1   0.240801     1    -1   blue    1   dashed    NA
      

