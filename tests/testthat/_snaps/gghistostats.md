# checking gghistostats plot and parametric stats - data with NAs

    Code
      pb$data
    Output
      [[1]]
           fill  y count   x xmin xmax     density  ncount ndensity flipped_aes PANEL
      1  orange  1     1  60   50   70 0.000617284 0.03125  0.03125       FALSE     1
      2  orange  2     2  80   70   90 0.001234568 0.06250  0.06250       FALSE     1
      3  orange  4     4 100   90  110 0.002469136 0.12500  0.12500       FALSE     1
      4  orange  2     2 120  110  130 0.001234568 0.06250  0.06250       FALSE     1
      5  orange  3     3 140  130  150 0.001851852 0.09375  0.09375       FALSE     1
      6  orange 15    15 160  150  170 0.009259259 0.46875  0.46875       FALSE     1
      7  orange 32    32 180  170  190 0.019753086 1.00000  1.00000       FALSE     1
      8  orange 15    15 200  190  210 0.009259259 0.46875  0.46875       FALSE     1
      9  orange  5     5 220  210  230 0.003086420 0.15625  0.15625       FALSE     1
      10 orange  1     1 240  230  250 0.000617284 0.03125  0.03125       FALSE     1
      11 orange  1     1 260  250  270 0.000617284 0.03125  0.03125       FALSE     1
         group ymin ymax colour size linetype alpha
      1     -1    0    1  black  0.5        1   0.7
      2     -1    0    2  black  0.5        1   0.7
      3     -1    0    4  black  0.5        1   0.7
      4     -1    0    2  black  0.5        1   0.7
      5     -1    0    3  black  0.5        1   0.7
      6     -1    0   15  black  0.5        1   0.7
      7     -1    0   32  black  0.5        1   0.7
      8     -1    0   15  black  0.5        1   0.7
      9     -1    0    5  black  0.5        1   0.7
      10    -1    0    1  black  0.5        1   0.7
      11    -1    0    1  black  0.5        1   0.7
      
      [[2]]
        xintercept PANEL group colour size linetype alpha
      1    174.358     1    -1   blue    1   dashed    NA
      

---

    Code
      within(pb$plot$labels, rm(subtitle, caption))
    Output
      $x
      [1] "character height"
      
      $y
      [1] "count"
      
      $title
      [1] "starwars: character heights"
      
      $fill
      [1] "count"
      
      $xintercept
      [1] "xintercept"
      
      $weight
      [1] "weight"
      attr(,"fallback")
      [1] TRUE
      
      $alt
      [1] ""
      

# checking gghistostats and non-parametric stats - data without NAs

    Code
      pb$data
    Output
      [[1]]
          fill  y count  x xmin xmax     density     ncount   ndensity flipped_aes
      1 grey50 33    33 10  7.5 12.5 0.028205128 0.33333333 0.33333333       FALSE
      2 grey50 99    99 15 12.5 17.5 0.084615385 1.00000000 1.00000000       FALSE
      3 grey50 84    84 20 17.5 22.5 0.071794872 0.84848485 0.84848485       FALSE
      4 grey50 13    13 25 22.5 27.5 0.011111111 0.13131313 0.13131313       FALSE
      5 grey50  3     3 30 27.5 32.5 0.002564103 0.03030303 0.03030303       FALSE
      6 grey50  2     2 35 32.5 37.5 0.001709402 0.02020202 0.02020202       FALSE
        PANEL group ymin ymax colour size linetype alpha
      1     1    -1    0   33  black  0.5        1   0.7
      2     1    -1    0   99  black  0.5        1   0.7
      3     1    -1    0   84  black  0.5        1   0.7
      4     1    -1    0   13  black  0.5        1   0.7
      5     1    -1    0    3  black  0.5        1   0.7
      6     1    -1    0    2  black  0.5        1   0.7
      
      [[2]]
        xintercept PANEL group colour size linetype alpha
      1         17     1    -1   blue    1   dashed    NA
      

---

    Code
      pb$layout$panel_params[[1]]$y.sec$break_info
    Output
      $range
      [1] -0.02115385  0.44423077
      
      $labels
      [1] "0%"  "10%" "20%" "30%" "40%"
      
      $major
      [1] 0.045 0.260 0.475 0.690 0.905
      
      $minor
      [1] 0.045 0.153 0.260 0.367 0.475 0.583 0.690 0.798 0.905
      
      $major_source
      [1] -0.04459459 23.39234234 46.82927928 70.15720721 93.59414414
      
      $minor_source
      [1] -0.04459459 11.72837838 23.39234234 35.05630631 46.82927928 58.49324324
      [7] 70.15720721 81.93018018 93.59414414
      
      $major_source_user
      [1] 0.0 0.1 0.2 0.3 0.4
      
      $minor_source_user
      [1] 0.00 0.05 0.10 0.15 0.20 0.25 0.30 0.35 0.40
      

---

    Code
      pb$plot$labels
    Output
      $x
      [1] "city miles per gallon"
      
      $y
      [1] "count"
      
      $title
      [1] "fuel economy"
      
      $subtitle
      NULL
      
      $caption
      [1] "source: government website"
      
      $fill
      [1] "count"
      
      $xintercept
      [1] "xintercept"
      
      $weight
      [1] "weight"
      attr(,"fallback")
      [1] TRUE
      
      $alt
      [1] ""
      

# checking robust stats and proportions

    Code
      pb$data
    Output
      [[1]]
           fill y count   x xmin xmax density    ncount  ndensity flipped_aes PANEL
      1  grey50 0     0 1.0   NA 1.25  0.0000 0.0000000 0.0000000       FALSE     1
      2  grey50 2     2 1.5 1.25 1.75  0.1250 0.2222222 0.2222222       FALSE     1
      3  grey50 4     4 2.0 1.75 2.25  0.2500 0.4444444 0.4444444       FALSE     1
      4  grey50 3     3 2.5 2.25 2.75  0.1875 0.3333333 0.3333333       FALSE     1
      5  grey50 7     7 3.0 2.75 3.25  0.4375 0.7777778 0.7777778       FALSE     1
      6  grey50 9     9 3.5 3.25 3.75  0.5625 1.0000000 1.0000000       FALSE     1
      7  grey50 4     4 4.0 3.75 4.25  0.2500 0.4444444 0.4444444       FALSE     1
      8  grey50 0     0 4.5 4.25 4.75  0.0000 0.0000000 0.0000000       FALSE     1
      9  grey50 1     1 5.0 4.75 5.25  0.0625 0.1111111 0.1111111       FALSE     1
      10 grey50 2     2 5.5 5.25 5.75  0.1250 0.2222222 0.2222222       FALSE     1
      11 grey50 0     0 6.0 5.75   NA  0.0000 0.0000000 0.0000000       FALSE     1
         group ymin ymax colour size linetype alpha
      1     -1    0    0  black  0.5        1   0.7
      2     -1    0    2  black  0.5        1   0.7
      3     -1    0    4  black  0.5        1   0.7
      4     -1    0    3  black  0.5        1   0.7
      5     -1    0    7  black  0.5        1   0.7
      6     -1    0    9  black  0.5        1   0.7
      7     -1    0    4  black  0.5        1   0.7
      8     -1    0    0  black  0.5        1   0.7
      9     -1    0    1  black  0.5        1   0.7
      10    -1    0    2  black  0.5        1   0.7
      11    -1    0    0  black  0.5        1   0.7
      
      [[2]]
        xintercept PANEL group colour size linetype alpha
      1      3.197     1    -1   blue    1   dashed    NA
      

---

    Code
      within(pb$plot$labels, rm(subtitle))
    Output
      $x
      [1] "wt"
      
      $y
      [1] "count"
      
      $title
      NULL
      
      $caption
      NULL
      
      $fill
      [1] "count"
      
      $xintercept
      [1] "xintercept"
      
      $weight
      [1] "weight"
      attr(,"fallback")
      [1] TRUE
      
      $alt
      [1] ""
      

# checking if normal curve work

    Code
      pb1$data
    Output
      [[1]]
           fill  y count  x xmin xmax    density     ncount   ndensity flipped_aes
      1  grey50  2     2  4  3.5  4.5 0.02409639 0.16666667 0.16666667       FALSE
      2  grey50  1     1  5  4.5  5.5 0.01204819 0.08333333 0.08333333       FALSE
      3  grey50  2     2  6  5.5  6.5 0.02409639 0.16666667 0.16666667       FALSE
      4  grey50  3     3  7  6.5  7.5 0.03614458 0.25000000 0.25000000       FALSE
      5  grey50  4     4  8  7.5  8.5 0.04819277 0.33333333 0.33333333       FALSE
      6  grey50  4     4  9  8.5  9.5 0.04819277 0.33333333 0.33333333       FALSE
      7  grey50  7     7 10  9.5 10.5 0.08433735 0.58333333 0.58333333       FALSE
      8  grey50  7     7 11 10.5 11.5 0.08433735 0.58333333 0.58333333       FALSE
      9  grey50  2     2 12 11.5 12.5 0.02409639 0.16666667 0.16666667       FALSE
      10 grey50  6     6 13 12.5 13.5 0.07228916 0.50000000 0.50000000       FALSE
      11 grey50 12    12 14 13.5 14.5 0.14457831 1.00000000 1.00000000       FALSE
      12 grey50  8     8 15 14.5 15.5 0.09638554 0.66666667 0.66666667       FALSE
      13 grey50  5     5 16 15.5 16.5 0.06024096 0.41666667 0.41666667       FALSE
      14 grey50  1     1 17 16.5 17.5 0.01204819 0.08333333 0.08333333       FALSE
      15 grey50  4     4 18 17.5 18.5 0.04819277 0.33333333 0.33333333       FALSE
      16 grey50  4     4 19 18.5 19.5 0.04819277 0.33333333 0.33333333       FALSE
      17 grey50  5     5 20 19.5 20.5 0.06024096 0.41666667 0.41666667       FALSE
      18 grey50  5     5 21 20.5 21.5 0.06024096 0.41666667 0.41666667       FALSE
      19 grey50  1     1 22 21.5 22.5 0.01204819 0.08333333 0.08333333       FALSE
         PANEL group ymin ymax colour size linetype alpha
      1      1    -1    0    2  black  0.5        1   0.7
      2      1    -1    0    1  black  0.5        1   0.7
      3      1    -1    0    2  black  0.5        1   0.7
      4      1    -1    0    3  black  0.5        1   0.7
      5      1    -1    0    4  black  0.5        1   0.7
      6      1    -1    0    4  black  0.5        1   0.7
      7      1    -1    0    7  black  0.5        1   0.7
      8      1    -1    0    7  black  0.5        1   0.7
      9      1    -1    0    2  black  0.5        1   0.7
      10     1    -1    0    6  black  0.5        1   0.7
      11     1    -1    0   12  black  0.5        1   0.7
      12     1    -1    0    8  black  0.5        1   0.7
      13     1    -1    0    5  black  0.5        1   0.7
      14     1    -1    0    1  black  0.5        1   0.7
      15     1    -1    0    4  black  0.5        1   0.7
      16     1    -1    0    4  black  0.5        1   0.7
      17     1    -1    0    5  black  0.5        1   0.7
      18     1    -1    0    5  black  0.5        1   0.7
      19     1    -1    0    1  black  0.5        1   0.7
      
      [[2]]
              x         y PANEL group colour size linetype alpha
      1    4.10 0.7752851     1    -1    red  0.8        1    NA
      2    4.28 0.8442004     1    -1    red  0.8        1    NA
      3    4.46 0.9177403     1    -1    red  0.8        1    NA
      4    4.64 0.9960569     1    -1    red  0.8        1    NA
      5    4.82 1.0792910     1    -1    red  0.8        1    NA
      6    5.00 1.1675703     1    -1    red  0.8        1    NA
      7    5.18 1.2610075     1    -1    red  0.8        1    NA
      8    5.36 1.3596977     1    -1    red  0.8        1    NA
      9    5.54 1.4637170     1    -1    red  0.8        1    NA
      10   5.72 1.5731206     1    -1    red  0.8        1    NA
      11   5.90 1.6879399     1    -1    red  0.8        1    NA
      12   6.08 1.8081815     1    -1    red  0.8        1    NA
      13   6.26 1.9338250     1    -1    red  0.8        1    NA
      14   6.44 2.0648210     1    -1    red  0.8        1    NA
      15   6.62 2.2010897     1    -1    red  0.8        1    NA
      16   6.80 2.3425193     1    -1    red  0.8        1    NA
      17   6.98 2.4889644     1    -1    red  0.8        1    NA
      18   7.16 2.6402454     1    -1    red  0.8        1    NA
      19   7.34 2.7961469     1    -1    red  0.8        1    NA
      20   7.52 2.9564176     1    -1    red  0.8        1    NA
      21   7.70 3.1207693     1    -1    red  0.8        1    NA
      22   7.88 3.2888770     1    -1    red  0.8        1    NA
      23   8.06 3.4603792     1    -1    red  0.8        1    NA
      24   8.24 3.6348781     1    -1    red  0.8        1    NA
      25   8.42 3.8119404     1    -1    red  0.8        1    NA
      26   8.60 3.9910984     1    -1    red  0.8        1    NA
      27   8.78 4.1718518     1    -1    red  0.8        1    NA
      28   8.96 4.3536688     1    -1    red  0.8        1    NA
      29   9.14 4.5359891     1    -1    red  0.8        1    NA
      30   9.32 4.7182256     1    -1    red  0.8        1    NA
      31   9.50 4.8997678     1    -1    red  0.8        1    NA
      32   9.68 5.0799845     1    -1    red  0.8        1    NA
      33   9.86 5.2582274     1    -1    red  0.8        1    NA
      34  10.04 5.4338349     1    -1    red  0.8        1    NA
      35  10.22 5.6061356     1    -1    red  0.8        1    NA
      36  10.40 5.7744530     1    -1    red  0.8        1    NA
      37  10.58 5.9381094     1    -1    red  0.8        1    NA
      38  10.76 6.0964306     1    -1    red  0.8        1    NA
      39  10.94 6.2487502     1    -1    red  0.8        1    NA
      40  11.12 6.3944144     1    -1    red  0.8        1    NA
      41  11.30 6.5327869     1    -1    red  0.8        1    NA
      42  11.48 6.6632528     1    -1    red  0.8        1    NA
      43  11.66 6.7852239     1    -1    red  0.8        1    NA
      44  11.84 6.8981427     1    -1    red  0.8        1    NA
      45  12.02 7.0014864     1    -1    red  0.8        1    NA
      46  12.20 7.0947716     1    -1    red  0.8        1    NA
      47  12.38 7.1775574     1    -1    red  0.8        1    NA
      48  12.56 7.2494495     1    -1    red  0.8        1    NA
      49  12.74 7.3101025     1    -1    red  0.8        1    NA
      50  12.92 7.3592237     1    -1    red  0.8        1    NA
      51  13.10 7.3965744     1    -1    red  0.8        1    NA
      52  13.28 7.4219726     1    -1    red  0.8        1    NA
      53  13.46 7.4352942     1    -1    red  0.8        1    NA
      54  13.64 7.4364738     1    -1    red  0.8        1    NA
      55  13.82 7.4255059     1    -1    red  0.8        1    NA
      56  14.00 7.4024440     1    -1    red  0.8        1    NA
      57  14.18 7.3674009     1    -1    red  0.8        1    NA
      58  14.36 7.3205476     1    -1    red  0.8        1    NA
      59  14.54 7.2621118     1    -1    red  0.8        1    NA
      60  14.72 7.1923759     1    -1    red  0.8        1    NA
      61  14.90 7.1116753     1    -1    red  0.8        1    NA
      62  15.08 7.0203950     1    -1    red  0.8        1    NA
      63  15.26 6.9189672     1    -1    red  0.8        1    NA
      64  15.44 6.8078674     1    -1    red  0.8        1    NA
      65  15.62 6.6876109     1    -1    red  0.8        1    NA
      66  15.80 6.5587487     1    -1    red  0.8        1    NA
      67  15.98 6.4218637     1    -1    red  0.8        1    NA
      68  16.16 6.2775656     1    -1    red  0.8        1    NA
      69  16.34 6.1264873     1    -1    red  0.8        1    NA
      70  16.52 5.9692793     1    -1    red  0.8        1    NA
      71  16.70 5.8066060     1    -1    red  0.8        1    NA
      72  16.88 5.6391403     1    -1    red  0.8        1    NA
      73  17.06 5.4675598     1    -1    red  0.8        1    NA
      74  17.24 5.2925415     1    -1    red  0.8        1    NA
      75  17.42 5.1147580     1    -1    red  0.8        1    NA
      76  17.60 4.9348733     1    -1    red  0.8        1    NA
      77  17.78 4.7535384     1    -1    red  0.8        1    NA
      78  17.96 4.5713882     1    -1    red  0.8        1    NA
      79  18.14 4.3890375     1    -1    red  0.8        1    NA
      80  18.32 4.2070781     1    -1    red  0.8        1    NA
      81  18.50 4.0260758     1    -1    red  0.8        1    NA
      82  18.68 3.8465679     1    -1    red  0.8        1    NA
      83  18.86 3.6690612     1    -1    red  0.8        1    NA
      84  19.04 3.4940298     1    -1    red  0.8        1    NA
      85  19.22 3.3219137     1    -1    red  0.8        1    NA
      86  19.40 3.1531176     1    -1    red  0.8        1    NA
      87  19.58 2.9880103     1    -1    red  0.8        1    NA
      88  19.76 2.8269238     1    -1    red  0.8        1    NA
      89  19.94 2.6701533     1    -1    red  0.8        1    NA
      90  20.12 2.5179575     1    -1    red  0.8        1    NA
      91  20.30 2.3705585     1    -1    red  0.8        1    NA
      92  20.48 2.2281430     1    -1    red  0.8        1    NA
      93  20.66 2.0908627     1    -1    red  0.8        1    NA
      94  20.84 1.9588360     1    -1    red  0.8        1    NA
      95  21.02 1.8321488     1    -1    red  0.8        1    NA
      96  21.20 1.7108561     1    -1    red  0.8        1    NA
      97  21.38 1.5949839     1    -1    red  0.8        1    NA
      98  21.56 1.4845309     1    -1    red  0.8        1    NA
      99  21.74 1.3794700     1    -1    red  0.8        1    NA
      100 21.92 1.2797507     1    -1    red  0.8        1    NA
      101 22.10 1.1853008     1    -1    red  0.8        1    NA
      
      [[3]]
        xintercept PANEL group colour size linetype alpha
      1   13.56747     1    -1   blue    1   dashed    NA
      

---

    Code
      pb1$plot$labels
    Output
      $x
      [1] "awake"
      
      $y
      [1] "count"
      
      $title
      NULL
      
      $subtitle
      NULL
      
      $caption
      NULL
      
      $fill
      [1] "count"
      
      $xintercept
      [1] "xintercept"
      
      $weight
      [1] "weight"
      attr(,"fallback")
      [1] TRUE
      
      $alt
      [1] ""
      

