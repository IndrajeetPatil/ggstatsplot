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
      
      [[3]]
          y       x                           label PANEL group colour  fill size
      1  20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      2  20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      3  20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      4  20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      5  20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      6  20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      7  20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      8  20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      9  20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      10 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      11 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      12 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      13 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      14 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      15 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      16 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      17 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      18 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      19 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      20 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      21 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      22 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      23 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      24 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      25 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      26 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      27 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      28 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      29 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      30 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      31 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      32 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      33 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      34 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      35 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      36 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      37 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      38 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      39 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      40 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      41 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      42 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      43 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      44 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      45 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      46 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      47 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      48 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      49 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      50 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      51 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      52 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      53 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      54 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      55 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      56 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      57 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      58 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      59 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      60 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      61 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      62 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      63 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      64 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      65 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      66 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      67 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      68 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      69 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      70 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      71 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      72 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      73 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      74 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      75 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      76 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      77 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      78 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      79 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      80 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
      81 20 174.358 list(~widehat(mu)[mean]=='174')     1    -1   blue white    3
         angle hjust vjust alpha family fontface lineheight
      1      0   0.5   0.5   0.5               1        1.2
      2      0   0.5   0.5   0.5               1        1.2
      3      0   0.5   0.5   0.5               1        1.2
      4      0   0.5   0.5   0.5               1        1.2
      5      0   0.5   0.5   0.5               1        1.2
      6      0   0.5   0.5   0.5               1        1.2
      7      0   0.5   0.5   0.5               1        1.2
      8      0   0.5   0.5   0.5               1        1.2
      9      0   0.5   0.5   0.5               1        1.2
      10     0   0.5   0.5   0.5               1        1.2
      11     0   0.5   0.5   0.5               1        1.2
      12     0   0.5   0.5   0.5               1        1.2
      13     0   0.5   0.5   0.5               1        1.2
      14     0   0.5   0.5   0.5               1        1.2
      15     0   0.5   0.5   0.5               1        1.2
      16     0   0.5   0.5   0.5               1        1.2
      17     0   0.5   0.5   0.5               1        1.2
      18     0   0.5   0.5   0.5               1        1.2
      19     0   0.5   0.5   0.5               1        1.2
      20     0   0.5   0.5   0.5               1        1.2
      21     0   0.5   0.5   0.5               1        1.2
      22     0   0.5   0.5   0.5               1        1.2
      23     0   0.5   0.5   0.5               1        1.2
      24     0   0.5   0.5   0.5               1        1.2
      25     0   0.5   0.5   0.5               1        1.2
      26     0   0.5   0.5   0.5               1        1.2
      27     0   0.5   0.5   0.5               1        1.2
      28     0   0.5   0.5   0.5               1        1.2
      29     0   0.5   0.5   0.5               1        1.2
      30     0   0.5   0.5   0.5               1        1.2
      31     0   0.5   0.5   0.5               1        1.2
      32     0   0.5   0.5   0.5               1        1.2
      33     0   0.5   0.5   0.5               1        1.2
      34     0   0.5   0.5   0.5               1        1.2
      35     0   0.5   0.5   0.5               1        1.2
      36     0   0.5   0.5   0.5               1        1.2
      37     0   0.5   0.5   0.5               1        1.2
      38     0   0.5   0.5   0.5               1        1.2
      39     0   0.5   0.5   0.5               1        1.2
      40     0   0.5   0.5   0.5               1        1.2
      41     0   0.5   0.5   0.5               1        1.2
      42     0   0.5   0.5   0.5               1        1.2
      43     0   0.5   0.5   0.5               1        1.2
      44     0   0.5   0.5   0.5               1        1.2
      45     0   0.5   0.5   0.5               1        1.2
      46     0   0.5   0.5   0.5               1        1.2
      47     0   0.5   0.5   0.5               1        1.2
      48     0   0.5   0.5   0.5               1        1.2
      49     0   0.5   0.5   0.5               1        1.2
      50     0   0.5   0.5   0.5               1        1.2
      51     0   0.5   0.5   0.5               1        1.2
      52     0   0.5   0.5   0.5               1        1.2
      53     0   0.5   0.5   0.5               1        1.2
      54     0   0.5   0.5   0.5               1        1.2
      55     0   0.5   0.5   0.5               1        1.2
      56     0   0.5   0.5   0.5               1        1.2
      57     0   0.5   0.5   0.5               1        1.2
      58     0   0.5   0.5   0.5               1        1.2
      59     0   0.5   0.5   0.5               1        1.2
      60     0   0.5   0.5   0.5               1        1.2
      61     0   0.5   0.5   0.5               1        1.2
      62     0   0.5   0.5   0.5               1        1.2
      63     0   0.5   0.5   0.5               1        1.2
      64     0   0.5   0.5   0.5               1        1.2
      65     0   0.5   0.5   0.5               1        1.2
      66     0   0.5   0.5   0.5               1        1.2
      67     0   0.5   0.5   0.5               1        1.2
      68     0   0.5   0.5   0.5               1        1.2
      69     0   0.5   0.5   0.5               1        1.2
      70     0   0.5   0.5   0.5               1        1.2
      71     0   0.5   0.5   0.5               1        1.2
      72     0   0.5   0.5   0.5               1        1.2
      73     0   0.5   0.5   0.5               1        1.2
      74     0   0.5   0.5   0.5               1        1.2
      75     0   0.5   0.5   0.5               1        1.2
      76     0   0.5   0.5   0.5               1        1.2
      77     0   0.5   0.5   0.5               1        1.2
      78     0   0.5   0.5   0.5               1        1.2
      79     0   0.5   0.5   0.5               1        1.2
      80     0   0.5   0.5   0.5               1        1.2
      81     0   0.5   0.5   0.5               1        1.2
      

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
      
      [[3]]
               y  x                               label PANEL group colour  fill size
      1   61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      2   61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      3   61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      4   61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      5   61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      6   61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      7   61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      8   61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      9   61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      10  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      11  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      12  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      13  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      14  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      15  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      16  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      17  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      18  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      19  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      20  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      21  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      22  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      23  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      24  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      25  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      26  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      27  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      28  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      29  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      30  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      31  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      32  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      33  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      34  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      35  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      36  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      37  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      38  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      39  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      40  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      41  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      42  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      43  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      44  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      45  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      46  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      47  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      48  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      49  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      50  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      51  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      52  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      53  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      54  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      55  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      56  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      57  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      58  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      59  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      60  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      61  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      62  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      63  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      64  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      65  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      66  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      67  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      68  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      69  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      70  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      71  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      72  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      73  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      74  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      75  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      76  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      77  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      78  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      79  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      80  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      81  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      82  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      83  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      84  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      85  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      86  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      87  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      88  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      89  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      90  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      91  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      92  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      93  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      94  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      95  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      96  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      97  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      98  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      99  61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      100 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      101 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      102 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      103 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      104 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      105 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      106 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      107 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      108 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      109 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      110 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      111 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      112 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      113 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      114 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      115 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      116 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      117 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      118 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      119 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      120 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      121 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      122 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      123 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      124 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      125 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      126 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      127 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      128 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      129 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      130 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      131 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      132 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      133 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      134 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      135 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      136 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      137 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      138 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      139 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      140 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      141 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      142 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      143 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      144 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      145 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      146 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      147 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      148 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      149 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      150 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      151 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      152 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      153 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      154 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      155 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      156 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      157 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      158 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      159 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      160 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      161 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      162 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      163 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      164 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      165 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      166 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      167 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      168 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      169 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      170 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      171 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      172 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      173 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      174 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      175 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      176 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      177 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      178 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      179 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      180 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      181 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      182 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      183 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      184 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      185 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      186 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      187 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      188 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      189 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      190 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      191 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      192 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      193 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      194 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      195 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      196 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      197 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      198 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      199 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      200 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      201 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      202 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      203 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      204 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      205 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      206 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      207 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      208 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      209 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      210 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      211 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      212 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      213 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      214 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      215 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      216 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      217 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      218 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      219 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      220 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      221 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      222 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      223 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      224 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      225 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      226 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      227 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      228 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      229 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      230 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      231 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      232 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      233 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
      234 61.875 17 list(~widehat(mu)[median]=='17.00')     1    -1   blue white    3
          angle hjust vjust alpha family fontface lineheight
      1       0   0.5   0.5   0.5               1        1.2
      2       0   0.5   0.5   0.5               1        1.2
      3       0   0.5   0.5   0.5               1        1.2
      4       0   0.5   0.5   0.5               1        1.2
      5       0   0.5   0.5   0.5               1        1.2
      6       0   0.5   0.5   0.5               1        1.2
      7       0   0.5   0.5   0.5               1        1.2
      8       0   0.5   0.5   0.5               1        1.2
      9       0   0.5   0.5   0.5               1        1.2
      10      0   0.5   0.5   0.5               1        1.2
      11      0   0.5   0.5   0.5               1        1.2
      12      0   0.5   0.5   0.5               1        1.2
      13      0   0.5   0.5   0.5               1        1.2
      14      0   0.5   0.5   0.5               1        1.2
      15      0   0.5   0.5   0.5               1        1.2
      16      0   0.5   0.5   0.5               1        1.2
      17      0   0.5   0.5   0.5               1        1.2
      18      0   0.5   0.5   0.5               1        1.2
      19      0   0.5   0.5   0.5               1        1.2
      20      0   0.5   0.5   0.5               1        1.2
      21      0   0.5   0.5   0.5               1        1.2
      22      0   0.5   0.5   0.5               1        1.2
      23      0   0.5   0.5   0.5               1        1.2
      24      0   0.5   0.5   0.5               1        1.2
      25      0   0.5   0.5   0.5               1        1.2
      26      0   0.5   0.5   0.5               1        1.2
      27      0   0.5   0.5   0.5               1        1.2
      28      0   0.5   0.5   0.5               1        1.2
      29      0   0.5   0.5   0.5               1        1.2
      30      0   0.5   0.5   0.5               1        1.2
      31      0   0.5   0.5   0.5               1        1.2
      32      0   0.5   0.5   0.5               1        1.2
      33      0   0.5   0.5   0.5               1        1.2
      34      0   0.5   0.5   0.5               1        1.2
      35      0   0.5   0.5   0.5               1        1.2
      36      0   0.5   0.5   0.5               1        1.2
      37      0   0.5   0.5   0.5               1        1.2
      38      0   0.5   0.5   0.5               1        1.2
      39      0   0.5   0.5   0.5               1        1.2
      40      0   0.5   0.5   0.5               1        1.2
      41      0   0.5   0.5   0.5               1        1.2
      42      0   0.5   0.5   0.5               1        1.2
      43      0   0.5   0.5   0.5               1        1.2
      44      0   0.5   0.5   0.5               1        1.2
      45      0   0.5   0.5   0.5               1        1.2
      46      0   0.5   0.5   0.5               1        1.2
      47      0   0.5   0.5   0.5               1        1.2
      48      0   0.5   0.5   0.5               1        1.2
      49      0   0.5   0.5   0.5               1        1.2
      50      0   0.5   0.5   0.5               1        1.2
      51      0   0.5   0.5   0.5               1        1.2
      52      0   0.5   0.5   0.5               1        1.2
      53      0   0.5   0.5   0.5               1        1.2
      54      0   0.5   0.5   0.5               1        1.2
      55      0   0.5   0.5   0.5               1        1.2
      56      0   0.5   0.5   0.5               1        1.2
      57      0   0.5   0.5   0.5               1        1.2
      58      0   0.5   0.5   0.5               1        1.2
      59      0   0.5   0.5   0.5               1        1.2
      60      0   0.5   0.5   0.5               1        1.2
      61      0   0.5   0.5   0.5               1        1.2
      62      0   0.5   0.5   0.5               1        1.2
      63      0   0.5   0.5   0.5               1        1.2
      64      0   0.5   0.5   0.5               1        1.2
      65      0   0.5   0.5   0.5               1        1.2
      66      0   0.5   0.5   0.5               1        1.2
      67      0   0.5   0.5   0.5               1        1.2
      68      0   0.5   0.5   0.5               1        1.2
      69      0   0.5   0.5   0.5               1        1.2
      70      0   0.5   0.5   0.5               1        1.2
      71      0   0.5   0.5   0.5               1        1.2
      72      0   0.5   0.5   0.5               1        1.2
      73      0   0.5   0.5   0.5               1        1.2
      74      0   0.5   0.5   0.5               1        1.2
      75      0   0.5   0.5   0.5               1        1.2
      76      0   0.5   0.5   0.5               1        1.2
      77      0   0.5   0.5   0.5               1        1.2
      78      0   0.5   0.5   0.5               1        1.2
      79      0   0.5   0.5   0.5               1        1.2
      80      0   0.5   0.5   0.5               1        1.2
      81      0   0.5   0.5   0.5               1        1.2
      82      0   0.5   0.5   0.5               1        1.2
      83      0   0.5   0.5   0.5               1        1.2
      84      0   0.5   0.5   0.5               1        1.2
      85      0   0.5   0.5   0.5               1        1.2
      86      0   0.5   0.5   0.5               1        1.2
      87      0   0.5   0.5   0.5               1        1.2
      88      0   0.5   0.5   0.5               1        1.2
      89      0   0.5   0.5   0.5               1        1.2
      90      0   0.5   0.5   0.5               1        1.2
      91      0   0.5   0.5   0.5               1        1.2
      92      0   0.5   0.5   0.5               1        1.2
      93      0   0.5   0.5   0.5               1        1.2
      94      0   0.5   0.5   0.5               1        1.2
      95      0   0.5   0.5   0.5               1        1.2
      96      0   0.5   0.5   0.5               1        1.2
      97      0   0.5   0.5   0.5               1        1.2
      98      0   0.5   0.5   0.5               1        1.2
      99      0   0.5   0.5   0.5               1        1.2
      100     0   0.5   0.5   0.5               1        1.2
      101     0   0.5   0.5   0.5               1        1.2
      102     0   0.5   0.5   0.5               1        1.2
      103     0   0.5   0.5   0.5               1        1.2
      104     0   0.5   0.5   0.5               1        1.2
      105     0   0.5   0.5   0.5               1        1.2
      106     0   0.5   0.5   0.5               1        1.2
      107     0   0.5   0.5   0.5               1        1.2
      108     0   0.5   0.5   0.5               1        1.2
      109     0   0.5   0.5   0.5               1        1.2
      110     0   0.5   0.5   0.5               1        1.2
      111     0   0.5   0.5   0.5               1        1.2
      112     0   0.5   0.5   0.5               1        1.2
      113     0   0.5   0.5   0.5               1        1.2
      114     0   0.5   0.5   0.5               1        1.2
      115     0   0.5   0.5   0.5               1        1.2
      116     0   0.5   0.5   0.5               1        1.2
      117     0   0.5   0.5   0.5               1        1.2
      118     0   0.5   0.5   0.5               1        1.2
      119     0   0.5   0.5   0.5               1        1.2
      120     0   0.5   0.5   0.5               1        1.2
      121     0   0.5   0.5   0.5               1        1.2
      122     0   0.5   0.5   0.5               1        1.2
      123     0   0.5   0.5   0.5               1        1.2
      124     0   0.5   0.5   0.5               1        1.2
      125     0   0.5   0.5   0.5               1        1.2
      126     0   0.5   0.5   0.5               1        1.2
      127     0   0.5   0.5   0.5               1        1.2
      128     0   0.5   0.5   0.5               1        1.2
      129     0   0.5   0.5   0.5               1        1.2
      130     0   0.5   0.5   0.5               1        1.2
      131     0   0.5   0.5   0.5               1        1.2
      132     0   0.5   0.5   0.5               1        1.2
      133     0   0.5   0.5   0.5               1        1.2
      134     0   0.5   0.5   0.5               1        1.2
      135     0   0.5   0.5   0.5               1        1.2
      136     0   0.5   0.5   0.5               1        1.2
      137     0   0.5   0.5   0.5               1        1.2
      138     0   0.5   0.5   0.5               1        1.2
      139     0   0.5   0.5   0.5               1        1.2
      140     0   0.5   0.5   0.5               1        1.2
      141     0   0.5   0.5   0.5               1        1.2
      142     0   0.5   0.5   0.5               1        1.2
      143     0   0.5   0.5   0.5               1        1.2
      144     0   0.5   0.5   0.5               1        1.2
      145     0   0.5   0.5   0.5               1        1.2
      146     0   0.5   0.5   0.5               1        1.2
      147     0   0.5   0.5   0.5               1        1.2
      148     0   0.5   0.5   0.5               1        1.2
      149     0   0.5   0.5   0.5               1        1.2
      150     0   0.5   0.5   0.5               1        1.2
      151     0   0.5   0.5   0.5               1        1.2
      152     0   0.5   0.5   0.5               1        1.2
      153     0   0.5   0.5   0.5               1        1.2
      154     0   0.5   0.5   0.5               1        1.2
      155     0   0.5   0.5   0.5               1        1.2
      156     0   0.5   0.5   0.5               1        1.2
      157     0   0.5   0.5   0.5               1        1.2
      158     0   0.5   0.5   0.5               1        1.2
      159     0   0.5   0.5   0.5               1        1.2
      160     0   0.5   0.5   0.5               1        1.2
      161     0   0.5   0.5   0.5               1        1.2
      162     0   0.5   0.5   0.5               1        1.2
      163     0   0.5   0.5   0.5               1        1.2
      164     0   0.5   0.5   0.5               1        1.2
      165     0   0.5   0.5   0.5               1        1.2
      166     0   0.5   0.5   0.5               1        1.2
      167     0   0.5   0.5   0.5               1        1.2
      168     0   0.5   0.5   0.5               1        1.2
      169     0   0.5   0.5   0.5               1        1.2
      170     0   0.5   0.5   0.5               1        1.2
      171     0   0.5   0.5   0.5               1        1.2
      172     0   0.5   0.5   0.5               1        1.2
      173     0   0.5   0.5   0.5               1        1.2
      174     0   0.5   0.5   0.5               1        1.2
      175     0   0.5   0.5   0.5               1        1.2
      176     0   0.5   0.5   0.5               1        1.2
      177     0   0.5   0.5   0.5               1        1.2
      178     0   0.5   0.5   0.5               1        1.2
      179     0   0.5   0.5   0.5               1        1.2
      180     0   0.5   0.5   0.5               1        1.2
      181     0   0.5   0.5   0.5               1        1.2
      182     0   0.5   0.5   0.5               1        1.2
      183     0   0.5   0.5   0.5               1        1.2
      184     0   0.5   0.5   0.5               1        1.2
      185     0   0.5   0.5   0.5               1        1.2
      186     0   0.5   0.5   0.5               1        1.2
      187     0   0.5   0.5   0.5               1        1.2
      188     0   0.5   0.5   0.5               1        1.2
      189     0   0.5   0.5   0.5               1        1.2
      190     0   0.5   0.5   0.5               1        1.2
      191     0   0.5   0.5   0.5               1        1.2
      192     0   0.5   0.5   0.5               1        1.2
      193     0   0.5   0.5   0.5               1        1.2
      194     0   0.5   0.5   0.5               1        1.2
      195     0   0.5   0.5   0.5               1        1.2
      196     0   0.5   0.5   0.5               1        1.2
      197     0   0.5   0.5   0.5               1        1.2
      198     0   0.5   0.5   0.5               1        1.2
      199     0   0.5   0.5   0.5               1        1.2
      200     0   0.5   0.5   0.5               1        1.2
      201     0   0.5   0.5   0.5               1        1.2
      202     0   0.5   0.5   0.5               1        1.2
      203     0   0.5   0.5   0.5               1        1.2
      204     0   0.5   0.5   0.5               1        1.2
      205     0   0.5   0.5   0.5               1        1.2
      206     0   0.5   0.5   0.5               1        1.2
      207     0   0.5   0.5   0.5               1        1.2
      208     0   0.5   0.5   0.5               1        1.2
      209     0   0.5   0.5   0.5               1        1.2
      210     0   0.5   0.5   0.5               1        1.2
      211     0   0.5   0.5   0.5               1        1.2
      212     0   0.5   0.5   0.5               1        1.2
      213     0   0.5   0.5   0.5               1        1.2
      214     0   0.5   0.5   0.5               1        1.2
      215     0   0.5   0.5   0.5               1        1.2
      216     0   0.5   0.5   0.5               1        1.2
      217     0   0.5   0.5   0.5               1        1.2
      218     0   0.5   0.5   0.5               1        1.2
      219     0   0.5   0.5   0.5               1        1.2
      220     0   0.5   0.5   0.5               1        1.2
      221     0   0.5   0.5   0.5               1        1.2
      222     0   0.5   0.5   0.5               1        1.2
      223     0   0.5   0.5   0.5               1        1.2
      224     0   0.5   0.5   0.5               1        1.2
      225     0   0.5   0.5   0.5               1        1.2
      226     0   0.5   0.5   0.5               1        1.2
      227     0   0.5   0.5   0.5               1        1.2
      228     0   0.5   0.5   0.5               1        1.2
      229     0   0.5   0.5   0.5               1        1.2
      230     0   0.5   0.5   0.5               1        1.2
      231     0   0.5   0.5   0.5               1        1.2
      232     0   0.5   0.5   0.5               1        1.2
      233     0   0.5   0.5   0.5               1        1.2
      234     0   0.5   0.5   0.5               1        1.2
      

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
      1   3.152692     1    -1   blue    1   dashed    NA
      
      [[3]]
             y        x                               label PANEL group colour  fill
      1  5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      2  5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      3  5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      4  5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      5  5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      6  5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      7  5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      8  5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      9  5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      10 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      11 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      12 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      13 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      14 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      15 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      16 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      17 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      18 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      19 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      20 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      21 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      22 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      23 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      24 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      25 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      26 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      27 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      28 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      29 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      30 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      31 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
      32 5.625 3.152692 list(~widehat(mu)[trimmed]=='3.15')     1    -1   blue white
         size angle hjust vjust alpha family fontface lineheight
      1     3     0   0.5   0.5   0.5               1        1.2
      2     3     0   0.5   0.5   0.5               1        1.2
      3     3     0   0.5   0.5   0.5               1        1.2
      4     3     0   0.5   0.5   0.5               1        1.2
      5     3     0   0.5   0.5   0.5               1        1.2
      6     3     0   0.5   0.5   0.5               1        1.2
      7     3     0   0.5   0.5   0.5               1        1.2
      8     3     0   0.5   0.5   0.5               1        1.2
      9     3     0   0.5   0.5   0.5               1        1.2
      10    3     0   0.5   0.5   0.5               1        1.2
      11    3     0   0.5   0.5   0.5               1        1.2
      12    3     0   0.5   0.5   0.5               1        1.2
      13    3     0   0.5   0.5   0.5               1        1.2
      14    3     0   0.5   0.5   0.5               1        1.2
      15    3     0   0.5   0.5   0.5               1        1.2
      16    3     0   0.5   0.5   0.5               1        1.2
      17    3     0   0.5   0.5   0.5               1        1.2
      18    3     0   0.5   0.5   0.5               1        1.2
      19    3     0   0.5   0.5   0.5               1        1.2
      20    3     0   0.5   0.5   0.5               1        1.2
      21    3     0   0.5   0.5   0.5               1        1.2
      22    3     0   0.5   0.5   0.5               1        1.2
      23    3     0   0.5   0.5   0.5               1        1.2
      24    3     0   0.5   0.5   0.5               1        1.2
      25    3     0   0.5   0.5   0.5               1        1.2
      26    3     0   0.5   0.5   0.5               1        1.2
      27    3     0   0.5   0.5   0.5               1        1.2
      28    3     0   0.5   0.5   0.5               1        1.2
      29    3     0   0.5   0.5   0.5               1        1.2
      30    3     0   0.5   0.5   0.5               1        1.2
      31    3     0   0.5   0.5   0.5               1        1.2
      32    3     0   0.5   0.5   0.5               1        1.2
      

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
      
      [[4]]
           y        x                             label PANEL group colour  fill size
      1  7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      2  7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      3  7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      4  7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      5  7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      6  7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      7  7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      8  7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      9  7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      10 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      11 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      12 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      13 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      14 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      15 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      16 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      17 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      18 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      19 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      20 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      21 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      22 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      23 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      24 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      25 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      26 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      27 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      28 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      29 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      30 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      31 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      32 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      33 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      34 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      35 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      36 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      37 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      38 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      39 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      40 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      41 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      42 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      43 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      44 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      45 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      46 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      47 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      48 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      49 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      50 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      51 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      52 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      53 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      54 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      55 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      56 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      57 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      58 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      59 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      60 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      61 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      62 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      63 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      64 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      65 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      66 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      67 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      68 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      69 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      70 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      71 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      72 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      73 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      74 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      75 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      76 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      77 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      78 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      79 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      80 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      81 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      82 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
      83 7.5 13.56747 list(~widehat(mu)[mean]=='13.57')     1    -1   blue white    3
         angle hjust vjust alpha family fontface lineheight
      1      0   0.5   0.5   0.5               1        1.2
      2      0   0.5   0.5   0.5               1        1.2
      3      0   0.5   0.5   0.5               1        1.2
      4      0   0.5   0.5   0.5               1        1.2
      5      0   0.5   0.5   0.5               1        1.2
      6      0   0.5   0.5   0.5               1        1.2
      7      0   0.5   0.5   0.5               1        1.2
      8      0   0.5   0.5   0.5               1        1.2
      9      0   0.5   0.5   0.5               1        1.2
      10     0   0.5   0.5   0.5               1        1.2
      11     0   0.5   0.5   0.5               1        1.2
      12     0   0.5   0.5   0.5               1        1.2
      13     0   0.5   0.5   0.5               1        1.2
      14     0   0.5   0.5   0.5               1        1.2
      15     0   0.5   0.5   0.5               1        1.2
      16     0   0.5   0.5   0.5               1        1.2
      17     0   0.5   0.5   0.5               1        1.2
      18     0   0.5   0.5   0.5               1        1.2
      19     0   0.5   0.5   0.5               1        1.2
      20     0   0.5   0.5   0.5               1        1.2
      21     0   0.5   0.5   0.5               1        1.2
      22     0   0.5   0.5   0.5               1        1.2
      23     0   0.5   0.5   0.5               1        1.2
      24     0   0.5   0.5   0.5               1        1.2
      25     0   0.5   0.5   0.5               1        1.2
      26     0   0.5   0.5   0.5               1        1.2
      27     0   0.5   0.5   0.5               1        1.2
      28     0   0.5   0.5   0.5               1        1.2
      29     0   0.5   0.5   0.5               1        1.2
      30     0   0.5   0.5   0.5               1        1.2
      31     0   0.5   0.5   0.5               1        1.2
      32     0   0.5   0.5   0.5               1        1.2
      33     0   0.5   0.5   0.5               1        1.2
      34     0   0.5   0.5   0.5               1        1.2
      35     0   0.5   0.5   0.5               1        1.2
      36     0   0.5   0.5   0.5               1        1.2
      37     0   0.5   0.5   0.5               1        1.2
      38     0   0.5   0.5   0.5               1        1.2
      39     0   0.5   0.5   0.5               1        1.2
      40     0   0.5   0.5   0.5               1        1.2
      41     0   0.5   0.5   0.5               1        1.2
      42     0   0.5   0.5   0.5               1        1.2
      43     0   0.5   0.5   0.5               1        1.2
      44     0   0.5   0.5   0.5               1        1.2
      45     0   0.5   0.5   0.5               1        1.2
      46     0   0.5   0.5   0.5               1        1.2
      47     0   0.5   0.5   0.5               1        1.2
      48     0   0.5   0.5   0.5               1        1.2
      49     0   0.5   0.5   0.5               1        1.2
      50     0   0.5   0.5   0.5               1        1.2
      51     0   0.5   0.5   0.5               1        1.2
      52     0   0.5   0.5   0.5               1        1.2
      53     0   0.5   0.5   0.5               1        1.2
      54     0   0.5   0.5   0.5               1        1.2
      55     0   0.5   0.5   0.5               1        1.2
      56     0   0.5   0.5   0.5               1        1.2
      57     0   0.5   0.5   0.5               1        1.2
      58     0   0.5   0.5   0.5               1        1.2
      59     0   0.5   0.5   0.5               1        1.2
      60     0   0.5   0.5   0.5               1        1.2
      61     0   0.5   0.5   0.5               1        1.2
      62     0   0.5   0.5   0.5               1        1.2
      63     0   0.5   0.5   0.5               1        1.2
      64     0   0.5   0.5   0.5               1        1.2
      65     0   0.5   0.5   0.5               1        1.2
      66     0   0.5   0.5   0.5               1        1.2
      67     0   0.5   0.5   0.5               1        1.2
      68     0   0.5   0.5   0.5               1        1.2
      69     0   0.5   0.5   0.5               1        1.2
      70     0   0.5   0.5   0.5               1        1.2
      71     0   0.5   0.5   0.5               1        1.2
      72     0   0.5   0.5   0.5               1        1.2
      73     0   0.5   0.5   0.5               1        1.2
      74     0   0.5   0.5   0.5               1        1.2
      75     0   0.5   0.5   0.5               1        1.2
      76     0   0.5   0.5   0.5               1        1.2
      77     0   0.5   0.5   0.5               1        1.2
      78     0   0.5   0.5   0.5               1        1.2
      79     0   0.5   0.5   0.5               1        1.2
      80     0   0.5   0.5   0.5               1        1.2
      81     0   0.5   0.5   0.5               1        1.2
      82     0   0.5   0.5   0.5               1        1.2
      83     0   0.5   0.5   0.5               1        1.2
      

