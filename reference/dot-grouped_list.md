# Split data frame into a list by grouping variable

This function splits the data frame into a list, with the length of the
list equal to the factor levels of the grouping variable.

## Usage

``` r
.grouped_list(data, grouping.var)
```

## Arguments

- data:

  A data frame (or a tibble) from which variables specified are to be
  taken. Other data types (e.g., matrix,table, array, etc.) will **not**
  be accepted. Additionally, grouped data frames from `{dplyr}` should
  be ungrouped before they are entered as `data`.

- grouping.var:

  A single grouping variable.

## Examples

``` r
ggstatsplot:::.grouped_list(ggplot2::msleep, grouping.var = vore)
#> $data
#> $data$carni
#> # A tibble: 19 × 11
#>    name   genus vore  order conservation sleep_total sleep_rem sleep_cycle awake
#>    <chr>  <chr> <chr> <chr> <chr>              <dbl>     <dbl>       <dbl> <dbl>
#>  1 Cheet… Acin… carni Carn… lc                  12.1      NA        NA      11.9
#>  2 North… Call… carni Carn… vu                   8.7       1.4       0.383  15.3
#>  3 Dog    Canis carni Carn… domesticated        10.1       2.9       0.333  13.9
#>  4 Long-… Dasy… carni Cing… lc                  17.4       3.1       0.383   6.6
#>  5 Domes… Felis carni Carn… domesticated        12.5       3.2       0.417  11.5
#>  6 Pilot… Glob… carni Ceta… cd                   2.7       0.1      NA      21.4
#>  7 Gray … Hali… carni Carn… lc                   6.2       1.5      NA      17.8
#>  8 Thick… Lutr… carni Dide… lc                  19.4       6.6      NA       4.6
#>  9 Slow … Nyct… carni Prim… NA                  11        NA        NA      13  
#> 10 North… Onyc… carni Rode… lc                  14.5      NA        NA       9.5
#> 11 Tiger  Pant… carni Carn… en                  15.8      NA        NA       8.2
#> 12 Jaguar Pant… carni Carn… nt                  10.4      NA        NA      13.6
#> 13 Lion   Pant… carni Carn… vu                  13.5      NA        NA      10.5
#> 14 Caspi… Phoca carni Carn… vu                   3.5       0.4      NA      20.5
#> 15 Commo… Phoc… carni Ceta… vu                   5.6      NA        NA      18.4
#> 16 Bottl… Turs… carni Ceta… NA                   5.2      NA        NA      18.8
#> 17 Genet  Gene… carni Carn… NA                   6.3       1.3      NA      17.7
#> 18 Arcti… Vulp… carni Carn… NA                  12.5      NA        NA      11.5
#> 19 Red f… Vulp… carni Carn… NA                   9.8       2.4       0.35   14.2
#> # ℹ 2 more variables: brainwt <dbl>, bodywt <dbl>
#> 
#> $data$herbi
#> # A tibble: 32 × 11
#>    name   genus vore  order conservation sleep_total sleep_rem sleep_cycle awake
#>    <chr>  <chr> <chr> <chr> <chr>              <dbl>     <dbl>       <dbl> <dbl>
#>  1 Mount… Aplo… herbi Rode… nt                  14.4       2.4      NA       9.6
#>  2 Cow    Bos   herbi Arti… domesticated         4         0.7       0.667  20  
#>  3 Three… Brad… herbi Pilo… NA                  14.4       2.2       0.767   9.6
#>  4 Roe d… Capr… herbi Arti… lc                   3        NA        NA      21  
#>  5 Goat   Capri herbi Arti… lc                   5.3       0.6      NA      18.7
#>  6 Guine… Cavis herbi Rode… domesticated         9.4       0.8       0.217  14.6
#>  7 Chinc… Chin… herbi Rode… domesticated        12.5       1.5       0.117  11.5
#>  8 Tree … Dend… herbi Hyra… lc                   5.3       0.5      NA      18.7
#>  9 Asian… Elep… herbi Prob… en                   3.9      NA        NA      20.1
#> 10 Horse  Equus herbi Peri… domesticated         2.9       0.6       1      21.1
#> # ℹ 22 more rows
#> # ℹ 2 more variables: brainwt <dbl>, bodywt <dbl>
#> 
#> $data$insecti
#> # A tibble: 5 × 11
#>   name    genus vore  order conservation sleep_total sleep_rem sleep_cycle awake
#>   <chr>   <chr> <chr> <chr> <chr>              <dbl>     <dbl>       <dbl> <dbl>
#> 1 Big br… Epte… inse… Chir… lc                  19.7       3.9       0.117   4.3
#> 2 Little… Myot… inse… Chir… NA                  19.9       2         0.2     4.1
#> 3 Giant … Prio… inse… Cing… en                  18.1       6.1      NA       5.9
#> 4 Easter… Scal… inse… Sori… lc                   8.4       2.1       0.167  15.6
#> 5 Short-… Tach… inse… Mono… NA                   8.6      NA        NA      15.4
#> # ℹ 2 more variables: brainwt <dbl>, bodywt <dbl>
#> 
#> $data$omni
#> # A tibble: 20 × 11
#>    name   genus vore  order conservation sleep_total sleep_rem sleep_cycle awake
#>    <chr>  <chr> <chr> <chr> <chr>              <dbl>     <dbl>       <dbl> <dbl>
#>  1 Owl m… Aotus omni  Prim… NA                  17         1.8      NA       7  
#>  2 Great… Blar… omni  Sori… lc                  14.9       2.3       0.133   9.1
#>  3 Grivet Cerc… omni  Prim… lc                  10         0.7      NA      14  
#>  4 Star-… Cond… omni  Sori… lc                  10.3       2.2      NA      13.7
#>  5 Afric… Cric… omni  Rode… NA                   8.3       2        NA      15.7
#>  6 Lesse… Cryp… omni  Sori… lc                   9.1       1.4       0.15   14.9
#>  7 North… Dide… omni  Dide… lc                  18         4.9       0.333   6  
#>  8 Europ… Erin… omni  Erin… lc                  10.1       3.5       0.283  13.9
#>  9 Patas… Eryt… omni  Prim… lc                  10.9       1.1      NA      13.1
#> 10 Galago Gala… omni  Prim… NA                   9.8       1.1       0.55   14.2
#> 11 Human  Homo  omni  Prim… NA                   8         1.9       1.5    16  
#> 12 Macaq… Maca… omni  Prim… NA                  10.1       1.2       0.75   13.9
#> 13 Chimp… Pan   omni  Prim… NA                   9.7       1.4       1.42   14.3
#> 14 Baboon Papio omni  Prim… NA                   9.4       1         0.667  14.6
#> 15 Potto  Pero… omni  Prim… lc                  11        NA        NA      13  
#> 16 Afric… Rhab… omni  Rode… NA                   8.7      NA        NA      15.3
#> 17 Squir… Saim… omni  Prim… NA                   9.6       1.4      NA      14.4
#> 18 Pig    Sus   omni  Arti… domesticated         9.1       2.4       0.5    14.9
#> 19 Tenrec Tenr… omni  Afro… NA                  15.6       2.3      NA       8.4
#> 20 Tree … Tupa… omni  Scan… NA                   8.9       2.6       0.233  15.1
#> # ℹ 2 more variables: brainwt <dbl>, bodywt <dbl>
#> 
#> 
#> $title
#> [1] "carni"   "herbi"   "insecti" "omni"   
#> 
```
