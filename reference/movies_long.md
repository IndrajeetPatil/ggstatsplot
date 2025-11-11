# Movie information and user ratings from IMDB.com (long format).

Movie information and user ratings from IMDB.com (long format).

## Usage

``` r
movies_long
```

## Format

A data frame with 1,579 rows and 8 variables

- title. Title of the movie.

- year. Year of release.

- budget. Total budget (if known) in US dollars

- length. Length in minutes.

- rating. Average IMDB user rating.

- votes. Number of IMDB users who rated this movie.

- mpaa. MPAA rating.

- genre. Different genres of movies (action, animation, comedy, drama,
  documentary, romance, short).

## Source

<https://CRAN.R-project.org/package=ggplot2movies>

## Details

Modified dataset from `{ggplot2movies}` package.

The internet movie database (IMDB) is a website devoted to collecting
movie data supplied by studios and fans. It claims to be the biggest
movie database on the web and is run by amazon.

## Examples

``` r
dim(movies_long)
#> [1] 1579    8
head(movies_long)
#> # A tibble: 6 × 8
#>   title                             year length budget rating  votes mpaa  genre
#>   <chr>                            <int>  <int>  <dbl>  <dbl>  <int> <fct> <fct>
#> 1 Shawshank Redemption, The         1994    142     25    9.1 149494 R     Drama
#> 2 Lord of the Rings: The Return o…  2003    251     94    9   103631 PG-13 Acti…
#> 3 Lord of the Rings: The Fellowsh…  2001    208     93    8.8 157608 PG-13 Acti…
#> 4 Lord of the Rings: The Two Towe…  2002    223     94    8.8 114797 PG-13 Acti…
#> 5 Pulp Fiction                      1994    168      8    8.8 132745 R     Drama
#> 6 Schindler's List                  1993    195     25    8.8  97667 R     Drama
dplyr::glimpse(movies_long)
#> Rows: 1,579
#> Columns: 8
#> $ title  <chr> "Shawshank Redemption, The", "Lord of the Rings: The Return of …
#> $ year   <int> 1994, 2003, 2001, 2002, 1994, 1993, 1977, 1980, 1968, 2002, 196…
#> $ length <int> 142, 251, 208, 223, 168, 195, 125, 129, 158, 135, 93, 113, 108,…
#> $ budget <dbl> 25.0, 94.0, 93.0, 94.0, 8.0, 25.0, 11.0, 18.0, 5.0, 3.3, 1.8, 5…
#> $ rating <dbl> 9.1, 9.0, 8.8, 8.8, 8.8, 8.8, 8.8, 8.8, 8.7, 8.7, 8.7, 8.7, 8.6…
#> $ votes  <int> 149494, 103631, 157608, 114797, 132745, 97667, 134640, 103706, …
#> $ mpaa   <fct> R, PG-13, PG-13, PG-13, R, R, PG, PG, PG-13, R, PG, R, R, R, R,…
#> $ genre  <fct> Drama, Action, Action, Action, Drama, Drama, Action, Action, Dr…
```
