# Edgar Anderson's Iris Data in long format.

Edgar Anderson's Iris Data in long format.

## Usage

``` r
iris_long
```

## Format

A data frame with 600 rows and 5 variables

- id. Dummy identity number for each flower (150 flowers in total).

- Species. The species are *Iris setosa*, *versicolor*, and *virginica*.

- condition. Factor giving a detailed description of the attribute (Four
  levels: `"Petal.Length"`, `"Petal.Width"`, `"Sepal.Length"`,
  `"Sepal.Width"`).

- attribute. What attribute is being measured (`"Sepal"` or `"Pepal"`).

- measure. What aspect of the attribute is being measured (`"Length"` or
  `"Width"`).

- value. Value of the measurement.

## Details

This famous (Fisher's or Anderson's) iris data set gives the
measurements in centimeters of the variables sepal length and width and
petal length and width, respectively, for 50 flowers from each of 3
species of iris. The species are Iris setosa, versicolor, and virginica.

This is a modified dataset from `{datasets}` package.

## Examples

``` r
dim(iris_long)
#> [1] 600   6
head(iris_long)
#> # A tibble: 6 × 6
#>      id Species condition    attribute measure value
#>   <int> <fct>   <fct>        <fct>     <fct>   <dbl>
#> 1     1 setosa  Sepal.Length Sepal     Length    5.1
#> 2     2 setosa  Sepal.Length Sepal     Length    4.9
#> 3     3 setosa  Sepal.Length Sepal     Length    4.7
#> 4     4 setosa  Sepal.Length Sepal     Length    4.6
#> 5     5 setosa  Sepal.Length Sepal     Length    5  
#> 6     6 setosa  Sepal.Length Sepal     Length    5.4
dplyr::glimpse(iris_long)
#> Rows: 600
#> Columns: 6
#> $ id        <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 1…
#> $ Species   <fct> setosa, setosa, setosa, setosa, setosa, setosa, setosa, seto…
#> $ condition <fct> Sepal.Length, Sepal.Length, Sepal.Length, Sepal.Length, Sepa…
#> $ attribute <fct> Sepal, Sepal, Sepal, Sepal, Sepal, Sepal, Sepal, Sepal, Sepa…
#> $ measure   <fct> Length, Length, Length, Length, Length, Length, Length, Leng…
#> $ value     <dbl> 5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6, 5.0, 4.4, 4.9, 5.4, 4.8, …
```
