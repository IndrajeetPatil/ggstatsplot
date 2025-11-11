# Titanic dataset.

Titanic dataset.

## Usage

``` r
Titanic_full
```

## Format

A data frame with 2201 rows and 5 variables

- id. Dummy identity number for each person.

- Class. 1st, 2nd, 3rd, Crew.

- Sex. Male, Female.

- Age. Child, Adult.

- Survived. No, Yes.

## Details

This data set provides information on the fate of passengers on the
fatal maiden voyage of the ocean liner 'Titanic', summarized according
to economic status (class), sex, age and survival.

This is a modified dataset from `{datasets}` package.

## Examples

``` r
dim(Titanic_full)
#> [1] 2201    5
head(Titanic_full)
#> # A tibble: 6 × 5
#>      id Class Sex   Age   Survived
#>   <dbl> <fct> <fct> <fct> <fct>   
#> 1     1 3rd   Male  Child No      
#> 2     2 3rd   Male  Child No      
#> 3     3 3rd   Male  Child No      
#> 4     4 3rd   Male  Child No      
#> 5     5 3rd   Male  Child No      
#> 6     6 3rd   Male  Child No      
dplyr::glimpse(Titanic_full)
#> Rows: 2,201
#> Columns: 5
#> $ id       <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18…
#> $ Class    <fct> 3rd, 3rd, 3rd, 3rd, 3rd, 3rd, 3rd, 3rd, 3rd, 3rd, 3rd, 3rd, 3…
#> $ Sex      <fct> Male, Male, Male, Male, Male, Male, Male, Male, Male, Male, M…
#> $ Age      <fct> Child, Child, Child, Child, Child, Child, Child, Child, Child…
#> $ Survived <fct> No, No, No, No, No, No, No, No, No, No, No, No, No, No, No, N…
```
