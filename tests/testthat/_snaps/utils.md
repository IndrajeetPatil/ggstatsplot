# .grouped_list works

    Code
      glimpse(ggstatsplot:::.grouped_list(sleep, grouping.var = group))
    Output
      List of 2
       $ data :List of 2
        ..$ 1: tibble [10 x 3] (S3: tbl_df/tbl/data.frame)
        ..$ 2: tibble [10 x 3] (S3: tbl_df/tbl/data.frame)
       $ title: chr [1:2] "1" "2"

---

    Code
      glimpse(ggstatsplot:::.grouped_list(sleep))
    Output
      Rows: 20
      Columns: 3
      $ extra [3m[38;5;246m<dbl>[39m[23m 0.7, -1.6, -0.2, -1.2, -0.1, 3.4, 3.7, 0.8, 0.0, 2.0, 1.9, 0.8, ~
      $ group [3m[38;5;246m<fct>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
      $ ID    [3m[38;5;246m<fct>[39m[23m 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

# .grouped_list works with non-syntactic group names

    Code
      sleep %>% rename(`my non-syntactic name` = group) %>% ggstatsplot:::.grouped_list(
        grouping.var = `my non-syntactic name`) %>% tibble::tbl_sum()
    Output
           Description 
      "named list [2]" 

# .palette_message is working

    Number of labels is greater than default palette color count.
    * Select another color `palette` (and/or `package`).

