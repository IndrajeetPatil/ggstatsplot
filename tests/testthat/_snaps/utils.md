# .grouped_list works

    Code
      str(ggstatsplot:::.grouped_list(sleep, grouping.var = group))
    Output
      List of 2
       $ data :List of 2
        ..$ 1: tibble [10 x 3] (S3: tbl_df/tbl/data.frame)
        .. ..$ extra: num [1:10] 0.7 -1.6 -0.2 -1.2 -0.1 3.4 3.7 0.8 0 2
        .. ..$ group: Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1
        .. ..$ ID   : Factor w/ 10 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10
        ..$ 2: tibble [10 x 3] (S3: tbl_df/tbl/data.frame)
        .. ..$ extra: num [1:10] 1.9 0.8 1.1 0.1 -0.1 4.4 5.5 1.6 4.6 3.4
        .. ..$ group: Factor w/ 2 levels "1","2": 2 2 2 2 2 2 2 2 2 2
        .. ..$ ID   : Factor w/ 10 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10
       $ title: chr [1:2] "1" "2"

---

    Code
      str(ggstatsplot:::.grouped_list(sleep))
    Output
      tibble [20 x 3] (S3: tbl_df/tbl/data.frame)
       $ extra: num [1:20] 0.7 -1.6 -0.2 -1.2 -0.1 3.4 3.7 0.8 0 2 ...
       $ group: Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1 ...
       $ ID   : Factor w/ 10 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...

# .grouped_list works with non-syntactic group names

    Code
      sleep %>% rename(`my non-syntactic name` = group) %>% ggstatsplot:::.grouped_list(
        grouping.var = `my non-syntactic name`) %>% str()
    Output
      List of 2
       $ data :List of 2
        ..$ 1: tibble [10 x 3] (S3: tbl_df/tbl/data.frame)
        .. ..$ extra                : num [1:10] 0.7 -1.6 -0.2 -1.2 -0.1 3.4 3.7 0.8 0 2
        .. ..$ my non-syntactic name: Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 1
        .. ..$ ID                   : Factor w/ 10 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10
        ..$ 2: tibble [10 x 3] (S3: tbl_df/tbl/data.frame)
        .. ..$ extra                : num [1:10] 1.9 0.8 1.1 0.1 -0.1 4.4 5.5 1.6 4.6 3.4
        .. ..$ my non-syntactic name: Factor w/ 2 levels "1","2": 2 2 2 2 2 2 2 2 2 2
        .. ..$ ID                   : Factor w/ 10 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10
       $ title: chr [1:2] "1" "2"

# .palette_message is working

    Code
      ggstatsplot:::.palette_message(package = "RColorBrewer", palette = "Dark2",
        min_length = 20L)
    Message <rlang_message>
      Number of labels is greater than default palette color count.
      * Select another color `palette` (and/or `package`).
    Output
      [1] FALSE

