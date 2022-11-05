# .grouped_list works

    Code
      names(df1)
    Output
      [1] "carni"   "herbi"   "insecti" "omni"   

# .grouped_list works with non-syntactic group names

    Code
      ggplot2::msleep %>% rename(`my non-syntactic name` = vore) %>% ggstatsplot:::.grouped_list(
        grouping.var = `my non-syntactic name`) %>% tibble::tbl_sum()
    Output
           Description 
      "named list [4]" 

# .palette_message is working

    Number of labels is greater than default palette color count.
    * Select another color `palette` (and/or `package`).

