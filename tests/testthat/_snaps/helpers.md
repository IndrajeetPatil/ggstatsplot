# grouped_list works

    Code
      list(length(df1), length(df2), length(df5), length(df6))
    Output
      [[1]]
      [1] 4
      
      [[2]]
      [1] 3
      
      [[3]]
      [1] 4
      
      [[4]]
      [1] 11
      

---

    Code
      list(names(df1), names(df2), names(df5), names(df6))
    Output
      [[1]]
      [1] "carni"   "herbi"   "insecti" "omni"   
      
      [[2]]
      [1] "carni"   "insecti" "omni"   
      
      [[3]]
      [1] "carni"   "herbi"   "insecti" "omni"   
      
      [[4]]
       [1] "name"         "genus"        "vore"         "order"        "conservation"
       [6] "sleep_total"  "sleep_rem"    "sleep_cycle"  "awake"        "brainwt"     
      [11] "bodywt"      
      

# palette_message is working

    Number of labels is greater than default palette color count.Select another color `palette` (and/or `package`).

