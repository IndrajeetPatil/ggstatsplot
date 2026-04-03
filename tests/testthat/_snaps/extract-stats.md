# checking if extract_stats works

    Code
      p1 <- ggbetweenstats(mtcars, am, mpg)
      list(length(extract_stats(p1)), extract_subtitle(p1), extract_caption(p1))
    Output
      [[1]]
      [1] 7
      
      [[2]]
      list(italic("t")["Welch"] * "(" * 18.33 * ")" == "-3.77", italic(p) == 
          "1.37e-03", widehat(italic("g"))["Hedges"] == "-1.35", CI["95%"] ~ 
          "[" * "-2.17", "-0.51" * "]", italic("n")["obs"] == "32")
      
      [[3]]
      list(log[e] * (BF["01"]) == "-4.46", widehat(delta)["difference"]^"posterior" == 
          "-6.44", CI["95%"]^ETI ~ "[" * "-10.14", "-2.74" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.71")
      

---

    Code
      p2 <- ggscatterstats(mtcars, wt, mpg, marginal = FALSE, type = "r")
      list(length(extract_stats(p2)), extract_subtitle(p2), extract_caption(p2))
    Output
      [[1]]
      [1] 7
      
      [[2]]
      list(italic("t")["Student"] * "(" * 30 * ")" == "-9.41", italic(p) == 
          "1.84e-10", widehat(italic("r"))["Winsorized"] == "-0.86", 
          CI["95%"] ~ "[" * "-0.93", "-0.74" * "]", italic("n")["pairs"] == 
              "32")
      
      [[3]]
      NULL
      

---

    Code
      p3 <- ggcorrmat(iris)
      list(length(extract_stats(p3)), extract_subtitle(p3), extract_caption(p3))
    Output
      [[1]]
      [1] 7
      
      [[2]]
      NULL
      
      [[3]]
      NULL
      

---

    Code
      p4 <- ggbetweenstats(mtcars, cyl, mpg)
      list(length(extract_stats(p4)), extract_subtitle(p4), extract_caption(p4))
    Output
      [[1]]
      [1] 7
      
      [[2]]
      list(italic("F")["Welch"](2, 18.03) == "31.62", italic(p) == 
          "1.27e-06", widehat(omega["p"]^2) == "0.74", CI["95%"] ~ 
          "[" * "0.53", "1.00" * "]", italic("n")["obs"] == "32")
      
      [[3]]
      list(log[e] * (BF["01"]) == "-14.92", widehat(italic(R^"2"))["Bayesian"]^"posterior" == 
          "0.71", CI["95%"]^HDI ~ "[" * "0.57", "0.79" * "]", italic("r")["Cauchy"]^"JZS" == 
          "0.71")
      

---

    Code
      p5 <- ggpiestats(mtcars, cyl)
      list(length(extract_stats(p5)), extract_subtitle(p5), extract_caption(p5))
    Output
      [[1]]
      [1] 7
      
      [[2]]
      list(chi["gof"]^2 * "(" * 2 * ")" == "2.31", italic(p) == "0.31", 
          widehat(italic("C"))["Pearson"] == "0.26", CI["95%"] ~ "[" * 
              "0.00", "0.50" * "]", italic("n")["obs"] == "32")
      
      [[3]]
      list(log[e] * (BF["01"]) == "2.81", italic("a")["Gunel-Dickey"] == 
          "1.00")
      

---

    Code
      p6 <- ggbarstats(mtcars, cyl, am)
      list(length(extract_stats(p6)), extract_subtitle(p6), extract_caption(p6))
    Output
      [[1]]
      [1] 7
      
      [[2]]
      list(chi["Pearson"]^2 * "(" * 2 * ")" == "8.74", italic(p) == 
          "0.01", widehat(italic("V"))["Cramer"] == "0.46", CI["95%"] ~ 
          "[" * "0.00", "0.82" * "]", italic("n")["obs"] == "32")
      
      [[3]]
      list(log[e] * (BF["01"]) == "-2.72", widehat(italic("V"))["Cramer"]^"posterior" == 
          "0.41", CI["95%"]^ETI ~ "[" * "0.00", "0.66" * "]", italic("a")["Gunel-Dickey"] == 
          "1.00")
      

---

    Code
      p7 <- ggcoefstats(lm(wt ~ mpg, mtcars))
      list(length(extract_stats(p7)), extract_subtitle(p7), extract_caption(p7))
    Output
      [[1]]
      [1] 7
      
      [[2]]
      NULL
      
      [[3]]
      NULL
      

# checking if extract_stats works for grouped plots

    Code
      p8 <- grouped_ggpiestats(mtcars, x = cyl, grouping.var = am)
      extracted_data <- extract_stats(p8)
      summary(extracted_data)
    Output
           Length Class             Mode
      [1,] 7      ggstatsplot_stats list
      [2,] 7      ggstatsplot_stats list
    Code
      extract_subtitle(p8)
    Output
      [[1]]
      list(chi["gof"]^2 * "(" * 2 * ")" == "7.68", italic(p) == "0.02", 
          widehat(italic("C"))["Pearson"] == "0.54", CI["95%"] ~ "[" * 
              "0.07", "0.73" * "]", italic("n")["obs"] == "19")
      
      [[2]]
      list(chi["gof"]^2 * "(" * 2 * ")" == "4.77", italic(p) == "0.09", 
          widehat(italic("C"))["Pearson"] == "0.52", CI["95%"] ~ "[" * 
              "0.00", "0.74" * "]", italic("n")["obs"] == "13")
      
    Code
      extract_caption(p8)
    Output
      [[1]]
      list(log[e] * (BF["01"]) == "-0.16", italic("a")["Gunel-Dickey"] == 
          "1.00")
      
      [[2]]
      list(log[e] * (BF["01"]) == "0.82", italic("a")["Gunel-Dickey"] == 
          "1.00")
      

