# between-subjects design --------------------------------------------------

# msleep dataset
msleep <- structure(list(name = c(
  "Cheetah", "Owl monkey", "Mountain beaver",
  "Greater short-tailed shrew", "Cow", "Three-toed sloth", "Northern fur seal",
  "Vesper mouse", "Dog", "Roe deer", "Goat", "Guinea pig", "Grivet",
  "Chinchilla", "Star-nosed mole", "African giant pouched rat",
  "Lesser short-tailed shrew", "Long-nosed armadillo", "Tree hyrax",
  "North American Opossum", "Asian elephant", "Big brown bat",
  "Horse", "Donkey", "European hedgehog", "Patas monkey", "Western american chipmunk",
  "Domestic cat", "Galago", "Giraffe", "Pilot whale", "Gray seal",
  "Gray hyrax", "Human", "Mongoose lemur", "African elephant",
  "Thick-tailed opposum", "Macaque", "Mongolian gerbil", "Golden hamster",
  "Vole ", "House mouse", "Little brown bat", "Round-tailed muskrat",
  "Slow loris", "Degu", "Northern grasshopper mouse", "Rabbit",
  "Sheep", "Chimpanzee", "Tiger", "Jaguar", "Lion", "Baboon", "Desert hedgehog",
  "Potto", "Deer mouse", "Phalanger", "Caspian seal", "Common porpoise",
  "Potoroo", "Giant armadillo", "Rock hyrax", "Laboratory rat",
  "African striped mouse", "Squirrel monkey", "Eastern american mole",
  "Cotton rat", "Mole rat", "Arctic ground squirrel", "Thirteen-lined ground squirrel",
  "Golden-mantled ground squirrel", "Musk shrew", "Pig", "Short-nosed echidna",
  "Eastern american chipmunk", "Brazilian tapir", "Tenrec", "Tree shrew",
  "Bottle-nosed dolphin", "Genet", "Arctic fox", "Red fox"
), genus = c(
  "Acinonyx",
  "Aotus", "Aplodontia", "Blarina", "Bos", "Bradypus", "Callorhinus",
  "Calomys", "Canis", "Capreolus", "Capri", "Cavis", "Cercopithecus",
  "Chinchilla", "Condylura", "Cricetomys", "Cryptotis", "Dasypus",
  "Dendrohyrax", "Didelphis", "Elephas", "Eptesicus", "Equus",
  "Equus", "Erinaceus", "Erythrocebus", "Eutamias", "Felis", "Galago",
  "Giraffa", "Globicephalus", "Haliochoerus", "Heterohyrax", "Homo",
  "Lemur", "Loxodonta", "Lutreolina", "Macaca", "Meriones", "Mesocricetus",
  "Microtus", "Mus", "Myotis", "Neofiber", "Nyctibeus", "Octodon",
  "Onychomys", "Oryctolagus", "Ovis", "Pan", "Panthera", "Panthera",
  "Panthera", "Papio", "Paraechinus", "Perodicticus", "Peromyscus",
  "Phalanger", "Phoca", "Phocoena", "Potorous", "Priodontes", "Procavia",
  "Rattus", "Rhabdomys", "Saimiri", "Scalopus", "Sigmodon", "Spalax",
  "Spermophilus", "Spermophilus", "Spermophilus", "Suncus", "Sus",
  "Tachyglossus", "Tamias", "Tapirus", "Tenrec", "Tupaia", "Tursiops",
  "Genetta", "Vulpes", "Vulpes"
), vore = c(
  "carni", "omni", "herbi",
  "omni", "herbi", "herbi", "carni", NA, "carni", "herbi", "herbi",
  "herbi", "omni", "herbi", "omni", "omni", "omni", "carni", "herbi",
  "omni", "herbi", "insecti", "herbi", "herbi", "omni", "omni",
  "herbi", "carni", "omni", "herbi", "carni", "carni", "herbi",
  "omni", "herbi", "herbi", "carni", "omni", "herbi", "herbi",
  "herbi", "herbi", "insecti", "herbi", "carni", "herbi", "carni",
  "herbi", "herbi", "omni", "carni", "carni", "carni", "omni",
  NA, "omni", NA, NA, "carni", "carni", "herbi", "insecti", NA,
  "herbi", "omni", "omni", "insecti", "herbi", NA, "herbi", "herbi",
  "herbi", NA, "omni", "insecti", "herbi", "herbi", "omni", "omni",
  "carni", "carni", "carni", "carni"
), order = c(
  "Carnivora", "Primates",
  "Rodentia", "Soricomorpha", "Artiodactyla", "Pilosa", "Carnivora",
  "Rodentia", "Carnivora", "Artiodactyla", "Artiodactyla", "Rodentia",
  "Primates", "Rodentia", "Soricomorpha", "Rodentia", "Soricomorpha",
  "Cingulata", "Hyracoidea", "Didelphimorphia", "Proboscidea",
  "Chiroptera", "Perissodactyla", "Perissodactyla", "Erinaceomorpha",
  "Primates", "Rodentia", "Carnivora", "Primates", "Artiodactyla",
  "Cetacea", "Carnivora", "Hyracoidea", "Primates", "Primates",
  "Proboscidea", "Didelphimorphia", "Primates", "Rodentia", "Rodentia",
  "Rodentia", "Rodentia", "Chiroptera", "Rodentia", "Primates",
  "Rodentia", "Rodentia", "Lagomorpha", "Artiodactyla", "Primates",
  "Carnivora", "Carnivora", "Carnivora", "Primates", "Erinaceomorpha",
  "Primates", "Rodentia", "Diprotodontia", "Carnivora", "Cetacea",
  "Diprotodontia", "Cingulata", "Hyracoidea", "Rodentia", "Rodentia",
  "Primates", "Soricomorpha", "Rodentia", "Rodentia", "Rodentia",
  "Rodentia", "Rodentia", "Soricomorpha", "Artiodactyla", "Monotremata",
  "Rodentia", "Perissodactyla", "Afrosoricida", "Scandentia", "Cetacea",
  "Carnivora", "Carnivora", "Carnivora"
), conservation = c(
  "lc",
  NA, "nt", "lc", "domesticated", NA, "vu", NA, "domesticated",
  "lc", "lc", "domesticated", "lc", "domesticated", "lc", NA, "lc",
  "lc", "lc", "lc", "en", "lc", "domesticated", "domesticated",
  "lc", "lc", NA, "domesticated", NA, "cd", "cd", "lc", "lc", NA,
  "vu", "vu", "lc", NA, "lc", "en", NA, "nt", NA, "nt", NA, "lc",
  "lc", "domesticated", "domesticated", NA, "en", "nt", "vu", NA,
  "lc", "lc", NA, NA, "vu", "vu", NA, "en", "lc", "lc", NA, NA,
  "lc", NA, NA, "lc", "lc", "lc", NA, "domesticated", NA, NA, "vu",
  NA, NA, NA, NA, NA, NA
), sleep_total = c(
  12.1, 17, 14.4, 14.9,
  4, 14.4, 8.7, 7, 10.1, 3, 5.3, 9.4, 10, 12.5, 10.3, 8.3, 9.1,
  17.4, 5.3, 18, 3.9, 19.7, 2.9, 3.1, 10.1, 10.9, 14.9, 12.5, 9.8,
  1.9, 2.7, 6.2, 6.3, 8, 9.5, 3.3, 19.4, 10.1, 14.2, 14.3, 12.8,
  12.5, 19.9, 14.6, 11, 7.7, 14.5, 8.4, 3.8, 9.7, 15.8, 10.4, 13.5,
  9.4, 10.3, 11, 11.5, 13.7, 3.5, 5.6, 11.1, 18.1, 5.4, 13, 8.7,
  9.6, 8.4, 11.3, 10.6, 16.6, 13.8, 15.9, 12.8, 9.1, 8.6, 15.8,
  4.4, 15.6, 8.9, 5.2, 6.3, 12.5, 9.8
), sleep_rem = c(
  NA, 1.8,
  2.4, 2.3, 0.7, 2.2, 1.4, NA, 2.9, NA, 0.6, 0.8, 0.7, 1.5, 2.2,
  2, 1.4, 3.1, 0.5, 4.9, NA, 3.9, 0.6, 0.4, 3.5, 1.1, NA, 3.2,
  1.1, 0.4, 0.1, 1.5, 0.6, 1.9, 0.9, NA, 6.6, 1.2, 1.9, 3.1, NA,
  1.4, 2, NA, NA, 0.9, NA, 0.9, 0.6, 1.4, NA, NA, NA, 1, 2.7, NA,
  NA, 1.8, 0.4, NA, 1.5, 6.1, 0.5, 2.4, NA, 1.4, 2.1, 1.1, 2.4,
  NA, 3.4, 3, 2, 2.4, NA, NA, 1, 2.3, 2.6, NA, 1.3, NA, 2.4
), sleep_cycle = c(
  NA,
  NA, NA, 0.133333333, 0.666666667, 0.766666667, 0.383333333, NA,
  0.333333333, NA, NA, 0.216666667, NA, 0.116666667, NA, NA, 0.15,
  0.383333333, NA, 0.333333333, NA, 0.116666667, 1, NA, 0.283333333,
  NA, NA, 0.416666667, 0.55, NA, NA, NA, NA, 1.5, NA, NA, NA, 0.75,
  NA, 0.2, NA, 0.183333333, 0.2, NA, NA, NA, NA, 0.416666667, NA,
  1.416666667, NA, NA, NA, 0.666666667, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, 0.183333333, NA, NA, 0.166666667, 0.15, NA, NA, 0.216666667,
  NA, 0.183333333, 0.5, NA, NA, 0.9, NA, 0.233333333, NA, NA, NA,
  0.35
), awake = c(
  11.9, 7, 9.6, 9.1, 20, 9.6, 15.3, 17, 13.9,
  21, 18.7, 14.6, 14, 11.5, 13.7, 15.7, 14.9, 6.6, 18.7, 6, 20.1,
  4.3, 21.1, 20.9, 13.9, 13.1, 9.1, 11.5, 14.2, 22.1, 21.35, 17.8,
  17.7, 16, 14.5, 20.7, 4.6, 13.9, 9.8, 9.7, 11.2, 11.5, 4.1, 9.4,
  13, 16.3, 9.5, 15.6, 20.2, 14.3, 8.2, 13.6, 10.5, 14.6, 13.7,
  13, 12.5, 10.3, 20.5, 18.45, 12.9, 5.9, 18.6, 11, 15.3, 14.4,
  15.6, 12.7, 13.4, 7.4, 10.2, 8.1, 11.2, 14.9, 15.4, 8.2, 19.6,
  8.4, 15.1, 18.8, 17.7, 11.5, 14.2
), brainwt = c(
  NA, 0.0155, NA,
  0.00029, 0.423, NA, NA, NA, 0.07, 0.0982, 0.115, 0.0055, NA,
  0.0064, 0.001, 0.0066, 0.00014, 0.0108, 0.0123, 0.0063, 4.603,
  3e-04, 0.655, 0.419, 0.0035, 0.115, NA, 0.0256, 0.005, NA, NA,
  0.325, 0.01227, 1.32, NA, 5.712, NA, 0.179, NA, 0.001, NA, 4e-04,
  0.00025, NA, 0.0125, NA, NA, 0.0121, 0.175, 0.44, NA, 0.157,
  NA, 0.18, 0.0024, NA, NA, 0.0114, NA, NA, NA, 0.081, 0.021, 0.0019,
  NA, 0.02, 0.0012, 0.00118, 0.003, 0.0057, 0.004, NA, 0.00033,
  0.18, 0.025, NA, 0.169, 0.0026, 0.0025, NA, 0.0175, 0.0445, 0.0504
), bodywt = c(
  50, 0.48, 1.35, 0.019, 600, 3.85, 20.49, 0.045,
  14, 14.8, 33.5, 0.728, 4.75, 0.42, 0.06, 1, 0.005, 3.5, 2.95,
  1.7, 2547, 0.023, 521, 187, 0.77, 10, 0.071, 3.3, 0.2, 899.995,
  800, 85, 2.625, 62, 1.67, 6654, 0.37, 6.8, 0.053, 0.12, 0.035,
  0.022, 0.01, 0.266, 1.4, 0.21, 0.028, 2.5, 55.5, 52.2, 162.564,
  100, 161.499, 25.235, 0.55, 1.1, 0.021, 1.62, 86, 53.18, 1.1,
  60, 3.6, 0.32, 0.044, 0.743, 0.075, 0.148, 0.122, 0.92, 0.101,
  0.205, 0.048, 86.25, 4.5, 0.112, 207.501, 0.9, 0.104, 173.33,
  2, 3.38, 4.23
)), class = c("tbl_df", "tbl", "data.frame"), row.names = c(
  NA,
  -83L
))

test_that(
  desc = "`pairwise_comparisons()` works for between-subjects design",
  code = {
    set.seed(123)

    options(tibble.width = Inf)

    # student's t
    set.seed(123)
    df1 <- pairwise_comparisons(
      data = msleep,
      x = vore,
      y = brainwt,
      type = "p",
      var.equal = TRUE,
      paired = FALSE,
      p.adjust.method = "bonferroni"
    )

    # games-howell
    df_msleep <- msleep

    # adding empty factor level (shouldn't change results)
    df_msleep %<>% dplyr::mutate(vore = as.factor(vore))

    df_msleep$vore <- factor(df_msleep$vore, levels = c(levels(df_msleep$vore), "Random"))

    set.seed(123)
    df2 <- pairwise_comparisons(
      data = df_msleep,
      x = vore,
      y = brainwt,
      type = "p",
      var.equal = FALSE,
      paired = FALSE,
      p.adjust.method = "bonferroni"
    )

    # Dunn test
    set.seed(123)
    df3 <- pairwise_comparisons(
      data = msleep,
      x = vore,
      y = brainwt,
      type = "np",
      paired = FALSE,
      p.adjust.method = "none"
    )

    # robust t test
    set.seed(123)
    df4 <- pairwise_comparisons(
      data = msleep,
      x = vore,
      y = brainwt,
      type = "r",
      paired = FALSE,
      p.adjust.method = "fdr"
    )

    # checking the edge case where factor level names contain `-`
    set.seed(123)
    df5 <- pairwise_comparisons(
      data = movies_long,
      x = mpaa,
      y = rating,
      var.equal = TRUE
    )

    # bayes test
    set.seed(123)
    df6 <- pairwise_comparisons(
      data = df_msleep,
      x = vore,
      y = brainwt,
      type = "bf",
      k = 3
    )

    expect_snapshot(list(df1, df2, df3, df4, df5))


    expect_equal(df6$log_e_bf10,
      c(
        -0.616556955077368,
        -0.331816123738985,
        -0.850766925918558,
        -0.615915090483787,
        -0.559562332764069,
        -0.6062922675725
      ),
      tolerance = 0.01
    )
  }
)

# dropped levels --------------------------------------------------

test_that(
  desc = "dropped levels are not included",
  code = {
    set.seed(123)

    # drop levels
    msleep2 <- dplyr::filter(.data = msleep, vore %in% c("carni", "omni"))

    # check those levels are not included
    set.seed(123)
    df1 <- pairwise_comparisons(
      data = msleep2,
      x = vore,
      y = brainwt,
      p.adjust.method = "none"
    )

    set.seed(123)
    df2 <- pairwise_comparisons(
      data = msleep,
      x = vore,
      y = brainwt,
      p.adjust.method = "none"
    ) %>%
      dplyr::filter(group2 == "omni", group1 == "carni")

    # tests
    expect_equal(df1$statistic, df2$statistic, tolerance = 0.01)
    expect_identical(df2$label, "list(~italic(p)[uncorrected]==0.865)")
  }
)

# data without NAs --------------------------------------------------

test_that(
  desc = "data without NAs",
  code = {
    set.seed(123)
    df <- pairwise_comparisons(
      data = iris,
      x = Species,
      y = Sepal.Length,
      type = "p",
      p.adjust.method = "fdr",
      var.equal = TRUE
    )

    expect_equal(
      df$label,
      c(
        "list(~italic(p)[FDR-corrected]==1.32e-15)",
        "list(~italic(p)[FDR-corrected]==6.64e-32)",
        "list(~italic(p)[FDR-corrected]==2.77e-09)"
      )
    )
  }
)
