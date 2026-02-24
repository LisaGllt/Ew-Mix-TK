# General setup ----

f_load_libraries <- function() {
  # 📦 Data Manipulation
  library(tidyverse) # Collection de packages pour manipulation et visualisation des données
  library(here) # Gestion des chemins de fichiers
  library(readxl) # Lecture des fichiers Excel
  library(reshape2) # Restructuration des données
  library(DT) # Génération de tables interactives
  library(knitr) # Génération de rapports dynamiques en RMarkdown/Quarto

  # 🎨 Visualization
  library(ggplot2) # Visualisation de données
  library(ggthemes) # Thèmes pour ggplot2
  library(ggdist) # Distribution et incertitude
  library(ggsci) # Palettes de couleurs scientifiques
  library(viridis) # Palette de couleurs perceptuellement uniforme
  library(wesanderson) # Palette de couleurs artistiques
  library(RColorBrewer) # Palettes de couleurs prédéfinies
  library(nord) # Palettes de couleurs inspirées du Nord
  library(plotly) # Graphiques interactifs
  library(ggiraph) # Graphiques interactifs pour ggplot2
  library(ggrepel) # Étiquettes non chevauchantes sur ggplot2
  library(patchwork) # Combinaison de plusieurs ggplots
  library(gridExtra) # Arrangements de graphiques en grille
  library(grid) # Outils de mise en page graphique
  library(ggbreak) # Briser les axes dans ggplot2
  library(ggtext) # Formatage avancé de texte dans ggplot2
  library(kableExtra) # Mise en forme avancée des tables
  library(flextable)
  library(gt) # Tables rendering
  library(processx)
  library(metR)
  library(rnaturalearth)
  library(sf)
  library(scales)
  library(colorspace)
  library(IDPmisc)
  library(ggforce)

  # 📊 Statistical Modeling & Bayesian Analysis
  library(brms) # Modélisation bayésienne avec Stan
  library(rstan) # Interface R pour Stan
  library(cmdstanr) # Interface alternative pour Stan (CmdStan)
  library(tidybayes) # Manipulation et visualisation des résultats bayésiens
  library(ggmcmc) # Diagnostics des chaînes MCMC
  library(rethinking) # Modélisation bayésienne avancée
  library(priorsense) # Analyse de sensibilité des priors
  library(coda)
  library(ggmcmc)
  library(bayesnec)
  library(truncnorm)
  library(minpack.lm)

  # 🔬 Regression & Hypothesis Testing
  library(car) # Tests statistiques et régressions avancées
  library(nlstools) # Outils pour modèles non linéaires
  library(lsmeans) # Comparaisons post-hoc
  library(ggpubr) # Outils pour publications scientifiques
  library(marginaleffects) # Effets marginaux des modèles
  library(brglm2) # Régressions logistiques biais-réduits
  library(multcomp)

  # ⚙️ Computational Tools & Parallelization
  library(parallel) # Calcul parallèle
  library(deSolve) # Équations différentielles
  library(tmvtnorm) # Distribution normale tronquée multivariée
  library(fdrtool) # Faux taux de découverte (FDR)
  library(drc) # Modélisation de réponses aux doses

  # 🛠️ Model Evaluation & Performance
  library(easystats) # Outils pour statistiques et modèles
  library(performance) # Diagnostics et évaluation de modèles
  library(modelsummary) # Résumé des modèles statistiques
  library(plan) # Planification de l'exécution des tâches

  # 🖋️ Math & LaTeX Support
  library(latex2exp) # Expressions LaTeX dans ggplot2
  library(extrafont) # Gestion des polices pour ggplot2

  library(data.table)
}


f_load_colors <- function() {
  col_blue <<- "#5E81AC"
  col_red <<- "#f42404"

  pal_blue <- c("#5E81AC", "#7F9DC4", "#A0C1D9", "#DCE9F2")
  pal_red <<- c("#f42404", "#F65E4B", "#F6876D", "#FBD3D0")

  Nord_frost <<- nord(palette = "frost")
  Nord_aurora <<- nord(palette = "aurora")
  Nord_polar <<- nord(palette = "polarnight")
  Nord_snow <<- nord(palette = "snowstorm")

  pal_col <<- c(Nord_aurora[1], Nord_frost[4])
  col_EPX <<- Nord_frost[2]
  col_IMD <<- Nord_frost[4]
  col_elim <<- Nord_aurora[4]
  col_uptake <<- Nord_aurora[1]
  shape_IMD <<- 16
  shape_EPX <<- 15
  col_Molecule <<- c(col_EPX, col_IMD)
  col_molec <<- c(col_EPX, col_IMD)
  sizetitle <<- 12

  shape_Molecule <<- c(shape_EPX, shape_IMD)
  shape_molec <<- c(shape_EPX, shape_IMD)

  set.seed(121212)
}

f_load_libraries_colors <- function() {
  f_load_libraries()
  f_load_colors()
}


# Add My Pet parameter recuperation ----

getDEB.species <- function() {
  require(rvest)
  library(rvest)
  url <- "https://www.bio.vu.nl/thb/deb/deblab/add_my_pet/species_list.html"
  d1 <- read_html(url)

  phylum <- d1 %>%
    html_nodes("td:nth-child(1)") %>%
    html_text()
  class <- d1 %>%
    html_nodes("td:nth-child(2)") %>%
    html_text()
  order <- d1 %>%
    html_nodes("td:nth-child(3)") %>%
    html_text()
  family <- d1 %>%
    html_nodes("td:nth-child(4)") %>%
    html_text()
  species <- d1 %>%
    html_nodes("td:nth-child(5)") %>%
    html_text()
  common <- d1 %>%
    html_nodes("td:nth-child(6)") %>%
    html_text()
  type <- d1 %>%
    html_nodes("td:nth-child(7)") %>%
    html_text()
  mre <- d1 %>%
    html_nodes("td:nth-child(8)") %>%
    html_text()
  smre <- d1 %>%
    html_nodes("td:nth-child(9)") %>%
    html_text()
  complete <- d1 %>%
    html_nodes("td:nth-child(10)") %>%
    html_text()
  all.species <- as.data.frame(
    cbind(
      phylum, class, order,
      family, species, common, type, mre, smre, complete
    ),
    stringsAsFactors = FALSE
  )
  all.species$species <- gsub(" ", "_", all.species$species)
  all.species$mre <- as.numeric(mre)
  all.species$smre <- as.numeric(smre)
  all.species$complete <- as.numeric(complete)
  return(all.species)
}

getDEB.pars <- function(species) {
  require(rvest)
  library(rvest)
  baseurl <- "https://www.bio.vu.nl/thb/deb/deblab/add_my_pet/entries_web/"
  d1 <- read_html(paste0(baseurl, species, "/", species, "_par.html"))
  symbol1 <- d1 %>%
    html_nodes("td:nth-child(1)") %>%
    html_text()

  value1 <- d1 %>%
    html_nodes("td:nth-child(2)") %>%
    html_text()

  units1 <- d1 %>%
    html_nodes("td:nth-child(3)") %>%
    html_text()

  description1 <- d1 %>%
    html_nodes("td:nth-child(4)") %>%
    html_text()

  extra1 <- d1 %>%
    html_nodes("td:nth-child(5)") %>%
    html_text()

  extra2 <- d1 %>%
    html_nodes("td:nth-child(6)") %>%
    html_text()
  end <- which(symbol1 == "T_ref")
  symbol <- symbol1[1:end]
  value <- value1[1:end]
  units <- units1[1:end]
  description <- description1[1:end]

  pars <- as.data.frame(cbind(symbol, value, units, description))
  pars$symbol <- as.character(symbol)
  pars$value <- as.numeric(value)
  pars$units <- as.character(units)
  pars$description <- as.character(description)

  chempot <- c(value1[end + 1], units1[end + 1], description1[end +
    1], extra1[1])
  dens <- c(value1[end + 2], units1[end + 2], description1[end +
    2], extra1[2])
  org.C <- c(
    units1[end + 3], description1[end + 3], extra1[3],
    extra2[1]
  )
  org.H <- c(value1[end + 4], units1[end + 4], description1[end +
    4], extra1[4])
  org.O <- c(value1[end + 5], units1[end + 5], description1[end +
    5], extra1[5])
  org.N <- c(value1[end + 6], units1[end + 6], description1[end +
    6], extra1[6])
  min.C <- c(
    units1[end + 7], description1[end + 7], extra1[7],
    extra2[2]
  )
  min.H <- c(value1[end + 8], units1[end + 8], description1[end +
    8], extra1[8])
  min.O <- c(value1[end + 9], units1[end + 9], description1[end +
    9], extra1[9])
  min.N <- c(value1[end + 10], units1[end + 10], description1[end +
    10], extra1[10])

  organics <- rbind(org.C, org.H, org.O, org.N)
  minerals <- rbind(min.C, min.H, min.O, min.N)
  colnames(organics) <- c("X", "V", "E", "P")
  colnames(minerals) <- c("CO2", "H2O", "O2", "N-waste")
  rownames(organics) <- c("C", "H", "O", "N")
  rownames(minerals) <- c("C", "H", "O", "N")
  class(chempot) <- "numeric"
  class(dens) <- "numeric"
  class(organics) <- "numeric"
  class(minerals) <- "numeric"

  return(list(
    pars = pars, chempot = chempot, dens = dens,
    organics = organics, minerals = minerals
  ))
}

getDEB.implied <- function(species) {
  require(rvest)
  library(rvest)
  baseurl <- "https://www.bio.vu.nl/thb/deb/deblab/add_my_pet/entries_web/"
  d1 <- read_html(paste0(baseurl, species, "/", species, "_stat.html"))
  symbol <- d1 %>%
    html_nodes("td:nth-child(1)") %>%
    html_text()

  value <- d1 %>%
    html_nodes("td:nth-child(2)") %>%
    html_text()

  units <- d1 %>%
    html_nodes("td:nth-child(3)") %>%
    html_text()

  description <- d1 %>%
    html_nodes("td:nth-child(4)") %>%
    html_text()

  final <- as.data.frame(cbind(symbol, value, units, description))
  final$symbol <- as.character(symbol)
  final$value <- as.numeric(value)
  final$units <- as.character(units)
  final$description <- as.character(description)
  return(final)
}

f_GetParsAddMyPet <- function(Texp) {
  Species <- "Aporrectodea_caliginosa"
  allpars <- getDEB.pars(Species)
  df_pars <- allpars$pars
  df_pars <- df_pars[-1, ] # remove duplicate T_A symbol

  df_rename <- data.frame(
    symbol = c(
      "p_Am", "F_m", "kap_X", "kap_P", "v", "kap", "kap_R", "p_M", "p_T",
      "k_J", "E_G", "E_Hb", "E_Hp", "h_a", "s_G", "L0", "T_AH", "T_H",
      "Wwg", "bw", "del_M", "f", "f_Bart_high", "f_Bart_high_b",
      "f_Bart_low", "f_Bart_low_b", "f_Bart_medium", "m_0", "max_r_mb",
      "mu_OM", "mu_c", "r_mb", "t_0", "wV", "T_A", "T_ref", "Wd_0"
    ),
    name = c(
      "pAm", "Fm", "kapX", "kapP", "v", "kap", "kapR", "pM", "pT",
      "kJ", "Eg", "Ehb", "Ehp", "ha", "sG", "L0", "TAH", "TH",
      "Wwg", "bw", "Shape", "f", "fBartHigh", "fBartHighb",
      "fBartLow", "fBartLowb", "fBartMedium", "m0", "maxRmb",
      "muOM", "muC", "Rmb", "t0", "wV", "TA", "Tref", "Wd0"
    )
  )

  df_pars <- df_pars |> left_join(df_rename)

  df_pars <- rbind(df_pars, data.frame(
    symbol = "UE0",
    value = 0.078, # from Gergs et al., 2022
    units = "cm^2/d",
    description = "Scaled cost of an egg",
    name = "UE0"
  ))
  df_pars <- rbind(df_pars, data.frame(
    symbol = "E0",
    value = 134.988, # from AddMyPet website
    units = "J",
    description = "Scaled cost of an egg",
    name = "E0"
  ))
  df_pars <- rbind(df_pars, data.frame(
    symbol = "w",
    value = 27.24, # from Gergs et al., 2022
    units = "g/cm^3",
    description = "Contribution of reserve to bw",
    name = "w"
  ))


  # Correction des valeurs par la température (Gergs et al. 2022)

  df_pars$corr_value <- df_pars$value

  TA <- subset(df_pars, name == "TA")$corr_value
  TAH <- subset(df_pars, name == "TAH")$corr_value
  TH <- subset(df_pars, name == "TH")$corr_value
  Tref <- subset(df_pars, name == "Tref")$corr_value

  sA <- exp(TA / Tref - TA / (Texp + 273.15))
  srH <- (1 + exp(TAH / TH - TAH / Tref)) / (1 + exp(TAH / TH - TAH / (Texp + 273.15)))
  Ft <- sA * ((T + 273.15 >= Tref) * srH + (T + 273.15 < Tref))

  df_pars_add <- df_pars |>
    filter(name %in% c("pAm", "pM", "v", "kJ")) |>
    mutate(corr_value = value * Ft) |>
    mutate(
      symbol = paste(symbol, "_t", sep = ""),
      name = paste(name, "_t", sep = "")
    )

  df_pars <- rbind(df_pars, df_pars_add)

  # Liste des paramètres pour le modèle
  vec_pars <- df_pars$corr_value
  names(vec_pars) <- df_pars$name
  vec_pars <- append(vec_pars, c(kapH = 1)) # set the maturation efficiency to 1
  return(df_pars)
}

f_vec_pars <- function(df_pars) {
  # Liste des paramètres pour le modèle
  vec_pars <- df_pars$corr_value
  names(vec_pars) <- df_pars$name
  vec_pars <- append(vec_pars, c(kapH = 1)) # set the maturation efficiency to 1
  return(vec_pars)
}

# TK models ----

# MCSim files creation 

f_read_data_TK <- function(Molecule) {
  df_TK <- read_excel(here::here("data/Data_TK_long.xlsx"), sheet = 1) |>
    mutate(
      Time_point_f = as.factor(Time_point),
     # ID = as.factor(ID),
      w = w / 1000, # Conversion mg to g
      Dose = Dose * 1000 # mg/kg to ng/g (mg/kg = microg/g = 1000 ng/g)
    ) |>
    filter(Keep == "Yes") |>
    group_by(ID) |>
    mutate(t = as.numeric(difftime(Date, first(Date), units = "days"))) |>
    ungroup()

  Dose_EPX <- subset(df_TK, Molecule == "EPX")$Dose[1]
  Dose_IMD <- subset(df_TK, Molecule == "IMD")$Dose[1]
  
  rW <- 0.851

  if (Molecule == "EPX") {
    df_TK_f <- df_TK |>
      dplyr::select(Molecule, Dose, ID, expo, Phase, w, C_worm_EPX, C_worm_IMD, t, Time_point, ID_recipient, Nb_rep, Experiment) |>
      filter(Molecule == "EPX") |>
      mutate(
        C_soil_IMD = case_when(
          t == 0 & ID_recipient == "1" ~ 0.089 * 1000, # ng/g
          t == 0 & ID_recipient == "2" ~ 0.072 * 1000 # ng/g
        ),
        C_soil_EPX = case_when(
          t == 0 & ID_recipient == "1" ~ 0.780 * 1000, # ng/g
          t == 0 & ID_recipient == "2" ~ 1.070 * 1000 # ng/g
        ),
        Weight_wg = if_else(Phase == "Frozen", -1, w),
        Weight_nog = if_else(!(Phase == "Frozen"), rW*w, w), 
      ) |>
      mutate(across(where(is.numeric), ~ replace_na(.x, -1))) |>
      arrange(ID)
    
    df_TK_Ctrl <- read_excel(here::here("data/Data_TK_long.xlsx"), sheet = 2)

    C_worm_t0_IMD <- mean(df_TK_Ctrl$C_worm_IMD, na.rm = T)
    C_worm_t0_EPX <- mean(df_TK_Ctrl$C_worm_EPX, na.rm = T)

    df_TK_f[df_TK_f$t == 0, ]$C_worm_IMD <- C_worm_t0_IMD
    df_TK_f[df_TK_f$t == 0, ]$C_worm_EPX <- C_worm_t0_EPX
  } else if (Molecule == "IMD") {
    df_TK_f <- df_TK |>
      dplyr::select(Molecule, Dose, ID, expo, Phase, w, C_worm_EPX, C_worm_IMD, t, Time_point, ID_recipient, Nb_rep, Experiment) |>
      filter(Molecule == "IMD") |>
      mutate(
        C_soil_IMD = case_when(
          t == 0 & ID_recipient == "1" ~ 0.089 * 1000, # ng/g
          t == 0 & ID_recipient == "2" ~ 0.072 * 1000 # ng/g
        ),
        C_soil_EPX = case_when(
          t == 0 & ID_recipient == "1" ~ 0.780 * 1000, # ng/g
          t == 0 & ID_recipient == "2" ~ 1.070 * 1000 # ng/g
        ),
        Weight_wg = if_else(Phase == "Frozen", -1, w),
        Weight_nog = if_else(!(Phase == "Frozen"), rW*w, w)
      ) |>
      mutate(across(where(is.numeric), ~ replace_na(.x, -1))) |>
      arrange(ID)
    
    df_TK_Ctrl <- read_excel(here::here("data/Data_TK_long.xlsx"), sheet = 2)

    C_worm_t0_IMD <- mean(df_TK_Ctrl$C_worm_IMD, na.rm = T)
    C_worm_t0_EPX <- mean(df_TK_Ctrl$C_worm_EPX, na.rm = T)

    df_TK_f[df_TK_f$t == 0, ]$C_worm_IMD <- C_worm_t0_IMD
    df_TK_f[df_TK_f$t == 0, ]$C_worm_EPX <- C_worm_t0_EPX
  } else {
    stop("Uncorrect specification of Molecule")
  }

  return(df_TK_f)
}

f_In_experiments <- function(Molecule) {
  
  df_TK_f <- f_read_data_TK(Molecule)

  C_clx_IMD <- 16 / 1000 # ng/g
  C_clx_EPX <- 90 / 1000 # ng/g
  
  C_worm_t0_IMD <-  df_TK_f[df_TK_f$t == 0, ]$C_worm_IMD[1]
  C_worm_t0_EPX <-  df_TK_f[df_TK_f$t == 0, ]$C_worm_EPX[1]


  if (Molecule == "IMD") {
    char_final <- ""
    for (i in as.numeric(unique(df_TK_f$ID))) {
      
      df_TK_i <- subset(df_TK_f, ID == i) 

      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww_nopopo <- paste(df_TK_i$Weight_nog, collapse = ",")

      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiIMD <- paste(df_TK_i$C_worm_IMD, collapse = ",")

      char_expo <- paste(df_TK_i$expo, collapse = ",")
      length_expo <- length(df_TK_i$expo)
      char_texpo <- char_tw
      
      if (i <= 64|i>128){
        char_Event <- ""
      } else {
        telim <- subset(df_TK_i, expo==0)$t[1]
        char_Event <- paste("    event_Ce=Events(CeIMD,1,", telim, ", Replace,", C_clx_IMD,");", sep="")
      }

      char_i <- paste(
        paste("Experiment { #", "TK IMD - i = ", i),
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight_nog, ";", sep = ""),
        paste("    Ci0IMD=", C_worm_t0_IMD, ";", sep = ""),
        paste("    Ce0IMD=", subset(df_TK_i, t == 0)$C_soil_IMD, ";", sep = ""),
        paste("    CclxIMD=", C_clx_IMD, ";", sep = ""),
        char_Event,
        paste("    Print(Weight,", char_tw, ");", sep = ""),
        paste("    Data(Weight,", char_Ww_nopopo, ");", sep = ""),
        paste("    Print(CiIMD,", char_tCi, ");", sep = ""),
        paste("    Data(CiIMD,", char_CiIMD, ");", sep = ""),
        paste("    expo=NDoses(", length_expo, ",\n               ",
          char_expo, ",\n               ", char_texpo, ");",
          sep = ""
        ),
        paste("}"),
        sep = "\n"
      )
      char_final <- paste(char_final, char_i, sep = "\n")
    } 
  } # End IMD
  
  else if (Molecule == "EPX") {
    char_final <- ""
    
    for (i in as.numeric(unique(df_TK_f$ID))) {

      df_TK_i <- subset(df_TK_f, ID == i) 

      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww_nopopo <- paste(df_TK_i$Weight_nog, collapse = ",")

      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiEPX <- paste(df_TK_i$C_worm_EPX, collapse = ",")

      char_expo <- paste(df_TK_i$expo, collapse = ",")
      length_expo <- length(df_TK_i$expo)
      char_texpo <- char_tw
      
      if (i <= 64|i>128){
        char_Event <- ""
      } else {
        telim <- subset(df_TK_i, expo==0)$t[1]
        char_Event <- paste("    event_Ce=Events(CeEPX,1,", telim, ", Replace,", C_clx_EPX,");", sep="")
      }

      char_i <- paste(
        paste("Experiment { #", "TK EPX - i = ", i),
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight_nog, ";", sep = ""),
        paste("    Ci0EPX=", C_worm_t0_EPX, ";", sep = ""),
        paste("    Ce0EPX=", subset(df_TK_i, t == 0)$C_soil_EPX, ";", sep = ""),
        paste("    CclxEPX=", C_clx_EPX, ";", sep = ""),
        char_Event,
        paste("    Print(Weight,", char_tw, ");", sep = ""),
        paste("    Data(Weight,", char_Ww_nopopo, ");", sep = ""),
        paste("    Print(CiEPX,", char_tCi, ");", sep = ""),
        paste("    Data(CiEPX,", char_CiEPX, ");", sep = ""),
        paste("    expo=NDoses(", length_expo, ",\n               ",
          char_expo, ",\n               ", char_texpo, ");",
          sep = ""
        ),
        paste("}"),
        sep = "\n"
      )

      char_final <- paste(char_final, char_i, sep = "\n")
    } # End EPX
  } else {
    stop("Uncorrect specification of Molecule")
  }
  return(char_final)
}

f_In_experiments_3Uptake_1 <- function(Molecule) {
  
  df_TK_f <- f_read_data_TK(Molecule) |> 
    mutate(ID = as.numeric(ID))
  
  C_clx_IMD <- 16 / 1000 # ng/g
  C_clx_EPX <- 90 / 1000 # ng/g
  
  C_worm_t0_IMD <-  df_TK_f[df_TK_f$t == 0, ]$C_worm_IMD[1]
  C_worm_t0_EPX <-  df_TK_f[df_TK_f$t == 0, ]$C_worm_EPX[1]
  
  sigma <- 1e-3
  
  df_TK_3x_2 <- subset(df_TK_f, Time_point < 22) |> 
    mutate(
      ID = ID + 1000,
      C_worm_EPX = ifelse(C_worm_EPX == -1, -1, C_worm_EPX + rnorm(n(),0,sigma)),
      Weight_nog = ifelse(w == -1, -1, Weight_nog + rnorm(n(),0,sigma))
      )
  
  df_TK_3x_3 <- subset(df_TK_f, Time_point < 22) |> 
    mutate(
      ID = ID + 2000,
      C_worm_EPX = ifelse(C_worm_EPX == -1, -1, C_worm_EPX + rnorm(n(),0,sigma)),
      Weight_nog = ifelse(w == -1, -1, Weight_nog + rnorm(n(),0,sigma))
      )
  
  df_TK_f <- rbind(df_TK_f, df_TK_3x_2, df_TK_3x_3)
  
  if (Molecule == "IMD") {
    char_final <- ""
    for (i in as.numeric(unique(df_TK_f$ID))) {
      
      df_TK_i <- subset(df_TK_f, ID == i) 
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww_nopopo <- paste(df_TK_i$Weight_nog, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiIMD <- paste(df_TK_i$C_worm_IMD, collapse = ",")
      
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      length_expo <- length(df_TK_i$expo)
      char_texpo <- char_tw
      
      if (i%%1000 <= 64){
        char_Event <- ""
      } else {
        telim <- subset(df_TK_i, expo==0)$t[1]
        char_Event <- paste("    event_Ce=Events(CeIMD,1,", telim, ", Replace,", C_clx_IMD,");", sep="")
      }
      
      char_i <- paste(
        paste("Experiment { #", "TK IMD - i = ", i),
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight_nog, ";", sep = ""),
        paste("    Ci0IMD=", C_worm_t0_IMD, ";", sep = ""),
        paste("    Ce0IMD=", subset(df_TK_i, t == 0)$C_soil_IMD, ";", sep = ""),
        paste("    CclxIMD=", C_clx_IMD, ";", sep = ""),
        char_Event,
        paste("    Print(Weight,", char_tw, ");", sep = ""),
        paste("    Data(Weight,", char_Ww_nopopo, ");", sep = ""),
        paste("    Print(CiIMD,", char_tCi, ");", sep = ""),
        paste("    Data(CiIMD,", char_CiIMD, ");", sep = ""),
        paste("    expo=NDoses(", length_expo, ",\n               ",
              char_expo, ",\n               ", char_texpo, ");",
              sep = ""
        ),
        paste("}"),
        sep = "\n"
      )
      char_final <- paste(char_final, char_i, sep = "\n")
    } 
  } # End IMD
  
  else if (Molecule == "EPX") {
    char_final <- ""
    
    for (i in as.numeric(unique(df_TK_f$ID))) {
      
      df_TK_i <- subset(df_TK_f, ID == i) 
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww_nopopo <- paste(df_TK_i$Weight_nog, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiEPX <- paste(df_TK_i$C_worm_EPX, collapse = ",")
      
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      length_expo <- length(df_TK_i$expo)
      char_texpo <- char_tw
      
      if (i%%1000 <= 64){
        char_Event <- ""
      } else {
        telim <- subset(df_TK_i, expo==0)$t[1]
        char_Event <- paste("    event_Ce=Events(CeEPX,1,", telim, ", Replace,", C_clx_EPX,");", sep="")
      }
      
      char_i <- paste(
        paste("Experiment { #", "TK EPX - i = ", i),
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight_nog, ";", sep = ""),
        paste("    Ci0EPX=", C_worm_t0_EPX, ";", sep = ""),
        paste("    Ce0EPX=", subset(df_TK_i, t == 0)$C_soil_EPX, ";", sep = ""),
        paste("    CclxEPX=", C_clx_EPX, ";", sep = ""),
        char_Event,
        paste("    Print(Weight,", char_tw, ");", sep = ""),
        paste("    Data(Weight,", char_Ww_nopopo, ");", sep = ""),
        paste("    Print(CiEPX,", char_tCi, ");", sep = ""),
        paste("    Data(CiEPX,", char_CiEPX, ");", sep = ""),
        paste("    expo=NDoses(", length_expo, ",\n               ",
              char_expo, ",\n               ", char_texpo, ");",
              sep = ""
        ),
        paste("}"),
        sep = "\n"
      )
      
      char_final <- paste(char_final, char_i, sep = "\n")
    } # End EPX
  } else {
    stop("Uncorrect specification of Molecule")
  }
  return(char_final)
}

f_In_experiments_3Uptake <- function(Molecule) {
  
  df_TK_f <- f_read_data_TK(Molecule) |> 
    mutate(
      ID = as.numeric(ID),
      t = as.numeric(t)
      )
  
  C_clx_IMD <- 16 / 1000 # ng/g
  C_clx_EPX <- 90 / 1000 # ng/g
  
  C_worm_t0_IMD <-  df_TK_f[df_TK_f$t == 0, ]$C_worm_IMD[1]
  C_worm_t0_EPX <-  df_TK_f[df_TK_f$t == 0, ]$C_worm_EPX[1]
  
  epsilon <- 1e-6
  
  df_TK_3x_p <- subset(df_TK_f, Time_point < 22) |> 
    mutate(
      t = abs(t + epsilon)
    )
  df_TK_3x_m <- subset(df_TK_f, Time_point < 22) |> 
    mutate(
      t = ifelse(t==0, t+1/2*epsilon, t - epsilon)
    )
  
  df_TK_f <- rbind(df_TK_f, df_TK_3x_p, df_TK_3x_m) |> 
    arrange(ID, t)
  
  
  if (Molecule == "IMD") {
    char_final <- ""
    for (i in as.numeric(unique(df_TK_f$ID))) {
      
      df_TK_i <- subset(df_TK_f, ID == i) 
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww_nopopo <- paste(df_TK_i$Weight_nog, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiIMD <- paste(df_TK_i$C_worm_IMD, collapse = ",")
      
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      length_expo <- length(df_TK_i$expo)
      char_texpo <- char_tw
      
      if (i <= 64){
        char_Event <- ""
      } else {
        telim <- subset(df_TK_i, expo==0)$t[1]
        char_Event <- paste("    event_Ce=Events(CeIMD,1,", telim, ", Replace,", C_clx_IMD,");", sep="")
      }
      
      char_i <- paste(
        paste("Experiment { #", "TK IMD - i = ", i),
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight_nog, ";", sep = ""),
        paste("    Ci0IMD=", C_worm_t0_IMD, ";", sep = ""),
        paste("    Ce0IMD=", subset(df_TK_i, t == 0)$C_soil_IMD, ";", sep = ""),
        paste("    CclxIMD=", C_clx_IMD, ";", sep = ""),
        char_Event,
        paste("    Print(Weight,", char_tw, ");", sep = ""),
        paste("    Data(Weight,", char_Ww_nopopo, ");", sep = ""),
        paste("    Print(CiIMD,", char_tCi, ");", sep = ""),
        paste("    Data(CiIMD,", char_CiIMD, ");", sep = ""),
        paste("    expo=NDoses(", length_expo, ",\n               ",
              char_expo, ",\n               ", char_texpo, ");",
              sep = ""
        ),
        paste("}"),
        sep = "\n"
      )
      char_final <- paste(char_final, char_i, sep = "\n")
    } 
  } # End IMD
  
  else if (Molecule == "EPX") {
    char_final <- ""
    
    for (i in as.numeric(unique(df_TK_f$ID))) {
      
      df_TK_i <- subset(df_TK_f, ID == i) 
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww_nopopo <- paste(df_TK_i$Weight_nog, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiEPX <- paste(df_TK_i$C_worm_EPX, collapse = ",")
      
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      length_expo <- length(df_TK_i$expo)
      char_texpo <- char_tw
      
      if (i <= 64){
        char_Event <- ""
      } else {
        telim <- subset(df_TK_i, expo==0)$t[1]
        char_Event <- paste("    event_Ce=Events(CeEPX,1,", telim, ", Replace,", C_clx_EPX,");", sep="")
      }
      
      char_i <- paste(
        paste("Experiment { #", "TK EPX - i = ", i),
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight_nog, ";", sep = ""),
        paste("    Ci0EPX=", C_worm_t0_EPX, ";", sep = ""),
        paste("    Ce0EPX=", subset(df_TK_i, t == 0)$C_soil_EPX, ";", sep = ""),
        paste("    CclxEPX=", C_clx_EPX, ";", sep = ""),
        char_Event,
        paste("    Print(Weight,", char_tw, ");", sep = ""),
        paste("    Data(Weight,", char_Ww_nopopo, ");", sep = ""),
        paste("    Print(CiEPX,", char_tCi, ");", sep = ""),
        paste("    Data(CiEPX,", char_CiEPX, ");", sep = ""),
        paste("    expo=NDoses(", length_expo, ",\n               ",
              char_expo, ",\n               ", char_texpo, ");",
              sep = ""
        ),
        paste("}"),
        sep = "\n"
      )
      
      char_final <- paste(char_final, char_i, sep = "\n")
    } # End EPX
  } else {
    stop("Uncorrect specification of Molecule")
  }
  return(char_final)
}

f_create_mcmc_block <- function(name, seed, Nb_Iter) {
  
  sprintf(
    'MCMC( "%s.out",  # output file
      "",                # name of restart file
      "",                # name of data file
      %d, 0,             # iterations, print prediction flag
      1, 10000,          # printing frequency, iters to print
      %d);               # random seed

Integrate (Lsodes,  1e-5, 1e-7, 0); # Integrate(Solver, RTOL, ATOL, ITOL);
    ',
    name, Nb_Iter, seed
  )
}

f_In_tot <- function(File_path, Molecule, text_priors, text_likelihood, text_param_ind = "", Nb_Iter = 10000, seeds = c(C1 = 3333, C2 = 6666, C3 = 1212)) {

  text_Level_global <-
    "Level{ # Global
    "

  text_Level_exp <- "
Level{
  "

  banner_exp <- "
  ############## Individuals ###################
  "

  text_experiment <- paste0(banner_exp, f_In_experiments(Molecule), sep = "\n")

  text_end <- "
} # End
} # End global

End."

  # .in generation and saving
  for (id in names(seeds)) {
    
    text_start <- f_create_mcmc_block(paste0("TK_", Molecule, "_", id), seeds[[id]], Nb_Iter)

    text_full <- paste(
      text_start,
      text_Level_global,
      text_priors,
      "\n",
      text_likelihood,
      text_Level_exp,
      text_param_ind,
      text_experiment,
      text_end,
      sep = "\n"
    )[1]
    
    file.remove(here::here(File_path, paste0("TK_", Molecule, "_", id, ".in")))

    writeLines(
      text_full,
      file(here::here(File_path, paste0("TK_", Molecule, "_", id, ".in")))
    )
  }
}

f_In_tot_3Uptake <- function(Molecule, text_priors, text_likelihood, text_param_ind = "", Nb_Iter = 10000, seeds = c(C1 = 3333, C2 = 6666, C3 = 1212)) {
  
  text_Level_global <-
    "Level{ # Global
    "
  
  text_Level_exp <- "
Level{
  "
  
  banner_exp <- "
  ############## Individuals ###################
  "
  
  text_experiment <- paste0(banner_exp, f_In_experiments_3Uptake(Molecule), sep = "\n")
  
  text_end <- "
} # End
} # End global

End."
  
  # .in generation and saving
  for (id in names(seeds)) {
    
    text_start <- f_create_mcmc_block(paste0("TK_", Molecule, "_", id), seeds[[id]], Nb_Iter)
    
    text_full <- paste(
      text_start,
      text_Level_global,
      text_priors,
      "\n",
      text_likelihood,
      text_Level_exp,
      text_param_ind,
      text_experiment,
      text_end,
      sep = "\n"
    )[1]
    
    File_path <- paste0("mod/TK_", Molecule)
    
    file.remove(here::here(File_path, paste0("TK_", Molecule, "_", id, ".in")))
    
    writeLines(
      text_full,
      file(here::here(File_path, paste0("TK_", Molecule, "_", id, ".in")))
    )
  }
}

f_Setpoint <- function(File_path, Molecule, PrintStep, l_param_name_tot) {
  
  df_TK_f <- f_read_data_TK(Molecule)
  
  Winit <- mean(subset(df_TK_f, t == 0)$Weight_nog)

  if (Molecule == "IMD") {
    Ci0 <- 3.92587673494744
    Ce0 <- 80.5
    Cclx <- 16 / 1000 # ng/g

    text_start <- paste0('#### Toxicokinetics of Imidacloprid in A. caliginosa
#===============================================

Integrate(Lsodes, 1E-6, 1E-8, 1);

SetPoints("Setpoints.out", "tab_setpoint.out", 0,', l_param_name_tot,');
')
    text_simulation <- paste0("
	Simulation {

  # Events
    Winit=", Winit, ";
    Ci0IMD=", Ci0, ";
    Ce0IMD=", Ce0, ";
    CclxIMD=", Cclx, ";
    expo=NDoses(2, 1, 0, 0, 21);
    event_Ce=Events(CeIMD,1,20.9951388888889, Replace,0.09);")
  } else if (Molecule == "EPX") {
    Ci0 <- 0.747549
    Ce0 <- 1000
    Cclx <- 90 / 1000 # ng/g

    text_start <- paste0('#### Toxicokinetics of Epoxiconazole in A. caliginosa
#===============================================

Integrate(Lsodes, 1E-6, 1E-8, 1);

SetPoints("Setpoints.out", "tab_setpoint.out", 0,' ,l_param_name_tot,');
')
    text_simulation <- paste0("
	Simulation {

  # Events
    Winit=", Winit, ";
    Ci0EPX=", Ci0, ";
    Ce0EPX=", Ce0, ";
    CclxEPX=", Cclx, ";
    expo=NDoses(2, 1, 0, 0, 21);
    event_Ce=Events(CeEPX,1,20.9951388888889, Replace,0.09);")
  }

  if (Molecule == "EPX"){
    text_print <- paste0("# Data

		PrintStep (Weight,"
                         , PrintStep, ");
		PrintStep (CiEPX,"
                         , PrintStep, ");
		PrintStep (C_exposure,"
                         , PrintStep, ");
	}")
  }else if (Molecule == "IMD"){
    text_print <- paste0("# Data

		PrintStep (Weight,"
                         , PrintStep, ");
		PrintStep (CiIMD,"
                         , PrintStep, ");
		PrintStep (C_exposure,"
                         , PrintStep, ");
	}")
  }
  

  text_end <- "End."

  text_full <- paste(
    text_start,
    text_simulation,
    text_print,
    text_end,
    sep = "\n"
  )

  writeLines(
    text_full,
    here::here(File_path, paste0("TK_", Molecule, "_Setpoint.in"))
  )
}

f_Setpoint_ind <- function(File_path, Molecule, l_param_name) {
  
  text_end <- "End."

  df_TK_f <- f_read_data_TK(Molecule)
  
  C_worm_t0_IMD <-  df_TK_f[df_TK_f$t == 0, ]$C_worm_IMD[1]
  C_worm_t0_EPX <-  df_TK_f[df_TK_f$t == 0, ]$C_worm_EPX[1]
  C_clx_IMD <- 16 / 1000 # ng/g
  C_clx_EPX <- 90 / 1000 # ng/g
  
  compteur_exp <- 0
  
  for (i in as.numeric(unique(df_TK_f$ID))){
    
    compteur_exp <- compteur_exp + 1
    
    if (Molecule == "IMD"){
      df_TK_i <- subset(df_TK_f, ID == i)
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww <- paste(df_TK_i$Weight_nog, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiIMD <- paste(df_TK_i$C_worm_IMD, collapse = ",")
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      
      char_ta <- paste(df_TK_i$t[1:2], collapse = ",")
      
      if (i <= 64 | i > 128){
        char_Event <- ""
      } else {
        telim <- subset(df_TK_i, expo==0)$t[1]
        char_Event <- paste("    event_Ce=Events(CeIMD,1,", telim, ", Replace,", C_clx_IMD,");", sep="")
      }
      
      char_i <- paste(
        paste("Simulation { #", "TK IMD - i = ", i),
        
        # paste("    a_growth=NDoses(2, a_growth_ind_", compteur_exp," , a_fasting_ind_", compteur_exp," , ", char_ta,");", sep=""),
        
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight_nog, ";", sep = ""),
        paste("    Ci0IMD=", C_worm_t0_IMD, ";", sep = ""),
        paste("    Ce0IMD=", subset(df_TK_i, t == 0)$C_soil_IMD, ";", sep = ""),
        paste("    CclxIMD=", C_clx_IMD, ";", sep = ""),
        paste("    expo=NDoses(", length(df_TK_i$expo), ",", char_expo, ",", char_tw, ");", sep = ""),
        char_Event,
        paste("    Print(Weight,", char_tw, ");", sep = ""),
        paste("    Print(CiIMD,", char_tw, ");", sep = ""),
        paste("    Print(C_exposure,", char_tw, ");", sep = ""),
        paste("}"),
        sep = "\n"
      )
      
      text_start <- paste0('#### Toxicokinetics of Imidacloprid in A. caliginosa
#===============================================


Integrate(Lsodes, 1E-6, 1E-8, 1);

SetPoints("Setpoints_ind_',compteur_exp,'.out", "tab_setpoint_ind_',compteur_exp,'.out", 0,', l_param_name,');

########## Individuals ################################################')
      
      text_full <- paste(
        text_start,
        char_i,
        text_end,
        sep = "\n"
      )
      
      writeLines(
        text_full,
        here::here(File_path, paste0("TK_", Molecule, "_Setpoint_ind_", compteur_exp, ".in"))
      )
      
      
    } else if (Molecule == "EPX"){
      df_TK_i <- subset(df_TK_f, ID == i)
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww <- paste(df_TK_i$Weight_nog, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiEPX <- paste(df_TK_i$C_worm_EPX, collapse = ",")
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      
      if (i <= 64 | i > 128){
        char_Event <- ""
      } else {
        telim <- subset(df_TK_i, expo==0)$t[1]
        char_Event <- paste("    event_Ce=Events(CeEPX,1,", telim, ", Replace,", C_clx_EPX,");", sep="")
      }
      
      char_i <- paste(
        paste("Simulation { #", "TK EPX - i = ", i),
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight_nog, ";", sep = ""),
        paste("    Ci0EPX=", C_worm_t0_EPX, ";", sep = ""),
        paste("    Ce0EPX=", subset(df_TK_i, t == 0)$C_soil_EPX, ";", sep = ""),
        paste("    CclxEPX=", C_clx_EPX, ";", sep = ""),
        paste("    expo=NDoses(", length(df_TK_i$expo), ",", char_expo, ",", char_tw, ");", sep = ""),
        char_Event,
        paste("    Print(Weight,", char_tw, ");", sep = ""),
        paste("    Print(CiEPX,", char_tw, ");", sep = ""),
        paste("    Print(C_exposure,", char_tw, ");", sep = ""),
        paste("}"),
        sep = "\n"
      )
      
      text_start <- paste0('#### Toxicokinetics of Epoxiconazole in A. caliginosa
#===============================================


Integrate(Lsodes, 1E-6, 1E-8, 1);

SetPoints("Setpoints_ind_',compteur_exp,'.out", "tab_setpoint_ind_',compteur_exp,'.out", 0,' , l_param_name, ');

########## Individuals ################################################')
      
      text_full <- paste(
      text_start,
      char_i,
      text_end,
      sep = "\n"
    )
    
    writeLines(
      text_full,
      here::here(File_path, paste0("TK_", Molecule, "_Setpoint_ind_", compteur_exp, ".in"))
    )
    } else {
    stop("Incorrect specification of Molecule")
  }
 }
  
}

f_Setpoint_ind_full <- function(File_path, Molecule, print_times, l_param_name) {
  
  text_end <- "End."
  
  df_TK_f <- f_read_data_TK(Molecule)
  
  C_worm_t0_IMD <-  df_TK_f[df_TK_f$t == 0, ]$C_worm_IMD[1]
  C_worm_t0_EPX <-  df_TK_f[df_TK_f$t == 0, ]$C_worm_EPX[1]
  C_clx_IMD <- 16 / 1000 # ng/g
  C_clx_EPX <- 90 / 1000 # ng/g
  
  compteur_exp <- 0
  
  for (i in as.numeric(unique(df_TK_f$ID))){
    
    compteur_exp <- compteur_exp + 1
    
    if (Molecule == "IMD"){
      df_TK_i <- subset(df_TK_f, ID == i)
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww <- paste(df_TK_i$Weight_nog, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiIMD <- paste(df_TK_i$C_worm_IMD, collapse = ",")
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      
      char_ta <- paste(df_TK_i$t[1:2], collapse = ",")
      
      if (i <= 64 | i > 128){
        char_Event <- ""
      } else {
        telim <- subset(df_TK_i, expo==0)$t[1]
        char_Event <- paste("    event_Ce=Events(CeIMD,1,", telim, ", Replace,", C_clx_IMD,");", sep="")
      }
      
      char_i <- paste(
        paste("Simulation { #", "TK IMD - i = ", i),
        
        # paste("    a_growth=NDoses(2, a_growth_ind_", compteur_exp," , a_fasting_ind_", compteur_exp," , ", char_ta,");", sep=""),
        
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight_nog, ";", sep = ""),
        paste("    Ci0IMD=", C_worm_t0_IMD, ";", sep = ""),
        paste("    Ce0IMD=", subset(df_TK_i, t == 0)$C_soil_IMD, ";", sep = ""),
        paste("    CclxIMD=", C_clx_IMD, ";", sep = ""),
        paste("    expo=NDoses(", length(df_TK_i$expo), ",", char_expo, ",", char_tw, ");", sep = ""),
        char_Event,
        paste("    PrintStep(Weight,", print_times, ");", sep = ""),
        paste("    PrintStep(CiIMD,", print_times, ");", sep = ""),
        paste("    PrintStep(C_exposure,", print_times, ");", sep = ""),
        paste("}"),
        sep = "\n"
      )
      
      text_start <- paste0('#### Toxicokinetics of Imidacloprid in A. caliginosa
#===============================================


Integrate(Lsodes, 1E-6, 1E-8, 1);

SetPoints("Setpoints_ind_full_',compteur_exp,'.out", "tab_setpoint_ind_',compteur_exp,'.out", 0,' , l_param_name, ');

########## Individuals ################################################')
      
      text_full <- paste(
        text_start,
        char_i,
        text_end,
        sep = "\n"
      )
      
      writeLines(
        text_full,
        here::here(File_path, paste0("TK_", Molecule, "_Setpoint_ind_full_", compteur_exp, ".in"))
      )
      
      
    } else if (Molecule == "EPX"){
      df_TK_i <- subset(df_TK_f, ID == i)
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww <- paste(df_TK_i$Weight_nog, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiEPX <- paste(df_TK_i$C_worm_EPX, collapse = ",")
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      
      if (i <= 64 | i > 128){
        char_Event <- ""
      } else {
        telim <- subset(df_TK_i, expo==0)$t[1]
        char_Event <- paste("    event_Ce=Events(CeEPX,1,", telim, ", Replace,", C_clx_EPX,");", sep="")
      }
      
      char_i <- paste(
        paste("Simulation { #", "TK EPX - i = ", i),
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight_nog, ";", sep = ""),
        paste("    Ci0EPX=", C_worm_t0_EPX, ";", sep = ""),
        paste("    Ce0EPX=", subset(df_TK_i, t == 0)$C_soil_EPX, ";", sep = ""),
        paste("    CclxEPX=", C_clx_EPX, ";", sep = ""),
        paste("    expo=NDoses(", length(df_TK_i$expo), ",", char_expo, ",", char_tw, ");", sep = ""),
        char_Event,
        paste("    PrintStep(Weight,", print_times, ");", sep = ""),
        paste("    PrintStep(CiEPX,", print_times, ");", sep = ""),
        paste("    PrintStep(C_exposure,", print_times, ");", sep = ""),
        paste("}"),
        sep = "\n"
      )
      
      text_start <- paste0('#### Toxicokinetics of Epoxiconazole in A. caliginosa
#===============================================


Integrate(Lsodes, 1E-6, 1E-8, 1);

SetPoints("Setpoints_ind_full_',compteur_exp,'.out", "tab_setpoint_ind_',compteur_exp,'.out", 0,', l_param_name,');

########## Individuals ################################################')
      
      text_full <- paste(
        text_start,
        char_i,
        text_end,
        sep = "\n"
      )
      
      writeLines(
        text_full,
        here::here(File_path, paste0("TK_", Molecule, "_Setpoint_ind_full_", compteur_exp, ".in"))
      )
    } else {
      stop("Incorrect specification of Molecule")
    }
  }
}


f_read_data_TK_mix <- function(){
  
  rW     <- 0.851
  Ci0IMD <- 4.929
  Ci0EPX <- 0.768
  
  df_TK_mix <- read_excel(here::here("data/Data_TK_mix.xlsx")) |>
    mutate(
      ID = as.factor(ID),
      w = w / 1000, # Conversion mg to g
      Dose_EPX = Dose_EPX * 1000, # mg/kg to ng/g (mg/kg = microg/g = 1000 ng/g)
      Dose_IMD = Dose_IMD * 1000
    ) |> 
    mutate(
      Weight = case_when(
        t < 28.5 ~ rW * w,
        t > 28.5 ~ w
      ), # Weight without gut content
      expo = case_when(
        t < 28 ~ 1,
        t >= 28 ~ 2
      ) # Exposure state
    )
  
  df_TK_mix[df_TK_mix$t == 0, ]$CiIMD <- Ci0IMD
  df_TK_mix[df_TK_mix$t == 0, ]$CiEPX <- Ci0EPX
  
  return(df_TK_mix)
}

f_MonteCarlo_mix_ind <- function(File_path, Molecule, N_draws) {
  
  text_end <- "End."
  
  df_TK_f <- f_read_data_TK_mix() |> 
    mutate(across(where(is.numeric), ~ replace_na(.x, -1)))
  
  C_worm_t0_IMD <-  df_TK_f[df_TK_f$t == 0, ]$CiIMD[1]
  C_worm_t0_EPX <-  df_TK_f[df_TK_f$t == 0, ]$CiEPX[1]
  C_clx_IMD <- 16 / 1000 # ng/g
  C_clx_EPX <- 90 / 1000 # ng/g
  
  compteur_exp <- 0
  
  for (i in as.numeric(as.character(unique(df_TK_f$ID)))){
    
    compteur_exp <- compteur_exp + 1
    
    if (Molecule == "IMD"){
      df_TK_i <- subset(df_TK_f, ID == i)
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww <- paste(df_TK_i$Weight, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiIMD <- paste(df_TK_i$CiIMD, collapse = ",")
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      
      char_i <- paste(
        paste("Simulation { #", "TK Mix IMD - i = ", i),
        
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight, ";", sep = ""),
        paste("    Ci0IMD=", C_worm_t0_IMD, ";", sep = ""),
        paste("    Ce0IMD=", subset(df_TK_i, t == 0)$CeIMD, ";", sep = ""),
        paste("    CclxIMD=", C_clx_IMD, ";", sep = ""),
        paste("    expo=NDoses(", length(df_TK_i$expo), ",", char_expo, ",", char_tw, ");", sep = ""),
        paste("    Print(Weight,", char_tw, ");", sep = ""),
        paste("    Print(CiIMD,", char_tw, ");", sep = ""),
        paste("    Print(C_exposure,", char_tw, ");", sep = ""),
        paste("}"),
        sep = "\n"
      )
      
      text_start <- paste0('#### Toxicokinetics of Imidacloprid in A. caliginosa (Mixture edition)
#===============================================


MonteCarlo ("MonteCarlo_ind_', Molecule, '_',compteur_exp,'.out",',N_draws,', 1212);

# IMD 1 comp 0%
kuIMD = 1.59155;
keIMD = 0.0446806;
# a_growth = 0.00351619;
# Vr_a_growth = 0.00124984;
Sigma_W = 0.0456952;
Sigma_CiIMD = 1.45777;

Distrib (a_growth, Normal, 0.00351619, 0.00124984);


########## Individuals ################################################')
      
      text_full <- paste(
        text_start,
        char_i,
        text_end,
        sep = "\n"
      )
      
      writeLines(
        text_full,
        here::here(File_path, paste0("MonteCarlo_ind_", Molecule, "_", compteur_exp, ".in"))
      )
      
      
    } else if (Molecule == "EPX"){
      #print(i)
      
      df_TK_i <- subset(df_TK_f, ID == i)
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww <- paste(df_TK_i$Weight, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiEPX <- paste(df_TK_i$CiEPX, collapse = ",")
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      
      char_i <- paste(
        paste("Simulation { #", "TK EPX - i = ", i),
        
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight, ";", sep = ""),
        paste("    Ci0EPX=", C_worm_t0_EPX, ";", sep = ""),
        paste("    Ce0EPX=", subset(df_TK_i, t == 0)$CeEPX, ";", sep = ""),
        paste("    CclxEPX=", C_clx_EPX, ";", sep = ""),
        paste("    expo=NDoses(", length(df_TK_i$expo), ",", char_expo, ",", char_tw, ");", sep = ""),
        paste("    Print(Weight,", char_tw, ");", sep = ""),
        paste("    Print(CiEPX,", char_tw, ");", sep = ""),
        paste("    Print(C_exposure,", char_tw, ");", sep = ""),
        paste("}"),
        sep = "\n"
      )
      
      text_start <- paste0('#### Toxicokinetics of Epoxiconazole in A. caliginosa (Mixture edition)
#===============================================


MonteCarlo ("MonteCarlo_ind_', Molecule, '_',compteur_exp,'.out",',N_draws,', 1212);

# EPX 2 comp 15%
kuEPX = 2.793920;
keEPX = 1.70593;
kperiph = 0.000041689;
# a_growth_mean = 0.00355594;
# Vr_a_growth = 0.00148897;
Sigma_W = 0.0448117;
Sigma_CiEPX = 1.2612;

Distrib(a_growth, Normal, 0.00355594, 0.00148897);

########## Individuals ################################################')
      
      text_full <- paste(
        text_start,
        char_i,
        text_end,
        sep = "\n"
      )
      
      writeLines(
        text_full,
        here::here(File_path, paste0("MonteCarlo_ind_", Molecule, "_", compteur_exp, ".in"))
      )
    } else {
      stop("Incorrect specification of Molecule")
    }
  }
  
}

f_MonteCarlo_mix_ind_full <- function(File_path, Molecule, N_draws, print_times) {
  
  text_end <- "End."
  
  df_TK_f <- f_read_data_TK_mix() |> 
    mutate(across(where(is.numeric), ~ replace_na(.x, -1)))
  
  C_worm_t0_IMD <-  df_TK_f[df_TK_f$t == 0, ]$CiIMD[1]
  C_worm_t0_EPX <-  df_TK_f[df_TK_f$t == 0, ]$CiEPX[1]
  C_clx_IMD <- 16 / 1000 # ng/g
  C_clx_EPX <- 90 / 1000 # ng/g
  
  compteur_exp <- 0
  
  for (i in as.numeric(as.character(unique(df_TK_f$ID)))){
    
    compteur_exp <- compteur_exp + 1
    
    if (Molecule == "IMD"){
      df_TK_i <- subset(df_TK_f, ID == i)
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww <- paste(df_TK_i$Weight, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiIMD <- paste(df_TK_i$CiIMD, collapse = ",")
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      
      char_i <- paste(
        paste("Simulation { #", "TK Mix IMD - i = ", i),
        
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight, ";", sep = ""),
        paste("    Ci0IMD=", C_worm_t0_IMD, ";", sep = ""),
        paste("    Ce0IMD=", subset(df_TK_i, t == 0)$CeIMD, ";", sep = ""),
        paste("    CclxIMD=", C_clx_IMD, ";", sep = ""),
        paste("    expo=NDoses(", length(df_TK_i$expo), ",", char_expo, ",", char_tw, ");", sep = ""),
        paste("    PrintStep(Weight,", print_times, ");", sep = ""),
        paste("    PrintStep(CiIMD,", print_times, ");", sep = ""),
        paste("    PrintStep(C_exposure,", print_times, ");", sep = ""),
        paste("}"),
        sep = "\n"
      )
      
      text_start <- paste0('#### Toxicokinetics of Imidacloprid in A. caliginosa (Mixture edition)
#===============================================


MonteCarlo ("MonteCarlo_ind_full_', Molecule, '_',compteur_exp,'.out",',N_draws,', 1212);

# IMD 1 comp 0%
kuIMD = 1.59155;
keIMD = 0.0446806;
# a_growth = 0.00351619;
# Vr_a_growth = 0.00124984;
Sigma_W = 0.0456952;
Sigma_CiIMD = 1.45777;

Distrib (a_growth, Normal, 0.00351619, 0.00124984);


########## Individuals ################################################')
      
      text_full <- paste(
        text_start,
        char_i,
        text_end,
        sep = "\n"
      )
      
      writeLines(
        text_full,
        here::here(File_path, paste0("MonteCarlo_ind_full_", Molecule, "_", compteur_exp, ".in"))
      )
      
      
    } else if (Molecule == "EPX"){
      #print(i)
      
      df_TK_i <- subset(df_TK_f, ID == i)
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww <- paste(df_TK_i$Weight, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiEPX <- paste(df_TK_i$CiEPX, collapse = ",")
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      
      char_i <- paste(
        paste("Simulation { #", "TK EPX - i = ", i),
        
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight, ";", sep = ""),
        paste("    Ci0EPX=", C_worm_t0_EPX, ";", sep = ""),
        paste("    Ce0EPX=", subset(df_TK_i, t == 0)$CeEPX, ";", sep = ""),
        paste("    CclxEPX=", C_clx_EPX, ";", sep = ""),
        paste("    expo=NDoses(", length(df_TK_i$expo), ",", char_expo, ",", char_tw, ");", sep = ""),
        paste("    PrintStep(Weight,", print_times, ");", sep = ""),
        paste("    PrintStep(CiEPX,", print_times, ");", sep = ""),
        paste("    PrintStep(C_exposure,", print_times, ");", sep = ""),
        paste("}"),
        sep = "\n"
      )
      
      text_start <- paste0('#### Toxicokinetics of Epoxiconazole in A. caliginosa (Mixture edition)
#===============================================


MonteCarlo ("MonteCarlo_ind_full_', Molecule, '_',compteur_exp,'.out",',N_draws,', 1212);

# EPX 2 comp 15%
kuEPX = 2.793920;
keEPX = 1.70593;
kperiph = 0.000041689;
# a_growth_mean = 0.00355594;
# Vr_a_growth = 0.00148897;
Sigma_W = 0.0448117;
Sigma_CiEPX = 1.2612;

Distrib(a_growth, Normal, 0.00355594, 0.00148897);

########## Individuals ################################################')
      
      text_full <- paste(
        text_start,
        char_i,
        text_end,
        sep = "\n"
      )
      
      writeLines(
        text_full,
        here::here(File_path, paste0("MonteCarlo_ind_full_", Molecule, "_", compteur_exp, ".in"))
      )
    } else {
      stop("Incorrect specification of Molecule")
    }
  }
  
}

f_GrowthRate <- function(data){
  
  df_GrowthRate <- data.frame() 
  
  for (i in unique(data$ID)){
    
    data_i <- subset(data, ID == i) 
    
    if (length(na.omit(data_i$L))<2) {
      a.Est <- NA
      
    }else if(length(na.omit(data_i$L))==2){ 
      
      a.Est <- (subset(data_i, !t==0)$L - subset(data_i, t==0)$L)/subset(data_i, !t==0)$t # (L(t=t)-L(t=0))/t
      
    }else{
      lm_i <- lm(L~t, data=data_i)
      a.Est <- lm_i$coefficients[2]
    }
    
    df_GrowthRate_i <- data.frame(
      ID = i,
      a_growth = a.Est
    )
    
    df_GrowthRate <- rbind(df_GrowthRate, df_GrowthRate_i)
  }
  
  return(df_GrowthRate) 
}

f_Simulation_mix_ind <- function(File_path, Molecule) {
  
  text_end <- "End."
  
  df_TK_f <- f_read_data_TK_mix() |> 
    mutate(across(where(is.numeric), ~ replace_na(.x, -1))) |> 
    mutate(
      L = Weight^(1/3)
    )
  
  df_growth_rate_mix <- f_GrowthRate(subset(df_TK_f, t < 28.5))
  
  C_worm_t0_IMD <-  mean(subset(df_TK_f, Ratio == "E")$CiIMD, na.rm=T)
  C_worm_t0_EPX <-  mean(subset(df_TK_mix, Ratio == "I")$CiEPX, na.rm=T)
  C_clx_IMD <- 16 / 1000 # ng/g
  C_clx_EPX <- 90 / 1000 # ng/g
  
  compteur_exp <- 0
  
  for (i in as.numeric(as.character(unique(df_TK_f$ID)))){
    
    compteur_exp <- compteur_exp + 1
    
    if (Molecule == "IMD"){
      df_TK_i <- subset(df_TK_f, ID == i)
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww <- paste(df_TK_i$Weight, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiIMD <- paste(df_TK_i$CiIMD, collapse = ",")
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      
      a_growth_i <- subset(df_growth_rate_mix, ID==i)$a_growth
      Dose_i <- subset(df_TK_i, t == 0)$CeIMD
      
      char_i <- paste(
        paste("Simulation { #", "TK Mix IMD - i = ", i),
        paste("    a_growth=", a_growth_i, ";", sep = ""),
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight, ";", sep = ""),
        paste("    Ci0IMD=", C_worm_t0_IMD, ";", sep = ""),
        paste("    Ce0IMD=", Dose_i, ";", sep = ""),
        paste("    CclxIMD=", C_clx_IMD, ";", sep = ""),
        paste("    expo=NDoses(", length(df_TK_i$expo), ",", char_expo, ",", char_tw, ");", sep = ""),
        paste("    Print(Weight,", char_tw, ");", sep = ""),
        paste("    Print(CiIMD,", char_tw, ");", sep = ""),
        paste("    Print(C_exposure,", char_tw, ");", sep = ""),
        paste("}"),
        sep = "\n"
      )
      
      text_start <- paste0('#### Toxicokinetics of Imidacloprid in A. caliginosa (Mixture edition)
#===============================================

# IMD 1 comp 0%
kuIMD = 1.58669;
keIMD = 0.048173;
# a_growth = 0.00351619;
# Vr_a_growth = 0.00124984;
Sigma_W = 0.0393328;
Sigma_CiIMD = 1.45184;


########## Individuals ################################################')
      
      text_full <- paste(
        text_start,
        char_i,
        text_end,
        sep = "\n"
      )
      
      writeLines(
        text_full,
        here::here(File_path, paste0("Simulation_ind_", Molecule, "_", compteur_exp, ".in"))
      )
      
      
    } else if (Molecule == "EPX"){
      #print(i)
      
      df_TK_i <- subset(df_TK_f, ID == i)
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww <- paste(df_TK_i$Weight, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiEPX <- paste(df_TK_i$CiEPX, collapse = ",")
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      
      a_growth_i <- subset(df_growth_rate_mix, ID==i)$a_growth
      Dose_i <- subset(df_TK_i, t == 0)$CeEPX
      
      char_i <- paste(
        paste("Simulation { #", "TK EPX - i = ", i),
        
        paste("    a_growth=", a_growth_i, ";", sep = ""),
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight, ";", sep = ""),
        paste("    Ci0EPX=", C_worm_t0_EPX, ";", sep = ""),
        paste("    Ce0EPX=", Dose_i, ";", sep = ""),
        paste("    CclxEPX=", C_clx_EPX, ";", sep = ""),
        paste("    expo=NDoses(", length(df_TK_i$expo), ",", char_expo, ",", char_tw, ");", sep = ""),
        paste("    Print(Weight,", char_tw, ");", sep = ""),
        paste("    Print(CiEPX,", char_tw, ");", sep = ""),
        paste("    Print(C_exposure,", char_tw, ");", sep = ""),
        paste("}"),
        sep = "\n"
      )
      
      text_start <- paste0('#### Toxicokinetics of Epoxiconazole in A. caliginosa (Mixture edition)
#===============================================

# EPX 2 comp Phi%
kuEPX = 1.64823;
keEPX = 1.68255;
kperiph = 0.00000003061920;
phi = 0.912028;
# a_growth_mean = 0.00355594;
# Vr_a_growth = 0.00148897;
Sigma_W = 0.0476993;
Sigma_CiEPX = 1.28122;

########## Individuals ################################################')
      
      text_full <- paste(
        text_start,
        char_i,
        text_end,
        sep = "\n"
      )
      
      writeLines(
        text_full,
        here::here(File_path, paste0("Simulation_ind_", Molecule, "_", compteur_exp, ".in"))
      )
    } else {
      stop("Incorrect specification of Molecule")
    }
  }
  
}

f_Simulation_mix_ind_full <- function(File_path, Molecule, print_times) {
  
  text_end <- "End."
  
  df_TK_f <- f_read_data_TK_mix() |> 
    mutate(across(where(is.numeric), ~ replace_na(.x, -1))) |> 
    mutate(
      L = Weight^(1/3)
    )
  
  df_growth_rate_mix <- f_GrowthRate(subset(df_TK_f, t < 28.5))
  
  C_worm_t0_IMD <-  mean(subset(df_TK_f, Ratio == "N")$CiIMD, na.rm=T)
  C_worm_t0_EPX <-  mean(subset(df_TK_mix, Ratio == "N")$CiEPX, na.rm=T)
  C_clx_IMD <- 16 / 1000 # ng/g
  C_clx_EPX <- 90 / 1000 # ng/g
  
  compteur_exp <- 0
  
  for (i in as.numeric(as.character(unique(df_TK_f$ID)))){
    
    compteur_exp <- compteur_exp + 1
    
    if (Molecule == "IMD"){
      df_TK_i <- subset(df_TK_f, ID == i)
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww <- paste(df_TK_i$Weight, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiIMD <- paste(df_TK_i$CiIMD, collapse = ",")
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      
      a_growth_i <- subset(df_growth_rate_mix, ID==i)$a_growth
      
      char_i <- paste(
        paste("Simulation { #", "TK Mix IMD - i = ", i),
        paste("    a_growth=", a_growth_i, ";", sep = ""),
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight, ";", sep = ""),
        paste("    Ci0IMD=", C_worm_t0_IMD, ";", sep = ""),
        paste("    Ce0IMD=", subset(df_TK_i, t == 0)$CeIMD, ";", sep = ""),
        paste("    CclxIMD=", C_clx_IMD, ";", sep = ""),
        paste("    expo=NDoses(", length(df_TK_i$expo), ",", char_expo, ",", char_tw, ");", sep = ""),
        paste("    PrintStep(Weight,", print_times, ");", sep = ""),
        paste("    PrintStep(CiIMD,", print_times, ");", sep = ""),
        paste("    PrintStep(C_exposure,", print_times, ");", sep = ""),
        paste("}"),
        sep = "\n"
      )
      
      text_start <- paste0('#### Toxicokinetics of Imidacloprid in A. caliginosa (Mixture edition)
#===============================================

# IMD 1 comp 0%
kuIMD = 1.58669;
keIMD = 0.048173;
# a_growth = 0.00351619;
# Vr_a_growth = 0.00124984;
Sigma_W = 0.0393328;
Sigma_CiIMD = 1.45184;


########## Individuals ################################################')
      
      text_full <- paste(
        text_start,
        char_i,
        text_end,
        sep = "\n"
      )
      
      writeLines(
        text_full,
        here::here(File_path, paste0("Simulation_ind_full_", Molecule, "_", compteur_exp, ".in"))
      )
      
      
    } else if (Molecule == "EPX"){
      #print(i)
      
      df_TK_i <- subset(df_TK_f, ID == i)
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww <- paste(df_TK_i$Weight, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiEPX <- paste(df_TK_i$CiEPX, collapse = ",")
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      
      a_growth_i <- subset(df_growth_rate_mix, ID==i)$a_growth
      
      Dose <- subset(df_TK_i, t == 0)$CeEPX
      
      char_i <- paste(
        paste("Simulation { #", "TK EPX - i = ", i),
        paste("    a_growth=", a_growth_i, ";", sep = ""),
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight, ";", sep = ""),
        paste("    Ci0EPX=", C_worm_t0_EPX, ";", sep = ""),
        paste("    Ce0EPX=", Dose, ";", sep = ""),
        paste("    CclxEPX=", C_clx_EPX, ";", sep = ""),
        paste("    expo=NDoses(", length(df_TK_i$expo), ",", char_expo, ",", char_tw, ");", sep = ""),
        paste("    PrintStep(Weight,", print_times, ");", sep = ""),
        paste("    PrintStep(CiEPX,", print_times, ");", sep = ""),
        paste("    PrintStep(C_exposure,", print_times, ");", sep = ""),
        paste("}"),
        sep = "\n"
      )
      
      text_start <- paste0('#### Toxicokinetics of Epoxiconazole in A. caliginosa (Mixture edition)
#===============================================

# EPX 2 comp Phi%
kuEPX = 1.64823;
keEPX = 1.68255;
kperiph = 0.00000003061920;
phi = 0.912028;
# a_growth_mean = 0.00355594;
# Vr_a_growth = 0.00148897;
Sigma_W = 0.0476993;
Sigma_CiEPX = 1.28122;

########## Individuals ################################################')
      
      text_full <- paste(
        text_start,
        char_i,
        text_end,
        sep = "\n"
      )
      
      writeLines(
        text_full,
        here::here(File_path, paste0("Simulation_ind_full_", Molecule, "_", compteur_exp, ".in"))
      )
    } else {
      stop("Incorrect specification of Molecule")
    }
  }
  
}

## MCSim to R ----

f_get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

f_MCSim_read_sim <- function(path_sim_out){
  library(readr)
  library(dplyr)
  
  # Read file
  lines <- read_lines(path_sim_out)
  
  # Find the begining of each simulation
  indices_sim <- which(str_detect(lines, "^Results of Simulation"))
  n_simulations <- length(indices_sim)
  
  # Initialisation of the list for the dataframes of each simulation
  list_simulations <- list()
  
  idx_begin <- indices_sim + 1
  idx_end <- c((indices_sim[-1] - 1), length(lines))
  
  # For each simulation
  for (i in seq_along(indices_sim)) {
    
    bloc <- lines[idx_begin[i]:idx_end[i]]
    
    # To data.frame
    df <- read_table2(paste(bloc, collapse = "\n"))
    df$ID_Simulation <- i  # Ad the ID of the simulation
    list_simulations[[i]] <- df
  }
  
  # Combine data.frames
  df_total <- bind_rows(list_simulations) |> 
    mutate(ID_Simulation = as.factor(ID_Simulation))
  
  return(df_total)
  
}

f_MCSim <- function(path_mod) {
  
  # 1. Read files ----
  Files_in  <- list.files(path = path_mod, pattern = "[0-3]\\.in")
  Files_out <- list.files(path = path_mod, pattern = "[0-3]\\.out")

  tmp <- grep(
    pattern = "out.kernel",
    Files_out
  )
  if (length(tmp) > 0) {
    Files_out <- Files_out[-tmp]
  }

  NC <- length(Files_out)

  # 2. Priors ----

  Initialize <- readLines(paste(path_mod, Files_in[1], sep = "/"))

  Exl.Par1 <- grep(pattern = "*\\#Distrib*", Initialize)
  Exl.Par2 <- grep(pattern = "*\\# Distrib*", Initialize)
  
  Parameter <- grep(pattern = "*Distrib*", Initialize)
  Parameter <- Parameter[!Parameter %in% Exl.Par1]
  Parameter <- Parameter[!Parameter %in% Exl.Par2]

  np <- length(Parameter)

  Experiment <- grep(pattern = "Experiment", Initialize, value = T)
  nexperiment <- length(Experiment)

  ParTable <- as.data.frame(matrix(NA, np, 6))
  colnames(ParTable) <- c("Nom", "Distribution", "P1", "P2", "P3", "P4")
  for (i in 1:np) {
    temp <- gsub("\t", "", Initialize[Parameter[i]])
    temp <- strsplit(temp, "\\#")[[1]][1]
    temp <- strsplit(temp, ".\\(|\\)|\\,| ")[[1]]
    temp <- temp[temp != ""]

    for (j in 2:(length(temp) - 1)) {
      ParTable[i, j - 1] <- temp[j]
    }
  }

  ParTable[, 3:ncol(ParTable)] <- apply(ParTable[, 3:ncol(ParTable)], 2, as.numeric)

  # 3. Results ----

  Data_T <- Data_V <- Data_L <- vector("list", length = NC) # sous forme liste

  for (i in 1:NC) {
    data <- read.table(eval(paste(path_mod, Files_out[i], sep = "/")), header = TRUE)

    if (i == 1) {
      times <- data[, 1]
    }
    tmp <- as.matrix(data[, -1])

    Data_L[[i]] <- tmp # Fichier complet pour les NC chaines
    Data_T[[i]] <- tmp[, 1:np] # Fichier parametres pour les NC chaines
    Data_V[[i]] <- tmp[, (np + 1):ncol(tmp)] # Fichier Vraisemblance pour les NC chaines
    names(Data_L)[i] <- paste0("Chain", i)
    names(Data_T)[i] <- paste0("Chain", i)
    names(Data_V)[i] <- paste0("Chain", i)
  }

  Niter <- nrow(Data_T[[1]]) # number of lines
  PI <- as.numeric(times[nrow(Data_T[[1]])] - times[(nrow(Data_T[[1]]) - 1)]) # pas de l'iteration

  nb <- 10000
  if (nb >= Niter) {
    nb <- Niter / 2
  } # control nb < Niter

  IterSelect <- ((Niter - nb / PI):Niter)

  Data_All <- NULL
  for (i in 1:NC) {
    Data_T[[i]] <- mcmc(Data_T[[i]], start = 1, end = Niter, thin = PI)
    Data_All <- rbind(Data_All, Data_L[[i]][IterSelect, ])
  }
  res.mcmc <- coda::as.mcmc.list(Data_T)
  df_res.mcmc <- ggs(res.mcmc)
  resmc <- summary(res.mcmc)

  Mode <- which.max(Data_All[, ncol(Data_All)]) # MPV computation
  Result_mode <- Data_All[Mode[1], ]

  Min <- apply(Data_All, 2, min)
  Max <- apply(Data_All, 2, max)
  Result_sd <- apply(Data_All, 2, sd)
  Result_mean <- apply(Data_All, 2, mean)
  Result_quantil <- apply(Data_All, 2, function(x) {
    quantile(x, c(0.025, 0.975))
  })

  Res <- rbind(Result_mode, Result_quantil, Min, Max, Result_mean, Result_sd)

  # 4. AIC and BIC calculations ----

  MaxVraiss <- Data_All[Mode[1], ncol(Data_All)]

  AIC <- -2 * MaxVraiss + 2 * np
  Ndata <- 13 # number of observed data used
  BIC <- -2 * MaxVraiss + log(Ndata) * np

  # 5. tab_setpoint.out construction ----

  Niter_chain <- 333 # Niter_chain last lines selected to setpoint.in / doit pas être trop grand sinon impossible de faire les graphs (marche au moins avec 1000)

  #Niter_chain <- 3
  
  Selected_chain <- NULL

  for (i in 1:NC) {
    Selected_chain <- rbind(Selected_chain, Data_L[[i]][(nrow(Data_L[[i]]) - Niter_chain + 1):nrow(Data_L[[i]]), ]) # Ajout du +1
  }

  tmp <- -c((ncol(Selected_chain) - 2):ncol(Selected_chain))
  Selected_chain <- Selected_chain[, tmp]
  MVP <- Result_mode[tmp] # "Meilleure" solution dans toutes les chaines

  Selected_chain <- rbind(Selected_chain, as.list(MVP)) # Ajout du as.list()

  # 6. Likelihood and deviance ratio ----

  tmp.names <- Devc <- LnData <- LnPost_start <- LnPost <- NULL

  for (i in 1:NC) {
    temp <- -2 * Data_V[[i]][IterSelect, 3] # -2log(vraisemblance), tableau
    Devc <- cbind(Devc, temp)
    LnPost_start <- cbind(LnPost_start, Data_V[[i]][(100):(2 * Niter / 4), 3])
    LnPost <- cbind(LnPost, Data_V[[i]][IterSelect, 3])
    tmp.names <- c(tmp.names, paste("Chain", i))
  }

  df_LnPost_start <- data.frame(iteration = (100):(2 * Niter / 4), LnPost_start) %>%
    pivot_longer(-iteration, names_to = "Chain", values_to = "LnPosterior")

  df_LnPost <- data.frame(iteration = IterSelect[1:nrow(Devc)], LnPost) %>%
    pivot_longer(-iteration, names_to = "Chain", values_to = "LnPosterior")

  df_Devc <- data.frame(iteration = IterSelect[1:nrow(Devc)], RatioDeviance = Devc / min(Devc)) %>%
    pivot_longer(-iteration, names_to = "Chain", values_to = "RatioDeviance")

  # 7. Outputs ----

  write.table(Selected_chain, file = paste(path_mod, "tab_setpoint.out", sep = "/"), quote = FALSE)

  MCMC_out <- list(
    NC              = NC,
    Files_in        = Files_in,
    Files_out       = Files_out,
    Nb_param        = np,
    Nb_experiment   = nexperiment,
    Priors          = ParTable,
    Nb_iter_kept    = nb, # Last iterations used to describe the posterior distributions
    Chains          = df_res.mcmc,
    df_LnPost_start = df_LnPost_start,
    df_LnPost       = df_LnPost,
    df_Devc         = df_Devc,
    Summary_res     = Res,
    AIC             = AIC,
    BIC             = BIC,
    N_iter_setpoint = Niter_chain # last lines selected to setpoint.in (cannot be too big)
  )

  return(MCMC_out)
}

f_MCSim_ind <- function(path_mod) {
  
  # 1. Read files ----

  Files_in <- list.files(path = path_mod, pattern = "C[0-3]\\.in")
  Files_out <- list.files(path = path_mod, pattern = "C[0-3]\\.out")

  tmp <- grep(
    pattern = "out.kernel",
    Files_out
  )
  if (length(tmp) > 0) {
    Files_out <- Files_out[-tmp]
  }

  NC <- length(Files_out)

  # 2. Priors ----

  Initialize <- readLines(paste(path_mod, Files_in[1], sep = "/"))

  Exl.Par1 <- grep(pattern = "*\\#Distrib*", Initialize)
  Exl.Par2 <- grep(pattern = "*\\# Distrib*", Initialize)
  Parameter <- grep(pattern = "*Distrib*", Initialize)
  Parameter <- Parameter[!Parameter %in% Exl.Par1]
  Parameter <- Parameter[!Parameter %in% Exl.Par2]

  np_tmp <- length(Parameter) # Used just to be able to count the number of parameters with individual variation

  Experiment <- grep(pattern = "Experiment", Initialize, value = T)
  nexperiment <- length(Experiment)
  ParTable <- as.data.frame(matrix(NA, np_tmp, 6))

  colnames(ParTable) <- c("Nom", "Distribution", "P1", "P2", "P3", "P4")
  for (i in 1:np_tmp) {
    temp <- gsub("\t", "", Initialize[Parameter[i]])
    temp <- strsplit(temp, "\\#")[[1]][1]
    temp <- strsplit(temp, ".\\(|\\)|\\,| ")[[1]]
    temp <- temp[temp != ""]

    for (j in 2:(length(temp) - 1)) {
      ParTable[i, j - 1] <- temp[j]
    }
  }

  Nb_param_IndVar <- length(gsub("^Vr_", "", ParTable$Nom[grepl("^Vr_", ParTable$Nom)]))
  np <- np_tmp - Nb_param_IndVar

  ParTable <- head(ParTable, -Nb_param_IndVar)

  ParTable[, 3:ncol(ParTable)] <- apply(ParTable[, 3:ncol(ParTable)], 2, as.numeric)

  # 3. Results ----

  Data_T <- Data_V <- Data_L <- vector("list", length = NC) # sous forme liste

  for (i in 1:NC) {
    data <- read.table(eval(paste(path_mod, Files_out[i], sep = "/")), header = TRUE)

    if (i == 1) {
      times <- data[, 1]
    }
    tmp <- as.matrix(data[, -1])

    Data_L[[i]] <- tmp # Fichier complet pour les NC chaines
    Data_T[[i]] <- tmp[, 1:np] # Fichier parametres pour les NC chaines
    Data_V[[i]] <- tmp[, (np + 1):ncol(tmp)] # Fichier Vraisemblance pour les NC chaines
    names(Data_L)[i] <- paste0("Chain", i)
    names(Data_T)[i] <- paste0("Chain", i)
    names(Data_V)[i] <- paste0("Chain", i)
  }

  # Chains

  Niter <- nrow(Data_T[[1]]) # number of lines
  PI <- as.numeric(times[nrow(Data_T[[1]])] - times[(nrow(Data_T[[1]]) - 1)]) # pas de l'iteration

  nb <- 10000
  if (nb >= Niter) {
    nb <- Niter / 2
  } # control nb < Niter

  IterSelect <- ((Niter - nb / PI):Niter)

  Data_All <- NULL
  for (i in 1:NC) {
    Data_T[[i]] <- mcmc(Data_T[[i]], start = 1, end = Niter, thin = PI)
    Data_All <- rbind(Data_All, Data_L[[i]][IterSelect, ])
  }
  res.mcmc <- coda::as.mcmc.list(Data_T)
  df_res.mcmc <- ggs(res.mcmc)
  resmc <- summary(res.mcmc)

  Mode <- which.max(Data_All[, ncol(Data_All)]) # MPV computation
  Result_mode <- Data_All[Mode[1], ]

  Min <- apply(Data_All, 2, min)
  Max <- apply(Data_All, 2, max)
  Result_sd <- apply(Data_All, 2, sd)

  Result_mean <- apply(Data_All, 2, mean)
  Result_quantil <- apply(Data_All, 2, function(x) {
    quantile(x, c(0.025, 0.975))
  })

  Res <- rbind(Result_mode, Result_quantil, Min, Max, Result_mean, Result_sd)

  # 4. AIC and BIC calculations ----

  # MaxVraiss <- Data_All[Mode[1], ncol(Data_All)]
  # 
  # AIC <- -2 * MaxVraiss + 2 * np
  # Ndata <- 13 # number of observed data used
  # BIC <- -2 * MaxVraiss + log(Ndata) * np

  # 5. tab_setpoint.out construction ----

  ## 5.1. tab_setpoint pop ----

 Niter_chain <- 333 # Niter_chain last lines selected to setpoint.in / doit pas être trop grand sinon impossible de faire les graphs (marche au moins avec 1000)
 #Niter_chain <- 1
  
  Selected_chain <- NULL

  for (i in 1:NC) {
    Selected_chain <- rbind(Selected_chain, Data_L[[i]][(nrow(Data_L[[i]]) - Niter_chain + 1):nrow(Data_L[[i]]), ]) # Ajout du +1
  }

  tmp <- -c((ncol(Selected_chain) - 2):ncol(Selected_chain))
  Selected_chain <- Selected_chain[, tmp]
  MVP <- Result_mode[tmp] # "Meilleure" solution dans toutes les chaines

  Selected_chain <- rbind(Selected_chain, as.list(MVP)) # Ajout du as.list()
  df_Selected_chain <- as.data.frame(Selected_chain)

  tab_setpoint <- df_Selected_chain[, 1:np]


  ## 5.2. Setpoint individuel ----
  
  col_ind <- df_Selected_chain[, (np + 1):length(df_Selected_chain)]

  colnames(tab_setpoint) <- gsub("\\.1\\.$", "", colnames(tab_setpoint))
  Names_par <- gsub("\\.1\\.$", "", names(tab_setpoint))
  names_param_ind <- grep("^Vr_", Names_par, value = TRUE)
  names_param_sigma <- grep("^Sigma_", Names_par, value = TRUE)
  names_param_pop_ind <- sub("^Vr_", "", names_param_ind)

  col_pop <- tab_setpoint |>
    dplyr::select(-all_of(c(names_param_sigma, names_param_ind, names_param_pop_ind)))

  # Version LG

  for (i in 1:nexperiment) {
    # print(i)
    start <- Nb_param_IndVar * (i - 1) + 1
    end <- Nb_param_IndVar * i
    
    col_ind_i <- as.data.frame(col_ind[, start:end, drop = FALSE])
    names(col_ind_i) <- names_param_pop_ind
    tab_setpoint_i_tmp <- cbind(col_pop, col_ind_i)
    
    cols_growth <- grep("^a_growth", colnames(tab_setpoint_i_tmp), value = TRUE)
    cols_other <- setdiff(colnames(tab_setpoint_i_tmp), cols_growth)
    
    tab_setpoint_ind_i <- tab_setpoint_i_tmp[, c(cols_other, cols_growth)]|>
      mutate(across(where(is.list), ~ simplify2array(.) |> unlist()))
    
    l_Iter <- seq(0,Niter_chain*3+1-1,1)
    
    tab_setpoint_ind_i <- cbind(l_Iter, tab_setpoint_ind_i)
    
    file_name_i <- paste0("tab_setpoint_ind_", i, ".out")
    write.table(tab_setpoint_ind_i,
                file = file.path(path_mod, file_name_i),
                quote = FALSE, row.names = FALSE)
  }

  # 6. Likelihood and deviance ratio ----

  tmp.names <- Devc <- LnData <- LnPost_start <- LnPost <- NULL

  for (i in 1:NC) {
    temp <- -2 * Data_V[[i]][IterSelect, ncol(Data_V[[i]])] # -2log(vraisemblance), tableau
    Devc <- cbind(Devc, temp)
    LnPost_start <- cbind(LnPost_start, Data_V[[i]][seq(100, 2 * Niter / 4), ncol(Data_V[[i]]), drop = FALSE])
    LnPost <- cbind(LnPost, Data_V[[i]][IterSelect, ncol(Data_V[[i]])])
    LnData <- cbind(LnData, Data_V[[i]][IterSelect, (ncol(Data_V[[i]])-1)])
    tmp.names <- c(tmp.names, paste("Chain", i))
  }

  df_LnPost_start <- data.frame(iteration = (100):(2 * Niter / 4), LnPost_start) %>%
    pivot_longer(-iteration, names_to = "Chain", values_to = "LnPosterior")

  df_LnPost <- data.frame(iteration = IterSelect[1:nrow(Devc)], LnPost) %>%
    pivot_longer(-iteration, names_to = "Chain", values_to = "LnPosterior")

  df_Devc <- data.frame(iteration = IterSelect[1:nrow(Devc)], RatioDeviance = Devc / min(Devc)) %>%
    pivot_longer(-iteration, names_to = "Chain", values_to = "RatioDeviance")
  
  df_LnData <- data.frame(iteration = IterSelect[1:nrow(Devc)], LnData) %>%
    pivot_longer(-iteration, names_to = "Chain", values_to = "LnData")

  # 7. Outputs ----
  tab_setpoint <- tab_setpoint |>
    mutate(across(where(is.list), ~ simplify2array(.) |> unlist()))

  write.table(tab_setpoint, file = paste(path_mod, "tab_setpoint.out", sep = "/"), quote = FALSE)
  

  MCMC_out <- list(
    NC              = NC,
    Files_in        = Files_in,
    Files_out       = Files_out,
    Nb_param        = np,
    Nb_experiment   = nexperiment,
    Priors          = ParTable,
    Nb_iter_kept    = nb,         # Last iterations used to describe the posterior distributions
    Chains          = df_res.mcmc,
    df_LnPost_start = df_LnPost_start,
    df_LnPost       = df_LnPost,
    df_LnData       = df_LnData,
    df_Devc         = df_Devc,
    Summary_res     = Res,
    AIC             = AIC,
    BIC             = BIC,
    N_iter_setpoint = Niter_chain # last lines selected to setpoint.in (cannot be too big)
  )

  return(MCMC_out)
}

# Mixture - Jonker interaction models ----

# Fonction calculating the surface dose - response knowing the dose - response curves

CA_complete2 <- function(C_mat, Max, Slopes, Ec50s, a = 0, b = 0, interact = "none", multicore = FALSE, mc.cores = 4) {
  param <- data.frame(Slopes, Ec50s)
  # try(cat(Slopes))
  # try(cat(Ec50s))

  # 1. CA model ----
  if (interact == "none") {
    a <- 0
    b <- 0
    f <- function(Y, x = x, Max = Max, param = param, a = a, b = b) {
      ecs <- param$Ec50s * ((Max - Y) / Y)^(1 / param$Slopes) # C1 corresponding to Y
      G <- (sum(x / ecs)) - 1
      return(abs(G))
    }
  }
  # 2. SA model ----
  else if (interact == "SA") {
    b <- 0
    f <- function(Y, x = x, Max = Max, param = param, a = a, b = b) {
      ecs <- param$Ec50s * ((Max - Y) / Y)^(1 / param$Slopes) # C1 corresponding to Y
      G <- (sum(x / ecs)) - exp(a * prod(x / param$Ec50s / (sum(x / param$Ec50s))))
      return(abs(G))
    }
  }
  # 3. DR model ----
  else if (interact == "DR") {
    if (length(b) != (dim(C_mat)[2] - 1)) {
      stop("length of b should be equal to the number of chemicals -1.")
    }
    f <- function(Y, x = x, Max = Max, param = param, a = a, b = b) {
      ecs <- param$Ec50s * ((Max - Y) / Y)^(1 / param$Slopes) # Cs corresponding to Y
      G <- (sum(x / ecs)) - exp((a + b %*% ((x / param$Ec50s / (sum(x / param$Ec50s))))[1:(dim(C_mat)[2] - 1)]) * prod(x / param$Ec50s / (sum(x / param$Ec50s))))
      return(abs(G))
    }
  }
  # 4. DR2 model ----
  else if (interact == "DR2") {
    if (length(b) != (dim(C_mat)[2] - 1)) {
      stop("length of b should be equal to the number of chemicals -1.")
    }
    f <- function(Y, x = x, Max = Max, param = param, a = a, b = b) {
      ecs <- param$Ec50s * ((Max - Y) / Y)^(1 / param$Slopes) # Cs corresponding to Y
      G <- (sum(x / ecs)) - exp((a + b %*% sin((x / param$Ec50s / (sum(x / param$Ec50s))) * 2 * pi)[1:(dim(C_mat)[2] - 1)]) * prod(x / param$Ec50s / (sum(x / param$Ec50s))))
      return(abs(G))
    }
  }
  # 5. DL model ----
  else if (interact == "DL") {
    f <- function(Y, x = x, Max = Max, param = param, a = a, b = b) {
      if (length(b) != (dim(C_mat)[2] - 1)) {
        stop("length of b should be equal to the number of chemicals -1.")
      }
      ecs <- param$Ec50s * ((Max - Y) / Y)^(1 / param$Slopes) # C1 corresponding to Y
      G <- (sum(x / ecs)) - exp(a * (1 - b * (sum(x / param$Ec50s))) * prod(x / param$Ec50s / (sum(x / param$Ec50s))))
      return(abs(G))
    }
  } else {
    stop("please specify interaction model")
  }

  Y_f <- function(x, Max = Max, param = param, a = a, b = b) {
    if (all(x == 0)) { # no chemical
      if (all(param$Slopes > 0)) {
        return(Max)
      }
      if (all(param$Slopes < 0)) {
        return(0)
      }
    }
    # Y<-optimize(f=function(Y) f(Y=Y, x=x, Max=Max, param=param, a=a, b=b), interval=c(0,Max*1.2))$minimum
    Y <- optimize(f = function(Y) f(Y = Y, x = x, Max = Max, param = param, a = a, b = b), interval = c(0, Max))$minimum

    return(Y)
  }
  if (multicore) {
    C_mat_list <- split(
      unique(C_mat), 
      cut(1:(dim(unique(C_mat))[1]), 
          breaks = dim(unique(C_mat))[1])
      )
    res_CA_unique <- unlist(
      mclapply(
        C_mat_list, 
        function(x) Y_f(x, Max = Max, param = param, a = a, b = b), 
        mc.cores = mc.cores
        )
      )
    res_CA <- rep(NA, dim(unique(C_mat))[1])
    for (i in 1:(dim(unique(C_mat))[1])) {
      res_CA[which((C_mat[, 1] == unique(C_mat)[i, 1]) & (C_mat[, 2] == unique(C_mat)[i, 2]))] <- res_CA_unique[i]
    }
  } else {
    res_CA_unique <- apply(
      unique(C_mat), 
      1, 
      function(x) Y_f(x, Max = Max, param = param, a = a, b = b)
      )
    res_CA <- rep(NA, dim(unique(C_mat))[1])
    for (i in 1:(dim(unique(C_mat))[1])) {
      res_CA[which((C_mat[, 1] == unique(C_mat)[i, 1]) & (C_mat[, 2] == unique(C_mat)[i, 2]))] <- res_CA_unique[i]
    }
  }
  if (any(is.na(res_CA))) {
    cat(param, a, b)
  }
  return(res_CA)
}

CA_complete2_fit_speed <- function(C_mat, Response, param = NULL, upper = NULL, lower = NULL, start = NULL, interact = "none", identical_slopes = FALSE, error_type = "Normal", iter = 500, multicore = FALSE, mc.cores = 4) {
  # 1. If dose-response curves known ----
  if (!is.null(param)) {
    if ((is.null(param$Max)) | (is.null(param$Slopes)) | (is.null(param$Ec50s))) {
      stop("param misspecification")
    }
    if (interact == "none") {
      stop("please specify interaction")
    }

    ## 1.1. SA model ----
    if (interact == "SA") {
      if (missing(upper)) {
        upper <- 20
      } # the intervals for a and b must be larger because they can compensate each other
      if (missing(lower)) {
        lower <- -20
      }

      # fit
      if (error_type == "Normal") {
        res_CA_optim <- optimize(
          f = function(x) {
            CA_complete2_RSS(
              C_mat     = C_mat,
              Response  = Response,
              Max       = param$Max,
              Slopes    = param$Slopes,
              Ec50s     = param$Ec50s,
              a         = x,
              interact  = interact,
              multicore = multicore,
              mc.cores = mc.cores
            )
          },
          upper = upper,
          lower = lower
        )

        Res <- list(
          a = res_CA_optim$minimum,
          Error = res_CA_optim$objective
        )
        return(Res)
      } else if (error_type == "Poisson") {
        res_CA_optim <- optimize(
          f = function(x) {
            CA_complete2_Poisson(
              C_mat     = C_mat,
              Response  = Response,
              Max       = param$Max,
              Slopes    = param$Slopes,
              Ec50s     = param$Ec50s,
              a         = x,
              interact  = interact,
              multicore = multicore,
              mc.cores  = mc.cores
            )
          },
          upper = upper,
          lower = lower
        )

        Res <- list(
          a     = res_CA_optim$minimum,
          Error = res_CA_optim$objective
        )

        return(Res)
      } else {
        stop("Misspecification of the error model")
      }
    } # End SA model
  } # End dose-response curves known

  # 2. If dose-response curves known ----
  if (is.null(param)) {
    require(dfoptim)

    if (any(C_mat == 0)) {
      mean_C_mat <- exp(apply(log(C_mat[-which(C_mat == 0, arr.ind = TRUE)[, 1], ]), 2, mean))
    } else {
      mean_C_mat <- exp(apply(log(C_mat), 2, mean))
    }

    ## 2.1. Different slopes ----
    if (!identical_slopes) {
      ### 2.1.1. CA model ----
      if (interact == "none") {
        if (missing(upper)) {
          upper <- c(max(Response) * 5, 0, 0, mean_C_mat * 10)
        }

        if (missing(lower)) {
          lower <- c(0, -100, -100, 0, 0)
        }

        if (missing(start)) {
          start <- c(max(Response), -1, -1, mean_C_mat)
        }

        # fit
        if (error_type == "Normal") {
          res_CA_nmk <- nmkb(
            par = start,
            fn = function(x) {
              CA_complete2_RSS(
                C_mat     = C_mat,
                Response  = Response,
                Max       = x[1],
                Slopes    = c(x[2], x[3]),
                Ec50s     = c(x[4], x[5]),
                interact  = interact,
                multicore = multicore,
                mc.cores  = mc.cores
              )
            },
            upper = upper,
            lower = lower,
            control = list(tol = 10^-6)
          )

          Res <- list(
            Max    = res_CA_nmk$par[1],
            Slopes = c(res_CA_nmk$par[2:3]),
            Ec50s  = c(res_CA_nmk$par[4:5]),
            Error  = res_CA_nmk$value
          )
          return(Res)
        } else if (error_type == "Poisson") {
          res_CA_nmk <- nmkb(
            par = start,
            fn = function(x) {
              CA_complete2_Poisson(
                C_mat     = C_mat,
                Response  = Response,
                Max       = x[1],
                Slopes    = c(x[2], x[3]),
                Ec50s     = c(x[4], x[5]),
                interact  = interact,
                multicore = multicore,
                mc.cores  = mc.cores
              )
            },
            upper = upper,
            lower = lower,
            control = list(tol = 10^-6)
          )

          Res <- list(
            Max    = res_CA_nmk$par[1],
            Slopes = c(res_CA_nmk$par[2:3]),
            Ec50s  = c(res_CA_nmk$par[4:5]),
            Error  = res_CA_nmk$value
          )
          return(Res)
        } else {
          stop("Misspecification of the error model")
        }
      } # End CA model

      ### 2.1.2. SA model ----
      if (interact == "SA") {
        if (missing(upper)) {
          upper <- c(max(Response) * 5, 0, 0, mean_C_mat * 10, 20)
        } # the intervals for a and b must be larger beacuse they can compensate each other

        if (missing(lower)) {
          lower <- c(0, -100, -100, 0, 0, -20)
        }

        if (missing(start)) {
          start <- c(max(Response), -1, -1, mean_C_mat, 0)
        }

        # fit
        if (error_type == "Normal") {
          res_CA_nmk <- nmkb(
            par = start,
            fn = function(x) {
              CA_complete2_RSS(
                C_mat     = C_mat,
                Response  = Response,
                Max       = x[1],
                Slopes    = c(x[2], x[3]),
                Ec50s     = c(x[4], x[5]),
                a         = x[6],
                interact  = interact,
                multicore = multicore,
                mc.cores  = mc.cores
              )
            },
            upper = upper,
            lower = lower,
            control = list(tol = 10^-6)
          )

          Res <- list(
            Max    = res_CA_nmk$par[1],
            Slopes = c(res_CA_nmk$par[2:3]),
            Ec50s  = c(res_CA_nmk$par[4:5]),
            a      = res_CA_nmk$par[6],
            Error  = res_CA_nmk$value
          )

          return(Res)
        } else if (error_type == "Poisson") {
          res_CA_nmk <- nmkb(
            par = start,
            fn = function(x) {
              CA_complete2_Poisson(
                C_mat     = C_mat,
                Response  = Response,
                Max       = x[1],
                Slopes    = c(x[2], x[3]),
                Ec50s     = c(x[4], x[5]),
                a         = x[6],
                interact  = interact,
                multicore = multicore,
                mc.cores  = mc.cores
              )
            },
            upper = upper,
            lower = lower,
            control = list(tol = 10^-6)
          )

          Res <- list(
            Max    = res_CA_nmk$par[1],
            Slopes = c(res_CA_nmk$par[2:3]),
            Ec50s  = c(res_CA_nmk$par[4:5]),
            a      = res_CA_nmk$par[6],
            Error  = res_CA_nmk$value
          )
          return(Res)
        } else {
          stop("Misspecification of the error model")
        }
      } # End SA model
    } # End common slopes

    ## 2.2. Common slopes ----
    else {
      ### 2.2.1. CA model ----
      if (interact == "none") {
        if (missing(upper)) {
          upper <- c(max(Response) * 5, 100, mean_C_mat * 10)
        }

        if (missing(lower)) {
          lower <- c(0, 0, 0, 0)
        }

        if (missing(start)) {
          start <- c(max(Response), 1, mean_C_mat)
        }

        # fit
        if (error_type == "Normal") {
          res_CA_nmk <- nmkb(
            par = start,
            fn = function(x) {
              CA_complete2_RSS(
                C_mat     = C_mat,
                Response  = Response,
                Max       = x[1],
                Slopes    = c(x[2], x[2]),
                Ec50s     = c(x[3], x[4]),
                interact  = interact,
                multicore = multicore,
                mc.cores  = mc.cores
              )
            },
            upper   = upper,
            lower   = lower,
            control = list(tol = 10^-6)
          )

          Res <- list(
            Max    = res_CA_nmk$par[1],
            Slopes = c(res_CA_nmk$par[2], res_CA_nmk$par[2]),
            Ec50s  = c(res_CA_nmk$par[3:4]),
            Error  = res_CA_nmk$value
          )
          return(Res)
        } else if (error_type == "Poisson") {
          res_CA_nmk <- nmkb(
            par = start,
            fn = function(x) {
              CA_complete2_Poisson(
                C_mat     = C_mat,
                Response  = Response,
                Max       = x[1],
                Slopes    = c(x[2], x[2]),
                Ec50s     = c(x[3], x[4]),
                interact  = interact,
                multicore = multicore,
                mc.cores  = mc.cores
              )
            },
            upper   = upper,
            lower   = lower,
            control = list(tol = 10^-6)
          )

          Res <- list(
            Max    = res_CA_nmk$par[1],
            Slopes = c(res_CA_nmk$par[2], res_CA_nmk$par[2]),
            Ec50s  = c(res_CA_nmk$par[3:4]),
            Error  = res_CA_nmk$value
          )
          return(Res)
        } else {
          stop("Misspecification of the error model")
        }
      } # End CA model

      ### 2.2.2. SA model ----
      if (interact == "SA") {
        if (missing(upper)) {
          upper <- c(max(Response) * 5, 100, mean_C_mat * 10, 20)
        } # the intervals for a and b must be larger beacuse they can compensate each other

        if (missing(lower)) {
          lower <- c(0, 0, 0, 0, -20)
        }

        if (missing(start)) {
          start <- c(max(Response), 1, mean_C_mat, 0)
        }

        # fit

        if (error_type == "Normal") {
          res_CA_nmk <- nmkb(
            par = start,
            fn = function(x) {
              CA_complete2_RSS(
                C_mat     = C_mat,
                Response  = Response,
                Max       = x[1],
                Slopes    = c(x[2], x[2]),
                Ec50s     = c(x[3], x[4]),
                a         = x[5],
                interact  = interact,
                multicore = multicore,
                mc.cores  = mc.cores
              )
            },
            upper   = upper,
            lower   = lower,
            control = list(tol = 10^-6)
          )

          Res <- list(
            Max    = res_CA_nmk$par[1],
            Slopes = c(res_CA_nmk$par[2], res_CA_nmk$par[2]),
            Ec50s  = c(res_CA_nmk$par[3:4]),
            a      = res_CA_nmk$par[5],
            Error  = res_CA_nmk$value
          )
          return(Res)
        } else if (error_type == "Poisson") {
          res_CA_nmk <- nmkb(
            par = start,
            fn = function(x) {
              CA_complete2_Poisson(
                C_mat     = C_mat,
                Response  = Response,
                Max       = x[1],
                Slopes    = c(x[2], x[2]),
                Ec50s     = c(x[3], x[4]),
                a         = x[5],
                interact  = interact,
                multicore = multicore,
                mc.cores  = mc.cores
              )
            },
            upper   = upper,
            lower   = lower,
            control = list(tol = 10^-6)
          )

          Res <- list(
            Max    = res_CA_nmk$par[1],
            Slopes = c(res_CA_nmk$par[2], res_CA_nmk$par[2]),
            Ec50s  = c(res_CA_nmk$par[3:4]),
            a      = res_CA_nmk$par[5],
            Error  = res_CA_nmk$value
          )
          return(Res)
        } else {
          stop("Misspecification of the error model")
        }
      } # End SA model
    } # End different slopes
  } # End dose-response curves not known
} # End.

CA_complete2_RSS <- function(C_mat, Response, Max, Slopes, Ec50s, a = 0, b = 0, interact = "none", multicore = FALSE, mc.cores = 4) {
  
  if (length(Response) != (dim(C_mat)[1])) {
    stop("Should be as many responses as there are conditions")
  }

  res_CA <- CA_complete2(
    C_mat     = C_mat,
    Max       = Max,
    Slopes    = Slopes,
    Ec50s     = Ec50s,
    a         = a,
    b         = b,
    interact  = interact,
    multicore = multicore,
    mc.cores  = mc.cores
  )
  res_CA_RSS <- sum((res_CA - Response)^2)

  return(res_CA_RSS)
}


CA_complete2_Poisson <- function(C_mat, Response, Max, Slopes, Ec50s, a = 0, b = 0, interact = "none", multicore = FALSE, mc.cores = 4) {
  
  if (length(Response) != (dim(C_mat)[1])) {
    stop("Should be as many responses as there are conditions")
  }

  res_CA <- CA_complete2(
    C_mat     = C_mat,
    Max       = Max,
    Slopes    = Slopes,
    Ec50s     = Ec50s,
    a         = a,
    b         = b,
    interact  = interact,
    multicore = multicore,
    mc.cores  = mc.cores
  )

  # Pour stabilité numérique, éviter log(0)
  if (any(res_CA <= 0)) {
    return(-Inf)
  }
  res_CA_Poisson <- -sum(Response * log(res_CA) - res_CA - lfactorial(Response)) # - Loglikelihood

  return(res_CA_Poisson)
}

CA_complete_fit_speed <- function(C_mat, Response, param = NULL, upper = NULL, lower = NULL, start = NULL, interact = "none", identical_slopes = FALSE, error_type = "Normal", iter = 500, multicore = FALSE, mc.cores = 4) {
  # 1. Dose-response curves known ----
  if (!is.null(param)) {
    if ((is.null(param$Max)) | (is.null(param$Slopes)) | (is.null(param$Ec50s))) {
      stop("param misspecification")
    }
    if (interact == "none") {
      stop("please specify interaction")
    }

    ## 1.1. SA model ----
    if (interact == "SA") {
      if (missing(upper)) {
        upper <- 20
      } # the intervals for a and b must be larger because they can compensate each other
      if (missing(lower)) {
        lower <- -20
      }

      if (error_type == "Normal") {
        res_CA_optim <- optimize(
          f = function(x) {
            CA_complete2_RSS(
              C_mat     = C_mat,
              Response  = Response,
              Max       = param$Max,
              Slopes    = param$Slopes,
              Ec50s     = param$Ec50s,
              a         = x,
              interact  = interact,
              multicore = multicore,
              mc.cores  = mc.cores
            )
          },
          upper = upper,
          lower = lower
        )
        Res <- list(
          a     = res_CA_optim$minimum,
          Error = res_CA_optim$objective
        )
        return(Res)
      } else if (error_type == "Poisson") {
        res_CA_optim <- optimize(
          f = function(x) {
            CA_complete2_Poisson(
              C_mat     = C_mat,
              Response  = Response,
              Max       = param$Max,
              Slopes    = param$Slopes,
              Ec50s     = param$Ec50s,
              a         = x,
              interact  = interact,
              multicore = multicore,
              mc.cores  = mc.cores
            )
          },
          upper = upper,
          lower = lower
        )
        Res <- list(
          a     = res_CA_optim$minimum,
          Error = res_CA_optim$objective
        )
        return(Res)
      } else {
        stop("Misspecification of the error model")
      }
    } # End SA

    ## 1.2. DR model ----
    if (interact == "DR") {
      if (error_type == "Normal") {
        res_CA_optim <- optim(
          par = rep(0, dim(C_mat)[2]),
          fn = function(x) {
            CA_complete2_RSS(
              C_mat     = C_mat,
              Response  = Response,
              Max       = param$Max,
              Slopes    = param$Slopes,
              Ec50s     = param$Ec50s,
              a         = x[1],
              b         = x[2:length(x)],
              interact  = interact,
              multicore = multicore,
              mc.cores  = mc.cores
            )
          }
        )
        cat(res_CA_optim$convergence)
        Res <- list(
          a     = res_CA_optim$par[1],
          b     = res_CA_optim$par[2:(2 + (dim(C_mat)[2] - 1) - 1)],
          Error = res_CA_optim$value
        )
        return(Res)
      } else if (error_type == "Poisson") {
        res_CA_optim <- optim(
          par = rep(0, dim(C_mat)[2]),
          fn = function(x) {
            CA_complete2_Poisson(
              C_mat     = C_mat,
              Response  = Response,
              Max       = param$Max,
              Slopes    = param$Slopes,
              Ec50s     = param$Ec50s,
              a         = x[1],
              b         = x[2:length(x)],
              interact  = interact,
              multicore = multicore,
              mc.cores  = mc.cores
            )
          }
        )
        cat(res_CA_optim$convergence)

        Res <- list(
          a     = res_CA_optim$par[1],
          b     = res_CA_optim$par[2:(2 + (dim(C_mat)[2] - 1) - 1)],
          Error = res_CA_optim$value
        )
        return(Res)
      } else {
        stop("Misspecification of the error model")
      }
    } # End DR

    ## 1.3. DR2 model ----
    if (interact == "DR2") {
      if (error_type == "Normal") {
        res_CA_optim <- optim(
          par = rep(0, dim(C_mat)[2]),
          fn = function(x) {
            CA_complete2_RSS(
              C_mat     = C_mat,
              Response  = Response,
              Max       = param$Max,
              Slopes    = param$Slopes,
              Ec50s     = param$Ec50s,
              a         = x[1],
              b         = x[2:length(x)],
              interact  = interact,
              multicore = multicore,
              mc.cores  = mc.cores
            )
          }
        )
        cat(res_CA_optim$convergence)

        Res <- list(
          a     = res_CA_optim$par[1],
          b     = res_CA_optim$par[2:(2 + (dim(C_mat)[2] - 1) - 1)],
          Error = res_CA_optim$value
        )
        return(Res)
      } else if (error_type == "Poisson") {
        res_CA_optim <- optim(
          par = rep(0, dim(C_mat)[2]),
          fn = function(x) {
            CA_complete2_Poisson(
              C_mat     = C_mat,
              Response  = Response,
              Max       = param$Max,
              Slopes    = param$Slopes,
              Ec50s     = param$Ec50s,
              a         = x[1],
              b         = x[2:length(x)],
              interact  = interact,
              multicore = multicore,
              mc.cores  = mc.cores
            )
          }
        )
        cat(res_CA_optim$convergence)
        Res <- list(
          a     = res_CA_optim$par[1],
          b     = res_CA_optim$par[2:(2 + (dim(C_mat)[2] - 1) - 1)],
          Error = res_CA_optim$value
        )
        return(Res)
      } else {
        stop("Misspecification of the error model")
      }
    } # End DR2

    ## 1.4. DL model ----
    if (interact == "DL") {
      if (error_type == "Normal") {
        res_CA_optim <- optim(
          par = c(0, 0),
          fn = function(x) {
            CA_complete2_RSS(
              C_mat = C_mat,
              Response = Response,
              Max = param$Max,
              Slopes = param$Slopes,
              Ec50s = param$Ec50s,
              a = x[1], b = x[2],
              interact = interact,
              multicore = multicore,
              mc.cores = mc.cores
            )
          }
        )
        cat(res_CA_optim$convergence)
        Res <- list(
          a     = res_CA_optim$par[1],
          b     = res_CA_optim$par[2],
          Error = res_CA_optim$value
        )
        return(Res)
      } else if (error_type == "Poisson") {
        res_CA_optim <- optim(
          par = c(0, 0),
          fn = function(x) {
            CA_complete2_Poisson(
              C_mat     = C_mat,
              Response  = Response,
              Max       = param$Max,
              Slopes    = param$Slopes,
              Ec50s     = param$Ec50s,
              a         = x[1],
              b         = x[2],
              interact  = interact,
              multicore = multicore,
              mc.cores  = mc.cores
            )
          }
        )
        cat(res_CA_optim$convergence)
        Res <- list(
          a     = res_CA_optim$par[1],
          b     = res_CA_optim$par[2],
          Error = res_CA_optim$value
        )
        return(Res)
      } else {
        stop("Misspecification of the error model")
      }
    } # End DL
  } # End dose-response curves known

  # 2. Dose-response curves not known ----
  if (is.null(param)) {
    # require(DEoptim)
    require(dfoptim)
    if (any(C_mat == 0)) {
      mean_C_mat <- exp(apply(log(C_mat[-which(C_mat == 0, arr.ind = TRUE)[, 1], ]), 2, mean))
    } else {
      mean_C_mat <- exp(apply(log(C_mat), 2, mean))
    }

    ## 2.1. Different slopes ----
    if (!identical_slopes) {
      ### 2.1.1. CA model ----
      if (interact == "none") {
        if (missing(upper)) {
          upper <- c(max(Response) * 5, 0, 0, mean_C_mat * 10)
        }
        if (missing(lower)) {
          lower <- c(0, -100, -100, 0, 0)
        }
        if (missing(start)) {
          start <- c(max(Response), -1, -1, mean_C_mat)
        }

        if (error_type == "Normal") {
          res_CA_nmk <- nmkb(
            par = start,
            fn = function(x) {
              CA_complete2_RSS(
                C_mat     = C_mat,
                Response  = Response,
                Max       = x[1],
                Slopes    = c(x[2], x[3]),
                Ec50s     = c(x[4], x[5]),
                interact  = interact,
                multicore = multicore,
                mc.cores  = mc.cores
              )
            },
            upper = upper,
            lower = lower,
            control = list(tol = 10^-6)
          )
          Res <- list(
            Max    = res_CA_nmk$par[1],
            Slopes = c(res_CA_nmk$par[2:3]),
            Ec50s  = c(res_CA_nmk$par[4:5]),
            Error  = res_CA_nmk$value
          )
          return(Res)
        } else if (error_type == "Poisson") {
          res_CA_nmk <- nmkb(
            par = start,
            fn = function(x) {
              CA_complete2_Poisson(
                C_mat     = C_mat,
                Response  = Response,
                Max       = x[1],
                Slopes    = c(x[2], x[3]),
                Ec50s     = c(x[4], x[5]),
                interact  = interact,
                multicore = multicore,
                mc.cores  = mc.cores
              )
            },
            upper = upper,
            lower = lower,
            control = list(tol = 10^-6)
          )
          Res <- list(
            Max    = res_CA_nmk$par[1],
            Slopes = c(res_CA_nmk$par[2:3]),
            Ec50s  = c(res_CA_nmk$par[4:5]),
            Error  = res_CA_nmk$value
          )
          return(Res)
        } else {
          stop("Misspecification of the error model")
        }
      }

      ### 2.1.2. SA model ----
      if (interact == "SA") {
        if (missing(upper)) {
          upper <- c(max(Response) * 5, 0, 0, mean_C_mat * 10, 20) # the intervals for a and b must be larger beacuse they can compensate each other
        }
        if (missing(lower)) {
          lower <- c(0, -100, -100, 0, 0, -20)
        }
        if (missing(start)) {
          start <- c(max(Response), -1, -1, mean_C_mat, 0)
        }

        if (error_type == "Normal") {
          res_CA_nmk <- nmkb(
            par = start,
            fn = function(x) {
              CA_complete2_RSS(
                C_mat     = C_mat,
                Response  = Response,
                Max       = x[1],
                Slopes    = c(x[2], x[3]),
                Ec50s     = c(x[4], x[5]),
                a         = x[6],
                interact  = interact,
                multicore = multicore,
                mc.cores  = mc.cores
              )
            },
            upper = upper,
            lower = lower,
            control = list(tol = 10^-6)
          )
          Res <- list(
            Max    = res_CA_nmk$par[1],
            Slopes = c(res_CA_nmk$par[2:3]),
            Ec50s  = c(res_CA_nmk$par[4:5]),
            a      = res_CA_nmk$par[6],
            Error  = res_CA_nmk$value
          )
          return(Res)
          
        } else if (error_type == "Poisson") {
          res_CA_nmk <- nmkb(
            par = start,
            fn = function(x) {
              CA_complete2_Poisson(
                C_mat     = C_mat, 
                Response  = Response,
                Max       = x[1], 
                Slopes    = c(x[2], x[3]), 
                Ec50s     = c(x[4], x[5]), 
                a         = x[6], 
                interact  = interact, 
                multicore = multicore, 
                mc.cores  = mc.cores
              )
            },
            upper   = upper,
            lower   = lower,
            control = list(tol = 10^-6)
          )
          Res <- list(
            Max    = res_CA_nmk$par[1],
            Slopes = c(res_CA_nmk$par[2:3]), 
            Ec50s  = c(res_CA_nmk$par[4:5]), 
            a      = res_CA_nmk$par[6], 
            Error  = res_CA_nmk$value
          )
          return(Res)
        } else {
          stop("Misspecification of the error model")
        }
      } # End SA 
      
      ### 2.1.3. DR model ----
      if (interact == "DR") {
        
        if (missing(upper)) {
          upper <- c(max(Response) * 5, 0, 0, mean_C_mat * 10, 20, rep(30, (dim(C_mat)[2] - 1)))
        } # the intervals for a and b must be larger beacuse they can compensate each other
        if (missing(lower)) {
          lower <- c(0, -100, -100, 0, 0, -20, rep(-30, (dim(C_mat)[2] - 1)))
        }
        if (missing(start)) {
          start <- c(max(Response), -1, -1, mean_C_mat, 0, rep(0, (dim(C_mat)[2] - 1)))
        }

        if (error_type == "Normal") {
          res_CA_nmk <- nmkb(
            par = start, 
            fn = function(x) {
              CA_complete2_RSS(
                C_mat     = C_mat, 
                Response  = Response, 
                Max       = x[1], 
                Slopes    = c(x[2], x[3]), 
                Ec50s     = c(x[4], x[5]), 
                a         = x[6], 
                b         = x[7:length(x)], 
                interact  = interact, 
                multicore = multicore, 
                mc.cores  = mc.cores
                )
              }, 
            upper   = upper, 
            lower   = lower, 
            control = list(tol = 10^-6)
            )
          Res <- list(
            Max    = res_CA_nmk$par[1], 
            Slopes = c(res_CA_nmk$par[2:3]),
            Ec50s  = c(res_CA_nmk$par[4:5]), 
            a      = res_CA_nmk$par[6], 
            b      = res_CA_nmk$par[7:(7 + (dim(C_mat)[2] - 1) - 1)], 
            Error  = res_CA_nmk$value
            )
          return(Res)
        } else if (error_type == "Poisson") {
          res_CA_nmk <- nmkb(
            par = start, 
            fn = function(x) {
              CA_complete2_Poisson(
                C_mat     = C_mat, 
                Response  = Response, 
                Max       = x[1], 
                Slopes    = c(x[2], x[3]),
                Ec50s     = c(x[4], x[5]), 
                a         = x[6],
                b         = x[7:length(x)], 
                interact  = interact, 
                multicore = multicore, 
                mc.cores  = mc.cores
                )
              },
            upper   = upper, 
            lower   = lower, 
            control = list(tol = 10^-6)
            )
          Res <- list(
            Max    = res_CA_nmk$par[1], 
            Slopes = c(res_CA_nmk$par[2:3]), 
            Ec50s  = c(res_CA_nmk$par[4:5]), 
            a      = res_CA_nmk$par[6], 
            b      = res_CA_nmk$par[7:(7 + (dim(C_mat)[2] - 1) - 1)], 
            Error  = res_CA_nmk$value
            )
          return(Res)
        } else {
          stop("Misspecification of the error model")
        }
      } # End DR 
      
      ### 2.1.4. DL model ----
      if (interact == "DL") {
        if (missing(upper)) {
          upper <- c(max(Response) * 5, 0, 0, mean_C_mat * 10, 20, 30)
        }
        if (missing(lower)) {
          lower <- c(0, -100, -100, 0, 0, -20, -30)
        }
        if (missing(start)) {
          start <- c(max(Response), -1, -1, mean_C_mat, 0, 0)
        }

        if (error_type == "Normal") {
          res_CA_nmk <- nmkb(
            par = start, 
            fn = function(x) {
              CA_complete2_RSS(
                C_mat     = C_mat, 
                Response  = Response,
                Max       = x[1], 
                Slopes    = c(x[2], x[3]), 
                Ec50s     = c(x[4], x[5]), 
                a         = x[6], b = x[7], 
                interact  = interact,
                multicore = multicore,
                mc.cores  = mc.cores
                )
              }, 
            upper   = upper, 
            lower   = lower, 
            control = list(tol = 10^-6)
            )
          Res <- list(
            Max    = res_CA_nmk$par[1], 
            Slopes = c(res_CA_nmk$par[2:3]), 
            Ec50s  = c(res_CA_nmk$par[4:5]), 
            a      = res_CA_nmk$par[6], 
            b      = res_CA_nmk$par[7], 
            Error  = res_CA_nmk$value
            )
          return(Res)
          
        } else if (error_type == "Poisson") {
          
          res_CA_nmk <- nmkb(
            par = start, 
            fn = function(x) {
              CA_complete2_Poisson(
                C_mat     = C_mat, 
                Response  = Response, 
                Max       = x[1], 
                Slopes    = c(x[2], x[3]), 
                Ec50s     = c(x[4], x[5]), 
                a         = x[6], b = x[7],
                interact  = interact, 
                multicore = multicore, 
                mc.cores  = mc.cores
                )
              }, 
              upper   = upper, 
              lower   = lower, 
              control = list(tol = 10^-6)
            )
          Res <- list(
            Max    = res_CA_nmk$par[1], 
            Slopes = c(res_CA_nmk$par[2:3]), 
            Ec50s  = c(res_CA_nmk$par[4:5]), 
            a      = res_CA_nmk$par[6], 
            b      = res_CA_nmk$par[7], 
            Error  = res_CA_nmk$value
            )
          return(Res)
        } else {
          stop("Misspecification of the error model")
        }
      } # End DL
    } 
    ## 2.2. Common slopes ----
    else {
      
      ### 2.2.1. CA model ----
      if (interact == "none") {
        
        if (missing(upper)) {
          upper <- c(max(Response) * 5, 0, mean_C_mat * 10)
        }
        if (missing(lower)) {
          lower <- c(0, -100, 0, 0)
        }
        if (missing(start)) {
          start <- c(max(Response), -1, mean_C_mat)
        }

        if (error_type == "Normal") {
          res_CA_nmk <- nmkb(
            par = start, 
            fn = function(x) {
              CA_complete2_RSS(
                C_mat     = C_mat, 
                Response  = Response, 
                Max       = x[1], 
                Slopes    = c(x[2], x[2]), 
                Ec50s     = c(x[3], x[4]), 
                interact  = interact, 
                multicore = multicore, 
                mc.cores  = mc.cores
                )
              }, 
            upper = upper, 
            lower = lower,
            control = list(tol = 10^-6)
            )
          Res <- list(
            Max    = res_CA_nmk$par[1], 
            Slopes = c(res_CA_nmk$par[2], res_CA_nmk$par[2]), 
            Ec50s  = c(res_CA_nmk$par[3:4]), 
            Error  = res_CA_nmk$value
            )
          return(Res)
        } else if (error_type == "Poisson") {
          res_CA_nmk <- nmkb(
            par = start, 
            fn = function(x) {
              CA_complete2_Poisson(
                C_mat     = C_mat, 
                Response  = Response, 
                Max       = x[1], 
                Slopes    = c(x[2], x[2]), 
                Ec50s     = c(x[3], x[4]), 
                interact  = interact, 
                multicore = multicore, 
                mc.cores  = mc.cores
                )
              }, 
            upper = upper, 
            lower = lower, 
            control = list(tol = 10^-6)
            )
          Res <- list(
            Max    = res_CA_nmk$par[1], 
            Slopes = c(res_CA_nmk$par[2], res_CA_nmk$par[2]), 
            Ec50s  = c(res_CA_nmk$par[3:4]), 
            Error  = res_CA_nmk$value
            )
          return(Res)
        } else {
          stop("Misspecification of the error model")
        }
      } # End CA
      
      ### 2.2.2. SA model ----
      if (interact == "SA") {
        
        if (missing(upper)) {
          upper <- c(max(Response) * 5, 0, mean_C_mat * 10, 20)
        } # the intervals for a and b must be larger beacuse they can compensate each other
        if (missing(lower)) {
          lower <- c(0, -100, 0, 0, -20)
        }
        if (missing(start)) {
          start <- c(max(Response), -1, mean_C_mat, 0)
        }

        if (error_type == "Normal") {
          res_CA_nmk <- nmkb(
            par = start, 
            fn = function(x) {
              CA_complete2_RSS(
                C_mat     = C_mat, 
                Response  = Response, 
                Max       = x[1], Slopes = c(x[2], x[2]), 
                Ec50s     = c(x[3], x[4]), a = x[5], 
                interact  = interact, 
                multicore = multicore, 
                mc.cores  = mc.cores
                )
              }, 
            upper = upper, 
            lower = lower, 
            control = list(tol = 10^-6)
            )
          Res <- list(
            Max    = res_CA_nmk$par[1], 
            Slopes = c(res_CA_nmk$par[2], res_CA_nmk$par[2]),
            Ec50s  = c(res_CA_nmk$par[3:4]), 
            a      = res_CA_nmk$par[5], 
            Error  = res_CA_nmk$value
            )
          return(Res)
        } else if (error_type == "Poisson") {
          res_CA_nmk <- nmkb(
            par = start, 
            fn = function(x) {
              CA_complete2_Poisson(
                C_mat = C_mat, 
                Response = Response, 
                Max = x[1], 
                Slopes = c(x[2], x[2]), 
                Ec50s = c(x[3], x[4]), 
                a = x[5], 
                interact = interact,
                multicore = multicore,
                mc.cores = mc.cores
                )
              }, 
            upper = upper, 
            lower = lower,
            control = list(tol = 10^-6)
            )
          Res <- list(
            Max = res_CA_nmk$par[1], 
            Slopes = c(res_CA_nmk$par[2], res_CA_nmk$par[2]), 
            Ec50s = c(res_CA_nmk$par[3:4]), 
            a = res_CA_nmk$par[5],
            Error = res_CA_nmk$value
            )
          return(Res)
        } else {
          stop("Misspecification of the error model")
        }
      } # End SA
      
      ### 2.2.3. DR model ----
      if (interact == "DR") {
        if (missing(upper)) {
          upper <- c(max(Response) * 5, 0, mean_C_mat * 10, 20, rep(30, (dim(C_mat)[2] - 1)))
        } # the intervals for a and b must be larger beacuse they can compensate each other
        if (missing(lower)) {
          lower <- c(0, -100, 0, 0, -20, rep(-30, (dim(C_mat)[2] - 1)))
        }
        if (missing(start)) {
          start <- c(max(Response), -1, mean_C_mat, 0, 0)
        }

        if (error_type == "Normal") {
          res_CA_nmk <- nmkb(
            par = start, 
            fn = function(x) {
              CA_complete2_RSS(
                C_mat     = C_mat, 
                Response  = Response,
                Max       = x[1], 
                Slopes    = c(x[2], x[2]),
                Ec50s     = c(x[3:4]), 
                a         = x[5], 
                b         = x[6:length(x)],
                interact  = interact,
                multicore = multicore, 
                mc.cores  = mc.cores
                )
              }, 
            upper = upper,
            lower = lower, 
            control = list(tol = 10^-6)
            )
          Res <- list(
            Max    = res_CA_nmk$par[1],
            Slopes = c(res_CA_nmk$par[2], res_CA_nmk$par[2]), 
            Ec50s  = c(res_CA_nmk$par[3:4]), 
            a      = res_CA_nmk$par[5],
            b      = res_CA_nmk$par[6:(6 + (dim(C_mat)[2] - 1) - 1)], 
            Error  = res_CA_nmk$value
            )
          return(Res)
        } else if (error_type == "Poisson") {
          res_CA_nmk <- nmkb(
            par = start, 
            fn = function(x) {
              CA_complete2_Poisson(
                C_mat     = C_mat, 
                Response  = Response, 
                Max       = x[1], 
                Slopes    = c(x[2], x[2]), 
                Ec50s     = c(x[3:4]), 
                a         = x[5], 
                b         = x[6:length(x)], 
                interact  = interact, 
                multicore = multicore,
                mc.cores  = mc.cores
                )
              }, 
            upper = upper, 
            lower = lower, 
            control = list(tol = 10^-6)
            )
          Res <- list(
            Max    = res_CA_nmk$par[1], 
            Slopes = c(res_CA_nmk$par[2], res_CA_nmk$par[2]), 
            Ec50s  = c(res_CA_nmk$par[3:4]), 
            a      = res_CA_nmk$par[5],
            b      = res_CA_nmk$par[6:(6 + (dim(C_mat)[2] - 1) - 1)], 
            Error  = res_CA_nmk$value
            )
          return(Res)
        } else {
          stop("Misspecification of the error model")
        }
      }
      
      ### 2.2.4. DL model ----
      if (interact == "DL") {
        if (missing(upper)) {
          upper <- c(max(Response) * 5, 0, mean_C_mat * 10, 20, 30)
        }
        if (missing(lower)) {
          lower <- c(0, -100, 0, 0, -20, -30)
        }
        if (missing(start)) {
          start <- c(max(Response), -1, mean_C_mat, 0, 0)
        }

        if (error_type == "Normal") {
          res_CA_nmk <- nmkb(
            par = start, fn = function(x) {
              CA_complete2_RSS(
                C_mat     = C_mat, 
                Response  = Response,
                Max       = x[1], 
                Slopes    = c(x[2], x[2]), 
                Ec50s     = c(x[3:4]),
                a         = x[5], 
                b         = x[6], 
                interact  = interact,
                multicore = multicore,
                mc.cores  = mc.cores
                )
              }, 
            upper = upper, 
            lower = lower, 
            control = list(tol = 10^-6)
            )
          Res <- list(
            Max    = res_CA_nmk$par[1], 
            Slopes = c(res_CA_nmk$par[2], res_CA_nmk$par[2]),
            Ec50s  = c(res_CA_nmk$par[3:4]), 
            a      = res_CA_nmk$par[5], 
            b      = res_CA_nmk$par[6],
            Error  = res_CA_nmk$value
            )
          return(Res)
        } else if (error_type == "Poisson") {
          res_CA_nmk <- nmkb(
            par = start, 
            fn = function(x) {
              CA_complete2_Poisson(
                C_mat     = C_mat, 
                Response  = Response,
                Max       = x[1], 
                Slopes    = c(x[2], x[2]), 
                Ec50s     = c(x[3:4]), 
                a         = x[5], 
                b         = x[6], 
                interact  = interact, 
                multicore = multicore, 
                mc.cores  = mc.cores
                )
              }, 
            upper = upper, 
            lower = lower, 
            control = list(tol = 10^-6)
            )
          Res <- list(
            Max    = res_CA_nmk$par[1], 
            Slopes = c(res_CA_nmk$par[2], res_CA_nmk$par[2]), 
            Ec50s  = c(res_CA_nmk$par[3:4]), 
            a      = res_CA_nmk$par[5],
            b      = res_CA_nmk$par[6], 
            Error  = res_CA_nmk$value
            )
          return(Res)
        } else {
          stop("Misspecification of the error model")
        }
      } # End DL
    } # End common slopes
  } # End Dose-response curves not known
} # End.
