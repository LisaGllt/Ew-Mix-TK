
# General setup ----

f_load_libraries <- function(){
  # 📦 Data Manipulation
  library(tidyverse)   # Collection de packages pour manipulation et visualisation des données
  library(here)        # Gestion des chemins de fichiers
  library(readxl)      # Lecture des fichiers Excel
  library(reshape2)    # Restructuration des données
  library(DT)          # Génération de tables interactives
  library(knitr)       # Génération de rapports dynamiques en RMarkdown/Quarto
  
  # 🎨 Visualization
  library(ggplot2)     # Visualisation de données
  library(ggthemes)    # Thèmes pour ggplot2
  library(ggdist)      # Distribution et incertitude
  library(ggsci)       # Palettes de couleurs scientifiques
  library(viridis)     # Palette de couleurs perceptuellement uniforme
  library(wesanderson) # Palette de couleurs artistiques
  library(RColorBrewer)# Palettes de couleurs prédéfinies
  library(nord)        # Palettes de couleurs inspirées du Nord
  library(plotly)      # Graphiques interactifs
  library(ggiraph)     # Graphiques interactifs pour ggplot2
  library(ggrepel)     # Étiquettes non chevauchantes sur ggplot2
  library(patchwork)   # Combinaison de plusieurs ggplots
  library(gridExtra)   # Arrangements de graphiques en grille
  library(grid)        # Outils de mise en page graphique
  library(ggbreak)     # Briser les axes dans ggplot2
  library(ggtext)      # Formatage avancé de texte dans ggplot2
  library(kableExtra)  # Mise en forme avancée des tables
  library(flextable); library(gt)   # Tables rendering 
  library(processx)
  library(metR)
  library(rnaturalearth)
  library(sf)
  library(scales)
  library(colorspace)
  
  # 📊 Statistical Modeling & Bayesian Analysis
  library(brms)        # Modélisation bayésienne avec Stan
  library(rstan)       # Interface R pour Stan
  library(cmdstanr)    # Interface alternative pour Stan (CmdStan)
  library(tidybayes)   # Manipulation et visualisation des résultats bayésiens
  library(ggmcmc)      # Diagnostics des chaînes MCMC
  library(rethinking)  # Modélisation bayésienne avancée
  library(priorsense)  # Analyse de sensibilité des priors
  library(coda)
  library(ggmcmc)
  
  # 🔬 Regression & Hypothesis Testing
  library(car)         # Tests statistiques et régressions avancées
  library(nlstools)    # Outils pour modèles non linéaires
  library(lsmeans)     # Comparaisons post-hoc
  library(ggpubr)      # Outils pour publications scientifiques
  library(marginaleffects) # Effets marginaux des modèles
  library(brglm2)      # Régressions logistiques biais-réduits
  
  # ⚙️ Computational Tools & Parallelization
  library(parallel)    # Calcul parallèle
  library(deSolve)     # Équations différentielles
  library(tmvtnorm)    # Distribution normale tronquée multivariée
  library(fdrtool)     # Faux taux de découverte (FDR)
  library(drc)         # Modélisation de réponses aux doses
  
  # 🛠️ Model Evaluation & Performance
  library(easystats)   # Outils pour statistiques et modèles
  library(performance) # Diagnostics et évaluation de modèles
  library(modelsummary)# Résumé des modèles statistiques
  library(plan)        # Planification de l'exécution des tâches
  
  # 🖋️ Math & LaTeX Support
  library(latex2exp)   # Expressions LaTeX dans ggplot2
  library(extrafont)   # Gestion des polices pour ggplot2
  
  library(data.table)
  
}


f_load_colors <- function(){

  
  col_blue <<- "#5E81AC"
  col_red <<- "#f42404"
  
 pal_blue  <- c("#5E81AC", "#7F9DC4", "#A0C1D9", "#DCE9F2")
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

f_load_libraries_colors <- function(){
  f_load_libraries()
  f_load_colors()
}


# Add My Pet parameter recuperation ----

getDEB.species <- function() {
  require(rvest)
  library(rvest)
  url <- "https://www.bio.vu.nl/thb/deb/deblab/add_my_pet/species_list.html"
  d1 <- read_html(url)
  
  phylum <- d1 %>% html_nodes("td:nth-child(1)") %>% html_text()
  class <- d1 %>% html_nodes("td:nth-child(2)") %>% html_text()
  order <- d1 %>% html_nodes("td:nth-child(3)") %>% html_text()
  family <- d1 %>% html_nodes("td:nth-child(4)") %>% html_text()
  species <- d1 %>% html_nodes("td:nth-child(5)") %>% html_text()
  common <- d1 %>% html_nodes("td:nth-child(6)") %>% html_text()
  type <- d1 %>% html_nodes("td:nth-child(7)") %>% html_text()
  mre <- d1 %>% html_nodes("td:nth-child(8)") %>% html_text()
  smre <- d1 %>% html_nodes("td:nth-child(9)") %>% html_text()
  complete <- d1 %>% html_nodes("td:nth-child(10)") %>% html_text()
  all.species <- as.data.frame(cbind(phylum, class, order, 
                                     family, species, common, type, mre, smre, complete), 
                               stringsAsFactors = FALSE)
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
  symbol1 <- d1 %>% html_nodes("td:nth-child(1)") %>% html_text()
  
  value1 <- d1 %>% html_nodes("td:nth-child(2)") %>% html_text()
  
  units1 <- d1 %>% html_nodes("td:nth-child(3)") %>% html_text()
  
  description1 <- d1 %>% html_nodes("td:nth-child(4)") %>% 
    html_text()
  
  extra1 <- d1 %>% html_nodes("td:nth-child(5)") %>% html_text()
  
  extra2 <- d1 %>% html_nodes("td:nth-child(6)") %>% html_text()
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
  org.C <- c(units1[end + 3], description1[end + 3], extra1[3], 
             extra2[1])
  org.H <- c(value1[end + 4], units1[end + 4], description1[end + 
                                                              4], extra1[4])
  org.O <- c(value1[end + 5], units1[end + 5], description1[end + 
                                                              5], extra1[5])
  org.N <- c(value1[end + 6], units1[end + 6], description1[end + 
                                                              6], extra1[6])
  min.C <- c(units1[end + 7], description1[end + 7], extra1[7], 
             extra2[2])
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
  
  return(list(pars = pars, chempot = chempot, dens = dens, 
              organics = organics, minerals = minerals))
}

getDEB.implied <- function(species) {
  require(rvest)
  library(rvest)
  baseurl <- "https://www.bio.vu.nl/thb/deb/deblab/add_my_pet/entries_web/"
  d1 <- read_html(paste0(baseurl, species, "/", species, "_stat.html"))
  symbol <- d1 %>% html_nodes("td:nth-child(1)") %>% html_text()
  
  value <- d1 %>% html_nodes("td:nth-child(2)") %>% html_text()
  
  units <- d1 %>% html_nodes("td:nth-child(3)") %>% html_text()
  
  description <- d1 %>% html_nodes("td:nth-child(4)") %>% html_text()
  
  final <- as.data.frame(cbind(symbol, value, units, description))
  final$symbol <- as.character(symbol)
  final$value <- as.numeric(value)
  final$units <- as.character(units)
  final$description <- as.character(description)
  return(final)
}

f_GetParsAddMyPet <- function(Texp){
  Species <- "Aporrectodea_caliginosa"
  allpars = getDEB.pars(Species)
  df_pars = allpars$pars
  df_pars = df_pars[-1,] # remove duplicate T_A symbol
  
  df_rename <- data.frame(
    symbol = c("p_Am", "F_m", "kap_X", "kap_P", "v", "kap", "kap_R", "p_M", "p_T", 
               "k_J", "E_G", "E_Hb", "E_Hp", "h_a", "s_G", "L0", "T_AH", "T_H",
               "Wwg", "bw", "del_M", "f", "f_Bart_high", "f_Bart_high_b", 
               "f_Bart_low", "f_Bart_low_b", "f_Bart_medium", "m_0", "max_r_mb",
               "mu_OM", "mu_c", "r_mb", "t_0", "wV", "T_A", "T_ref", "Wd_0"),
    name =   c("pAm",  "Fm",  "kapX",  "kapP",  "v", "kap", "kapR",  "pM",  "pT",  
               "kJ",  "Eg",  "Ehb",  "Ehp",  "ha",  "sG",  "L0", "TAH",  "TH",
               "Wwg", "bw", "Shape", "f", "fBartHigh", "fBartHighb", 
               "fBartLow",   "fBartLowb",    "fBartMedium",   "m0",  "maxRmb", 
               "muOM",  "muC",  "Rmb",  "t0",  "wV", "TA",  "Tref",  "Wd0")
  )
  
  df_pars <- df_pars |> left_join(df_rename)
  
  df_pars = rbind(df_pars, data.frame(symbol = "UE0",
                                      value = 0.078, # from Gergs et al., 2022
                                      units = "cm^2/d",
                                      description = "Scaled cost of an egg",
                                      name = "UE0"))
  df_pars = rbind(df_pars, data.frame(symbol = "E0",
                                      value = 134.988, # from AddMyPet website
                                      units = "J",
                                      description = "Scaled cost of an egg",
                                      name = "E0"))
  df_pars = rbind(df_pars, data.frame(symbol = "w",
                                      value = 27.24, # from Gergs et al., 2022
                                      units = "g/cm^3",
                                      description = "Contribution of reserve to bw",
                                      name = "w"))
  
  
  # Correction des valeurs par la température (Gergs et al. 2022)
  
  df_pars$corr_value <- df_pars$value
  
  TA   <- subset(df_pars, name =="TA")$corr_value
  TAH  <- subset(df_pars, name =="TAH")$corr_value
  TH   <- subset(df_pars, name =="TH")$corr_value
  Tref <- subset(df_pars, name =="Tref")$corr_value
  
  sA   <- exp(TA/Tref-TA/(Texp+273.15))
  srH  <- (1 + exp(TAH/TH-TAH/Tref))/(1+exp(TAH/TH-TAH/(Texp+273.15)))
  Ft   <- sA*((T+273.15>=Tref)*srH + (T+273.15<Tref))
  
  df_pars_add <- df_pars |> 
    filter(name %in% c("pAm", "pM", "v", "kJ")) |> 
    mutate(corr_value = value*Ft)|> 
    mutate(
      symbol = paste(symbol,"_t", sep=""),
      name = paste(name,"_t", sep="")
      )
  
  df_pars <- rbind(df_pars, df_pars_add)
  
  # Liste des paramètres pour le modèle
  vec_pars = df_pars$corr_value
  names(vec_pars) = df_pars$name
  vec_pars = append(vec_pars, c(kapH = 1)) # set the maturation efficiency to 1
  return(df_pars)
}

f_vec_pars <- function(df_pars){
  # Liste des paramètres pour le modèle
  vec_pars = df_pars$corr_value
  names(vec_pars) = df_pars$name
  vec_pars = append(vec_pars, c(kapH = 1)) # set the maturation efficiency to 1
  return(vec_pars)
}

# TK models ----

## MCSim files creation ----

f_read_data_TK <- function(Molecule){
  df_TK <- read_excel(here::here("data/Data_TK_long.xlsx")) |> 
    mutate(
      Time_point_f = as.factor(Time_point),
      ID = as.factor(ID),
      w = w/1000, # Conversion mg to g
      Dose = Dose*1000 # mg/kg to ng/g (mg/kg = microg/g = 1000 ng/g)
    ) |> 
    filter(Keep == "Yes") |> 
    group_by(ID) |> 
    mutate(t = as.numeric(difftime(Date, first(Date), units = "days"))) |> 
    ungroup()
  
  Dose_EPX <- subset(df_TK, Molecule=="EPX")$Dose[1]
  Dose_IMD <- subset(df_TK, Molecule=="IMD")$Dose[1]
  
  if(Molecule == "EPX"){
    df_TK_f <- df_TK |> 
      dplyr::select(Molecule, Dose, ID, expo, Phase, w, C_worm_EPX, C_worm_IMD, t, Time_point, ID_recipient)|> 
      filter(Molecule == "EPX") |> 
      mutate(
        C_soil_IMD = case_when(
          t == 0 & ID_recipient == "1" ~ 0.089*1000, # ng/g
          t == 0 & ID_recipient == "2" ~ 0.072*1000  # ng/g
        ),
        C_soil_EPX = case_when(
          t == 0 & ID_recipient == "1" ~ 0.780*1000, # ng/g
          t == 0 & ID_recipient == "2" ~ 1.070*1000  # ng/g
        ),
        Weight_popo = if_else(Phase == "Frozen", -1, w),
        Weight_nopopo = if_else(!(Phase == "Frozen"), -1, w)
      ) |> 
      mutate(across(where(is.numeric), ~ replace_na(.x, -1))) |> 
      arrange(ID)
    
    C_worm_t0_IMD <- mean(subset(df_TK,Molecule =="EPX")$C_worm_IMD, na.rm = T)
    C_worm_t0_EPX <- mean(subset(df_TK,Molecule =="IMD")$C_worm_EPX, na.rm = T)
    
    df_TK_f[df_TK_f$t == 0,]$C_worm_IMD <- C_worm_t0_IMD
    df_TK_f[df_TK_f$t == 0,]$C_worm_EPX <- C_worm_t0_EPX
  }else if(Molecule == "IMD"){
    df_TK_f <- df_TK |> 
      dplyr::select(Molecule, Dose, ID, expo, Phase, w, C_worm_EPX, C_worm_IMD, t, Time_point, ID_recipient)|> 
      filter(Molecule == "IMD") |> 
      mutate(
        C_soil_IMD = case_when(
          t == 0 & ID_recipient == "1" ~ 0.089*1000, # ng/g
          t == 0 & ID_recipient == "2" ~ 0.072*1000  # ng/g
        ),
        C_soil_EPX = case_when(
          t == 0 & ID_recipient == "1" ~ 0.780*1000, # ng/g
          t == 0 & ID_recipient == "2" ~ 1.070*1000  # ng/g
        ),
        Weight_popo = if_else(Phase == "Frozen", -1, w),
        Weight_nopopo = if_else(!(Phase == "Frozen"), -1, w)
      ) |> 
      mutate(across(where(is.numeric), ~ replace_na(.x, -1))) |> 
      arrange(ID)
    
    C_worm_t0_IMD <- mean(subset(df_TK,Molecule =="EPX")$C_worm_IMD, na.rm = T)
    C_worm_t0_EPX <- mean(subset(df_TK,Molecule =="IMD")$C_worm_EPX, na.rm = T)
    
    df_TK_f[df_TK_f$t == 0,]$C_worm_IMD <- C_worm_t0_IMD
    df_TK_f[df_TK_f$t == 0,]$C_worm_EPX <- C_worm_t0_EPX
  }else{
    stop("Uncorrect specification of Molecule")
  }
  
  return(df_TK_f)
  
}

f_In_experiments <- function(Molecule){
  
  df_TK_f <- f_read_data_TK(Molecule)
  
  C_clx_IMD <- 16/1000 # ng/g
  C_clx_EPX <- 90/1000 # ng/g
  
  char_final <- ""
  
  if(Molecule == "IMD"){
    for (i in unique(df_TK_f$ID)){
      
      df_TK_i <- subset(df_TK_f, ID == i) |> 
        mutate(w = if_else(Phase == "Frozen" & expo == 2, -1, w))
      
      char_tw <- paste(df_TK_i$t, collapse=",")
      char_Ww_nopopo <- paste(df_TK_i$Weight_nopopo, collapse=",")
      char_Ww_popo <- paste(df_TK_i$Weight_popo, collapse=",")
      
      char_tCi <- paste(df_TK_i$t, collapse=",")
      char_CiIMD <- paste(df_TK_i$C_worm_IMD, collapse=",")

      char_expo <- paste(df_TK_i$expo, collapse=",")
      length_expo <- length(df_TK_i$expo)
      char_texpo <- char_tw

      
      char_i <- paste(
        paste("Experiment { #", "TK IMD - i = ", i), 
        
        paste("    Wpopoinit=", subset(df_TK_i, t==0)$Weight_popo, ";", sep=""),
        
        paste("    Ci0IMD=", C_worm_t0_IMD, ";", sep=""),
        paste("    Ce0IMD=", subset(df_TK_i, t==0)$C_soil_IMD, ";", sep=""),
        paste("    CclxIMD=", C_clx_IMD, ";", sep=""),
        
        paste("    Print(Weight_nopopo,", char_tw, ");" , sep=""),
        paste("    Data(Weight_nopopo,", char_Ww_nopopo, ");" , sep=""),
        
        paste("    Print(Weight_popo,", char_tw, ");" , sep=""),
        paste("    Data(Weight_popo,", char_Ww_popo, ");" , sep=""),
        
        paste("    Print(CiIMD,", char_tCi, ");" , sep=""),
        paste("    Data(CiIMD,", char_CiIMD, ");" , sep=""),
        
        paste("    expo=NDoses(", length_expo,",\n               ", 
              char_expo,",\n               ", char_texpo, ");", sep=""),
              
        paste("}"),
        sep="\n"
      )
      char_final <- paste(char_final, char_i, sep="\n")
    }
  }else if (Molecule == "EPX"){
    for (i in unique(df_TK_f$ID)){
      
      df_TK_i <- subset(df_TK_f, ID == i)|> 
        mutate(w = if_else(expo == 2, -1, w))
      
      char_tw <- paste(df_TK_i$t, collapse=",")
      char_Ww_nopopo <- paste(df_TK_i$Weight_nopopo, collapse=",")
      char_Ww_popo <- paste(df_TK_i$Weight_popo, collapse=",")
      
      char_tCi <- paste(df_TK_i$t, collapse=",")
      char_CiEPX <- paste(df_TK_i$C_worm_EPX, collapse=",")
      
      char_expo <- paste(df_TK_i$expo, collapse=",")
      length_expo <- length(df_TK_i$expo)
      char_texpo <- char_tw
      
      char_i <- paste(
        paste("Experiment { #", "TK EPX - i = ", i), 
        
        paste("    Winit=", subset(df_TK_i, t==0)$w, ";", sep=""),
        
        paste("    Ci0EPX=", C_worm_t0_EPX, ";", sep=""),
        paste("    Ce0EPX=", subset(df_TK_i, t==0)$C_soil_EPX, ";", sep=""),
        paste("    CclxEPX=", C_clx_EPX, ";", sep=""),
        
        paste("    Print(Weight_nopopo,", char_tw, ");" , sep=""),
        paste("    Data(Weight_nopopo,", char_Ww_nopopo, ");" , sep=""),
        
        paste("    Print(Weight_popo,", char_tw, ");" , sep=""),
        paste("    Data(Weight_popo,", char_Ww_popo, ");" , sep=""),
        
        paste("    Print(CiEPX,", char_tCi, ");" , sep=""),
        paste("    Data(CiEPX,", char_CiEPX, ");" , sep=""),
        
        paste("    expo=NDoses(", length_expo,",\n               ", 
              char_expo,",\n               ", char_texpo, ");", sep=""),
        
        paste("}"),
        sep="\n"
      )
      char_final <- paste(char_final, char_i, sep="\n")
    }
  }else{
    stop("Uncorrect specification of Molecule")
  }
    return(char_final)
}

f_create_mcmc_block <- function(name, seed) {
  sprintf(
    'MCMC( "%s.out",  # output file
      "",                # name of restart file
      "",                # name of data file
      100000, 0,          # iterations, print prediction flag
      1, 10000,          # printing frequency, iters to print
      %d);             # random seed
      
Integrate (Lsodes,  1e-8, 1e-10, 0); # Integrate(Solver, RTOL, ATOL, ITOL);
    ',
    name, seed
  )
}

f_In_tot <- function(Molecule, text_priors, text_likelihood, text_param_ind = ""){
  
  seeds <- c(C1 = 3333, C2 = 6666, C3 = 1212)
  
  text_Level_global <- 
    'Level{ # Global
    '
  
  text_Level_exp <- '
Level{
  '
  
  banner_exp <- '
  ############## Experiments ###################
  '
  
  text_experiment <- paste0(banner_exp, f_In_experiments(Molecule), sep="\n")
  
  text_end <- '
} # End
} # End global

End.'
  
  # .in generation and saving
  for (id in names(seeds)) {
    
    text_start <- f_create_mcmc_block(paste0("TK_", Molecule, "_", id), seeds[[id]])
    
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
    )
    
    File_path <- paste0("mod/TK_", Molecule)
    
    writeLines(
      text_full,
      here::here(File_path, paste0("TK_", Molecule, "_", id, ".in"))
    )
  }
  
}

## MCSim to R ----

f_MCSim <- function(path_mod){
  
  # Read files
  
  Files_in  = list.files(path = path_mod, pattern = "[0-3]\\.in")
  Files_out = list.files(path = path_mod, pattern = "[0-3]\\.out")
  
  tmp = grep(
    pattern="out.kernel", 
    Files_out
  )
  if(length(tmp)>0){ Files_out = Files_out [ -tmp ] }
  
  NC = length(Files_out)
  
  # Priors
  
  Initialize = readLines(paste(path_mod,Files_in[1], sep="/"))
  
  Exl.Par1   = grep(pattern="*\\#Distrib*", Initialize)  
  Exl.Par2   = grep(pattern="*\\# Distrib*", Initialize) 
  Parameter = grep(pattern="*Distrib*", Initialize)
  Parameter =  Parameter[!Parameter %in% Exl.Par1]
  Parameter =  Parameter[!Parameter %in% Exl.Par2]
  
  np = length(Parameter)
  
  Experiment = grep(pattern="Experiment", Initialize, value =T)
  nexperiement =  length(Experiment)
  
  ParTable =as.data.frame(matrix(NA,np,6))
  colnames(ParTable)=c("Nom","Distribution","P1","P2","P3","P4")
  for (i in 1:np){ 
    
    temp = gsub("\t","", Initialize[Parameter[i]] )
    temp = strsplit( temp, "\\#")[[1]][1]
    temp = strsplit( temp, ".\\(|\\)|\\,| ")[[1]]
    temp = temp[temp!=""]
    
    for(j in 2:(length(temp)-1) ) {  ParTable[i,j-1] = temp[j] }
  }
  
  ParTable[,3:ncol(ParTable)]=apply(ParTable[,3:ncol(ParTable)],2,as.numeric)
  
  # Results 
  
  Data_T = Data_V = Data_L =  vector("list", length=NC) # sous forme liste
  
  for (i in 1:NC){
    
    data <- read.table( eval(paste(path_mod,Files_out[i], sep="/"))   , header = TRUE)
    
    if(i ==1){  times = data[,1] }
    tmp = as.matrix(data[,-1]) 
    
    Data_L[[i]] = tmp                                     # Fichier complet pour les NC chaines
    Data_T[[i]] = tmp[,1:np]                              # Fichier parametres pour les NC chaines
    Data_V[[i]] = tmp[,(np+1):ncol(tmp)]                  # Fichier Vraisemblance pour les NC chaines
    names(Data_L)[i]= paste0("Chain",i) 
    names(Data_T)[i]= paste0("Chain",i) 
    names(Data_V)[i]= paste0("Chain",i)
    
  }
  
    # Chains
  
  Niter = nrow(Data_T[[1]]) # number of lines
  PI    = as.numeric(times[nrow(Data_T[[1]])] - times[(nrow(Data_T[[1]])-1)]) # pas de l'iteration
  
  nb = 10000
  if(nb>=Niter){nb=Niter/2} #control nb < Niter
  
  IterSelect = (( Niter- nb / PI ): Niter  )
  
  Data_All = NULL
  for (i in 1:NC){
    Data_T[[i]] =  mcmc(Data_T[[i]], start = 1, end = Niter , thin = PI)
    Data_All = rbind(Data_All, Data_L[[i]][IterSelect,])  }
  res.mcmc =  coda::as.mcmc.list(Data_T)
  df_res.mcmc = ggs(res.mcmc)
  resmc  = summary(res.mcmc)
  
  Mode    = which.max(Data_All[,ncol(Data_All)]) # MPV computation
  Result_mode = Data_All[ Mode[1], ] 
  
  Min = apply(Data_All,2,min)
  Max = apply(Data_All,2,max)
  Result_sd     = apply( Data_All , 2, sd)
  Result_mean   = apply( Data_All , 2, mean)
  Result_quantil = apply( Data_All , 2, function(x){ quantile(x, c(0.025, 0.975) ) }  )
  
  Res = rbind(Result_mode, Result_quantil, Min, Max, Result_mean, Result_sd )
  
  # AIC and BIC calculations
  
  MaxVraiss = Data_All[ Mode[1], ncol(Data_All)] 
  
  AIC <- -2*MaxVraiss+2*np
  Ndata = 13 # number of observed data used
  BIC <- -2*MaxVraiss+log(Ndata)*np
  
  # tab_setpoint.out construction
  
  Niter_chain = 333 # Niter_chain last lines selected to setpoint.in / doit pas être trop grand sinon impossible de faire les graphs (marche au moins avec 1000)
  
  Selected_chain = NULL
  
  for (i in 1:NC){
    Selected_chain = rbind( Selected_chain , Data_L[[i]][ (nrow (Data_L[[i]])- Niter_chain+1) : nrow (Data_L[[i]]),] ) # Ajout du +1
  }
  
  tmp = -c( (ncol(Selected_chain)-2) : ncol(Selected_chain)  )
  Selected_chain <-Selected_chain[,tmp]
  MVP =  Result_mode[ tmp] # "Meilleure" solution dans toutes les chaines
  
  Selected_chain <-rbind( Selected_chain, as.list(MVP)) # Ajout du as.list()
  
  # Likelihood and deviance ratio 
  
  tmp.names = Devc = LnData = LnPost_start= LnPost = NULL
  
  for(i in 1:NC){
    temp = -2 *  Data_V[[i]][IterSelect,3]   # -2log(vraisemblance), tableau
    Devc =cbind(Devc,temp)
    LnPost_start =  cbind(LnPost_start, Data_V[[i]][(100):(2* Niter/4),3]) 
    LnPost = cbind(LnPost, Data_V[[i]][IterSelect,3]) 
    tmp.names <- c(tmp.names,paste("Chain",i))
  }
  
  df_LnPost_start <- data.frame(iteration = (100):(2 * Niter/4), LnPost_start) %>%
    pivot_longer(-iteration, names_to = "Chain", values_to = "LnPosterior")
  
  df_LnPost <- data.frame(iteration = IterSelect[1:nrow(Devc)], LnPost) %>%
    pivot_longer(-iteration, names_to = "Chain", values_to = "LnPosterior")
  
  df_Devc <- data.frame(iteration = IterSelect[1:nrow(Devc)], RatioDeviance = Devc/min(Devc)) %>%
    pivot_longer(-iteration, names_to = "Chain", values_to = "RatioDeviance")
  
  # Outputs 
  
  write.table(Selected_chain, file=paste(path_mod, "tab_setpoint.out", sep="/"),quote=FALSE)
  
  MCMC_out <- list(
    NC = NC,
    Files_in = Files_in,
    Files_out = Files_out,
    Nb_param = np,
    Nb_experiement = nexperiement,
    Priors = ParTable, 
    Nb_iter_kept = nb, # Last iterations used to describe the posterior distributions
    Chains = df_res.mcmc,
    df_LnPost_start = df_LnPost_start,
    df_LnPost = df_LnPost,
    df_Devc = df_Devc,
    Summary_res = Res,
    AIC = AIC,
    BIC = BIC, 
    N_iter_setpoint = Niter_chain # last lines selected to setpoint.in (cannot be too big)
  )
  
  return(MCMC_out)
  
}

f_MCSim_ind <- function(path_mod){
  
  # Read files
  
  Files_in  = list.files(path = path_mod, pattern = "C[0-3]\\.in")
  Files_out = list.files(path = path_mod, pattern = "C[0-3]\\.out")
  
  tmp = grep(
    pattern="out.kernel", 
    Files_out
  )
  if(length(tmp)>0){ Files_out = Files_out [ -tmp ] }
  
  NC = length(Files_out)
  
  # Priors
  
  Initialize = readLines(paste(path_mod,Files_in[1], sep="/"))
  
  Exl.Par1   = grep(pattern="*\\#Distrib*", Initialize)  
  Exl.Par2   = grep(pattern="*\\# Distrib*", Initialize) 
  Parameter = grep(pattern="*Distrib*", Initialize)
  Parameter =  Parameter[!Parameter %in% Exl.Par1]
  Parameter =  Parameter[!Parameter %in% Exl.Par2]
  
  np_tmp = length(Parameter) # Used just to be able to count the number of parameters with individual variation
  
  Experiment = grep(pattern="Experiment", Initialize, value =T)
  nexperiement =  length(Experiment)
  ParTable =as.data.frame(matrix(NA,np_tmp,6))
  
  colnames(ParTable)=c("Nom","Distribution","P1","P2","P3","P4")
  for (i in 1:np_tmp){ 
    
    temp = gsub("\t","", Initialize[Parameter[i]] )
    temp = strsplit( temp, "\\#")[[1]][1]
    temp = strsplit( temp, ".\\(|\\)|\\,| ")[[1]]
    temp = temp[temp!=""]
    
    for(j in 2:(length(temp)-1) ) {  ParTable[i,j-1] = temp[j] }
  }
  
  Nb_param_IndVar <- length(gsub("^Vr_", "", ParTable$Nom[grepl("^Vr_", ParTable$Nom)]))
  np <- np_tmp - Nb_param_IndVar
  
  ParTable = head(ParTable,-Nb_param_IndVar)
  
  ParTable[,3:ncol(ParTable)]=apply(ParTable[,3:ncol(ParTable)],2,as.numeric)
  
  # Results 
  
  Data_T = Data_V = Data_L =  vector("list", length=NC) # sous forme liste
  
  for (i in 1:NC){
    
    data <- read.table( eval(paste(path_mod,Files_out[i], sep="/"))   , header = TRUE)
    
    if(i ==1){  times = data[,1] }
    tmp = as.matrix(data[,-1]) 
    
    Data_L[[i]] = tmp                                     # Fichier complet pour les NC chaines
    Data_T[[i]] = tmp[,1:np]                              # Fichier parametres pour les NC chaines
    Data_V[[i]] = tmp[,(np+1):ncol(tmp)]                  # Fichier Vraisemblance pour les NC chaines
    names(Data_L)[i]= paste0("Chain",i) 
    names(Data_T)[i]= paste0("Chain",i) 
    names(Data_V)[i]= paste0("Chain",i)
    
  }
  
  # Chains
  
  Niter = nrow(Data_T[[1]]) # number of lines
  PI    = as.numeric(times[nrow(Data_T[[1]])] - times[(nrow(Data_T[[1]])-1)]) # pas de l'iteration
  
  nb = 10000
  if(nb>=Niter){nb=Niter/2} #control nb < Niter
  
  IterSelect = (( Niter- nb / PI ): Niter  )
  
  Data_All = NULL
  for (i in 1:NC){
    Data_T[[i]] =  mcmc(Data_T[[i]], start = 1, end = Niter , thin = PI)
    Data_All = rbind(Data_All, Data_L[[i]][IterSelect,])  }
  res.mcmc =  coda::as.mcmc.list(Data_T)
  df_res.mcmc = ggs(res.mcmc)
  resmc  = summary(res.mcmc)
  
  Mode    = which.max(Data_All[,ncol(Data_All)]) # MPV computation
  Result_mode = Data_All[ Mode[1], ] 
  
  Min = apply(Data_All,2,min)
  Max = apply(Data_All,2,max)
  Result_sd     = apply( Data_All , 2, sd)
  
  Result_mean   = apply( Data_All , 2, mean)
  Result_quantil = apply( Data_All , 2, function(x){ quantile(x, c(0.025, 0.975) ) }  )
  
  Res = rbind(Result_mode, Result_quantil, Min, Max, Result_mean, Result_sd )
  
  # AIC and BIC calculations
  
  MaxVraiss = Data_All[ Mode[1], ncol(Data_All)] 
  
  AIC <- -2*MaxVraiss+2*np
  Ndata = 13 # number of observed data used
  BIC <- -2*MaxVraiss+log(Ndata)*np
  
  # tab_setpoint.out construction
  
      # tab_setpoint pop
  
  Niter_chain = 333 # Niter_chain last lines selected to setpoint.in / doit pas être trop grand sinon impossible de faire les graphs (marche au moins avec 1000)
  
  Selected_chain = NULL
  
  for (i in 1:NC){
    Selected_chain = rbind( Selected_chain , Data_L[[i]][ (nrow (Data_L[[i]])- Niter_chain+1) : nrow (Data_L[[i]]),] ) # Ajout du +1
  }
  
  tmp = -c( (ncol(Selected_chain)-2) : ncol(Selected_chain)  )
  Selected_chain <-Selected_chain[,tmp]
  MVP =  Result_mode[ tmp] # "Meilleure" solution dans toutes les chaines
  
  Selected_chain <-rbind( Selected_chain, as.list(MVP)) # Ajout du as.list()
  df_Selected_chain <- as.data.frame(Selected_chain)
  
  tab_setpoint <- df_Selected_chain[,1:np]
  
  
      # Setpoint individuel
  col_ind <- df_Selected_chain[, (np+1):length(df_Selected_chain)]
  
  colnames(tab_setpoint) <- gsub("\\.1\\.$", "", colnames(tab_setpoint))
  Names_par <- gsub("\\.1\\.$", "", names(tab_setpoint))
  names_param_ind <- grep("^Vr_", Names_par, value = TRUE)
  names_param_sigma <- grep("^Sigma_", Names_par, value = TRUE)
  names_param_pop_ind <- sub("^Vr_", "", names_param_ind)
  
  col_pop <- tab_setpoint |> 
    dplyr::select(-all_of(c(names_param_sigma, names_param_ind, names_param_pop_ind)))
  
  # Version LG
  col_ind_piled <- NULL

  for (i in 1:nexperiement){

    start = Nb_param_IndVar*(i-1)+1
    end = Nb_param_IndVar*(i)

    col_ind_i <- as.data.frame(unlist((col_ind[, start:end])))
    names(col_ind_i) <- names_param_pop_ind
    col_ind_piled <- rbind(col_ind_piled, col_ind_i)
  }
  
  n_repeats <- nrow(col_ind_piled) / nrow(col_pop)
  col_pop_rep <- col_pop[rep(1:nrow(col_pop), each = n_repeats), ]
  
  tab_setpoint_ind_tmp <- cbind(col_pop_rep, col_ind_piled)
  
  cols_growth <- grep("^a_growth", colnames(tab_setpoint_ind_tmp), value = TRUE)
  cols_fasting <- grep("^a_fasting", colnames(tab_setpoint_ind_tmp), value = TRUE)
  cols_other <- setdiff(colnames(tab_setpoint_ind_tmp), c(cols_growth, cols_fasting))
  
  # Réorganisation
  tab_setpoint_ind <- tab_setpoint_ind_tmp[, c(cols_other, cols_growth, cols_fasting)]
  
  
  # Likelihood and deviance ratio 
  
  tmp.names = Devc = LnData = LnPost_start= LnPost = NULL
  
  for(i in 1:NC){
    temp = -2 *  Data_V[[i]][IterSelect,3]   # -2log(vraisemblance), tableau
    Devc =cbind(Devc,temp)
    LnPost_start =  cbind(LnPost_start, Data_V[[i]][(100):(2* Niter/4),3]) 
    LnPost = cbind(LnPost, Data_V[[i]][IterSelect,3]) 
    tmp.names <- c(tmp.names,paste("Chain",i))
  }
  
  df_LnPost_start <- data.frame(iteration = (100):(2 * Niter/4), LnPost_start) %>%
    pivot_longer(-iteration, names_to = "Chain", values_to = "LnPosterior")
  
  df_LnPost <- data.frame(iteration = IterSelect[1:nrow(Devc)], LnPost) %>%
    pivot_longer(-iteration, names_to = "Chain", values_to = "LnPosterior")
  
  df_Devc <- data.frame(iteration = IterSelect[1:nrow(Devc)], RatioDeviance = Devc/min(Devc)) %>%
    pivot_longer(-iteration, names_to = "Chain", values_to = "RatioDeviance")
  
  # Outputs 
  tab_setpoint <- tab_setpoint |> 
    mutate(across(where(is.list), ~ simplify2array(.) |> unlist()))
  tab_setpoint_ind <- tab_setpoint_ind |> 
    mutate(across(where(is.list), ~ simplify2array(.) |> unlist()))
  
  write.table(tab_setpoint, file=paste(path_mod, "tab_setpoint.out", sep="/"),quote=FALSE)
  write.table(tab_setpoint_ind, file=paste(path_mod, "tab_setpoint_ind.out", sep="/"),quote=FALSE)
  
  MCMC_out <- list(
    NC = NC,
    Files_in = Files_in,
    Files_out = Files_out,
    Nb_param = np,
    Nb_experiement = nexperiement,
    Priors = ParTable, 
    Nb_iter_kept = nb, # Last iterations used to describe the posterior distributions
    Chains = df_res.mcmc,
    df_LnPost_start = df_LnPost_start,
    df_LnPost = df_LnPost,
    df_Devc = df_Devc,
    Summary_res = Res,
    AIC = AIC,
    BIC = BIC, 
    N_iter_setpoint = Niter_chain # last lines selected to setpoint.in (cannot be too big)
  )
  
  return(MCMC_out)
  
}

# Mixture - Jonker simple interaction model ----

# Fonction calculating the surface dose - response knowing the dose - response curves

CA_complete2 <- function(C_mat, Max, Slopes, Ec50s, a=0, b=0, interact="none", multicore=FALSE, mc.cores=4){
  param<-data.frame(Slopes, Ec50s)
  #try(cat(Slopes))
  #try(cat(Ec50s))
  if (interact=="none"){
    a<-0
    b<-0
    f<-function(Y, x=x, Max=Max, param=param, a=a, b=b){
      ecs <- param$Ec50s * ((Max - Y)/Y) ^ (1 / param$Slopes)  # C1 corresponding to Y 
      G<-(sum(x / ecs))-1
      return(abs(G))
    }
  }
  else if (interact=="SA"){
    b<-0
    f<-function(Y, x=x, Max=Max, param=param, a=a, b=b){
      ecs <- param$Ec50s * ((Max - Y)/Y) ^ (1 / param$Slopes)  # C1 corresponding to Y 
      G<-(sum(x / ecs))-exp(a*prod(x/param$Ec50s/(sum(x/param$Ec50s))))
      return(abs(G))
    }
  }
  else if (interact=="DR"){
    if (length(b)!=(dim(C_mat)[2]-1))
      stop("length of b should be equal to the number of chemicals -1.")
    f<-function(Y, x=x, Max=Max, param=param, a=a, b=b){
      ecs <- param$Ec50s * ((Max - Y)/Y) ^ (1 / param$Slopes)  # Cs corresponding to Y 
      G<-(sum(x / ecs))-exp((a+b%*%((x/param$Ec50s/(sum(x/param$Ec50s))))[1:(dim(C_mat)[2]-1)])*prod(x/param$Ec50s/(sum(x/param$Ec50s))))
      return(abs(G))
    }
  }
  else if (interact=="DR2"){
    if (length(b)!=(dim(C_mat)[2]-1))
      stop("length of b should be equal to the number of chemicals -1.")
    f<-function(Y, x=x, Max=Max, param=param, a=a, b=b){
      ecs <- param$Ec50s * ((Max - Y)/Y) ^ (1 / param$Slopes)  # Cs corresponding to Y 
      G<-(sum(x / ecs))-exp((a+b%*%sin((x/param$Ec50s/(sum(x/param$Ec50s)))*2*pi)[1:(dim(C_mat)[2]-1)])*prod(x/param$Ec50s/(sum(x/param$Ec50s))))
      return(abs(G))
    }
  }
  else if (interact=="DL"){
    f<-function(Y, x=x, Max=Max, param=param, a=a, b=b){
      if (length(b)!=(dim(C_mat)[2]-1))
        stop("length of b should be equal to the number of chemicals -1.")
      ecs <- param$Ec50s * ((Max - Y)/Y) ^ (1 / param$Slopes)  # C1 corresponding to Y 
      G<-(sum(x / ecs))-exp(a*(1-b*(sum(x/param$Ec50s)))*prod(x/param$Ec50s/(sum(x/param$Ec50s))))
      return(abs(G))
    }
  }
  else 
    stop("please specify interaction model")
  Y_f<-function(x, Max=Max, param=param, a=a, b=b) {
    if (all(x==0)){          #no chemical
      if  (all(param$Slopes > 0))
        return(Max)
      if  (all(param$Slopes < 0))
        return(0)
    }
    #Y<-optimize(f=function(Y) f(Y=Y, x=x, Max=Max, param=param, a=a, b=b), interval=c(0,Max*1.2))$minimum
    Y<-optimize(f=function(Y) f(Y=Y, x=x, Max=Max, param=param, a=a, b=b), interval=c(0,Max))$minimum
    
    return(Y)
  }
  if (multicore){
    C_mat_list<-split(unique(C_mat), cut(1:(dim(unique(C_mat))[1]), breaks=dim(unique(C_mat))[1]))
    res_CA_unique<-unlist(mclapply(C_mat_list, function(x) Y_f(x,Max=Max, param=param, a=a, b=b), mc.cores=mc.cores))
    res_CA<-rep(NA, dim(unique(C_mat))[1])
    for (i in 1:(dim(unique(C_mat))[1]))
      res_CA[which((C_mat[,1]==unique(C_mat)[i,1])&(C_mat[,2]==unique(C_mat)[i,2]))]<-res_CA_unique[i]
  }
  else {
    res_CA_unique<-apply(unique(C_mat),1, function(x) Y_f(x, Max=Max, param=param, a=a, b=b))
    res_CA<-rep(NA, dim(unique(C_mat))[1])
    for (i in 1:(dim(unique(C_mat))[1]))
      res_CA[which((C_mat[,1]==unique(C_mat)[i,1])&(C_mat[,2]==unique(C_mat)[i,2]))]<-res_CA_unique[i]
  }
  if (any(is.na(res_CA)))
    cat(param, a, b)
  return(res_CA)
} 

CA_complete2_fit_speed<-function(C_mat, Response, param=NULL, upper=NULL, lower=NULL, start=NULL, interact="none", identical_slopes=FALSE, iter=500, multicore=FALSE, mc.cores=4){
  
  if (!is.null(param)){
    
    if ((is.null(param$Max))|(is.null(param$Slopes))|(is.null(param$Ec50s)))
      stop("param misspecification")
    
    if (interact=="none")
      stop("please specify interaction")
    
    if (interact=="SA"){ 
      if (missing(upper))
        upper=20 # the intervals for a and b must be larger because they can compensate each other
      if (missing(lower))
        lower=-20
      
      # fit 
      res_CA_optim<-optimize(f=function(x){ 
        CA_complete2_RSS(C_mat=C_mat, 
                         Response=Response, 
                         Max=param$Max, 
                         Slopes=param$Slopes, 
                         Ec50s=param$Ec50s, 
                         a=x, 
                         interact=interact, 
                         multicore=multicore, 
                         mc.cores=mc.cores)}, 
        upper=upper, 
        lower=lower)
      
      return(list(a=res_CA_optim$minimum, 
                  RSS=res_CA_optim$objective))
    }
  }
  if (is.null(param)){
    
    require(dfoptim)
    
    if (any(C_mat==0))
      mean_C_mat<-exp(apply(log(C_mat[-which(C_mat==0, arr.ind=TRUE)[,1],]),2,mean))
    
    else
      mean_C_mat<-exp(apply(log(C_mat),2,mean))
    
    if (!identical_slopes){
      
      if (interact=="none"){
        
        if (missing(upper))
          upper=c(max(Response)*5, 0,0, mean_C_mat*10)
        
        if (missing(lower))
          lower=c(0,-100,-100,0,0)
        
        if (missing(start))
          start=c(max(Response),-1,-1,mean_C_mat)
        
        res_CA_nmk<-nmkb(par=start, 
                         fn=function(x){CA_complete2_RSS(C_mat=C_mat, 
                                                         Response=Response, 
                                                         Max=x[1], 
                                                         Slopes=c(x[2], x[3]), 
                                                         Ec50s=c(x[4], x[5]), 
                                                         interact=interact, 
                                                         multicore=multicore, 
                                                         mc.cores=mc.cores)}, 
                         upper=upper, 
                         lower=lower, 
                         control=list(tol=10^-6))
        
        return(list(Max=res_CA_nmk$par[1],
                    Slopes=c(res_CA_nmk$par[2:3]), 
                    Ec50s=c(res_CA_nmk$par[4:5]), 
                    RSS=res_CA_nmk$value))
      }
      
      if (interact=="SA"){
        
        if (missing(upper))
          upper=c(max(Response)*5, 0,0, mean_C_mat*10, 20)  # the intervals for a and b must be larger beacuse they can compensate each other
        
        if (missing(lower))
          lower=c(0,-100,-100,0,0, -20)
        
        if (missing(start))
          start=c(max(Response),-1,-1,mean_C_mat,0)
        
        res_CA_nmk<-nmkb(par=start, 
                         fn=function(x){CA_complete2_RSS(C_mat=C_mat, 
                                                         Response=Response, 
                                                         Max=x[1], 
                                                         Slopes=c(x[2], x[3]), 
                                                         Ec50s=c(x[4], x[5]), 
                                                         a=x[6], 
                                                         interact=interact, 
                                                         multicore=multicore, 
                                                         mc.cores=mc.cores)}, 
                         upper=upper, 
                         lower=lower, 
                         control=list(tol=10^-6))
        
        return(list(Max=res_CA_nmk$par[1], 
                    Slopes=c(res_CA_nmk$par[2:3]), 
                    Ec50s=c(res_CA_nmk$par[4:5]), 
                    a=res_CA_nmk$par[6], 
                    RSS=res_CA_nmk$value))
      }
    }
    
    else {
      
      if (interact=="none"){
        
        if (missing(upper))
          upper=c(max(Response)*5, 0, mean_C_mat*10)
        
        if (missing(lower))
          lower=c(0,-100,0,0)
        
        if (missing(start))
          start=c(max(Response),-1,mean_C_mat)
        
        res_CA_nmk<-nmkb(par=start, 
                         fn=function(x){CA_complete2_RSS(C_mat=C_mat, 
                                                         Response=Response, 
                                                         Max=x[1], 
                                                         Slopes=c(x[2], x[2]), 
                                                         Ec50s=c(x[3], x[4]), 
                                                         interact=interact, 
                                                         multicore=multicore, 
                                                         mc.cores=mc.cores)}, 
                         upper=upper, 
                         lower=lower, 
                         control=list(tol=10^-6))
        
        return(list(Max=res_CA_nmk$par[1], 
                    Slopes=c(res_CA_nmk$par[2], res_CA_nmk$par[2]), 
                    Ec50s=c(res_CA_nmk$par[3:4]), 
                    RSS=res_CA_nmk$value))
      }
      
      if (interact=="SA"){
        
        if (missing(upper))
          upper=c(max(Response)*5, 0, mean_C_mat*10, 20)  # the intervals for a and b must be larger beacuse they can compensate each other
        
        if (missing(lower))
          lower=c(0,-100,0,0, -20)
        
        if (missing(start))
          start=c(max(Response),-1,mean_C_mat,0)
        
        # fit
        res_CA_nmk<-nmkb(par=start, 
                         fn=function(x){CA_complete2_RSS(C_mat=C_mat, 
                                                         Response=Response, 
                                                         Max=x[1], 
                                                         Slopes=c(x[2], x[2]), 
                                                         Ec50s=c(x[3], x[4]), 
                                                         a=x[5], 
                                                         interact=interact, 
                                                         multicore=multicore, 
                                                         mc.cores=mc.cores)}, 
                         upper=upper, 
                         lower=lower, 
                         control=list(tol=10^-6))
        
        return(list(Max=res_CA_nmk$par[1], 
                    Slopes=c(res_CA_nmk$par[2], res_CA_nmk$par[2]), 
                    Ec50s=c(res_CA_nmk$par[3:4]), 
                    a=res_CA_nmk$par[5], 
                    RSS=res_CA_nmk$value))
      }
    }
  }
}

CA_complete2_RSS<-function(C_mat, Response, Max, Slopes, Ec50s, a=0, b=0, interact="none", multicore=FALSE, mc.cores=4){
  
  if (length(Response)!=(dim(C_mat)[1]))
    stop("Should be as many responses as there are conditions")
  
  res_CA<-CA_complete2(C_mat=C_mat, Max=Max, Slopes=Slopes, Ec50s=Ec50s, a=a, b=b, interact=interact, multicore=multicore,
                       mc.cores=mc.cores)
  res_CA_RSS<-sum((res_CA-Response)^2)
  
  return(res_CA_RSS)
}



CA_complete_fit_speed<-function(C_mat, Response, param=NULL, upper=NULL, lower=NULL, start=NULL, interact="none", identical_slopes=FALSE, iter=500, multicore=FALSE, mc.cores=4){
  if (!is.null(param)){
    if ((is.null(param$Max))|(is.null(param$Slopes))|(is.null(param$Ec50s)))
      stop("param misspecification")
    if (interact=="none")
      stop("please specify interaction")
    if (interact=="SA"){
      if (missing(upper))
        upper=20  # the intervals for a and b must be larger beacuse they can compensate each other
      if (missing(lower))
        lower=-20
      res_CA_optim<-optimize(f=function(x){
        CA_complete2_RSS(C_mat=C_mat, Response=Response, Max=param$Max, Slopes=param$Slopes, Ec50s=param$Ec50s, a=x, interact=interact, multicore=multicore, mc.cores=mc.cores)
      }, upper=upper, lower=lower)
      return(list(a=res_CA_optim$minimum, RSS=res_CA_optim$objective))
    }
    if (interact=="DR"){
      res_CA_optim<-optim(par=rep(0, dim(C_mat)[2]), fn=function(x){
        CA_complete2_RSS(C_mat=C_mat, Response=Response, Max=param$Max, Slopes=param$Slopes, Ec50s=param$Ec50s, a=x[1], b=x[2:length(x)], interact=interact, multicore=multicore, mc.cores=mc.cores)
      })
      cat(res_CA_optim$convergence)
      return(list(a=res_CA_optim$par[1], b=res_CA_optim$par[2: (2+(dim(C_mat)[2]-1)-1)], RSS=res_CA_optim$value))
    }
    if (interact=="DR2"){
      res_CA_optim<-optim(par=rep(0, dim(C_mat)[2]), fn=function(x){
        CA_complete2_RSS(C_mat=C_mat, Response=Response, Max=param$Max, Slopes=param$Slopes, Ec50s=param$Ec50s, a=x[1], b=x[2:length(x)], interact=interact, multicore=multicore, mc.cores=mc.cores)
      })
      cat(res_CA_optim$convergence)
      return(list(a=res_CA_optim$par[1], b=res_CA_optim$par[2: (2+(dim(C_mat)[2]-1)-1)], RSS=res_CA_optim$value))
    }
    if (interact=="DL"){
      res_CA_optim<-optim(par=c(0,0), fn=function(x){
        CA_complete2_RSS(C_mat=C_mat, Response=Response, Max=param$Max, Slopes=param$Slopes, Ec50s=param$Ec50s, a=x[1], b=x[2], interact=interact, multicore=multicore, mc.cores=mc.cores)
      })
      cat(res_CA_optim$convergence)
      return(list(a=res_CA_optim$par[1], b=res_CA_optim$par[2], RSS=res_CA_optim$value))
    }
  }
  if (is.null(param)){
    #require(DEoptim)
    require(dfoptim)
    if (any(C_mat==0))
      mean_C_mat<-exp(apply(log(C_mat[-which(C_mat==0, arr.ind=TRUE)[,1],]),2,mean))
    else
      mean_C_mat<-exp(apply(log(C_mat),2,mean))
    if (!identical_slopes){
      if (interact=="none"){
        if (missing(upper))
          upper=c(max(Response)*5, 0,0, mean_C_mat*10)
        if (missing(lower))
          lower=c(0,-100,-100,0,0)
        if (missing(start))
          start=c(max(Response),-1,-1,mean_C_mat)
        res_CA_nmk<-nmkb(par=start, fn=function(x){
          CA_complete2_RSS(C_mat=C_mat, Response=Response, Max=x[1], Slopes=c(x[2], x[3]), Ec50s=c(x[4], x[5]), interact=interact, multicore=multicore, mc.cores=mc.cores)
        }, upper=upper, lower=lower, control=list(tol=10^-6))
        return(list(Max=res_CA_nmk$par[1], Slopes=c(res_CA_nmk$par[2:3]), Ec50s=c(res_CA_nmk$par[4:5]), RSS=res_CA_nmk$value))
      }
      if (interact=="SA"){
        if (missing(upper))
          upper=c(max(Response)*5, 0,0, mean_C_mat*10, 20)  # the intervals for a and b must be larger beacuse they can compensate each other
        if (missing(lower))
          lower=c(0,-100,-100,0,0, -20)
        if (missing(start))
          start=c(max(Response),-1,-1,mean_C_mat,0)
        res_CA_nmk<-nmkb(par=start, fn=function(x){
          CA_complete2_RSS(C_mat=C_mat, Response=Response, Max=x[1], Slopes=c(x[2], x[3]), Ec50s=c(x[4], x[5]), a=x[6], interact=interact, multicore=multicore, mc.cores=mc.cores)
        }, upper=upper, lower=lower, control=list(tol=10^-6))
        return(list(Max=res_CA_nmk$par[1], Slopes=c(res_CA_nmk$par[2:3]), Ec50s=c(res_CA_nmk$par[4:5]), a=res_CA_nmk$par[6], RSS=res_CA_nmk$value))
      }
      if (interact=="DR"){
        if (missing(upper))
          upper=c(max(Response)*5, 0,0, mean_C_mat*10, 20, rep(30, (dim(C_mat)[2]-1)))  # the intervals for a and b must be larger beacuse they can compensate each other
        if (missing(lower))
          lower=c(0,-100,-100,0,0, -20, rep(-30, (dim(C_mat)[2]-1)))
        if (missing(start))
          start=c(max(Response),-1,-1,mean_C_mat,0,rep(0, (dim(C_mat)[2]-1)))
        res_CA_nmk<-nmkb(par=start, fn=function(x){
          CA_complete2_RSS(C_mat=C_mat, Response=Response, Max=x[1], Slopes=c(x[2], x[3]), Ec50s=c(x[4], x[5]), a=x[6], b=x[7:length(x)], interact=interact, multicore=multicore, mc.cores=mc.cores)
        }, upper=upper, lower=lower, control=list(tol=10^-6))
        return(list(Max=res_CA_nmk$par[1], Slopes=c(res_CA_nmk$par[2:3]), Ec50s=c(res_CA_nmk$par[4:5]), a=res_CA_nmk$par[6], b=res_CA_nmk$par[7: (7+(dim(C_mat)[2]-1)-1)], RSS=res_CA_nmk$value))
      }
      if (interact=="DL"){
        if (missing(upper))
          upper=c(max(Response)*5, 0,0, mean_C_mat*10, 20,30)
        if (missing(lower))
          lower=c(0,-100,-100,0,0, -20, -30)
        if (missing(start))
          start=c(max(Response),-1,-1,mean_C_mat,0,0)
        res_CA_nmk<-nmkb(par=start, fn=function(x){
          CA_complete2_RSS(C_mat=C_mat, Response=Response, Max=x[1], Slopes=c(x[2], x[3]), Ec50s=c(x[4], x[5]), a=x[6], b=x[7], interact=interact, multicore=multicore, mc.cores=mc.cores)
        }, upper=upper, lower=lower, control=list(tol=10^-6))
        return(list(Max=res_CA_nmk$par[1], Slopes=c(res_CA_nmk$par[2:3]), Ec50s=c(res_CA_nmk$par[4:5]), a=res_CA_nmk$par[6], b=res_CA_nmk$par[7], RSS=res_CA_nmk$value))
      }
    }
    else {
      if (interact=="none"){
        if (missing(upper))
          upper=c(max(Response)*5, 0, mean_C_mat*10)
        if (missing(lower))
          lower=c(0,-100,0,0)
        if (missing(start))
          start=c(max(Response),-1,mean_C_mat)
        res_CA_nmk<-nmkb(par=start, fn=function(x){
          CA_complete2_RSS(C_mat=C_mat, Response=Response, Max=x[1], Slopes=c(x[2], x[2]), Ec50s=c(x[3], x[4]), interact=interact, multicore=multicore, mc.cores=mc.cores)
        }, upper=upper, lower=lower, control=list(tol=10^-6))
        return(list(Max=res_CA_nmk$par[1], Slopes=c(res_CA_nmk$par[2], res_CA_nmk$par[2]), Ec50s=c(res_CA_nmk$par[3:4]), RSS=res_CA_nmk$value))
      }
      if (interact=="SA"){
        if (missing(upper))
          upper=c(max(Response)*5, 0, mean_C_mat*10, 20)  # the intervals for a and b must be larger beacuse they can compensate each other
        if (missing(lower))
          lower=c(0,-100,0,0, -20)
        if (missing(start))
          start=c(max(Response),-1,mean_C_mat,0)
        res_CA_nmk<-nmkb(par=start, fn=function(x){
          CA_complete2_RSS(C_mat=C_mat, Response=Response, Max=x[1], Slopes=c(x[2], x[2]), Ec50s=c(x[3], x[4]), a=x[5], interact=interact, multicore=multicore, mc.cores=mc.cores)
        }, upper=upper, lower=lower, control=list(tol=10^-6))
        return(list(Max=res_CA_nmk$par[1], Slopes=c(res_CA_nmk$par[2], res_CA_nmk$par[2]), Ec50s=c(res_CA_nmk$par[3:4]), a=res_CA_nmk$par[5], RSS=res_CA_nmk$value))
      }
      if (interact=="DR"){
        if (missing(upper))
          upper=c(max(Response)*5, 0,mean_C_mat*10, 20, rep(30, (dim(C_mat)[2]-1)))  # the intervals for a and b must be larger beacuse they can compensate each other
        if (missing(lower))
          lower=c(0,-100,0,0, -20, rep(-30, (dim(C_mat)[2]-1)))
        if (missing(start))
          start=c(max(Response),-1,mean_C_mat,0,0)
        res_CA_nmk<-nmkb(par=start, fn=function(x){
          CA_complete2_RSS(C_mat=C_mat, Response=Response, Max=x[1], Slopes=c(x[2], x[2]), Ec50s=c(x[3:4]), a=x[5], b=x[6:length(x)], interact=interact, multicore=multicore, mc.cores=mc.cores)
        }, upper=upper, lower=lower, control=list(tol=10^-6))
        return(list(Max=res_CA_nmk$par[1], Slopes=c(res_CA_nmk$par[2],res_CA_nmk$par[2]), Ec50s=c(res_CA_nmk$par[3:4]), a=res_CA_nmk$par[5], b=res_CA_nmk$par[6: (6+(dim(C_mat)[2]-1)-1)], RSS=res_CA_nmk$value))
      }
      if (interact=="DL"){
        if (missing(upper))
          upper=c(max(Response)*5, 0,mean_C_mat*10, 20,30)
        if (missing(lower))
          lower=c(0,-100,0,0, -20, -30)
        if (missing(start))
          start=c(max(Response),-1,mean_C_mat,0,0)
        res_CA_nmk<-nmkb(par=start, fn=function(x){
          CA_complete2_RSS(C_mat=C_mat, Response=Response, Max=x[1], Slopes=c(x[2], x[2]), Ec50s=c(x[3:4]), a=x[5], b=x[6], interact=interact, multicore=multicore, mc.cores=mc.cores)
        }, upper=upper, lower=lower, control=list(tol=10^-6))
        return(list(Max=res_CA_nmk$par[1], Slopes=c(res_CA_nmk$par[2], res_CA_nmk$par[2]), Ec50s=c(res_CA_nmk$par[3:4]), a=res_CA_nmk$par[5], b=res_CA_nmk$par[6], RSS=res_CA_nmk$value))
      }
    }
  }
}



















