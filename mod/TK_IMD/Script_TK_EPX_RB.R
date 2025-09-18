
# 0. Pour les graphiques ----

Nord_frost <- nord(palette = "frost")
Nord_aurora <- nord(palette = "aurora")
Nord_polar <- nord(palette = "polarnight")
Nord_snow <- nord(palette = "snowstorm")

pal_col <- c(Nord_aurora[1], Nord_frost[4])
col_EPX <- Nord_frost[2]
col_IMD <- Nord_frost[4]
col_elim <- Nord_aurora[4]
col_uptake <- Nord_aurora[1]
shape_IMD <- 16
shape_EPX <- 15
col_Molecule <- c(col_EPX, col_IMD)
col_molec <- c(col_EPX, col_IMD)
sizetitle <- 12

shape_Molecule <- c(shape_EPX, shape_IMD)
shape_molec <- c(shape_EPX, shape_IMD)

# 1. Les fonctions ----

# Lire les données 
f_read_data_TK <- function(Molecule) {
  df_TK <- read_excel(here::here("data/Data_TK_long.xlsx")) |> #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    mutate(
      Time_point_f = as.factor(Time_point),
      ID = as.factor(ID),
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
      dplyr::select(Molecule, Dose, ID, expo, Phase, w, C_worm_EPX, C_worm_IMD, t, Time_point, ID_recipient) |>
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
    
    C_worm_t0_IMD <- mean(subset(df_TK, Molecule == "EPX")$C_worm_IMD, na.rm = T)
    C_worm_t0_EPX <- mean(subset(df_TK, Molecule == "IMD")$C_worm_EPX, na.rm = T)
    
    df_TK_f[df_TK_f$t == 0, ]$C_worm_IMD <- C_worm_t0_IMD
    df_TK_f[df_TK_f$t == 0, ]$C_worm_EPX <- C_worm_t0_EPX
  } else if (Molecule == "IMD") {
    df_TK_f <- df_TK |>
      dplyr::select(Molecule, Dose, ID, expo, Phase, w, C_worm_EPX, C_worm_IMD, t, Time_point, ID_recipient) |>
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
        Weight_wg = if_else(Phase == "Frozen", -1, w),      # With gut content
        Weight_nog = if_else(!(Phase == "Frozen"), rW*w, w) # Without gut content
      ) |>
      mutate(across(where(is.numeric), ~ replace_na(.x, -1))) |>
      arrange(ID)
    
    C_worm_t0_IMD <- mean(subset(df_TK, Molecule == "EPX")$C_worm_IMD, na.rm = T)
    C_worm_t0_EPX <- mean(subset(df_TK, Molecule == "IMD")$C_worm_EPX, na.rm = T)
    
    df_TK_f[df_TK_f$t == 0, ]$C_worm_IMD <- C_worm_t0_IMD
    df_TK_f[df_TK_f$t == 0, ]$C_worm_EPX <- C_worm_t0_EPX
  } else {
    stop("Uncorrect specification of Molecule")
  }
  
  return(df_TK_f)
}


# Ecrire les expériences pour les .in
f_In_experiments <- function(Molecule) {
  
  df_TK_f <- f_read_data_TK(Molecule)
  
  C_clx_IMD <- 16 / 1000 # ng/g
  C_clx_EPX <- 90 / 1000 # ng/g
  
  C_worm_t0_IMD <-  df_TK_f[df_TK_f$t == 0, ]$C_worm_IMD[1]
  C_worm_t0_EPX <-  df_TK_f[df_TK_f$t == 0, ]$C_worm_EPX[1]
  
  
  
  if (Molecule == "IMD") {
    char_final <- ""
    for (i in unique(df_TK_f$ID)) {
      
      df_TK_i <- subset(df_TK_f, ID == i) 
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww_nopopo <- paste(df_TK_i$Weight_nog, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiIMD <- paste(df_TK_i$C_worm_IMD, collapse = ",")
      
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      length_expo <- length(df_TK_i$expo)
      char_texpo <- char_tw
      
      
      char_i <- paste(
        paste("Experiment { #", "TK IMD - i = ", i),
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight_nog, ";", sep = ""),
        paste("    Ci0IMD=", C_worm_t0_IMD, ";", sep = ""),
        paste("    Ce0IMD=", subset(df_TK_i, t == 0)$C_soil_IMD, ";", sep = ""),
        paste("    CclxIMD=", C_clx_IMD, ";", sep = ""),
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
    
    for (i in unique(df_TK_f$ID)) {
      
      df_TK_i <- subset(df_TK_f, ID == i) 
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww_nopopo <- paste(df_TK_i$Weight_nog, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiEPX <- paste(df_TK_i$C_worm_EPX, collapse = ",")
      
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      length_expo <- length(df_TK_i$expo)
      char_texpo <- char_tw
      
      char_i <- paste(
        paste("Experiment { #", "TK EPX - i = ", i),
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight_nog, ";", sep = ""),
        paste("    Ci0EPX=", C_worm_t0_EPX, ";", sep = ""),
        paste("    Ce0EPX=", subset(df_TK_i, t == 0)$C_soil_EPX, ";", sep = ""),
        paste("    CclxEPX=", C_clx_EPX, ";", sep = ""),
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

# Ecrire le header des CX.in 
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

# Ecriture des CX.in
f_In_tot <- function(Molecule, text_priors, text_likelihood, text_param_ind = "") {
  seeds <- c(C1 = 3333, C2 = 6666, C3 = 1212)
  
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
    )[1]
    
    File_path <- paste0("mod/TK_", Molecule) #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    file.remove(here::here(File_path, paste0("TK_", Molecule, "_", id, ".in")))
    
    writeLines(
      text_full,
      file(here::here(File_path, paste0("TK_", Molecule, "_", id, ".in")))
    )
  }
}

# Ecriture du setpoint pour simuler l'individu moyen 
f_Setpoint <- function(Molecule, PrintStep) {
  
  Winit <- mean(subset(df_TK_f, t == 0)$Weight_nog)
  
  if (Molecule == "IMD") {
    Ci0 <- 3.92587673494744
    Ce0 <- 80.5
    Cclx <- 16 / 1000 # ng/g
    
    text_start <- '#### Toxicokinetics of Imidacloprid in A. caliginosa
#===============================================

Integrate(Lsodes, 1E-6, 1E-8, 1);

SetPoints("Setpoints.out", "tab_setpoint.out", 0, kuIMD , keIMD, a_growth, Vr_a_growth, Sigma_W, Sigma_CiIMD);
'
    text_simulation <- paste0("
	Simulation {

  # Events
    Winit=", Winit, ";
    Ci0IMD=", Ci0IMD, ";
    Ce0IMD=", Ce0IMD, ";
    CclxIMD=", CclxIMD, ";
    expo=NDoses(2, 1, 0, 0, 21);")
  } else if (Molecule == "EPX") {
    Ci0 <- 0.747549
    Ce0 <- 1000
    Cclx <- 90 / 1000 # ng/g
    
    text_start <- '#### Toxicokinetics of Epoxiconazole in A. caliginosa
#===============================================

Integrate(Lsodes, 1E-6, 1E-8, 1);

SetPoints("Setpoints.out", "tab_setpoint.out", 0, kuEPX , keEPX, a_growth, Vr_a_growth, Sigma_W, Sigma_CiEPX);
'
    text_simulation <- paste0("
	Simulation {

  # Events
    Winit=", Winit, ";
    Ci0EPX=", Ci0, ";
    Ce0EPX=", Ce0, ";
    CclxEPX=", Cclx, ";
    expo=NDoses(2, 1, 0, 0, 21);")
  }
  
  text_print <- paste0("# Data

		PrintStep (Weight,
					0, 42, ", PrintStep, ");
		PrintStep (CiIMD,
					0, 42, ", PrintStep, ");
	}")
  
  text_end <- "End."
  
  text_full <- paste(
    text_start,
    text_simulation,
    text_print,
    text_end,
    sep = "\n"
  )
  
  File_path <- paste0("mod/TK_", Molecule) #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  writeLines(
    text_full,
    here::here(File_path, paste0("TK_", Molecule, "_Setpoint.in"))
  )
}

# Ecriture du setpoint pour faire le graph pred/obs 
f_Setpoint_ind <- function(Molecule) {
  
  text_end <- "End."
  
  File_path <- paste0("mod/TK_", Molecule) #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  df_TK_f <- f_read_data_TK(Molecule)
  
  C_worm_t0_IMD <-  df_TK_f[df_TK_f$t == 0, ]$C_worm_IMD[1]
  C_worm_t0_EPX <-  df_TK_f[df_TK_f$t == 0, ]$C_worm_EPX[1]
  C_clx_IMD <- 16 / 1000 # ng/g
  C_clx_EPX <- 90 / 1000 # ng/g
  
  compteur_exp <- 0
  
  for (i in unique(df_TK_f$ID)){
    
    compteur_exp <- compteur_exp + 1
    
    if (Molecule == "IMD"){
      df_TK_i <- subset(df_TK_f, ID == i)
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww <- paste(df_TK_i$Weight_nog, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiIMD <- paste(df_TK_i$C_worm_IMD, collapse = ",")
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      
      char_ta <- paste(df_TK_i$t[1:2], collapse = ",")
      
      char_i <- paste(
        paste("Simulation { #", "TK IMD - i = ", i),
        
        # paste("    a_growth=NDoses(2, a_growth_ind_", compteur_exp," , a_fasting_ind_", compteur_exp," , ", char_ta,");", sep=""),
        
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight_nog, ";", sep = ""),
        paste("    Ci0IMD=", C_worm_t0_IMD, ";", sep = ""),
        paste("    Ce0IMD=", subset(df_TK_i, t == 0)$C_soil_IMD, ";", sep = ""),
        paste("    CclxIMD=", C_clx_IMD, ";", sep = ""),
        paste("    expo=NDoses(", length(df_TK_i$expo), ",", char_expo, ",", char_tw, ");", sep = ""),
        paste("    Print(Weight,", char_tw, ");", sep = ""),
        paste("    Print(CiIMD,", char_tw, ");", sep = ""),
        paste("}"),
        sep = "\n"
      )
      
      text_start <- paste0('#### Toxicokinetics of Imidacloprid in A. caliginosa
#===============================================


Integrate(Lsodes, 1E-6, 1E-8, 1);

SetPoints("Setpoints_ind_',compteur_exp,'.out", "tab_setpoint_ind_',compteur_exp,'.out", 0, kuIMD , keIMD, a_growth);

########## Individuals ################################################')
      
      text_full <- paste(
        text_start,
        char_i,
        text_end,
        sep = "\n"
      )
      
      writeLines(
        text_full,
        here::here(File_path, paste0("TK_", Molecule, "_Setpoint_ind", compteur_exp, ".in"))
      )
      
      
    } else if (Molecule == "EPX"){
      df_TK_i <- subset(df_TK_f, ID == i)
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww <- paste(df_TK_i$Weight_nog, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiEPX <- paste(df_TK_i$C_worm_EPX, collapse = ",")
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      
      char_i <- paste(
        paste("Simulation { #", "TK EPX - i = ", i),
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight_nog, ";", sep = ""),
        paste("    Ci0EPX=", C_worm_t0_EPX, ";", sep = ""),
        paste("    Ce0EPX=", subset(df_TK_i, t == 0)$C_soil_EPX, ";", sep = ""),
        paste("    CclxEPX=", C_clx_EPX, ";", sep = ""),
        paste("    expo=NDoses(", length(df_TK_i$expo), ",", char_expo, ",", char_tw, ");", sep = ""),
        paste("    Print(Weight,", char_tw, ");", sep = ""),
        paste("    Print(CiEPX,", char_tw, ");", sep = ""),
        paste("}"),
        sep = "\n"
      )
      
      text_start <- paste0('#### Toxicokinetics of Epoxiconazole in A. caliginosa
#===============================================


Integrate(Lsodes, 1E-6, 1E-8, 1);

SetPoints("Setpoints_ind_',compteur_exp,'.out", "tab_setpoint_ind_',compteur_exp,'.out", 0, kuEPX , keEPX, a_growth);

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

# Ecrire le setpoint pour faire les trajectoires de tous les individus
f_Setpoint_ind_full <- function(Molecule, print_times, nb_split) {
  
  text_end <- "End."
  
  File_path <- paste0("mod/TK_", Molecule) #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  df_TK_f <- f_read_data_TK(Molecule)
  
  C_worm_t0_IMD <-  df_TK_f[df_TK_f$t == 0, ]$C_worm_IMD[1]
  C_worm_t0_EPX <-  df_TK_f[df_TK_f$t == 0, ]$C_worm_EPX[1]
  C_clx_IMD <- 16 / 1000 # ng/g
  C_clx_EPX <- 90 / 1000 # ng/g
  
  compteur_exp <- 0
  
  for (i in unique(df_TK_f$ID)){
    
    compteur_exp <- compteur_exp + 1
    
    if (Molecule == "IMD"){
      df_TK_i <- subset(df_TK_f, ID == i)
      
      char_tw <- paste(df_TK_i$t, collapse = ",")
      char_Ww <- paste(df_TK_i$Weight_nog, collapse = ",")
      
      char_tCi <- paste(df_TK_i$t, collapse = ",")
      char_CiIMD <- paste(df_TK_i$C_worm_IMD, collapse = ",")
      char_expo <- paste(df_TK_i$expo, collapse = ",")
      
      char_ta <- paste(df_TK_i$t[1:2], collapse = ",")
      
      char_i <- paste(
        paste("Simulation { #", "TK IMD - i = ", i),
        
        # paste("    a_growth=NDoses(2, a_growth_ind_", compteur_exp," , a_fasting_ind_", compteur_exp," , ", char_ta,");", sep=""),
        
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight_nog, ";", sep = ""),
        paste("    Ci0IMD=", C_worm_t0_IMD, ";", sep = ""),
        paste("    Ce0IMD=", subset(df_TK_i, t == 0)$C_soil_IMD, ";", sep = ""),
        paste("    CclxIMD=", C_clx_IMD, ";", sep = ""),
        paste("    expo=NDoses(", length(df_TK_i$expo), ",", char_expo, ",", char_tw, ");", sep = ""),
        paste("    PrintStep(Weight,", print_times, ");", sep = ""),
        paste("    PrintStep(CiIMD,", print_times, ");", sep = ""),
        paste("}"),
        sep = "\n"
      )
      
      text_start <- paste0('#### Toxicokinetics of Imidacloprid in A. caliginosa
#===============================================


Integrate(Lsodes, 1E-6, 1E-8, 1);

SetPoints("Setpoints_ind_full_',compteur_exp,'.out", "tab_setpoint_ind_',compteur_exp,'.out", 0, kuIMD , keIMD, a_growth);

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
      
      char_i <- paste(
        paste("Simulation { #", "TK EPX - i = ", i),
        paste("    Winit=", subset(df_TK_i, t == 0)$Weight_nog, ";", sep = ""),
        paste("    Ci0EPX=", C_worm_t0_EPX, ";", sep = ""),
        paste("    Ce0EPX=", subset(df_TK_i, t == 0)$C_soil_EPX, ";", sep = ""),
        paste("    CclxEPX=", C_clx_EPX, ";", sep = ""),
        paste("    expo=NDoses(", length(df_TK_i$expo), ",", char_expo, ",", char_tw, ");", sep = ""),
        paste("    PrintStep(Weight,", print_times, ");", sep = ""),
        paste("    PrintStep(CiIMD,", print_times, ");", sep = ""),
        paste("}"),
        sep = "\n"
      )
      
      text_start <- paste0('#### Toxicokinetics of Epoxiconazole in A. caliginosa
#===============================================


Integrate(Lsodes, 1E-6, 1E-8, 1);

SetPoints("Setpoints_ind_full_',compteur_exp,'.out", "tab_setpoint_ind_',compteur_exp,'.out", 0, kuEPX , keEPX, a_growth);

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

# Traitement des résultats et écriture des tab_setpoint (équivalent à Diagnostic MCMC)
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
  nexperiement <- length(Experiment)
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
  
  MaxVraiss <- Data_All[Mode[1], ncol(Data_All)]
  
  AIC <- -2 * MaxVraiss + 2 * np
  Ndata <- 13 # number of observed data used
  BIC <- -2 * MaxVraiss + log(Ndata) * np
  
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
  
  for (i in 1:nexperiement) {
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
  tab_setpoint <- tab_setpoint |>
    mutate(across(where(is.list), ~ simplify2array(.) |> unlist()))
  
  write.table(tab_setpoint, file = paste(path_mod, "tab_setpoint.out", sep = "/"), quote = FALSE)
  
  
  MCMC_out <- list(
    NC              = NC,
    Files_in        = Files_in,
    Files_out       = Files_out,
    Nb_param        = np,
    Nb_experiement  = nexperiement,
    Priors          = ParTable,
    Nb_iter_kept    = nb,         # Last iterations used to describe the posterior distributions
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


# 2. Faire les fichiers .in ----

Molecule <- "EPX"
text_priors <- "Distrib(kuEPX,     LogUniform,   0.001, 10000); # min, max 
Distrib(keEPX,    LogUniform,   0.001, 10000); # min, max

Distrib(a_growth,  TruncNormal,   0.005,   0.005,    -0.5,    1); # mean, sd, min, max
Distrib (Vr_a_growth,  TruncNormal, 0.0015, 0.0015, 0, 1);

Distrib(Sigma_W,     TruncNormal,  0.03, 0.01, 0.0001, 1);
Distrib(Sigma_CiEPX, TruncNormal,  3.0, 3.0,    1, 1000);"

text_likelihood <- "Likelihood(CiEPX,  LogNormal, Prediction(CiEPX),  Sigma_CiEPX);  # Calcul des vraisemblances (geometric mean, geometric standard deviation)
Likelihood(Weight, Normal,    Prediction(Weight), Sigma_W);      # Calcul des vraisemblances"

text_param_ind <- "Distrib(a_growth,  TruncNormal, a_growth,  Vr_a_growth, -1.0, 1);"

f_In_tot(Molecule, text_priors, text_likelihood, text_param_ind)

# 3. Obtenir les sorties du modèles ----

path_mod_EPX <- here::here("mod/TK_EPX") # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MCMC_out_EPX <- f_MCSim_ind(path_mod_EPX)

NC <- MCMC_out_EPX$NC
Nb_param <- MCMC_out_EPX$Nb_param
Nb_experiement <- MCMC_out_EPX$Nb_experiement
Nb_iter_kept <- MCMC_out_EPX$Nb_iter_kept
N_iter_setpoint <- MCMC_out_EPX$N_iter_setpoint

L_parameters <- unique(MCMC_out_EPX$Priors$Nom)

# 4. Ecriture des différents Setpoints ----

f_Setpoint(Molecule)

f_Setpoint_ind(Molecule)

print_times <- "0, 44, 0.25"
nb_split <- 4
f_Setpoint_ind_full(Molecule, print_times, nb_split)

# 5. Priors ----

MCMC_out_EPX$Priors |> datatable(
  options = list(
    dom = 't',
    autoWidth = TRUE,
    columnDefs = list(
      list(className = 'dt-left', targets = "_all")
    )
  ), 
  class="hover"
) 

n_samples <- 1e6 # Simulation of the distribution

df_priors_dist <- MCMC_out_EPX$Priors |> 
  dplyr::select(Nom, Distribution, P1, P2, P3, P4) |> 
  rowwise() |> 
  mutate(samples = case_when(
    Distribution == "Uniform" ~ list(runif(n_samples, min = P1, max = P2)),
    Distribution == "Normal" ~ list(rnorm(n_samples, mean = P1, sd = P2)),
    Distribution == "LogUniform" ~ list(exp(runif(n_samples, min = log(P1), max = log(P2)))),
    Distribution == "TruncNormal" ~ list(rtruncnorm(n_samples, a = P3, b = P4, mean = P1, sd = P2))
  )) |> 
  unnest(samples) |> 
  filter(!(Nom %in% c("kuEPX", "keEPX") & samples > 0.25))

col_prior <- Nord_polar[4]
alpha_prior <- 0.5

p <- ggplot() +
  facet_wrap(~Nom, scales = "free")+
  
  stat_halfeye(
    data = df_priors_dist, 
    aes(
      x = samples
    ),
    alpha = alpha_prior,
    fill = col_prior,
    color = col_prior,
    normalize = "xy" 
  ) +
  
  labs(
    title = "Priors distributions", 
    x = "Parameter value", 
    y = "Density"
  ) +
  theme_minimal()

p

# 6. Chains ----

plot <-  
  ggplot(
    data = MCMC_out_EPX$Chains, 
    aes(
      x = Iteration, 
      y = value, 
      color = as.factor(Chain), 
      group = as.factor(Chain)
    )
  )+
  geom_line(alpha=0.7)+
  scale_y_log10()+
  facet_wrap(~Parameter, scales = "free", ncol = 2)+
  
  scale_color_manual(name = "Chains", values = nord(palette = "frost"))+
  
  theme_bw()+
  theme(
    legend.position = "right", 
    title=element_text(size=12, face="plain"), 
    axis.title.x = element_text(face="plain"),
    strip.background = element_rect(fill="white")
  )

plot

p1 <- ggplot(
  MCMC_out_EPX$df_LnPost_start, 
  aes(
    x = iteration, 
    y = LnPosterior, 
    color = Chain
  )
) +
  geom_line() +
  scale_color_manual(name = "Chains", label = c("1", "2", "3"), values = Nord_frost) +
  labs(
    x = "First half iterations", 
    y = "LnPosterior"
  ) +
  theme_minimal()

p2 <- ggplot(
  MCMC_out_EPX$df_LnPost, 
  aes(
    x = iteration, 
    y = LnPosterior, 
    color = Chain
  )
) +
  geom_line() +
  scale_color_manual(name = "Chains", label = c("1", "2", "3"), values = Nord_frost) +
  labs(
    x = paste(MCMC_out_EPX$Nb_iter_kept, "last iterations"), 
    y = "LnPosterior"
  ) +
  theme_minimal()

p3 <- ggplot(
  MCMC_out_EPX$df_Devc, 
  aes(
    x = iteration, 
    y = RatioDeviance, 
    color = Chain
  )
) +
  geom_line() +
  scale_color_manual(name = "Chains", label = c("1", "2", "3"), values = Nord_frost) +
  labs(
    x = paste(MCMC_out_EPX$Nb_iter_kept, "last iterations"), 
    y = "Ratio Deviance"
  ) +
  theme_minimal()

p <- p1 + p2 + p3 +
  plot_layout(ncol=3, guides = "collect")
p

# 7. Posteriors ----

alpha_prior <- 0.5
alpha_post <- 0.5
col_prior <- Nord_snow[1]
col_post <- col_EPX


MCMC_out_EPX$Chains <- MCMC_out_EPX$Chains |> 
  mutate(Nom = str_remove(Parameter, "\\.\\d+\\.$"))

p <- ggplot() +
  
  # Priors
  # stat_halfeye(
  #   data = df_priors_dist, 
  #   aes(
  #      x = samples,
  #      #y = ..scaled..
  #      ),
  #   alpha = alpha_prior,
  #   fill = col_prior,
  #   color = col_prior,
  #   normalize = "xy" 
  #   ) +
  
  # Posterior
  stat_halfeye(
    data = subset(MCMC_out_EPX$Chains, Nom %in% L_parameters),
    mapping = aes(
      x=value
    ),
    fill = col_post,
    color = col_post,
    alpha = alpha_post,
    normalize = "xy"
  ) +
  
  facet_wrap(~Nom, scales = "free")+
  labs(
    title = "<span style = 'color:#88C0D0;'>Posterior</span>
       and <span style = 'color:#D8DEE9;'>prior</span> distributions", 
    x = "Parameter value", 
    y = "Density"
  ) +
  theme_minimal()+
  theme(
    title=element_markdown(face="bold")
  )

p

# 8. Tableau résultats ----

f_get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

ku_samples <- MCMC_out_EPX$Chains |> filter(Nom == "kuEPX")
ke_samples <- MCMC_out_EPX$Chains |> filter(Nom == "keEPX")

BCF_samples <- ku_samples$value / ke_samples$value

BCF_mode <- f_get_mode(BCF_samples)
BCF_CI <- quantile(BCF_samples, probs = c(0.025, 0.975))
BCF_CI_low <- as.numeric(BCF_CI[1])
BCF_CI_up <- as.numeric(BCF_CI[2])
BCF_min <- min(BCF_samples)
BCF_max <- max(BCF_samples)
BCF_mean <- mean(BCF_samples)
BCF_sd <- sd(BCF_samples)

BCF <- c(
  BCF_mode, 
  BCF_CI_low, BCF_CI_up, 
  BCF_min, BCF_max, 
  BCF_mean, BCF_sd
)

Summary_res_EPX_tmp <- cbind(as.data.frame(MCMC_out_EPX$Summary_res)[1:length(L_parameters)], BCF)

Summary_res_EPX_f <- Summary_res_EPX_tmp |> 
  mutate(across(where(is.numeric), signif, 2))

Summary_res_EPX_f |> datatable(
  options = list(
    dom = 't',
    autoWidth = TRUE,
    columnDefs = list(
      list(className = 'dt-left', targets = "_all")
    )
  ), 
  class="hover"
) 

# 9. Graph pred/obs ----

dObs_ind <- df_TK_EPX |> 
  mutate(ID = as.numeric(ID))
colnames(dObs_ind)[colnames(dObs_ind) == "Weight"] <- "Weight_obs"
colnames(dObs_ind)[colnames(dObs_ind) == "CiEPX"] <- "CiEPX_obs"
colnames(dObs_ind)[colnames(dObs_ind) == "t"] <- "Times"

Sim.Res.ind <- lapply(1:62, function(j) {
  read_tsv(file.path(path_mod_EPX, paste0("Setpoints_ind_", j, ".out"))) %>%
    mutate(sim = j)
}) %>%
  bind_rows()

df_mod_ind <- NULL

for (i in 1:Nb_experiement){
  
  ID_i <- unique(dObs_ind$ID)[i]
  
  Sim.Res.ind_i  <- subset(Sim.Res.ind, sim == i) |> 
    dplyr::select(where(~ !all(is.na(.x))))
  
  dObs_i <- subset(dObs_ind, ID == ID_i)
  
  Time_pred = dObs_i$Time # !!!
  
  Nom_Endpoints = c("Weight", "CiEPX")
  
  Nsortie = 2
  
  MPV = IC_min = IC_max = Endpoint = index = NULL
  
  for (j in 1:length(Nom_Endpoints)){
    
    index =grep(paste0("^",Nom_Endpoints[j],"_") , colnames(Sim.Res.ind_i), fixed=FALSE)
    
    
    Data_sim = as.matrix( Sim.Res.ind_i[ , index] )
    
    MPV    = c(  MPV,  as.numeric( Data_sim[nrow(Data_sim), ] ) )
    IC_min = c(IC_min,  apply(Data_sim, 2, quantile,  0.025, na.rm=TRUE) )
    IC_max = c(IC_max,  apply(Data_sim, 2, quantile,  0.975, na.rm=TRUE) ) 
    
    Endpoint = c(Endpoint, rep(Nom_Endpoints[j], ncol(Data_sim)))
  }
  
  df_mod_ind_i <- data.frame(
    predict.endpoint = MPV,
    Time    = rep(Time_pred, Nsortie),
    low     = IC_min,
    up      = IC_max,
    Endpt   = Endpoint
  )
  
  df_mod_ind_i <- df_mod_ind_i %>%
    mutate(ID=ID_i)
  
  df_mod_ind <- rbind(df_mod_ind, df_mod_ind_i)
}

df_mod_Weight_ind <- subset(df_mod_ind, Endpt == "Weight")
df_mod_CiEPX_ind <- subset(df_mod_ind, Endpt == "CiEPX")

df_predobs_CiEPX_ind <- dObs_ind |> 
  left_join(df_mod_CiEPX_ind, suffix = c("_obs", "_pred")) |> 
  dplyr::select(-c(Weight_obs, CiIMD)) |> 
  mutate(across(everything(), ~replace(.x, .x == -1, NA))) |> 
  drop_na()

df_predobs_Weight_ind <- dObs_ind |> 
  #filter(!(expo == 2)) |> 
  left_join(df_mod_Weight_ind, suffix = c("_obs", "_pred"))|>
  dplyr::select(-c(CiEPX_obs, CiIMD)) |> 
  mutate(across(everything(), ~replace(.x, .x == -1, NA))) |> 
  drop_na()

fact_1 <- 2
fact_2 <- 5

col_line0 <- Nord_polar[1]
col_line1 <- Nord_polar[4]
col_line2 <- Nord_snow[1]

seq_line <- seq(0.5, 1.5, 0.001)

df_line <- data.frame(
  x1  = seq_line,
  y1  = seq_line*fact_1,
  x_1 = seq_line,
  y_1 = seq_line/fact_1,
  x2  = seq_line,
  y2  = seq_line*fact_2,
  x_2 = seq_line,
  y_2 = seq_line/fact_2
)

p_W <- ggplot(
  data = df_predobs_Weight_ind,
  mapping = aes(
    x = Weight_obs,
    y = predict.endpoint
  )
)+
  geom_line(
    data=df_line,
    mapping=aes(x=x1, y=y1),
    color = col_line1
  )+
  geom_line(
    data=df_line,
    mapping=aes(x=x_1, y=y_1),
    color = col_line1
  )+
  geom_line(
    data=df_line,
    mapping=aes(x=x2, y=y2),
    color = col_line2
  )+
  geom_line(
    data=df_line,
    mapping=aes(x=x_2, y=y_2),
    color = col_line2
  )+
  
  geom_abline(
    slope=1, 
    intercept = 0, 
    color = col_line0,
    linewidth = 1
  )+
  geom_pointrange(
    mapping = aes(
      ymin = low,
      ymax = up
    ),
    alpha = 0.5,
    color = col_EPX
  )+
  xlim(
    xmin = 0.5,
    xmax = max(df_predobs_Weight_ind$Weight_obs)
  )+
  ylim(
    ymin = 0.5,
    ymax = max(df_predobs_Weight_ind$predict.endpoint)
  )+
  scale_x_log10(limits=c(0.5, 1.2))+
  scale_y_log10(limits=c(0.5, 1.2))+
  labs(
    x = "Observed weight of earthworms (g)",
    y = "Predicted weight of earthworms (g)"
  )+
  theme_minimal()

fact_1 <- 2
fact_2 <- 5

seq_line <- seq(0.5, 1500, 0.01)

df_line <- data.frame(
  x1  = seq_line,
  y1  = seq_line*fact_1,
  x_1 = seq_line,
  y_1 = seq_line/fact_1,
  x2  = seq_line,
  y2  = seq_line*fact_2,
  x_2 = seq_line,
  y_2 = seq_line/fact_2
)


p_CiEPX_predobs <- ggplot(
  data = df_predobs_CiEPX_ind,
  mapping = aes(
    x = CiEPX_obs,
    y = predict.endpoint
  )
)+
  geom_path(
    data=df_line,
    mapping=aes(x=x1, y=y1),
    color = col_line1
  )+
  geom_line(
    data=df_line,
    mapping=aes(x=x_1, y=y_1),
    color = col_line1
  )+
  geom_line(
    data=df_line,
    mapping=aes(x=x2, y=y2),
    color = col_line2
  )+
  geom_line(
    data=df_line,
    mapping=aes(x=x_2, y=y_2),
    color = col_line2
  )+
  geom_abline(
    slope=1, 
    intercept = 0, 
    color = col_line0,
    linewidth = 1
  )+
  geom_pointrange(
    mapping = aes(
      ymin = low,
      ymax = up
    ),
    alpha = 0.5,
    color = col_EPX
  )+
  
  xlim(
    xmin = 0.5,
    xmax = max(df_predobs_CiEPX_ind$CiEPX_obs)
  )+
  ylim(
    ymin = 0.5,
    ymax = max(df_predobs_CiEPX_ind$predict.endpoint)
  )+
  labs(
    x = "Observed concentrations of EPX\nin earthworms (ng/g)",
    y = "Predicted concentrations of EPX\nin earthworms (ng/g)"
  )+
  scale_x_log10(limits=c(1, 500))+
  scale_y_log10(limits=c(1, 500))+
  theme_minimal()

p_CiEPX_zoom_predobs <- ggplot(
  data = df_predobs_CiEPX_ind,
  mapping = aes(
    x = CiEPX_obs,
    y = predict.endpoint
  )
)+
  geom_path(
    data=df_line,
    mapping=aes(x=x1, y=y1),
    color = col_line1
  )+
  geom_line(
    data=df_line,
    mapping=aes(x=x_1, y=y_1),
    color = col_line1
  )+
  geom_line(
    data=df_line,
    mapping=aes(x=x2, y=y2),
    color = col_line2
  )+
  geom_line(
    data=df_line,
    mapping=aes(x=x_2, y=y_2),
    color = col_line2
  )+
  geom_abline(
    slope=1, 
    intercept = 0, 
    color = col_line0,
    linewidth = 1
  )+
  geom_pointrange(
    mapping = aes(
      ymin = low,
      ymax = up
    ),
    alpha = 0.5,
    color = col_EPX
  )+
  
  xlim(
    xmin = 0.5,
    xmax = max(df_predobs_CiEPX_ind$CiEPX_obs)
  )+
  ylim(
    ymin = 0.5,
    ymax = max(df_predobs_CiEPX_ind$predict.endpoint)
  )+
  labs(
    x = "Observed concentrations of EPX\nin earthworms (ng/g)",
    y = "Predicted concentrations of EPX\nin earthworms (ng/g)"
  )+
  scale_x_log10(limits=c(100, 2000))+
  scale_y_log10(limits=c(100, 2000))+
  theme_minimal()


p <- p_W + p_CiEPX_predobs
p

df_predobs_Weight_ind <- df_predobs_Weight_ind |> 
  mutate(
    Ratio_predobs = predict.endpoint/Weight_obs,
    Residuals = predict.endpoint-Weight_obs
  )

Weight_count_fold2 <- length(subset(df_predobs_Weight_ind, Ratio_predobs >= 0.5 & Ratio_predobs <= 2)$Weight_obs)/length(df_predobs_Weight_ind$Weight_obs)*100

Weight_count_fold5 <- length(subset(df_predobs_Weight_ind, Ratio_predobs >= 0.2 & Ratio_predobs <= 5)$Weight_obs)/length(df_predobs_Weight_ind$Weight_obs)*100

df_predobs_CiEPX_ind <- df_predobs_CiEPX_ind |> 
  mutate(
    Ratio_predobs = predict.endpoint/CiEPX_obs,
    Residuals = predict.endpoint-CiEPX_obs
  )

CiEPX_count_fold2 <- length(subset(df_predobs_CiEPX_ind, Ratio_predobs >= 0.5 & Ratio_predobs <= 2)$CiEPX_obs)/length(df_predobs_CiEPX_ind$CiEPX_obs)*100

CiEPX_count_fold5 <- length(subset(df_predobs_CiEPX_ind, Ratio_predobs >= 0.2 & Ratio_predobs <= 5)$CiEPX_obs)/length(df_predobs_CiEPX_ind$CiEPX_obs)*100



cat("For weights : \n\n",
    "Percentage of prediction in fold 2 = ", round(Weight_count_fold2, 1), "%\n",
    "Percentage of prediction in fold 5 = ", round(Weight_count_fold5, 1), "%")
cat("\n\nFor internal concentrations : \n\n",
    "Percentage of prediction in fold 2 = ", round(CiEPX_count_fold2, 1), "%\n",
    "Percentage of prediction in fold 5 = ", round(CiEPX_count_fold5, 1), "%")
