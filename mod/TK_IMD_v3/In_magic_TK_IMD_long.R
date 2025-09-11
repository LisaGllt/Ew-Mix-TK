library(here)
source(file = here::here("functions/fun.R"))
f_load_libraries_colors()

Molecule <- "IMD"

# 1. In creation ----

text_priors <- "Distrib (kuIMD,     Uniform,   0.001, 10); # min, max 
Distrib (keIMD,     Uniform,   0.001, 10); # min, max

Distrib (a_growth,  TruncNormal,   0.005,   0.005,    -1.0,    1); # mean, sd, min, max
Distrib (Vr_a_growth,  TruncNormal, 0.0015, 0.0015, 0, 1);

Distrib (Sigma_W,     TruncNormal,  0.03, 0.01, 0.0001, 1);
Distrib (Sigma_CiIMD, TruncNormal,  3.0, 3.0,    1, 1000);"

text_likelihood <- "Likelihood (CiIMD,  LogNormal, Prediction(CiIMD),  Sigma_CiIMD);  # Calcul des vraisemblances (geometric mean, geometric standard deviation)
Likelihood (Weight, Normal,    Prediction(Weight), Sigma_W);      # Calcul des vraisemblances"

text_param_ind <- "Distrib (a_growth,  TruncNormal, a_growth,  Vr_a_growth, -1.0, 1);"

f_In_tot(Molecule, text_priors, text_likelihood, text_param_ind)


# PB avec les weights 
  
# 3. Setpoint_ind.in for predictions at times with data ----
f_Setpoint_ind(Molecule)

# 4. Setpoint_ind.in for full predictions of what led to our data points----

char_final_1 <- ""
char_final_2 <- ""
char_final_3 <- ""
char_final_4 <- ""

compteur_exp <- 0

for (i in unique(df_TK_f$ID)){
  
  compteur_exp <- compteur_exp+1
  
  df_TK_i <- subset(df_TK_f, ID == i)
  
  char_tw <- paste(df_TK_i$t, collapse=",")
  char_Ww <- paste(df_TK_i$w, collapse=",")
  
  char_tCi <- paste(df_TK_i$t, collapse=",")
  char_CiIMD <- paste(df_TK_i$C_worm_IMD, collapse=",")
  char_expo <- paste(df_TK_i$expo, collapse=",")
  
  char_ta <- paste(df_TK_i$t[1:2], collapse=",")
  
  char_i <- paste(
    paste("Simulation { #", "TK IMD - i = ", i), 
    
    paste("    Winit=", subset(df_TK_i, t==0)$w, ";", sep=""),
    paste("    Ci0IMD=", C_worm_t0_IMD, ";", sep=""),
    paste("    Ce0IMD=", subset(df_TK_i, t==0)$C_soil_IMD, ";", sep=""),
    paste("    CclxIMD=", C_clx_IMD, ";", sep=""),
    
    paste("    expo=NDoses(", length(df_TK_i$expo),",", char_expo,",", char_tw, ");", sep=""),
    
    paste("    PrintStep(Weight,0,44, 0.25);", sep=""),
    paste("    PrintStep(CiIMD, 0,44, 0.25);", sep=""),
    
    paste("}"),
    sep="\n"
  )
  if(compteur_exp <= 16){
    char_final_1 <- paste(char_final_1, char_i, sep="\n") # Uptake individuals
  }
  if(16 < compteur_exp & compteur_exp <= 32 ){
    char_final_2 <- paste(char_final_2, char_i, sep="\n") # Uptake individuals
  }
  if(32 < compteur_exp & compteur_exp <= 48 ){
    char_final_3 <- paste(char_final_3, char_i, sep="\n") # Uptake individuals
  }
  if(compteur_exp > 48){
    char_final_4 <- paste(char_final_4, char_i, sep="\n") # Elimination individuals
  }
}

Fout_1 <- file(here::here("mod/TK_IMD/In_TK_IMD_Setpoint_ind_full_1.txt"))
writeLines(char_final_1, Fout_1)
Fout_2 <- file(here::here("mod/TK_IMD/In_TK_IMD_Setpoint_ind_full_2.txt"))
writeLines(char_final_2, Fout_2)
Fout_3 <- file(here::here("mod/TK_IMD/In_TK_IMD_Setpoint_ind_full_3.txt"))
writeLines(char_final_3, Fout_3)
Fout_4 <- file(here::here("mod/TK_IMD/In_TK_IMD_Setpoint_ind_full_4.txt"))
writeLines(char_final_4, Fout_4)
