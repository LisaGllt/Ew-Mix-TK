library(here)
source(file = here::here("functions/fun_2.R"))
f_load_libraries_colors()

Molecule <- "EPX"

# 1. In creation ----

text_priors <- "Distrib(kuEPX,     LogUniform,   0.001, 10); # min, max 
Distrib(keEPX,    LogUniform,   0.001, 10); # min, max

Distrib(a_growth,  TruncNormal,   0.005,   0.005,    -1.0,    1); # mean, sd, min, max
Distrib(Vr_a_growth,  TruncNormal, 0.0015, 0.0015, 0, 1);

Distrib(Sigma_Wwgc,     TruncNormal,  0.03, 0.01, 0.0001, 1);
Distrib(Sigma_Wngc,     TruncNormal,  0.03, 0.01, 0.0001, 1);
Distrib(Sigma_CiEPX, TruncNormal,  3.0, 3.0,    1, 1000);"

text_likelihood <- "Likelihood(CiEPX,  LogNormal, Prediction(CiEPX),  Sigma_CiEPX);      # Calcul des vraisemblances (geometric mean, geometric standard deviation)
Likelihood(Weight_wgc, Normal,    Prediction(Weight_wgc), Sigma_Wwgc);      # Calcul des vraisemblances
Likelihood(Weight_ngc, Normal,    Prediction(Weight_ngc), Sigma_Wngc);      # Calcul des vraisemblances"

text_param_ind <- "Distrib(a_growth,  TruncNormal, a_growth,  Vr_a_growth, -1.0, 1);"

f_In_tot(Molecule, text_priors, text_likelihood, text_param_ind)

# 3. Setpoint_ind.in for predictions at times with data ----
f_Setpoint_ind(Molecule)

# 4. Setpoint_ind.in for full predictions of what led to our data points----

print_times <- "0, 44, 0.25"
nb_split <- 4
f_Setpoint_ind_full(Molecule, print_times, nb_split)
