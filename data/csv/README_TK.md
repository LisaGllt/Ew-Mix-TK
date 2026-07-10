RDG README File Template --- General --- Version: 0.1 (2026-06-03)

This README file was generated on 2026-06-03 by Raphaël Royauté.

Last updated: 2026-06-03.

# GENERAL INFORMATION:

## Dataset title: "Toxicokinetics of a Pesticide Mixture in Earthworms Reveal Concentration-Dependent Bioaccumulation and Limited Interactions"

## DOI:

# Contact email: raphael.royaute\@inrae.fr

# METHODOLOGICAL INFORMATION

## Environmental/experimental conditions:

This dataset contains toxicokinetic data for earthworms exposed to individual pesticide substances and to their mixture. It includes time-course measurements of pesticide concentrations in earthworms exposed to single substances, concentration measurements in earthworms exposed to the mixture, and data on the depuration time of earthworm gut contents.

## Methods for processing the data:

All processing steps are available at the [following website](https://lisagllt.github.io/Ew-Mix-TK/) which explains all data analyses steps.

# DATA & FILE OVERVIEW

All data are stored under the `Data_TK_single.csv`, `Data_TK_mixture.csv` & `Data_GUT.csv`, files.

## Variables names :

The `Data_TK_single.csv` file contains 17 columns and 504 rows. The columns defintions and units are as follows:

-   `Experiment` : Experiment identifier
    -   "TK" : First batch of earthworms. Earthworms were put on wet filter paper in Petri dishes for about 24h before being frozen for chemical analysis.
    -   "TKBIS" : Second batch of earthworms. Earthworms were only gently massaged to void their gut content before being frozen for chemical analysis.
-   `date` : Date and hour of the measurement (DD-MM-YYY HH-MM format)
-   `t` : Number of days since the start of the experiment.
    -   0 : Start of the experiment.
    -   Void cells : Time to be calculated thank to the `date` column.
-   `Molecule` : Studied substance.
    -   "EPX" : Epoxiconazole exposure.
    -   "IMD" : Imidacloprid exposure.
    -   "Ctrl" : Only natural soil.
-   `Dose` : Nominal exposure concentration (mg/kg, range: 0.1-1).
-   `ID_recipient` : Identification number of the recipient in which the contaminated soil was prepared (range: 0-2).
-   `ID` : Identification number of the earthworm (range: 1-148).
-   `Nb_rep` : Replicate number (range: 1-4).
-   `Time_point` : Dedicated time measurement for the earthworm (range: 0-42).
-   `expo` : Exposure status.
    -   0 : Earthworm in natural soil.
    -   1 : Earthworm in contaminated soil.
    -   2 : Earthworm in Petri dish.
-   `Phase` : Phase of the experiment.
    -   `Uptake` : Earthworm only exposed to contaminated soil.
    -   `Elimination` : Earthworm exposed to contaminated soil and then put in natural soil.
    -   `Frozen` : Measurements corresponding to the time just before the earthworm is frozen at -80°C.
-   `w` : Fresh weight of the earthworm (mg, range 222-1275).
-   `C_worm_EPX` : Measured epoxiconazole concentration in the earthworm (ng/g, range: 0.03-847).
-   `C_worm_IMD` : Measured imidacloprid concentration in the earthworm (ng/g, range: 0.07-1460).
-   `C_soil_EPX` : Measured epoxiconazole concentration in the soil (ng/g, range: 0.7-1.1).
-   `C_soil_IMD` : Measured imidacloprid concentration in the soil (ng/g, range: 0.07-0.08).
-   `Keep` : Selection criteria (Yes-No). Selects data with no concentration measurement outliers.

The `Data_TK_mixture.csv` file contains 16 columns and 130 rows. The columns defintions and units are as follows:

-   `Lot` : Two batches of earthworms were use and started at different times (D & E).
-   `Date` : Date and hour of the measurement (DD-MM-YYY HH-MM format)
-   `t` : Number of days since the start of the experiment (range: 0-29).
-   `ID` : Identification number of the earthworm (range: 1-288).
-   `ID_cosm` : Identification number of the cosm (range: 1-143).
-   `Dose_EPX` : Nominal exposure concentration of epoxiconazole (mg/kg, range: 0-167).
-   `Dose_IMD` : Nominal exposure concentration of imidacloprid (mg/kg, range: 0-0.5).
-   `Ratio` : Toxic unit ratio between the two substances.
    -   "E" : 100% Epoxiconazole & 0% Imidacloprid.
    -   "F" : 75% Epoxiconazole & 25% Imidacloprid.
    -   "G" : 50% Epoxiconazole & 50% Imidacloprid.
    -   "H" : 25% Epoxiconazole & 75% Imidacloprid.
    -   "I" : 0% Epoxiconazole & 100% Imidacloprid.
-   `Line` : Identification number of the effect level (effect isobole under CA hypothesis) (range: 0-5)
-   `Nb_rep` : Replicate number (range: 1-3).
-   `No_vdt` : Identification number of the earthworm per replicate (range: 1-2).
-   `w` : Fresh weight of the earthworm (mg, range 296-1152).
-   `CiEPX` : Measured epoxiconazole concentration in the earthworm (ng/g, range: 22.4-95693).
-   `CiIMD` : Measured imidacloprid concentration in the earthworm (ng/g, range: 0.17-2343).
-   `CeEPX` : Measured epoxiconazole concentration in the soil (ng/g, range: 0-422).
-   `CeIMD` : Measured imidacloprid concentration in the soil (ng/g, range: 0-127730).

The `Data_GUT.csv` file contains 6 columns and 775 rows. The columns defintions and units are as follows:

-   `date` : Date of the measurement (DD-MM-YYY format)
-   `Heure` : Hour of the measurement (HH-MM format)
-   `t` : Number of days since the start of the experiment (range: 0-32)
-   `ID` : Identification number of the earthworm (range: 1-10).
-   `Weight` : Fresh weight of the earthworm (mg, range 315-817).
-   `Handling` :
    -   “None” : Nothing special done to the earthworm before measurement.
    -   “Massaged” : The earthworm is gently massaged to remove the remaining of its gut content before measurement.

## Missing data codes

Missing data are represented by empty cells which are transformed to `NA` by default during data processing with R.
