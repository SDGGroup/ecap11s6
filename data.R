#---------------- ecap11s6 ----------------------------------------------------#
#-------------------   Test Step 6    -----------------------------------------#
rm(list=ls(all = TRUE))

# Carica pacchetto corrente (ecap11s5)
require(readxl)
require(readr)
require(dplyr)
require(ecap11s6)


################################################################################
#---------------------------- FASE DI INPUT ------------------------------------
################################################################################

#----------------- SETTINGS path_ e file_  ------------------------------------#

path <- "/data/isp/08shift/"
path_in_local <- file.path(path,"input.xls/")
path_out_local <- file.path(path,"output.csv/")
# file di dati
# mapping entity
file_mapping_entity <- 'TE_IRRBB_MAPPING_ENTITY_V2.xlsx'
# term structure
file_term_structure <- "curve_1y.xlsx"
# notional - output sezione 5
file_notional <- "notional_prova.csv"
file_notional_base <- "notional_base_prova.csv"
# shock effettivi - output sezione 5
file_shock_effettivi <- "shock_effettivi_prova.csv"

#---------------- CARICAMENTO FILE --------------------------------------------#

# caricamento entity
mapping_entity <- read_excel(file.path(path_in_local, file_mapping_entity),
                             skip = 1,
                             col_names = c("COD_ENTITY", "DES_ENTITY", "COD_NU_TDB", "COD_BU_RND", "FLG_CAPOGRUPPO"))

# caricamento term structure
curve_1y <- read_excel(file.path(path_in_local, file_term_structure))

# notional base
notional_base <- read_delim(file.path(path_out_local, file_notional_base),
                            skip = 1,
                            delim = ";",
                            col_names = c("COD_VALUTA_FINALE", "COD_ENTITY", "ID_MESE_MAT", "DES_SHOCK_FINALE", "VAL_NOTIONAL"),
                            col_types = "ccdcd",
                            show_col_types = F)

# caricamento notional
notional <- read_delim(file.path(path_out_local, file_notional),
                       skip = 1,
                       delim = ";",
                       col_names = c("COD_VALUTA_FINALE", "COD_ENTITY", "ID_MESE_MAT", "DES_SHOCK_FINALE", "VAL_NOTIONAL"),
                       col_types = "ccdcd",
                       show_col_types = F)


# shock effettivi
shock_effettivi <- read_delim(file.path(path_out_local, file_shock_effettivi),
                              skip = 1,
                              delim = ";",
                              col_names = c("COD_VALUTA", "DES_SHOCK_FINALE", "ID_MESE_MAT", "VAL_SHOCK_EFFETTIVO_BPS", "VAL_SHOCK_NOMINALE_BPS"),
                              col_types = "ccddd",
                              show_col_types = F)


#---------------- Crea i file per i test --------------------------------------#

# curve_1y_tst
curve_1y_tst <- curve_1y %>% 
  filter(ID_SCEN<=100)


# mapping_entity_tst
mapping_entity_tst <- mapping_entity

# notional_base_tst
notional_base_tst <- notional_base

# notional_tst
notional_tst <- notional

# shock_effettivi_tst
shock_effettivi_tst <- shock_effettivi

# salva i dati per i test
usethis::use_data(mapping_entity_tst, overwrite = TRUE)
usethis::use_data(curve_1y_tst, overwrite = TRUE)
usethis::use_data(notional_base_tst, overwrite = TRUE)
usethis::use_data(notional_tst, overwrite = TRUE)
usethis::use_data(shock_effettivi_tst, overwrite = TRUE)




