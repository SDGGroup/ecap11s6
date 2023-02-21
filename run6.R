#---------------------    ecap11    -------------------------------------------#
# --------------------    step 6    -------------------------------------------#
rm(list=ls(all = TRUE))

# Carica pacchetto corrente (ecap11s6)
require(readxl)
require(readr)
require(ecap11s6)
require(dplyr) # to import curve_1y_big
require(tidyr) # to import curve_1y_big
require(parallel)
require(tictoc)


################################################################################
#---------------------------- FASE DI INPUT ------------------------------------
################################################################################

#----------------- 001 SETTINGS path_ e file_  --------------------------------#

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

#----------------- 002 SETTINGS parameters  -----------------------------------#

mesi_tenor_prepayment <- 180
prepayment <- 'SI' #SI/NO
percentile1 <- 0.999
percentile2 <- 0.960
formula_delta_pv <- 'GESTIONALE' #GESTIONALE/SEGNALETICA
storicizza_delta_pv <- 'SI' #SI/NO
max_x <- 480
scenario_no_prepayment <- "100"

n_core = 25

#---------------- 003 CARICAMENTO FILE ----------------------------------------#

# caricamento entity
mapping_entity <- read_excel(file.path(path_in_local, file_mapping_entity),
                             skip = 1,
                             col_names = c("COD_ENTITY", "DES_ENTITY", "COD_NU_TDB", "COD_BU_RND", "FLG_CAPOGRUPPO"))
message('LOAD 001: mapping_entity')

# caricamento term structure
curve_1y <- read_excel(file.path(path_in_local, file_term_structure)) %>% 
  mutate(key_split = paste(ID_YEAR, COD_VALUTA, ID_SCEN, sep='_')) %>% 
  mutate(ID_SCEN_CLASS = ntile(key_split, 1000))
message('LOAD 002: curve_1y')

# caricamento dati grossi
File_term_structure_EUR <- 'TermStr_Eur_2022_12_sstd_5Y_II_UPLOAD.csv'
File_term_structure_GBP <- 'TermStr_GBP_2022_12_sstd_5Y.csv'
File_term_structure_JPY <- 'TermStr_JPY_2022_12_sstd_5Y.csv'
File_term_structure_USD <- 'TermStr_USD_2022_12_sstd_5Y.csv'

ID_MESE_MAT <- curve_1y %>%
  distinct(ID_MESE_MAT) %>%
  pull()

# curve 1y grandi
curve_EUR <- read_delim(file.path(path_in_local, File_term_structure_EUR),
                        col_names =  c("ID_SCEN", paste0("M_", ID_MESE_MAT)), skip = 1) %>%
  pivot_longer(cols = paste0("M_", ID_MESE_MAT), names_to = "ID_MESE_MAT", names_prefix = "M_", values_to = "VAL_TASSO") %>%
  mutate(COD_VALUTA = "EUR",
         ID_MESE_MAT = as.numeric(ID_MESE_MAT),
         ID_YEAR = 1)
curve_USD <- read_delim(file.path(path_in_local, File_term_structure_USD),
                        col_names =  c("ID_SCEN", paste0("M_", ID_MESE_MAT)), skip = 1) %>%
  pivot_longer(cols = paste0("M_", ID_MESE_MAT), names_to = "ID_MESE_MAT", names_prefix = "M_", values_to = "VAL_TASSO") %>%
  mutate(COD_VALUTA = "USD",
         ID_MESE_MAT = as.numeric(ID_MESE_MAT),
         ID_YEAR = 1)
curve_JPY <- read_delim(file.path(path_in_local, File_term_structure_JPY),
                        col_names =  c("ID_SCEN", paste0("M_", ID_MESE_MAT)), skip = 1) %>%
  pivot_longer(cols = paste0("M_", ID_MESE_MAT), names_to = "ID_MESE_MAT", names_prefix = "M_", values_to = "VAL_TASSO") %>%
  mutate(COD_VALUTA = "JPY",
         ID_MESE_MAT = as.numeric(ID_MESE_MAT),
         ID_YEAR = 1)
curve_GBP <- read_delim(file.path(path_in_local, File_term_structure_GBP),
                        col_names =  c("ID_SCEN", paste0("M_", ID_MESE_MAT)), skip = 1) %>%
  pivot_longer(cols = paste0("M_", ID_MESE_MAT), names_to = "ID_MESE_MAT", names_prefix = "M_", values_to = "VAL_TASSO") %>%
  mutate(COD_VALUTA = "GBP",
         ID_MESE_MAT = as.numeric(ID_MESE_MAT),
         ID_YEAR = 1)

curve_1y_big <- bind_rows(curve_EUR, curve_GBP, curve_JPY, curve_USD)
rm(curve_EUR,curve_USD, curve_JPY, curve_GBP)

curve_1y <- curve_1y_big  %>%
mutate(key_split = paste(ID_YEAR, COD_VALUTA, ID_SCEN, sep='_')) %>%
  mutate(ID_SCEN_CLASS = ntile(key_split, 5000))
rm(curve_1y_big)


#--------------- 004 CARICAMENTO FILE OUTPUT SEZIONI PRECEDENTI ---------------#

# notional base
# TODO: notional_base VAL_NOTIONAL_BASE per COD_ENTITY "00001"
# e COD_ENTITY "00005
notional_base <- read_delim(file.path(path_out_local, file_notional_base),
                            skip = 1,
                            delim = ";",
                            col_names = c("COD_VALUTA_FINALE", "COD_ENTITY", "ID_MESE_MAT", "DES_SHOCK_FINALE", "VAL_NOTIONAL"),
                            col_types = "ccdcd",
                            show_col_types = F)
message('LOAD 003: notional_base')

# caricamento notional
notional <- read_delim(file.path(path_out_local, file_notional),
                       skip = 1,
                       delim = ";",
                       col_names = c("COD_VALUTA_FINALE", "COD_ENTITY", "ID_MESE_MAT", "DES_SHOCK_FINALE", "VAL_NOTIONAL"),
                       col_types = "ccdcd",
                       show_col_types = F)
message('LOAD 004: notional')


# shock effettivi
shock_effettivi <- read_delim(file.path(path_out_local, file_shock_effettivi),
                              skip = 1,
                              delim = ";",
                              col_names = c("COD_VALUTA", "DES_SHOCK_FINALE", "ID_MESE_MAT", "VAL_SHOCK_EFFETTIVO_BPS", "VAL_SHOCK_NOMINALE_BPS"),
                              col_types = "ccddd",
                              show_col_types = F)
message('LOAD 005: shock_effettivi')


################################################################################
#---------------------------- FASE DI CALCOLO-----------------------------------
################################################################################

#---------------------- 000 DIVISIONE NOTIONAL: PREP - NO PREP ----------------#
.notional_diviso <- do_notional_prep_noprep(.notional = notional)
message('CALC 000: divisione_notional')

notional_prep <- .notional_diviso$notional_prep

notional_noprep <- .notional_diviso$notional_noprep

rm(.notional_diviso)


#---------------------- 001 CALCOLO ENTITY AGGREGATA --------------------------#

.notional <- do_entity_aggregata(.notional_prep = notional_prep,
                                .notional_noprep = notional_noprep,
                                .mapping_entity = mapping_entity)
message('CALC 001: entity_aggregata')

notional <- .notional$notional

notional_prep <- .notional$notional_prep
rm(.notional)

#---------------------- 002 CALCOLO INTERPOLAZIONE SPLINE ---------------------#
tic()
curve_1y_interpol <- do_interpolazione_spline(.curve_1y = curve_1y, .max_x = max_x, .n_core = n_core)
message('CALC 002: interpolazione_spline')
toc()

rm(curve_1y)
gc()

#---------------------- 003 CALCOLO DISCOUNT FACTOR ---------------------------#
curve_1y_interpol <- do_discount_factor(.curve_1y_interpol = curve_1y_interpol)
message('CALC 003: discount_factor')
gc()

# --------------------- 004 SELEZIONE SCENARIO SHOCK --------------------------#
tic()
.selezione_scenario_shock <- do_selezione_scenario_shock(.curve_1y_interpol = curve_1y_interpol,
                                                         .shock_effettivi = shock_effettivi,
                                                         .prepayment = prepayment,
                                                         .scenario_no_prepayment = scenario_no_prepayment,
                                                         .mesi_tenor_prepayment = mesi_tenor_prepayment,
                                                         .n_core = n_core)
message('CALC 004: selezione_scenario_shock')
toc()
scenari_noprep <- .selezione_scenario_shock$scenari_noprep

scenari_prep <- .selezione_scenario_shock$scenari_prep
rm(.selezione_scenario_shock)
gc()

# -------------------- 005 CALCOLO DELTA PV -----------------------------------#
tic()
deltapv <- do_deltapv(.formula_delta_pv = formula_delta_pv,
                      .prepayment = prepayment,
                      .scenari_prep = scenari_prep,
                      .scenari_noprep = scenari_noprep,
                      .notional = notional,
                      .notional_prep = notional_prep,
                      .notional_noprep = notional_noprep,
                      .notional_base = notional_base,
                      .curve_1y_interpol = curve_1y_interpol,
                      .n_core = n_core)
message('CALC 005: delta_pv')
toc()
gc()

# ------------------- 006 CALCOLO ECAP ----------------------------------------#

ecap <- do_ecap(.deltapv = deltapv,
                .mapping_entity = mapping_entity,
                .quantiles = c(percentile1, percentile2))
message('CALC 006: ecap')

# ------------------- 007 SELEZIONE CURVE ECAP --------------------------------#

curve <- do_selezione_curve_ecap(.ecap = ecap,
                                 .curve_1y_interpol = curve_1y_interpol)
message('CALC 007: selezione_curve_ecap')


################################################################################
#---------------------------- FASE DI OUTPUT -----------------------------------
################################################################################

# TODO: cosa deve essere esportato?
#------------------001 ESPORTA notional SU CSV --------------------------------#
# write_delim(notional, file.path(path_out_local, ... ), delim = ';')

