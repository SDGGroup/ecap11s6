#---------------------    ecap11    -------------------------------------------#
# --------------------    step 6    -------------------------------------------#
rm(list=ls(all = TRUE))

# Carica pacchetto corrente (ecap11s6)
require(readxl)
require(readr)
require(ecap11s6)
require(dplyr)
require(tidyr)
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
file_mapping_entity <- 'mapping_entity_chk.xlsx'
# term structure
file_term_structure <- "curve_1y_chk.xlsx"
# notional - output sezione 5
file_notional <- "notional_equivalent_chk.xlsx"
file_notional_base <- "notional_equivalent_base_chk.xlsx"
# shock effettivi - output sezione 5
file_shock_effettivi <- "shock_effettivi_chk.xlsx"

#----------------- 002 SETTINGS parameters  -----------------------------------#

mesi_tenor_prepayment <- 180
prepayment <- 'SI' #SI/NO
percentile1 <- 0.999
percentile2 <- 0.960
formula_delta_pv <- 'GESTIONALE' #GESTIONALE/SEGNALETICA
storicizza_delta_pv <- 'NO' #SI/NO
max_x <- 480
scenario_no_prepayment <- "+100"
n_split <- 500
n_core <- 8


#---------------- 003 CARICAMENTO FILE ----------------------------------------#

# caricamento entity
mapping_entity <- read_excel(file.path(path_in_local, file_mapping_entity),
                             skip = 1,
                             col_names = c("COD_ENTITY", "DES_ENTITY", "COD_NU_TDB", "COD_BU_RND", "FLG_CAPOGRUPPO"))
message('LOAD 001: mapping_entity')

# caricamento term structure
curve_1y <- read_excel(file.path(path_in_local, file_term_structure)) %>% 
  mutate(ID_YEAR = as.integer(ID_YEAR),
         ID_SCEN = as.integer(ID_SCEN),
         ID_MESE_MAT = as.integer(ID_MESE_MAT))
message('LOAD 002: curve_1y')

# caricamento dati grossi
# File_term_structure_EUR <- 'TermStr_Eur_2022_12_sstd_5Y_II_UPLOAD.csv'
# File_term_structure_GBP <- 'TermStr_GBP_2022_12_sstd_5Y.csv'
# File_term_structure_JPY <- 'TermStr_JPY_2022_12_sstd_5Y.csv'
# File_term_structure_USD <- 'TermStr_USD_2022_12_sstd_5Y.csv'
# 
# ID_MESE_MAT <- curve_1y %>%
#   distinct(ID_MESE_MAT) %>%
#   pull()
# 
# # curve 1y grandi
# curve_EUR <- read_delim(file.path(path_in_local, File_term_structure_EUR),
#                         col_names =  c("ID_SCEN", paste0("M_", ID_MESE_MAT)), skip = 1) %>%
#   pivot_longer(cols = paste0("M_", ID_MESE_MAT), names_to = "ID_MESE_MAT", names_prefix = "M_", values_to = "VAL_TASSO") %>%
#   mutate(COD_VALUTA = "EUR",
#          ID_MESE_MAT = as.integer(ID_MESE_MAT),
#          ID_YEAR = as.integer(1),
#          ID_SCEN = as.integer(ID_SCEN))
# curve_USD <- read_delim(file.path(path_in_local, File_term_structure_USD),
#                         col_names =  c("ID_SCEN", paste0("M_", ID_MESE_MAT)), skip = 1) %>%
#   pivot_longer(cols = paste0("M_", ID_MESE_MAT), names_to = "ID_MESE_MAT", names_prefix = "M_", values_to = "VAL_TASSO") %>%
#   mutate(COD_VALUTA = "USD",
#          ID_MESE_MAT = as.integer(ID_MESE_MAT),
#          ID_YEAR = as.integer(1),
#          ID_SCEN = as.integer(ID_SCEN))
# curve_JPY <- read_delim(file.path(path_in_local, File_term_structure_JPY),
#                         col_names =  c("ID_SCEN", paste0("M_", ID_MESE_MAT)), skip = 1) %>%
#   pivot_longer(cols = paste0("M_", ID_MESE_MAT), names_to = "ID_MESE_MAT", names_prefix = "M_", values_to = "VAL_TASSO") %>%
#   mutate(COD_VALUTA = "JPY",
#          ID_MESE_MAT = as.integer(ID_MESE_MAT),
#          ID_YEAR = as.integer(1),
#          ID_SCEN = as.integer(ID_SCEN))
# curve_GBP <- read_delim(file.path(path_in_local, File_term_structure_GBP),
#                         col_names =  c("ID_SCEN", paste0("M_", ID_MESE_MAT)), skip = 1) %>%
#   pivot_longer(cols = paste0("M_", ID_MESE_MAT), names_to = "ID_MESE_MAT", names_prefix = "M_", values_to = "VAL_TASSO") %>%
#   mutate(COD_VALUTA = "GBP",
#          ID_MESE_MAT = as.integer(ID_MESE_MAT),
#          ID_YEAR = as.integer(1),
#          ID_SCEN = as.integer(ID_SCEN))
# 
# curve_1y <- bind_rows(curve_EUR, curve_GBP, curve_JPY, curve_USD)
# # Aggiungo lo scenario 0 altrimenti va in errore do_ecap
# curve_1y <- bind_rows(curve_1y, curve_1y %>% filter(ID_SCEN==1) %>% mutate(ID_SCEN = 0))

#--------------- 004 CARICAMENTO FILE OUTPUT SEZIONI PRECEDENTI ---------------#

# notional base
# TODO: notional_base VAL_NOTIONAL_BASE per COD_ENTITY "00001"
# e COD_ENTITY "00005
# notional_base <- read_delim(file.path(path_out_local, file_notional_base),
#                             skip = 1,
#                             delim = ";",
#                             col_names = c("COD_VALUTA_FINALE", "COD_ENTITY", "ID_MESE_MAT", "DES_SHOCK_FINALE", "VAL_NOTIONAL"),
#                             col_types = "cicd",
#                             show_col_types = F)
notional_base <- read_excel(file.path(path_out_local, file_notional_base))
message('LOAD 003: notional_base')

# caricamento notional
# notional <- read_delim(file.path(path_out_local, file_notional),
#                        skip = 1,
#                        delim = ";",
#                        col_names = c("COD_VALUTA_FINALE", "COD_ENTITY", "ID_MESE_MAT", "DES_SHOCK_FINALE", "VAL_NOTIONAL"),
#                        col_types = "ccicd",
#                        show_col_types = F)
notional <- read_excel(file.path(path_out_local, file_notional))
message('LOAD 004: notional')


# shock effettivi
# shock_effettivi <- read_delim(file.path(path_out_local, file_shock_effettivi),
#                               skip = 1,
#                               delim = ";",
#                               col_names = c("COD_VALUTA", "DES_SHOCK_FINALE", "ID_MESE_MAT", "VAL_SHOCK_EFFETTIVO_BPS", "VAL_SHOCK_NOMINALE_BPS"),
#                               col_types = "ccidd",
#                               show_col_types = F)
shock_effettivi <- read_excel(file.path(path_out_local, file_shock_effettivi))
message('LOAD 005: shock_effettivi')


################################################################################
#---------------------------- FASE DI CALCOLO-----------------------------------
################################################################################
tic()
out <- do_bl(.notional = notional,
             .notional_base = notional_base,
              .mapping_entity = mapping_entity,
              .curve_1y = curve_1y,
              .max_x = max_x,
              .shock_effettivi = shock_effettivi,
              .prepayment = prepayment,
              .scenario_no_prepayment = scenario_no_prepayment,
              .mesi_tenor_prepayment = mesi_tenor_prepayment,
              .formula_delta_pv = formula_delta_pv,
              .percentile1 = percentile1,
              .percentile2 = percentile2,
              .n_split = n_split,
              .n_core = n_core)
toc()

################################################################################
#---------------------------- FASE DI OUTPUT -----------------------------------
################################################################################

# TODO: cosa deve essere esportato?
#------------------001 ESPORTA notional SU CSV --------------------------------#
# write_delim(notional, file.path(path_out_local, ... ), delim = ';')

