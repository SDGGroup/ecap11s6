#---------------------    ecap11    -------------------------------------------#
# --------------------    step 6    -------------------------------------------#
rm(list=ls(all = TRUE))

# Carica pacchetto corrente (ecap11s6)
require(readxl)
require(readr)
require(ecap11s6)


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
file_notional <- "notional.csv"

#----------------- 002 SETTINGS parameters  -----------------------------------#

mesi_tenor_prepayement <- 180
prepayment <- 'SI' #SI/NO
percentile1=0.999
percentile2=0.960
formula_delta_pv <- 'GESTIONALE' #GESTIONALE/SEGNALETICA
storicizza_delta_pv <- 'SI' #SI/NO
max_x <- 480
scenario_no_prepayment <- "100"

#---------------- 003 CARICAMENTO FILE ----------------------------------------#

# caricamento entity
mapping_entity <- read_excel(file.path(path_in_local, file_mapping_entity),
                             skip = 1,
                             col_names = c("COD_ENTITY", "DES_ENTITY", "COD_NU_TDB", "COD_BU_RND", "FLG_CAPOGRUPPO"))


# caricamento term structure
curve_1y <- read_excel(file.path(path_in_local, file_term_structure))

#--------------- 004 CARICAMENTO FILE OUTPUT SEZIONI PRECEDENTI ---------------#

# caricamento notional
notional <- read_delim(file.path(path_out_local, file_notional),
                       skip = 1,
                       delim = ";",
                       col_names = c("COD_VALUTA_FINALE", "COD_ENTITY", "ID_MESE_MAT", "DES_SHOCK_FINALE", "VAL_NOTIONAL"),
                       show_col_types = F)


################################################################################
#---------------------------- FASE DI CALCOLO-----------------------------------
################################################################################


#---------------------- 001 CALCOLO ENTITY AGGREGATA --------------------------#

notional <- do_entity_aggregata(.notional = notional,
                                .mapping_entity = mapping_entity)

#---------------------- 002 CALCOLO INTERPOLAZIONE SPLINE ---------------------#

curve_1y_interpol <- do_interpolazione_spline(.curve_1y = curve_1y)

