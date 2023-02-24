#' do_deltapv.R
#' @description
#' Calcola DELTA PV
#' @param .curve_interpol tibble  con 7 variabili:
#' * COD_VALUTA chr,
#' * ID_YEAR int,
#' * ID_SCEN int,
#' * ID_SCEN_CLASS int,
#' * ID_MESE_MAT int,
#' * VAL_TASSO dbl,
#' * DISCOUNT_FACTOR dbl
#' @param .curve_interpol_scen0 tibble con 7 variabili:
#' * COD_VALUTA chr,
#' * ID_YEAR int,
#' * ID_SCEN int,
#' * ID_SCEN_CLASS int,
#' * ID_MESE_MAT int,
#' * VAL_TASSO dbl,
#' * DISCOUNT_FACTOR dbl
#' @param .formula_delta_pv chr.
#' @param .prepayment chr.
#' @param .scenari_prep tibble con 4 variabili:
#' * ID_YEAR int,
#' * COD_VALUTA chr,
#' * ID_SCEN int,
#' * DES_SHOCK_FINALE chr
#' @param .scenari_noprep tibble con 4 variabili:
#' * ID_YEAR int,
#' * COD_VALUTA chr,
#' * ID_SCEN int,
#' * DES_SHOCK_FINALE chr
#' @param .notional tibble con 5 variabili:
#' * COD_VALUTA_FINALE chr,
#' * COD_ENTITY chr,
#' * ID_MESE_MAT int,
#' * DES_SHOCK_FINALE chr,
#' * VAL_NOTIONAL dbl
#' @param .notional_prep tibble con 5 variabili:
#' * COD_VALUTA_FINALE chr,
#' * COD_ENTITY chr,
#' * ID_MESE_MAT int,
#' * DES_SHOCK_FINALE chr,
#' * VAL_NOTIONAL dbl
#' @param .notional_noprep tibble con 5 variabili:
#' * COD_VALUTA_FINALE chr,
#' * COD_ENTITY chr,
#' * ID_MESE_MAT int,
#' * DES_SHOCK_FINALE chr,
#' * VAL_NOTIONAL dbl.
#' @param .notional_base tibble con 5 variabili:
#' * COD_ENTITY chr,
#' * DES_SHOCK_FINALE chr,
#' * ID_MESE_MAT int,
#' * COD_VALUTA_FINALE chr,
#' * VAL_NOTIONAL dbl
#' @return a tibble con 5 variabili:
#' * ID_YEAR int,
#' * COD_VALUTA chr,
#' * ID_SCEN int,
#' * DES_SHOCK_FINALE chr,
#' * COD_ENTITY chr,
#' * VAL_DELTA_PV dbl,
#' * DES_PREPAYMENT chr
#' @export

do_deltapv <- function(.curve_interpol,
                       .curve_interpol_scen0,
                       .formula_delta_pv,
                       .prepayment,
                       .scenari_prep,
                       .scenari_noprep,
                       .notional,
                       .notional_prep,
                       .notional_noprep,
                       .notional_base){

  if(.formula_delta_pv == "GESTIONALE"){
    if(.prepayment == "NO"){

      deltaPV_noprep <- .do_deltapv_gestionale(.curve_interpol,  .curve_interpol_scen0, .scenari_noprep, .notional) %>%
        mutate(DES_PREPAYMENT = "N")

      deltaPV_prep <- tibble()

    } else {

      #calcoliamo il caso senza prepayment (in due pezzi distinti, perchè uno ci servirà anche per il caso con prepayment)
      deltaPV_NN <- .do_deltapv_gestionale(.curve_interpol, .curve_interpol_scen0, .scenari_noprep, .notional_noprep)
      deltaPV_NS <- .do_deltapv_gestionale(.curve_interpol, .curve_interpol_scen0,.scenari_noprep, .notional_prep)
      deltaPV_noprep <- deltaPV_NN %>%
        bind_rows(deltaPV_NS) %>%
        mutate(DES_PREPAYMENT = "N")

      #calcoliamo il caso con prepayment (e)
      deltaPV_SS <- .do_deltapv_gestionale(.curve_interpol, .curve_interpol_scen0, .scenari_prep, .notional_prep)
      deltaPV_prep <- bind_rows(deltaPV_SS,
                                deltaPV_NN) %>%
        mutate(DES_PREPAYMENT = "Y")

    }
  } else {

    if(.prepayment == "NO"){

      deltaPV_noprep <- .do_deltapv_segnaletico(.curve_interpol, .curve_interpol_scen0, .scenari_noprep, .notional, .notional_base) %>%
        mutate(DES_PREPAYMENT = "N")

      deltaPV_prep <- tibble()

    } else {

      #calcoliamo il caso senza prepayment (in due pezzi distinti, perchè uno ci servirà anche per il caso con prepayment)
      deltaPV_NN <- .do_deltapv_segnaletico(.curve_interpol, .curve_interpol_scen0, .scenari_noprep, .notional_noprep, .notional_base)
      deltaPV_NS <- .do_deltapv_segnaletico(.curve_interpol, .curve_interpol_scen0, .scenari_noprep, .notional_prep, .notional_base)


      deltaPV_noprep <- bind_rows(deltaPV_NN,
                                  deltaPV_NS) %>%
        mutate(DES_PREPAYMENT = "N")

      deltaPV_SS <- .do_deltapv_segnaletico(.curve_interpol, .curve_interpol_scen0, .scenari_prep, .notional_prep, .notional_base)
      deltaPV_prep <- bind_rows(deltaPV_SS,
                                deltaPV_NN) %>%
        mutate(DES_PREPAYMENT = "Y")
    }
  }

  deltaPV <- deltaPV_prep %>%
    bind_rows(deltaPV_noprep) %>%
    select(ID_YEAR,
           COD_VALUTA,
           ID_SCEN,
           DES_SHOCK_FINALE,
           COD_ENTITY,
           VAL_DELTA_PV = DELTA_PV,
           DES_PREPAYMENT)

  return(deltaPV)
}

#' .do_deltapv_gestionale
#' @description
#' Calcola DELTA_PV gestionale per ogni scenario contenuto in scenari, valuta ed
#' entity contenuta in notional.
#' @param .curve_1y_interpol tibble
#' @param .curve_1y_interpol_scen0 tibble
#' @param .scenari tibble
#' @param .notional tibble
#' @return a tibble
#' @export
.do_deltapv_gestionale <- function(.curve_interpol, .curve_interpol_scen0, .scenari, .notional){

  .curr_scen_class <- .curve_interpol %>% distinct(ID_SCEN_CLASS) %>% pull
  scenari_notional_curve <- .scenari %>%
    filter(ID_SCEN_CLASS %in% .curr_scen_class) %>%
    inner_join(.notional,by = c('COD_VALUTA' = 'COD_VALUTA_FINALE', 'DES_SHOCK_FINALE' = 'DES_SHOCK_FINALE'), multiple = "all") %>%
    select(ID_YEAR,
           COD_VALUTA,
           ID_SCEN,
           ID_SCEN_CLASS,
           DES_SHOCK_FINALE,
           COD_ENTITY,
           ID_MESE_MAT,
           VAL_NOTIONAL) %>%
    left_join(.curve_interpol, by = c("ID_YEAR", 'COD_VALUTA', 'ID_MESE_MAT', 'ID_SCEN', "ID_SCEN_CLASS"), multiple = "all") %>%
    select(ID_YEAR,
           COD_VALUTA,
           ID_SCEN,
           ID_SCEN_CLASS,
           DES_SHOCK_FINALE,
           COD_ENTITY,
           ID_MESE_MAT,
           VAL_NOTIONAL,
           DISCOUNT_FACTOR)

  discount_factor_0 <- .curve_interpol_scen0 %>%
    filter (ID_SCEN == 0) %>%
    select(ID_YEAR,
           COD_VALUTA,
           ID_MESE_MAT,
           DISCOUNT_FACTOR_0 = DISCOUNT_FACTOR)

  scenari_notional_curve_df0 <- scenari_notional_curve %>%
    left_join(discount_factor_0, by = c("ID_YEAR", 'COD_VALUTA','ID_MESE_MAT'),
              multiple = "all") 
  
  deltaPV <- scenari_notional_curve_df0 %>%
    mutate(PV_SIM = VAL_NOTIONAL * DISCOUNT_FACTOR,
           PV_0 = VAL_NOTIONAL * DISCOUNT_FACTOR_0) %>%
    group_by(ID_YEAR, COD_VALUTA , ID_SCEN , ID_SCEN_CLASS, DES_SHOCK_FINALE,  COD_ENTITY ) %>%
    summarise(PV_SIM = sum(PV_SIM, na.rm  = TRUE),
              PV_0 = sum(PV_0),
              .groups = 'drop') %>%
    mutate(DELTA_PV = PV_SIM - PV_0) %>%
    select(ID_YEAR,
           COD_VALUTA,
           ID_SCEN,
           ID_SCEN_CLASS,
           DES_SHOCK_FINALE,
           COD_ENTITY,
           DELTA_PV)

  return(deltaPV)
}

#' .do_deltapv_segnaletico
#' @description
#' Calcola DELTA_PV segnaletico per ogni scenario contenuto in scenari, valuta ed
#' entity contenuta in notional.
#' @param .curve_1y_interpol tibble
#' @param .curve_1y_interpol_scen0 tibble
#' @param .scenari tibble
#' @param .notional tibble
#' @return a tibble
#' @export
.do_deltapv_segnaletico <- function(.curve_interpol, .curve_interpol_scen0, .scenari, .notional, .notional_base){

  .curr_scen_class <- .curve_interpol %>% distinct(ID_SCEN_CLASS) %>% pull
  scenari_notional_curve <- .scenari %>%
    filter(ID_SCEN_CLASS %in% .curr_scen_class) %>%
    inner_join(.notional,
               by = c('COD_VALUTA' = 'COD_VALUTA_FINALE', 'DES_SHOCK_FINALE' = 'DES_SHOCK_FINALE'),
               multiple = "all") %>%
    select(ID_YEAR,
           COD_VALUTA,
           ID_SCEN,
           ID_SCEN_CLASS,
           DES_SHOCK_FINALE,
           COD_ENTITY,
           ID_MESE_MAT,
           VAL_NOTIONAL) %>%
    left_join(.notional_base, by = c("COD_VALUTA" = "COD_VALUTA_FINALE", "COD_ENTITY", "ID_MESE_MAT"),
              multiple = "all") %>%
    select(ID_YEAR,
           COD_VALUTA,
           ID_SCEN,
           ID_SCEN_CLASS,
           DES_SHOCK_FINALE = DES_SHOCK_FINALE.x,
           COD_ENTITY,
           ID_MESE_MAT,
           VAL_NOTIONAL = VAL_NOTIONAL.x,
           VAL_NOTIONAL_BASE = VAL_NOTIONAL.y) %>%
    left_join(.curve_interpol, by = c("ID_YEAR", 'COD_VALUTA', 'ID_MESE_MAT', 'ID_SCEN', "ID_SCEN_CLASS"),
              multiple = "all") %>%
    select(ID_YEAR,
           COD_VALUTA,
           ID_SCEN,
           ID_SCEN_CLASS,
           DES_SHOCK_FINALE,
           COD_ENTITY,
           ID_MESE_MAT,
           VAL_NOTIONAL,
           VAL_NOTIONAL_BASE,
           DISCOUNT_FACTOR)

  discount_factor_0 <- .curve_interpol_scen0 %>%
    filter (ID_SCEN == 0) %>%
    select(ID_YEAR,
           COD_VALUTA,
           ID_MESE_MAT,
           DISCOUNT_FACTOR_0 = DISCOUNT_FACTOR)

  scenari_notional_curve_df0 <- scenari_notional_curve %>%
    left_join(discount_factor_0, by = c("ID_YEAR", 'COD_VALUTA','ID_MESE_MAT'),
              multiple = "all") 
  
  deltaPV <- scenari_notional_curve_df0 %>%
    mutate(PV_SIM = VAL_NOTIONAL * DISCOUNT_FACTOR,
           PV_0 = VAL_NOTIONAL_BASE * DISCOUNT_FACTOR_0) %>%
    group_by(ID_YEAR, COD_VALUTA , ID_SCEN , ID_SCEN_CLASS, DES_SHOCK_FINALE,  COD_ENTITY ) %>%
    summarise(PV_SIM = sum(PV_SIM, na.rm  = TRUE),
              PV_0 = sum(PV_0),
              .groups = 'drop') %>%
    mutate(DELTA_PV = PV_SIM - PV_0) %>%
    select(ID_YEAR,
           COD_VALUTA,
           ID_SCEN,
           ID_SCEN_CLASS,
           DES_SHOCK_FINALE,
           COD_ENTITY,
           DELTA_PV)

  return(deltaPV)
}
