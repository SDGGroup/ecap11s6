#' do_deltapv.R
#' @description
#' Calcola DELTA PV.
#' @param .formula_delta_pv chr.
#' @param .prepayment chr.
#' @param .scenari_prep tibble object with 4 variables:
#' * COD_VALUTA chr,
#' * ID_YEAR dbl,
#' * ID_SCEN dbl,
#' * DES_SHOCK_FINALE chr.
#' @param .scenari_noprep tibble object with 4 variables:
#' * COD_VALUTA chr,
#' * ID_YEAR dbl,
#' * ID_SCEN dbl,
#' * DES_SHOCK_FINALE chr.
#' @param .notional_prep tibble object with 5 variables:
#' * COD_VALUTA_FINALE chr,
#' * COD_ENTITY chr,
#' * ID_MESE_MAT dbl,
#' * DES_SHOCK_FINALE dbl,
#' * VAL_NOTIONAL dbl.
#' @param .notional_noprep tibble object with 5 variables:
#' * COD_VALUTA_FINALE chr,
#' * COD_ENTITY chr,
#' * ID_MESE_MAT dbl,
#' * DES_SHOCK_FINALE dbl,
#' * VAL_NOTIONAL dbl.
#' @param .notional_base tibble object with 5 variables:
#' * COD_VALUTA_FINALE chr,
#' * COD_ENTITY chr,
#' * ID_MESE_MAT dbl,
#' * DES_SHOCK_FINALE dbl,
#' * VAL_NOTIONAL dbl.
#' @param .curve_1y_interpol tibble object with 5 variables:
#' *  COD_VALUTA chr,
#' *  ID_YEAR dbl,
#' *  ID_SCEN dbl,
#' *  ID_MESE_MAT int,
#' *  VAL_TASSO dbl,
#' *  DISCOUNT_FACTOR dbl.
#' @return a tibble object with 7 variables:
#' * ID_YEAR dbl,
#' * COD_VALUTA chr,
#' * ID_SCEN dbl,
#' * DES_SHOCK_FINALE chr,
#' * COD_ENTITY chr,
#' * DELTA_PV dbl,
#' * DES_PREPAYMENT chr.
#' @export

do_deltapv <- function(.formula_delta_pv,
                       .prepayment,
                       .scenari_prep,
                       .scenari_noprep,
                       .notional,
                       .notional_prep,
                       .notional_noprep,
                       .notional_base,
                       .curve_1y_interpol){

  if(.formula_delta_pv == "GESTIONALE"){
    if(.prepayment == "NO"){

      deltaPV_noprep <- .do_deltapv_gestionale(.scenari_noprep, .notional, .curve_1y_interpol) %>%
        mutate(DES_PREPAYMENT = "N")

      deltaPV_prep <- tibble()

    } else {

      #calcoliamo il caso senza prepayment (in due pezzi distinti, perchè uno ci servirà anche per il caso con prepayment)
      deltaPV_NN <- .do_deltapv_gestionale(.scenari_noprep, .notional_noprep, .curve_1y_interpol)
      deltaPV_NS <- .do_deltapv_gestionale(.scenari_noprep, .notional_prep, .curve_1y_interpol)
      deltaPV_noprep <- deltaPV_NN %>%
        bind_rows(deltaPV_NS) %>%
        mutate(DES_PREPAYMENT = "N")

      #calcoliamo il caso con prepayment (e)
      deltaPV_SS <- .do_deltapv_gestionale(.scenari_prep, .notional_prep, .curve_1y_interpol)
      deltaPV_prep <- bind_rows(deltaPV_SS,
                                deltaPV_NN) %>%
        mutate(DES_PREPAYMENT = "Y")

    }
  } else {

    if(.prepayment == "NO"){

      deltaPV_noprep <- .do_deltapv_segnaletico(.scenari_noprep, .notional, .notional_base, .curve_1y_interpol) %>%
        mutate(DES_PREPAYMENT = "N")

      deltaPV_prep <- tibble()

    } else {

      #calcoliamo il caso senza prepayment (in due pezzi distinti, perchè uno ci servirà anche per il caso con prepayment)
      deltaPV_NN <- .do_deltapv_segnaletico(.scenari_noprep, .notional_noprep, .notional_base, .curve_1y_interpol)
      deltaPV_NS <- .do_deltapv_segnaletico(.scenari_noprep, .notional_prep, .notional_base, .curve_1y_interpol)


      deltaPV_noprep <- bind_rows(deltaPV_NN,
                                  deltaPV_NS) %>%
        mutate(DES_PREPAYMENT = "N")

      deltaPV_SS <- .do_deltapv_segnaletico(.scenari_prep, .notional_prep, .notional_base, .curve_1y_interpol)
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
#' @param .scenari tba
#' @param .notional tba
#' @param .curve_1y_interpol tba
#' @return a tibble tba
#' @export
.do_deltapv_gestionale <- function(.scenari, .notional, .curve_1y_interpol){

  scenari_notional <- .scenari %>%
    inner_join(.notional,
               by = c('COD_VALUTA' = 'COD_VALUTA_FINALE', 'DES_SHOCK_FINALE' = 'DES_SHOCK_FINALE'),
               multiple = "all") %>%
    select(ID_YEAR, COD_VALUTA, ID_SCEN, DES_SHOCK_FINALE, COD_ENTITY, ID_MESE_MAT, VAL_NOTIONAL)
  # num = years*scenari*480*cod_entity

  scenari_notional_curve <- scenari_notional %>%
    left_join(.curve_1y_interpol, by = c("ID_YEAR", 'COD_VALUTA', 'ID_MESE_MAT', 'ID_SCEN'),
              multiple = "all") %>%
    select(ID_YEAR, COD_VALUTA, ID_SCEN, DES_SHOCK_FINALE, COD_ENTITY, ID_MESE_MAT, VAL_NOTIONAL, DISCOUNT_FACTOR)

  discount_factor_0 <- .curve_1y_interpol %>%
    filter (ID_SCEN == 0) %>%
    select(ID_YEAR, COD_VALUTA,ID_MESE_MAT,DISCOUNT_FACTOR_0 = DISCOUNT_FACTOR)

  scenari_notional_curve_df0 <- scenari_notional_curve %>%
    left_join(discount_factor_0, by = c("ID_YEAR", 'COD_VALUTA','ID_MESE_MAT'),
              multiple = "all")

  deltaPV <- scenari_notional_curve_df0 %>%
    mutate(PV_SIM = VAL_NOTIONAL * DISCOUNT_FACTOR,
           PV_0 = VAL_NOTIONAL * DISCOUNT_FACTOR_0) %>%
    group_by(ID_YEAR, COD_VALUTA , ID_SCEN , DES_SHOCK_FINALE,  COD_ENTITY ) %>%
    summarise(PV_SIM = sum(PV_SIM, na.rm  = TRUE),
              PV_0 = sum(PV_0),
              .groups = 'drop') %>%
    mutate(DELTA_PV = PV_SIM - PV_0) %>%
    select(ID_YEAR, COD_VALUTA, ID_SCEN, DES_SHOCK_FINALE, COD_ENTITY, DELTA_PV)

  return(deltaPV)
}

#' .do_deltapv_segnaletico
#' @description
#' Calcola DELTA_PV segnaletico per ogni scenario contenuto in scenari, valuta ed
#' entity contenuta in notional.
#' @param .scenari tba
#' @param .notional tba
#' @param .curve_1y_interpol tba
#' @return a tibble tba
#' @export
.do_deltapv_segnaletico <- function(.scenari, .notional, .notional_base, .curve_1y_interpol){

  scenari_notional <- .scenari %>%
    inner_join(.notional,
               by = c('COD_VALUTA' = 'COD_VALUTA_FINALE', 'DES_SHOCK_FINALE' = 'DES_SHOCK_FINALE'),
               multiple = "all") %>%
    select(ID_YEAR, COD_VALUTA, ID_SCEN, DES_SHOCK_FINALE, COD_ENTITY, ID_MESE_MAT, VAL_NOTIONAL) %>%
    left_join(.notional_base, by = c("COD_VALUTA" = "COD_VALUTA_FINALE", "COD_ENTITY", "ID_MESE_MAT"),
              multiple = "all") %>%
    select(ID_YEAR,
           COD_VALUTA,
           ID_SCEN,
           DES_SHOCK_FINALE = DES_SHOCK_FINALE.x,
           COD_ENTITY,
           ID_MESE_MAT,
           VAL_NOTIONAL = VAL_NOTIONAL.x,
           VAL_NOTIONAL_BASE = VAL_NOTIONAL.y)

  scenari_notional_curve <- scenari_notional %>%
    left_join(.curve_1y_interpol, by = c("ID_YEAR", 'COD_VALUTA', 'ID_MESE_MAT', 'ID_SCEN'),
              multiple = "all") %>%
    select(ID_YEAR,
           COD_VALUTA,
           ID_SCEN,
           DES_SHOCK_FINALE,
           COD_ENTITY,
           ID_MESE_MAT,
           VAL_NOTIONAL,
           VAL_NOTIONAL_BASE,
           DISCOUNT_FACTOR)

  discount_factor_0 <- .curve_1y_interpol %>%
    filter (ID_SCEN == 0) %>%
    select(ID_YEAR, COD_VALUTA,ID_MESE_MAT,DISCOUNT_FACTOR_0 = DISCOUNT_FACTOR)

  scenari_notional_curve_df0 <- scenari_notional_curve %>%
    left_join(discount_factor_0, by = c("ID_YEAR", 'COD_VALUTA','ID_MESE_MAT'),
              multiple = "all")

  deltaPV <- scenari_notional_curve_df0 %>%
    mutate(PV_SIM = VAL_NOTIONAL * DISCOUNT_FACTOR,
           PV_0 = VAL_NOTIONAL_BASE * DISCOUNT_FACTOR_0) %>%
    group_by(ID_YEAR, COD_VALUTA , ID_SCEN , DES_SHOCK_FINALE,  COD_ENTITY ) %>%
    summarise(PV_SIM = sum(PV_SIM, na.rm  = TRUE),
              PV_0 = sum(PV_0),
              .groups = 'drop') %>%
    mutate(DELTA_PV = PV_SIM - PV_0) %>%
    select(ID_YEAR, COD_VALUTA, ID_SCEN, DES_SHOCK_FINALE, COD_ENTITY, DELTA_PV)

  return(deltaPV)
}
