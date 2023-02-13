#' do_deltapv.R
#' @description
#' Calcola DELTA PV.
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
#' * COD_VALUTA_FINALE chr,
#' * COD_ENTITY chr,
#' * ID_MESE_MAT dbl,
#' * DES_SHOCK_FINALE dbl,
#' * VAL_NOTIONAL dbl.
#' @param .formula_delta_pv chr.
#' @return a tibble object with 5 variables:
#' * COD_VALUTA chr,
#' * ID_MESE_MAT dbl,
#' * ID_YEAR dbl,
#' * ID_SCEN dbl,
#' * VAL_TASSO dbl.
#' @export

do_deltapv <- function(.scenari_prep, .scenari_noprep, .notional_prep, .notional_noprep, .notional_base, .curve_1y_interpol, .formula_delta_pv){

  if(.formula_delta_pv == "GESTIONALE"){
    if(prepayment == "NO"){

      deltaPV_noprep <- .do_deltapv_gestionale(.scenari_noprep, .notional_noprep, .curve_1y_interpol) %>%
        mutate(DES_PREPAYMENT = "N")

      deltaPV_prep <- tibble()

    } else {

      deltaPV_NN <- .do_deltapv_gestionale(.scenari_noprep, .notional_noprep, .curve_1y_interpol)
      deltaPV_NS <- .do_deltapv_gestionale(.scenari_noprep, .notional_prep, .curve_1y_interpol)
      deltaPV_SS <- .do_deltapv_gestionale(.scenari_prep, .notional_prep, .curve_1y_interpol)

      deltaPV_noprep <- bind_rows(deltaPV_NN,
                                  deltaPV_NS) %>%
        mutate(DES_PREPAYMENT = "N")

      deltaPV_prep <- bind_rows(deltaPV_SS,
                                deltaPV_NN) %>%
        mutate(DES_PREPAYMENT = "Y")

    }
  } else {

    if(prepayment == "NO"){

      deltaPV_noprep <- .do_deltapv_segnaletico(.scenari_noprep, .notional_noprep, .notional_base, .curve_1y_interpol) %>%
        mutate(DES_PREPAYMENT = "N")

      deltaPV_prep <- tibble()

    } else {

      deltaPV_NN <- .do_deltapv_segnaletico(.scenari_noprep, .notional_noprep, .notional_base, .curve_1y_interpol)
      deltaPV_NS <- .do_deltapv_segnaletico(.scenari_noprep, .notional_prep, .notional_base, .curve_1y_interpol)
      deltaPV_SS <- .do_deltapv_segnaletico(.scenari_prep, .notional_prep, .notional_base, .curve_1y_interpol)

      deltaPV_noprep <- bind_rows(deltaPV_NN,
                                  deltaPV_NS) %>%
        mutate(DES_PREPAYMENT = "N")

      deltaPV_prep <- bind_rows(deltaPV_SS,
                                deltaPV_NN) %>%
        mutate(DES_PREPAYMENT = "Y")
    }
  }

  # per alcune coppie valuta-entity, possono non esserci più scenari di shock
  # e quindi il deltaPV può essere missing: sostituiamo con il deltaPV senza prepayment

  missing2replace = deltaPV_prep %>%
    filter(is.na(DELTA_PV)) %>%
    left_join(deltaPV_noprep, by = c("ID_YEAR", "COD_VALUTA", "ID_SCEN", "COD_ENTITY")) %>%
    select(ID_YEAR,
           COD_VALUTA,
           ID_SCEN,
           DES_SHOCK_FINALE = DES_SHOCK_FINALE.x,
           COD_ENTITY,
           DES_PREPAYMENT = DES_PREPAYMENT.x,
           DELTA_PV = DELTA_PV.y)

  deltaPV_prep <- deltaPV_prep %>%
    rows_update(missing2replace, by = c("ID_YEAR", "COD_VALUTA", "ID_SCEN", "DES_SHOCK_FINALE",
                                        "COD_ENTITY", "DES_PREPAYMENT"))
  deltaPV <- deltaPV_prep %>%
    bind_rows(deltaPV_noprep)

  return(deltaPV)
}
