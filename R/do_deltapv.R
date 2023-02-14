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
#' *  COD_VALUTA chr,
#' *  ID_YEAR dbl,
#' *  ID_SCEN dbl,
#' *  ID_MESE_MAT int,
#' *  VAL_TASSO dbl,
#' *  DISCOUNT_FACTOR dbl.
#' @param .formula_delta_pv chr.
#' @return a tibble object with 7 variables:
#' * ID_YEAR dbl,
#' * COD_VALUTA chr,
#' * ID_SCEN dbl,
#' * DES_SHOCK_FINALE chr,
#' * COD_ENTITY chr,
#' * DELTA_PV dbl,
#' * DES_PREPAYMENT chr.
#' @export

do_deltapv <- function(.scenari_prep, .scenari_noprep, .notional, .notional_prep, .notional_noprep, .notional_base, .curve_1y_interpol, .formula_delta_pv){

  if(.formula_delta_pv == "GESTIONALE"){
    if(prepayment == "NO"){

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

    if(prepayment == "NO"){

      deltaPV_noprep <- .do_deltapv_segnaletico(.scenari_noprep, .notional, .notional_base, .curve_1y_interpol) %>%
        mutate(DES_PREPAYMENT = "N")

      deltaPV_prep <- tibble()

    } else {

      #calcoliamo il caso senza prepayment (in due pezzi distinti, perchè uno ci servirà anche per il caso con prepayment)
      deltaPV_NN <- .do_deltapv_segnaletico(.scenari_noprep, .notional_noprep, .notional_base, .curve_1y_interpol)
      deltaPV_NS <- .do_deltapv_segnaletico(.scenari_noprep, .notional_prep, .notional_base, c.urve_1y_interpol)


      deltaPV_noprep <- bind_rows(deltaPV_NN,
                                  deltaPV_NS) %>%
        mutate(DES_PREPAYMENT = "N")

      deltaPV_SS <- .do_deltapv_segnaletico(.scenari_prep, .notional_prep, .notional_base, curve_1y_interpol)
      deltaPV_prep <- bind_rows(deltaPV_SS,
                                deltaPV_NN) %>%
        mutate(DES_PREPAYMENT = "Y")
    }
  }

  deltaPV <- deltaPV_prep %>%
    bind_rows(deltaPV_noprep)

  return(deltaPV)
}
