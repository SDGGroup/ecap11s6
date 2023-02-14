#' do_selezione_scenario_shock.R
#' @description
#' Crea la struttura base per scenari simulati, in cui per ogni scenario
#' viene agganciato lo schock corretto. Per no prepayment, si utilizza lo shock inserito in input.
#' @param .curve_1y_interpol tibble with 5 variables:
#' * COD_VALUTA chr,
#' * ID_MESE_MAT dbl,
#' * ID_YEAR dbl,
#' * ID_SCEN dbl,
#' * VAL_TASSO dbl.
#' @param .shock_effettivi effettivi tibble object with 5 variables:
#' * COD_VALUTA chr,
#' * DES_SHOCK_FINALE dbl,
#' * ID_MESE_MAT dbl,
#' * VAL_SHOCK_EFFETTIVO_BPS dbl,
#' * VAL_SHOCK_NOMINALE_BPS dbl
#' @param .prepayment chr.
#' @param .scenario_no_prepayment chr.
#' @param .mesi_tenor_prepayment int.
#' @return a list with 2 tibble, each with 5 variables:
#' * COD_VALUTA chr,
#' * ID_MESE_MAT dbl,
#' * ID_YEAR dbl,
#' * ID_SCEN dbl,
#' * VAL_TASSO dbl.
#' @export

do_selezione_scenario_shock <- function(.curve_1y_interpol, .shock_effettivi, .prepayment, .scenario_no_prepayment, .mesi_tenor_prepayment) {

  scenari_noprep <- .curve_1y_interpol %>%
  filter(ID_SCEN != 0) %>%
  distinct(COD_VALUTA, ID_YEAR, ID_SCEN) %>%
  mutate(DES_SHOCK_FINALE = .scenario_no_prepayment)

  if(.prepayment == "SI"){
    curve_1y_interpol_tenor_0 <- .curve_1y_interpol %>%
      filter (ID_MESE_MAT == .mesi_tenor_prepayment , ID_SCEN == 0) %>%
      select(ID_YEAR, COD_VALUTA, ID_MESE_MAT,VAL_TASSO_0 = VAL_TASSO)

    curve_1y_interpol_tenor_1 <- .curve_1y_interpol %>%
      filter (ID_MESE_MAT == .mesi_tenor_prepayment , ID_SCEN != 0) %>%
      select(ID_YEAR, COD_VALUTA , ID_SCEN, ID_MESE_MAT, VAL_TASSO_1 = VAL_TASSO)

    scenari_prep <- curve_1y_interpol_tenor_1  %>%
      left_join(curve_1y_interpol_tenor_0, by = c("ID_YEAR", "COD_VALUTA","ID_MESE_MAT"))

    scenari_prep <- scenari_prep %>%
      mutate(SHOCK_SIMULATO = 10000*(VAL_TASSO_1 - VAL_TASSO_0)) %>%
      select(ID_YEAR, COD_VALUTA, ID_SCEN,ID_MESE_MAT, SHOCK_SIMULATO)

    scenari_prep <- scenari_prep %>%
      left_join(shock_effettivi, by = c('COD_VALUTA', 'ID_MESE_MAT'))

    scenari_prep <- scenari_prep %>%
      mutate(CONCORDANZA_SEGNO = .concorda_segno(SHOCK_SIMULATO, VAL_SHOCK_NOMINALE_BPS)) %>%
      mutate(DELTA_SHOCK_EFFETTIVO = abs(VAL_SHOCK_EFFETTIVO_BPS-SHOCK_SIMULATO)) %>%
      mutate(DELTA_SHOCK_NOMINALE = abs(VAL_SHOCK_NOMINALE_BPS-SHOCK_SIMULATO))

    scenari_prep <- scenari_prep %>%
      group_by(ID_YEAR, COD_VALUTA, ID_SCEN) %>%
      mutate(min_DELTA_SHOCK_EFFETTIVO = min(DELTA_SHOCK_EFFETTIVO),
             min_DELTA_SHOCK_NOMINALE = min(DELTA_SHOCK_NOMINALE),
             peso = if_else(CONCORDANZA_SEGNO == 1, 10, 0) +
                    if_else(DELTA_SHOCK_EFFETTIVO == min_DELTA_SHOCK_EFFETTIVO, 5, 0) +
                    if_else(DELTA_SHOCK_NOMINALE == min_DELTA_SHOCK_NOMINALE, 1, 0)) %>%
      filter(peso == max(peso)) %>%
      slice(1) %>% # TODO aggiungere un warning se questo accade (riga non univoca)
      ungroup() %>%
      select(ID_YEAR, COD_VALUTA,ID_SCEN , DES_SHOCK_FINALE)
  } else {
    scenari_prep = NULL
  }

  return(list(scenari_prep = scenari_prep, scenari_noprep = scenari_noprep))

}
