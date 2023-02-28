#' do_selezione_scenario_shock.R
#' @description
#' Crea la struttura base per scenari simulati, in cui per ogni scenario
#' viene agganciato lo schock corretto. Per no prepayment, si utilizza lo shock inserito in input.
#' @param .curve_interpol tibble con 7 variabili:
#' * COD_VALUTA chr,
#' * ID_YEAR int,
#' * ID_SCEN int,
#' * ID_SCEN_CLASS int,
#' * ID_MESE_MAT int,
#' * VAL_TASSO dbl.
#' * DISCOUNT_FACTOR dbl.
#' @param .curve_interpol_scen0 tibble con 7 variabili (scenario 0 completo):
#' * COD_VALUTA chr,
#' * ID_YEAR int,
#' * ID_SCEN int,
#' * ID_SCEN_CLASS int,
#' * ID_MESE_MAT int,
#' * VAL_TASSO dbl,
#' * DISCOUNT_FACTOR dbl.
#' @param .shock_effettivi tibble  con 5 variabili:
#' * DES_SHOCK_FINALE chr,
#' * COD_VALUTA chr,
#' * ID_MESE_MAT int,
#' * VAL_SHOCK_NOMINALE_BPS dbl
#' * VAL_SHOCK_EFFETTIVO_BPS dbl.
#' @param .prepayment chr.
#' @param .scenario_no_prepayment chr.
#' @param .mesi_tenor_prepayment int.
#' @return a list con 2 tibble, ognuno con 5 variabili:
#' * ID_YEAR int,
#' * COD_VALUTA chr,
#' * ID_SCEN int,
#' * ID_SCEN_CLASS int,
#' * DES_SHOCK_FINALE chr
#' @export


do_selezione_scenario_shock <- function(.curve_interpol,
                                        .curve_interpol_scen0,
                                        .shock_effettivi,
                                        .prepayment,
                                        .scenario_no_prepayment,
                                        .mesi_tenor_prepayment) {
  scenari_noprep <- .curve_interpol %>%
    filter(ID_SCEN > 0) %>%
    select(COD_VALUTA, ID_YEAR, ID_SCEN, ID_SCEN_CLASS) %>%
    distinct() %>%
    mutate(DES_SHOCK_FINALE = .scenario_no_prepayment) %>%
    select(COD_VALUTA,
           ID_YEAR,
           ID_SCEN,
           ID_SCEN_CLASS,
           DES_SHOCK_FINALE)

  if(.prepayment == "SI"){
    curve_1y_interpol_tenor_0 <- .curve_interpol_scen0 %>%
      filter (ID_MESE_MAT == .mesi_tenor_prepayment , ID_SCEN == 0) %>%
      select(ID_YEAR,
             COD_VALUTA,
             ID_MESE_MAT,
             VAL_TASSO_0 = VAL_TASSO)

    curve_1y_interpol_tenor_1 <- .curve_interpol %>%
      filter (ID_MESE_MAT == .mesi_tenor_prepayment , ID_SCEN > 0) %>%
      select(ID_YEAR,
             COD_VALUTA,
             ID_SCEN,
             ID_SCEN_CLASS,
             ID_MESE_MAT,
             VAL_TASSO_1 = VAL_TASSO)
    
    scenari_prep <- curve_1y_interpol_tenor_1  %>%
      left_join(curve_1y_interpol_tenor_0, by = c("ID_YEAR", "COD_VALUTA","ID_MESE_MAT"),
                multiple = "all")
    
    scenari_prep <- scenari_prep %>%
      mutate(SHOCK_SIMULATO = 10000*(VAL_TASSO_1 - VAL_TASSO_0)) %>%
      select(ID_YEAR,
             COD_VALUTA,
             ID_SCEN,
             ID_SCEN_CLASS,
             ID_MESE_MAT,
             SHOCK_SIMULATO)
    
    scenari_prep <- scenari_prep %>%
      left_join(.shock_effettivi, by = c('COD_VALUTA', 'ID_MESE_MAT'), multiple = "all")
    
    scenari_prep <- scenari_prep %>%
      mutate(CONCORDANZA_SEGNO = .concorda_segno(SHOCK_SIMULATO, VAL_SHOCK_NOMINALE_BPS)) %>%
      mutate(DELTA_SHOCK_EFFETTIVO = abs(VAL_SHOCK_EFFETTIVO_BPS-SHOCK_SIMULATO)) %>%
      mutate(DELTA_SHOCK_NOMINALE = abs(VAL_SHOCK_NOMINALE_BPS-SHOCK_SIMULATO))
  
    scenari_prep <- scenari_prep %>%
      group_by(ID_YEAR, COD_VALUTA, ID_SCEN, ID_SCEN_CLASS) %>%
      mutate(min_DELTA_SHOCK_EFFETTIVO = min(DELTA_SHOCK_EFFETTIVO),
             min_DELTA_SHOCK_NOMINALE = min(DELTA_SHOCK_NOMINALE)) %>% 
      ungroup() %>% 
      mutate(peso = if_else(CONCORDANZA_SEGNO == 1, 10, 0) +
                    if_else(DELTA_SHOCK_EFFETTIVO == min_DELTA_SHOCK_EFFETTIVO, 5, 0) +
                    if_else(DELTA_SHOCK_NOMINALE == min_DELTA_SHOCK_NOMINALE, 1, 0)) %>%
      group_by(ID_YEAR, COD_VALUTA, ID_SCEN, ID_SCEN_CLASS) %>%
      filter(peso == max(peso)) %>%
      slice(1) %>% # TODO aggiungere un warning se questo accade (riga non univoca)
      ungroup() %>%
      select(ID_YEAR,
             COD_VALUTA,
             ID_SCEN,
             ID_SCEN_CLASS,
             DES_SHOCK_FINALE)
  } else {
    scenari_prep = NULL
  }
  return(list(scenari_prep = scenari_prep, scenari_noprep = scenari_noprep))

}
