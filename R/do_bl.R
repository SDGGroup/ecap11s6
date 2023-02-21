#' do_bl.R
#' @description
#' Esegue tutta la business logic
#' @return tba
#' @export

do_bl <- function(.notional,
                  .notional_base,
                  .mapping_entity,
                  .curve_1y,
                  .max_x,
                  .shock_effettivi,
                  .prepayment,
                  .scenario_no_prepayment,
                  .mesi_tenor_prepayment,
                  .formula_delta_pv,
                  .percentile1,
                  .percentile2
                  ){

  #---------------------- 000 DIVISIONE NOTIONAL: PREP - NO PREP --------------#
  .notional_diviso <- do_notional_prep_noprep(.notional = .notional)
  message('CALC 000: divisione_notional')

  .notional_prep <- .notional_diviso$notional_prep

  .notional_noprep <- .notional_diviso$notional_noprep


  #---------------------- 001 CALCOLO ENTITY AGGREGATA --------------------------#

  .notional_lst <- do_entity_aggregata(.notional_prep = .notional_prep,
                                   .notional_noprep = .notional_noprep,
                                   .mapping_entity = .mapping_entity)
  message('CALC 001: entity_aggregata')

  .notional <- .notional_lst$notional

  .notional_prep <- .notional_lst$notional_prep


  #---------------------- 002 CALCOLO INTERPOLAZIONE SPLINE ---------------------#

  .curve_1y_interpol <- do_interpolazione_spline(.curve_1y = .curve_1y, .max_x = .max_x)
  message('CALC 002: interpolazione_spline')

  #---------------------- 003 CALCOLO DISCOUNT FACTOR ---------------------------#

  .curve_1y_interpol <- do_discount_factor(.curve_1y_interpol = .curve_1y_interpol)
  message('CALC 003: discount_factor')

  # --------------------- 004 SELEZIONE SCENARIO SHOCK --------------------------#

  .selezione_scenario_shock <- do_selezione_scenario_shock(.curve_1y_interpol = .curve_1y_interpol,
                                                           .shock_effettivi = .shock_effettivi,
                                                           .prepayment = .prepayment,
                                                           .scenario_no_prepayment = .scenario_no_prepayment,
                                                           .mesi_tenor_prepayment = .mesi_tenor_prepayment)
  message('CALC 004: selezione_scenario_shock')

  .scenari_noprep <- .selezione_scenario_shock$scenari_noprep

  .scenari_prep <- .selezione_scenario_shock$scenari_prep

  # -------------------- 005 CALCOLO DELTA PV -----------------------------------#

  .deltapv <- do_deltapv(.formula_delta_pv = .formula_delta_pv,
                        .prepayment = .prepayment,
                        .scenari_prep = .scenari_prep,
                        .scenari_noprep = .scenari_noprep,
                        .notional = .notional,
                        .notional_prep = .notional_prep,
                        .notional_noprep = .notional_noprep,
                        .notional_base = .notional_base,
                        .curve_1y_interpol = .curve_1y_interpol)
  message('CALC 005: delta_pv')

  # ------------------- 006 CALCOLO ECAP ----------------------------------------#

  .ecap <- do_ecap(.deltapv = .deltapv,
                  .mapping_entity = .mapping_entity,
                  .quantiles = c(.percentile1, .percentile2))
  message('CALC 006: ecap')

  # ------------------- 007 SELEZIONE CURVE ECAP --------------------------------#

  .curve <- do_selezione_curve_ecap(.ecap = .ecap,
                                   .curve_1y_interpol = .curve_1y_interpol)
  message('CALC 007: selezione_curve_ecap')

  return(list(deltapv = .deltapv, ecap = .ecap, curve = .curve))

}

#' do_bl.R
#' @description
#' Esegue tutta la business logic in parallelo
#' @return tba
#' @export
do_bl_par <- function(.notional,
                      .notional_base,
                      .mapping_entity,
                      .curve_1y,
                      .max_x,
                      .shock_effettivi,
                      .prepayment,
                      .scenario_no_prepayment,
                      .mesi_tenor_prepayment,
                      .formula_delta_pv,
                      .percentile1,
                      .percentile2,
                      .n_core){

  list_split <- .curve_1y %>%
    group_by(ID_YEAR, COD_VALUTA) %>%
    mutate(ID_SCEN_CLASS = cut(ID_SCEN, 10)) %>%
    group_by(ID_YEAR, COD_VALUTA, ID_SCEN_CLASS) %>%
    group_split()

  cl <- makeCluster(.n_core)
  clusterEvalQ(cl, {library(dplyr, tidyr)})
  clusterExport(cl, c(".notional",
                      ".notional_base",
                      ".mapping_entity",
                      ".max_x",
                      ".shock_effettivi",
                      ".prepayment",
                      ".scenario_no_prepayment",
                      ".mesi_tenor_prepayment",
                      ".formula_delta_pv",
                      ".percentile1",
                      ".percentile2",
                      "do_notional_prep_noprep",
                      "do_deltapv",
                      "do_entity_aggregata",
                      "do_interpolazione_spline",
                      "do_discount_factor",
                      "do_selezione_scenario_shock",
                      "do_ecap",
                      "do_selezione_curve_ecap",
                      ".do_ecap_base",
                      ".do_deltapv_segnaletico",
                      ".do_deltapv_gestionale",
                      ".concorda_segno",
                      ".interpolazione_spline"
                      ),
                envir = environment())
  out <- parLapply(cl, list_split, function(.x) { do_bl(.notional,
                                                        .notional_base,
                                                        .mapping_entity,
                                                        .curve_1y,
                                                        .max_x,
                                                        .shock_effettivi,
                                                        .prepayment,
                                                        .scenario_no_prepayment,
                                                        .mesi_tenor_prepayment,
                                                        .formula_delta_pv,
                                                        .percentile1,
                                                        .percentile2) })

  #out <- parLapply(cl, list_split, function(.x) .shock_effettivi %>% group_by(COD_VALUTA) %>% summarise(sum(VAL_SHOCK_EFFETTIVO_BPS)))
  stopCluster(cl) # kill cluster
  closeAllConnections()
  gc()

  return(out)
}
