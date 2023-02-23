#' do_bl.R
#' @description
#' Run business logic of ecap11-step6
#' @param .notional tibble object with 5 variables:
#' * COD_VALUTA_FINALE chr,
#' * COD_ENTITY chr,
#' * ID_MESE_MAT int,
#' * DES_SHOCK_FINALE chr,
#' * VAL_NOTIONAL dbl.
#' @param .notional_base tibble object with 5 variables:
#' * COD_VALUTA_FINALE chr,
#' * COD_ENTITY chr,
#' * ID_MESE_MAT int,
#' * DES_SHOCK_FINALE chr,
#' * VAL_NOTIONAL dbl.
#' @param .mapping_entity tibble object with 5 variables:
#' * COD_ENTITY chr,
#' * DES_ENTITY chr,
#' * COD_BU_TDB chr,
#' * COD_BU_RND chr,
#' * FLG_CAPOGRUPPO chr.
#' @param .curve_1y tibble object with 5 variables:
#' * COD_VALUTA chr,
#' * ID_MESE_MAT int,
#' * ID_YEAR int,
#' * ID_SCEN int,
#' * VAL_TASSO dbl.
#' @param .max_x int
#' @param .shock_effettivi
#' * DES_SHOCK_FINALE chr,
#' * COD_VALUTA des,
#' * ID_MESE_MAT int,
#' * VAL_SHOCK_NOMINALE_BPS dbl,
#' * VAL_SHOCK_EFFETTIVO_BPS dbl.
#' @param .prepayment chr
#' @param .scenario_no_prepayment chr
#' @param .mesi_tenor_prepayment int
#' @param .formula_delta_pv chr
#' @param .percentile1 dbl
#' @param .percentile2 dbl
#' @param .n_split 	a number giving the number of partitions into which curve_1y is cut for parallel computing.
#' @param .n_core the number of cores to use for parallel computing.
#' @return list object with 3 tibbles:
#' * ecap tibble,
#'     * ID_YEAR int,
#'     * COD_VALUTA chr,
#'     * COD_ENTITY chr,
#'     * VAL_ECAP dbl,
#'     * VAL_PERCENTILE dbl,
#'     * ID_SCEN int,
#'     * DES_SHOCK_FINALE chr,
#'     * DES_PREPAYMENT chr,
#'     * COD_RIPARTIZIONE chr
#' * curve_var tibble,
#'     * ID_YEAR int,
#'     * COD_VALUTA chr,
#'     * COD_ENTITY chr,
#'     * VAL_PERCENTILE dbl,
#'     * ID_SCEN int,
#'     * DES_SHOCK_FINALE chr,
#'     * DES_PREPAYMENT chr,
#'     * ID_MESE_MAT int,
#'     * VAL_TASSO dbl
#' * delta_pv tibble
#'     * ID_YEAR int,
#'     * COD_VALUTA chr,
#'     * ID_SCEN int,
#'     * DES_SHOCK_FINALE chr,
#'     * COD_ENTITY chr,
#'     * VAL_DELTA_PV dbl,
#'     * DES_PREPAYMENT chr
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
                  .percentile2,
                  .n_split,
                  .n_core){

  # make cluster for parallelization
  cl <- makeCluster(.n_core)

  #---------------------- 000 PARTIZIONE CURVE_1y -----------------------------#
  .curve_1y <- .curve_1y %>%
    left_join(.curve_1y %>%
                select(ID_YEAR, COD_VALUTA, ID_SCEN) %>%
                distinct() %>%
                mutate(key_split = 1:n()) %>%
                mutate(ID_SCEN_CLASS = cut(key_split,.n_split,labels = FALSE))
    ) %>%
    select(-key_split)

  #---------------------- 000 DIVISIONE NOTIONAL: PREP - NO PREP --------------#
  .notional_diviso <- do_notional_prep_noprep(.notional = .notional)
  message('CALC 000: divisione_notional')

  .notional_prep <- .notional_diviso$notional_prep

  .notional_noprep <- .notional_diviso$notional_noprep


  #---------------------- 001 CALCOLO ENTITY AGGREGATA ------------------------#

  .notional_lst <- do_entity_aggregata(.notional_prep = .notional_prep,
                                       .notional_noprep = .notional_noprep,
                                       .mapping_entity = .mapping_entity)
  message('CALC 001: entity_aggregata')

  .notional <- .notional_lst$notional

  .notional_prep <- .notional_lst$notional_prep

  #---------------------- 002 CALCOLO INTERPOLAZIONE SPLINE -------------------#

  .curve_1y_interpol <- parLapply(cl,
                                  .curve_1y %>% group_split(ID_SCEN_CLASS),
                                  do_interpolazione_spline,
                                  .max_x = .max_x) %>%
    bind_rows()

  message('CALC 002: interpolazione_spline')

  #---------------------- 003 CALCOLO DISCOUNT FACTOR -------------------------#

  .curve_1y_interpol <- do_discount_factor(.curve_1y_interpol = .curve_1y_interpol)

  .curve_1y_interpol_scen0 <- .curve_1y_interpol %>%
    filter(ID_SCEN == 0)

  message('CALC 003: discount_factor')


  # --------------------- 004 SELEZIONE SCENARIO SHOCK ------------------------#
  .selezione_screnario_shock <- parLapply(cl,
                                          .curve_1y_interpol %>% group_split(ID_SCEN_CLASS),
                                          do_selezione_scenario_shock,
                                          .curve_interpol_scen0 = .curve_1y_interpol_scen0,
                                          .shock_effettivi = .shock_effettivi,
                                          .prepayment = .prepayment,
                                          .scenario_no_prepayment = .scenario_no_prepayment,
                                          .mesi_tenor_prepayment = .mesi_tenor_prepayment)
  message('CALC 004: selezione_scenario_shock')

  .scenari_noprep <-  bind_rows(lapply(.selezione_screnario_shock, function(x) x$scenari_noprep))

  .scenari_prep <- bind_rows(lapply(.selezione_screnario_shock, function(x) x$scenari_prep))


  # -------------------- 005 CALCOLO DELTA PV ---------------------------------#

  .deltapv <-  parLapply(cl,
                         .curve_1y_interpol %>% group_split(ID_SCEN_CLASS),
                         do_deltapv,
                        .curve_interpol_scen0 = .curve_1y_interpol_scen0,
                        .formula_delta_pv = .formula_delta_pv,
                        .prepayment = .prepayment,
                        .scenari_prep = .scenari_prep,
                        .scenari_noprep = .scenari_noprep,
                        .notional = .notional,
                        .notional_prep = .notional_prep,
                        .notional_noprep = .notional_noprep,
                        .notional_base = .notional_base) %>%
    bind_rows()
  message('CALC 005: delta_pv')


  # ------------------- 006 CALCOLO ECAP --------------------------------------#

  .ecap <- do_ecap(.deltapv = .deltapv,
                  .mapping_entity = .mapping_entity,
                  .quantiles = c(.percentile1, .percentile2))
  message('CALC 006: ecap')

  # ------------------- 007 SELEZIONE CURVE ECAP ------------------------------#

  .curve <- do_selezione_curve_ecap(.ecap = .ecap,
                                   .curve_1y_interpol = .curve_1y_interpol)
  message('CALC 007: selezione_curve_ecap')

  # stop clusters
  stopCluster(cl) # kill cluster
  closeAllConnections()
  gc()

  return(list(delta_pv = .deltapv, ecap = .ecap, curve_var = .curve))
}
