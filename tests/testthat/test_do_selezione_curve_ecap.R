test_that("do_selezione_curve_ecap", {

  .notional_diviso <- do_notional_prep_noprep(.notional = notional_tst)
  .notional_prep <- .notional_diviso$notional_prep
  .notional_noprep <- .notional_diviso$notional_noprep
  .notional_lst <- do_entity_aggregata(.notional_prep = .notional_prep,
                                       .notional_noprep = .notional_noprep,
                                       .mapping_entity = mapping_entity_tst)
  .notional <- .notional_lst$notional
  .notional_prep <- .notional_lst$notional_prep
  .curve_1y_interpol <- do_interpolazione_spline(.curve = .create_split_var(curve_1y_tst, 100), .max_x = 480)
  .curve_1y_interpol <- do_discount_factor(.curve_1y_interpol)
  .selezione_scenario_shock <- do_selezione_scenario_shock(.curve_interpol = .curve_1y_interpol,
                                                           .curve_interpol_scen0 =  .curve_1y_interpol %>% filter(ID_SCEN == 0),
                                                           .shock_effettivi = shock_effettivi_tst,
                                                           .prepayment = 'SI',
                                                           .scenario_no_prepayment = "100",
                                                           .mesi_tenor_prepayment = 180)
  .scenari_noprep <- .selezione_scenario_shock$scenari_noprep
  .scenari_prep <- .selezione_scenario_shock$scenari_prep
  .deltapv <- do_deltapv(.curve_interpol = .curve_1y_interpol,
                         .curve_interpol_scen0 =  .curve_1y_interpol %>% filter(ID_SCEN == 0),
                         .formula_delta_pv = 'GESTIONALE',
                         .prepayment = 'SI',
                         .scenari_prep = .scenari_prep,
                         .scenari_noprep = .scenari_noprep,
                         .notional = .notional,
                         .notional_prep = .notional_prep,
                         .notional_noprep = .notional_noprep,
                         .notional_base = notional_base_tst)
  .percentile1 <- 0.999
  .percentile2 <- 0.960

  .ecap <- do_ecap(.deltapv = .deltapv,
                  .mapping_entity = mapping_entity_tst,
                  .quantiles = c(.percentile1, .percentile2))

  out <- do_selezione_curve_ecap(.ecap = .ecap,
                                 .curve_1y_interpol = .curve_1y_interpol)

  expect_equal(nrow(out), 476160)
  expect_equal(sum(out$VAL_TASSO), 13648, tolerance = 1e-4)

})
