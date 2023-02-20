test_that("do_deltapv_gestionale", {

  .notional_diviso <- do_notional_prep_noprep(.notional = notional_tst)
  .notional_prep <- .notional_diviso$notional_prep
  .notional_noprep <- .notional_diviso$notional_noprep
  .notional_lst <- do_entity_aggregata(.notional_prep = .notional_prep,
                              .notional_noprep = .notional_noprep,
                              .mapping_entity = mapping_entity_tst)
  .notional <- .notional_lst$notional
  .notional_prep <- .notional_lst$notional_prep
  .curve_1y_interpol <- do_interpolazione_spline(.curve_1y = curve_1y_tst, .max_x = 480)
  .curve_1y_interpol <- do_discount_factor(.curve_1y_interpol)
  .selezione_scenario_shock <- do_selezione_scenario_shock(.curve_1y_interpol = .curve_1y_interpol,
                                                           .shock_effettivi = shock_effettivi_tst,
                                                           .prepayment = 'SI',
                                                           .scenario_no_prepayment = "100",
                                                           .mesi_tenor_prepayment = 180)
  .scenari_noprep <- .selezione_scenario_shock$scenari_noprep
  .scenari_prep <- .selezione_scenario_shock$scenari_prep
  out <- do_deltapv(.formula_delta_pv = 'GESTIONALE',
                    .prepayment = 'SI',
                    .scenari_prep = .scenari_prep,
                    .scenari_noprep = .scenari_noprep,
                    .notional_prep = .notional_prep,
                    .notional_noprep = .notional_noprep,
                    .notional_base = notional_base_tst,
                    .curve_1y_interpol = .curve_1y_interpol)

  expect_equal(nrow(out), 492000)
  expect_equal(sum(out$DELTA_PV), -1.8739e+12, tolerance = 1e-4)

})

# TODO: aggiornare quando nel notional_base ci sarà VAL_NOTIONAL_BASE per COD_ENTITY "00001"
# e COD_ENTITY "00005
test_that("do_deltapv_segnaletico", {

  .notional_diviso <- do_notional_prep_noprep(.notional = notional_tst)
  .notional_prep <- .notional_diviso$notional_prep
  .notional_noprep <- .notional_diviso$notional_noprep
  .notional_lst <- do_entity_aggregata(.notional_prep = .notional_prep,
                                       .notional_noprep = .notional_noprep,
                                       .mapping_entity = mapping_entity_tst)
  .notional <- .notional_lst$notional
  .notional_prep <- .notional_lst$notional_prep
  .curve_1y_interpol <- do_interpolazione_spline(.curve_1y = curve_1y_tst, .max_x = 480)
  .curve_1y_interpol <- do_discount_factor(.curve_1y_interpol)
  .selezione_scenario_shock <- do_selezione_scenario_shock(.curve_1y_interpol = .curve_1y_interpol,
                                                           .shock_effettivi = shock_effettivi_tst,
                                                           .prepayment = 'SI',
                                                           .scenario_no_prepayment = "100",
                                                           .mesi_tenor_prepayment = 180)
  .scenari_noprep <- .selezione_scenario_shock$scenari_noprep
  .scenari_prep <- .selezione_scenario_shock$scenari_prep
  out <- do_deltapv(.formula_delta_pv = 'SEGNALETICO',
                    .prepayment = 'SI',
                    .scenari_prep = .scenari_prep,
                    .scenari_noprep = .scenari_noprep,
                    .notional_prep = .notional_prep,
                    .notional_noprep = .notional_noprep,
                    .notional_base = notional_base_tst,
                    .curve_1y_interpol = .curve_1y_interpol)

  expect_equal(nrow(out), 492000)
  expect_equal(sum(out$DELTA_PV, na.rm = T), 1.922435e+12, tolerance = 1e-4)

})
