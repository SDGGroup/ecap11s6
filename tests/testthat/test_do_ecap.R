test_that("do_ecap", {

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
  .deltapv <- do_deltapv(.formula_delta_pv = 'GESTIONALE',
                    .prepayment = 'SI',
                    .scenari_prep = .scenari_prep,
                    .scenari_noprep = .scenari_noprep,
                    .notional_prep = .notional_prep,
                    .notional_noprep = .notional_noprep,
                    .notional_base = notional_base_tst,
                    .curve_1y_interpol = .curve_1y_interpol)
  .percentile1 <- 0.999
  .percentile2 <- 0.960

  out <- do_ecap(.deltapv = .deltapv,
                  .mapping_entity = mapping_entity_tst,
                  .quantiles = c(.percentile1, .percentile2))

  expect_equal(nrow(out), 3428)
  expect_equal(sum(out$VAL_ECAP), 116566526907, tolerance = 1e-4)

})
