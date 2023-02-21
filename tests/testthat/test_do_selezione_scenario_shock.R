test_that("do_selezione_scenario_shock", {

  .curve_1y_interpol <- do_interpolazione_spline(.curve_1y = curve_1y_tst, .max_x = 480, .n_core = 5)
  out <- do_selezione_scenario_shock(.curve_1y_interpol = .curve_1y_interpol,
                                     .shock_effettivi = shock_effettivi_tst,
                                     .prepayment = 'SI',
                                     .scenario_no_prepayment = "100",
                                     .mesi_tenor_prepayment = 180,
                                     .n_core = 5)

  expect_equal(nrow(out$scenari_prep), 8000)
  expect_equal(nrow(out$scenari_noprep), 8000)
  expect_equal(out$scenari_prep %>% distinct(ID_SCEN) %>% count() %>% pull(), 1000)
  expect_equal(out$scenari_noprep %>% distinct(ID_SCEN) %>% count() %>% pull(), 1000)

})
