test_that("do_selezione_scenario_shock", {

  .curve_1y_interpol <- do_interpolazione_spline(.curve = .create_split_var(curve_1y_tst, 100), .max_x = 480)
  out <- do_selezione_scenario_shock(.curve_interpol = .curve_1y_interpol,
                                     .curve_interpol_scen0 =  .curve_1y_interpol %>% filter(ID_SCEN == 0),
                                     .shock_effettivi = shock_effettivi_tst,
                                     .prepayment = 'SI',
                                     .scenario_no_prepayment = "100",
                                     .mesi_tenor_prepayment = 180)

  # expect_equal(nrow(out$scenari_prep), 8000)
  # expect_equal(nrow(out$scenari_noprep), 8000)
  # expect_equal(out$scenari_prep %>% distinct(ID_SCEN) %>% count() %>% pull(), 1000)
  # expect_equal(out$scenari_noprep %>% distinct(ID_SCEN) %>% count() %>% pull(), 1000)
  
  expect_equal(nrow(out$scenari_prep), 800)
  expect_equal(nrow(out$scenari_noprep), 800)
  expect_equal(out$scenari_prep %>% distinct(ID_SCEN) %>% count() %>% pull(), 100)
  expect_equal(out$scenari_noprep %>% distinct(ID_SCEN) %>% count() %>% pull(), 100)

})
