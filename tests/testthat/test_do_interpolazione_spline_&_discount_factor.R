test_that("do_interpolazione_spline_&_discount_factor", {

  .curve_1y_interpol <- do_interpolazione_spline(.curve = curve_1y_tst, .max_x = 480)
  out <- do_discount_factor(.curve_1y_interpol)

  expect_equal(nrow(out), 3843840)
  expect_equal(sum(out$VAL_TASSO), 101279, tolerance = 1e-4)
  expect_equal(sum(out$DISCOUNT_FACTOR), 2454330, tolerance = 1e-4)
  expect_equal(out %>% distinct(ID_SCEN) %>% count() %>% pull(), 1001)


})
