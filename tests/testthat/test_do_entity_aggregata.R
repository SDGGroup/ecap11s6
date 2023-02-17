test_that("do_entity_aggregata", {
  .notional_diviso <- do_notional_prep_noprep(.notional = notional_tst)
  .notional_prep <- .notional_diviso$notional_prep
  .notional_noprep <- .notional_diviso$notional_noprep
  .out <- do_entity_aggregata(.notional_prep = .notional_prep,
                                   .notional_noprep = .notional_noprep,
                                   .mapping_entity = mapping_entity_tst)
  out_notional <- .out$notional

  out_notional_prep <- .out$notional_prep
  out_notional_totali <- .out$notional_totali

  expect_equal(nrow(out_notional), 122400)
  expect_equal(nrow(out_notional_prep), 84480)
  expect_equal(nrow(out_notional_totali), 15360)
  expect_equal(sum(out_notional$VAL_NOTIONAL), 79780732628)
  expect_equal(sum(out_notional_prep$VAL_NOTIONAL), 71404553303)
  expect_equal(sum(out_notional_totali$VAL_NOTIONAL), 21497499500)

})
