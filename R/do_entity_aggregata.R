#' do_entity_aggregata
#' @description
#' Aggiunge entity aggregata al notional.
#' @param .notional_prep tibble with 5 variables:
#' * COD_VALUTA_FINALE chr,
#' * COD_ENTITY chr,
#' * ID_MESE_MAT dbl,
#' * DES_SHOCK_FINALE dbl,
#' * VAL_NOTIONAL dbl.
#' @param .notional_noprep tibble with 5 variables:
#' * COD_VALUTA_FINALE chr,
#' * COD_ENTITY chr,
#' * ID_MESE_MAT dbl,
#' * DES_SHOCK_FINALE dbl,
#' * VAL_NOTIONAL dbl.
#' @param .mapping_shock tibble with 4 variables:
#' * DES_SHOCK_SHIFT_SENSITIVITY chr,
#' * DES_SHOCK_DISCOUNT_FACTOR chr,
#' * DES_SHOCK_FINALE dbl,
#' * VAL_SHOCK_NOMINALE_BPS dbl.
#' @return a list with 3 tibbles, each with 5 variables:
#' * COD_VALUTA_FINALE chr,
#' * COD_ENTITY chr,
#' * ID_MESE_MAT dbl,
#' * DES_SHOCK_FINALE dbl,
#' * VAL_NOTIONAL dbl.
#' @export
do_entity_aggregata <- function(.notional_prep, .notional_noprep, .mapping_entity) {

  # espansione notional_noprep
  elenco_shock <- .notional_prep %>%
    distinct(DES_SHOCK_FINALE) %>%
    pull()

  notional_espanso <- expand_grid(.notional_noprep, elenco_shock) %>%
    select(COD_VALUTA_FINALE,
           COD_ENTITY,
           ID_MESE_MAT,
           VAL_NOTIONAL,
           DES_SHOCK_FINALE = elenco_shock)

  notional <- bind_rows(.notional_prep, notional_espanso)

  entity_capogruppo <- .mapping_entity %>%
    filter(FLG_CAPOGRUPPO == 'Y') %>%
    select(COD_ENTITY) %>%
    distinct()

  totale00001 <- notional %>%
    group_by(DES_SHOCK_FINALE, COD_VALUTA_FINALE, ID_MESE_MAT) %>%
    summarise(VAL_NOTIONAL = sum(VAL_NOTIONAL, na.rm = TRUE), .groups = 'drop') %>%
    mutate(COD_ENTITY = "00001")


  totale00005 <- notional %>%
    semi_join(entity_capogruppo, by="COD_ENTITY") %>%
    group_by(DES_SHOCK_FINALE, COD_VALUTA_FINALE, ID_MESE_MAT) %>%
    summarise(VAL_NOTIONAL = sum(VAL_NOTIONAL,na.rm = TRUE), .groups = 'drop') %>%
    mutate(COD_ENTITY = "00005")

  #accodiamo alla shift_aggregata
  notional_prep <- bind_rows(.notional_prep, totale00001,totale00005)

  notional <- notional_prep %>%
    bind_rows(.notional_noprep) %>%
    select(COD_VALUTA_FINALE,
           COD_ENTITY,
           ID_MESE_MAT,
           DES_SHOCK_FINALE,
           VAL_NOTIONAL)

  notional_totali <- totale00001 %>%
    bind_rows(totale00005)

  # return
  return(list(notional = notional, notional_prep = notional_prep, notional_totali = notional_totali))

}
