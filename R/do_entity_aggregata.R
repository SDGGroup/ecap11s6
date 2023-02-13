#' do_entity_aggregata
#' @description
#' Aggiunge entity aggregata al notional.
#' @param .notional tibble with 5 variables:
#' * COD_VALUTA_FINALE chr,
#' * COD_ENTITY chr,
#' * ID_MESE_MAT dbl,
#' * DES_SHOCK_FINALE dbl,
#' * VAL_NOTIONAL dbl
#' @param .mapping_shock tibble with 4 variables:
#' * DES_SHOCK_SHIFT_SENSITIVITY chr,
#' * DES_SHOCK_DISCOUNT_FACTOR chr,
#' * DES_SHOCK_FINALE dbl,
#' * VAL_SHOCK_NOMINALE_BPS dbl
#' @return tibble with 5 variables:
#' * COD_VALUTA_FINALE chr,
#' * COD_ENTITY chr,
#' * ID_MESE_MAT dbl,
#' * DES_SHOCK_FINALE dbl,
#' * VAL_NOTIONAL dbl
#' @export
do_entity_aggregata <- function(.notional, .mapping_entity) {

  .entity_capogruppo <- .mapping_entity %>%
    filter(FLG_CAPOGRUPPO == 'Y') %>%
    select(COD_ENTITY) %>%
    distinct()

  .totale00001 <- .notional %>%
    group_by(DES_SHOCK_FINALE,COD_VALUTA_FINALE,ID_MESE_MAT) %>%
    summarise(VAL_NOTIONAL = sum(VAL_NOTIONAL, na.rm = TRUE), .groups = 'drop') %>%
    mutate(COD_ENTITY = "00001")


  .totale00005 <- .notional %>%
    semi_join(.entity_capogruppo, by = "COD_ENTITY") %>%
    group_by(DES_SHOCK_FINALE,COD_VALUTA_FINALE,ID_MESE_MAT) %>%
    summarise(VAL_NOTIONAL = sum(VAL_NOTIONAL, na.rm = TRUE), .groups = 'drop') %>%
    mutate(COD_ENTITY = "00005")


  #accodiamo alla shift_aggregata
  .notional <- bind_rows(.notional, .totale00001, .totale00005)

  # return
  return(.notional)

}
