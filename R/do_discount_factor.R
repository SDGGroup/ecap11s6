#' do_discount_factor
#' @description
#' Aggiunge DISCOUNT FACTOR.
#' @param .curve_1y_interpol tibble with 5 variables:
#' * COD_VALUTA chr,
#' * ID_MESE_MAT dbl,
#' * ID_YEAR dbl,
#' * ID_SCEN dbl,
#' * VAL_TASSO dbl.
#' @return tibble with 6 variables:
#' * COD_VALUTA chr,
#' * ID_MESE_MAT dbl,
#' * ID_YEAR dbl,
#' * ID_SCEN dbl,
#' * VAL_TASSO dbl.
#' * DISCOUNT_FACTOR dbl.
#' @export

do_discount_factor <- function(.curve_1y_interpol){
  .curve_1y_interpol %>%
    mutate(DISCOUNT_FACTOR = exp(-VAL_TASSO*ID_MESE_MAT/12)) %>%
    select(COD_VALUTA,
           ID_YEAR,
           ID_SCEN,
           ID_MESE_MAT,
           VAL_TASSO,
           DISCOUNT_FACTOR,
           ID_SCEN_CLASS)
}
