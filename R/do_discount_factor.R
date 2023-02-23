#' do_discount_factor
#' @description
#' Aggiunge DISCOUNT FACTOR.
#' @param .curve_1y_interpol tibble with 6 variables:
#' * COD_VALUTA chr,
#' * ID_YEAR int,
#' * ID_SCEN int,
#' * ID_SCEN_CLASS int,
#' * ID_MESE_MAT int,
#' * VAL_TASSO dbl.
#' @return tibble with 7 variables:
#' * COD_VALUTA chr,
#' * ID_YEAR int,
#' * ID_SCEN int,
#' * ID_SCEN_CLASS int,
#' * ID_MESE_MAT int,
#' * VAL_TASSO dbl,
#' * DISCOUNT_FACTOR dbl.
#' @export

do_discount_factor <- function(.curve_1y_interpol){
  .curve_1y_interpol %>%
    mutate(DISCOUNT_FACTOR = exp(-VAL_TASSO*ID_MESE_MAT/12)) %>%
    select(COD_VALUTA,
           ID_YEAR,
           ID_SCEN,
           ID_SCEN_CLASS,
           ID_MESE_MAT,
           VAL_TASSO,
           DISCOUNT_FACTOR)
}
