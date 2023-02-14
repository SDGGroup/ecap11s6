#' do_selezione_curve_ecap
#' @description
#' tba
#' @param ecap a tibble object with 9 variables:
#' * ID_YEAR dbl,
#' * COD_VALUTA chr,
#' * COD_ENTITY chr,
#' * ECAP dbl,
#' * VAL_PERCENTILE dbl,
#' * ID_SCEN dbl,
#' * DES_SHOCK_FINALE chr,
#' * DES_PREPAYMENT chr,
#' * COD_RIPARTIZIONE dbl.
#' @param .curve_1y_interpol tibble with 6 variables:
#' * COD_VALUTA chr,
#' * ID_MESE_MAT dbl,
#' * ID_YEAR dbl,
#' * ID_SCEN dbl,
#' * VAL_TASSO dbl.
#' * DISCOUNT_FACTOR dbl.
#' @return a list with 2 tibble, each with 5 variables:
#' * COD_VALUTA chr,
#' * ID_MESE_MAT dbl,
#' * ID_YEAR dbl,
#' * ID_SCEN dbl,
#' * VAL_TASSO dbl.
#' @export

do_selezione_curve_ecap <- function(.ecap, .curve_1y_interpol){

  .ecap %>%
    filter(COD_RIPARTIZIONE == 0 & !(COD_VALUTA =="TOT") )%>%
    left_join(.curve_1y_interpol, by = c( "ID_YEAR", "COD_VALUTA", "ID_SCEN")) %>%
    select(ID_YEAR,
           COD_VALUTA,
           COD_ENTITY,
           VAL_PERCENTILE,
           ID_SCEN,
           DES_SHOCK_FINALE,
           DES_PREPAYMENT,
           ID_MESE_MAT,
           VAL_TASSO)

}
