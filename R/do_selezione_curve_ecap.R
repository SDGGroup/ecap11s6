#' do_selezione_curve_ecap
#' @description
#' tba
#' @param ecap a tibble object with 9 variables:
#' * ID_YEAR int,
#' * COD_VALUTA chr,
#' * COD_ENTITY chr,
#' * VAL_ECAP dbl,
#' * VAL_PERCENTILE dbl,
#' * ID_SCEN int,
#' * DES_SHOCK_FINALE chr,
#' * DES_PREPAYMENT chr,
#' * COD_RIPARTIZIONE chr.
#' @param .curve_1y_interpol tibble with 7 variables:
#' * COD_VALUTA chr,
#' * ID_YEAR int,
#' * ID_SCEN int,
#' * ID_SCEN_CLASS int,
#' * ID_MESE_MAT int,
#' * VAL_TASSO dbl.
#' * DISCOUNT_FACTOR dbl.
#' @return tibble with 9 variables:
#' * ID_YEAR int,
#' * COD_VALUTA chr,
#' * COD_ENTITY chr,
#' * VAL_PERCENTILE dbl,
#' * ID_SCEN int,
#' * DES_SHOCK_FINALE chr,
#' * DES_PREPAYMENT chr,
#' * ID_MESE_MAT int,
#' * VAL_TASSO dbl.
#' @export

do_selezione_curve_ecap <- function(.ecap, .curve_1y_interpol){

  ecap <- .ecap %>%
    filter(COD_RIPARTIZIONE == 0 & !(COD_VALUTA =="TOT") )%>%
    left_join(.curve_1y_interpol, by = c( "ID_YEAR", "COD_VALUTA", "ID_SCEN"),
              multiple = "all") %>%
    select(ID_YEAR,
           COD_VALUTA,
           COD_ENTITY,
           VAL_PERCENTILE,
           ID_SCEN,
           DES_SHOCK_FINALE,
           DES_PREPAYMENT,
           ID_MESE_MAT,
           VAL_TASSO)

  # aggiungo curva today (ID_SCEN == 0) e la curva forward (ID_SCEN == -1)
  curve2add <- .curve_1y_interpol %>%
    filter(ID_SCEN == 0 | ID_SCEN == -1)

  ecap %>%
    bind_rows(curve2add) %>%
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
