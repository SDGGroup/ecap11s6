#' do_interpolazione_curve
#' @description
#' Esegue interpolazione spline su VAL_TASSO, per ogni combinazione di ID_YEAR,
#' COD_VALUTA e ID_SCEN.
#' @param .curve_1y tibble with 5 variables:
#' * COD_VALUTA chr,
#' * ID_MESE_MAT dbl,
#' * ID_YEAR dbl,
#' * ID_SCEN dbl,
#' * VAL_TASSO dbl.
#' @return tibble with 5 variables:
#' * COD_VALUTA chr,
#' * ID_MESE_MAT dbl,
#' * ID_YEAR dbl,
#' * ID_SCEN dbl,
#' * VAL_TASSO dbl.
#' @export
do_interpolazione_spline <- function(.curve_1y) {

  curve_1y_interpol <- curve_1y %>%
    group_by(ID_YEAR, COD_VALUTA, ID_SCEN) %>%
    # Returning more (or less) than 1 row per `summarise()` group was deprecated in dplyr 1.1.0.
    # use reframe()
    reframe(.interpolazione_spline(.x = ID_MESE_MAT, .y = VAL_TASSO, .max_x = max_x),
              .groups = "drop") %>%
    select(COD_VALUTA,
           ID_YEAR,
           ID_SCEN,
           ID_MESE_MAT = x,
           VAL_TASSO = y)

  return(curve_1y_interpol)

}
