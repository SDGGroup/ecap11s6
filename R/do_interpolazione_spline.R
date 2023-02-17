#' do_interpolazione_spline_
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
# do_interpolazione_spline <- function(.curve_1y) {
#
#   curve_1y_interpol <- curve_1y %>%
#     group_by(ID_YEAR, COD_VALUTA, ID_SCEN) %>%
#     # Returning more (or less) than 1 row per `summarise()` group was deprecated in dplyr 1.1.0.
#     # use reframe()
#     reframe(.interpolazione_spline(.x = ID_MESE_MAT, .y = VAL_TASSO, .max_x = max_x),
#               .groups = "drop") %>%
#     select(COD_VALUTA,
#            ID_YEAR,
#            ID_SCEN,
#            ID_MESE_MAT = x,
#            VAL_TASSO = y)
#
#   return(curve_1y_interpol)
#
# }


#' do_interpolazione_spline
#' @description
#' Esegue interpolazione spline su VAL_TASSO, per ogni combinazione di ID_YEAR,
#' COD_VALUTA e ID_SCEN.
#' @param .curve_1y tibble with 5 variables:
#' * COD_VALUTA chr,
#' * ID_MESE_MAT dbl,
#' * ID_YEAR dbl,
#' * ID_SCEN dbl,
#' * VAL_TASSO dbl.
#' @param max_x max x
#' @param .n_core n_core for multisession
#' @return tibble with 5 variables:
#' * COD_VALUTA chr,
#' * ID_MESE_MAT dbl,
#' * ID_YEAR dbl,
#' * ID_SCEN dbl,
#' * VAL_TASSO dbl.
#' @export
do_interpolazione_spline <- function(.curve_1y, .max_x, .n_core) {

  plan(multisession, workers = .n_core)
  curve_1y_interpol <- .curve_1y %>%
    group_split(ID_YEAR, COD_VALUTA, ID_SCEN_CLASS, .keep = TRUE) %>%
    future_map_dfr(function(.x) {
      .x %>%
        group_by(ID_YEAR, COD_VALUTA, ID_SCEN, ID_SCEN_CLASS) %>%
        reframe(.interpolazione_spline(.x = ID_MESE_MAT, .y = VAL_TASSO, .max_x = .max_x),.groups = "drop") }) %>%
    select(COD_VALUTA,
           ID_YEAR,
           ID_SCEN,
           ID_MESE_MAT = x,
           VAL_TASSO = y,
           ID_SCEN_CLASS)
  plan(sequential)

  # curve_1y_interpol_ex <- .curve_1y %>%
  #   group_by(ID_YEAR, COD_VALUTA, ID_SCEN) %>%
  #   reframe(.interpolazione_spline(.x = ID_MESE_MAT, .y = VAL_TASSO, .max_x = max_x),.groups = "drop") %>%
  #     select(COD_VALUTA,
  #            ID_YEAR,
  #            ID_SCEN,
  #            ID_MESE_MAT = x,
  #            VAL_TASSO = y)


  # identical(curve_1y_interpol_ex, curve_1y_interpol)

  return(curve_1y_interpol)

}
