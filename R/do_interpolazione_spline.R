#' do_interpolazione_spline
#' @description
#' Esegue interpolazione spline su VAL_TASSO, per ogni combinazione di ID_YEAR,
#' COD_VALUTA e ID_SCEN.
#' @param .curve tibble con 6 variabili:
#' * COD_VALUTA chr,
#' * ID_MESE_MAT int,
#' * ID_YEAR int,
#' * ID_SCEN int,
#' * VAL_TASSO dbl,
#' * ID_SCEN_CLASS int.
#' @param .max_x int
#' @return tibble with con 6 variabili:
#' * COD_VALUTA chr,
#' * ID_YEAR int,
#' * ID_SCEN int,
#' * ID_SCEN_CLASS int,
#' * ID_MESE_MAT int,
#' * VAL_TASSO dbl.
#' @export
do_interpolazione_spline <- function(.curve, .max_x) {

  curve_interpol <- .curve %>%
    group_by(ID_SCEN_CLASS, ID_YEAR, COD_VALUTA, ID_SCEN) %>%
    # Returning more (or less) than 1 row per `summarise()` group was deprecated in dplyr 1.1.0.
    # use reframe()
    reframe(.interpolazione_spline(.x = ID_MESE_MAT, .y = VAL_TASSO, .max_x = .max_x),
              .groups = "drop") %>%
    select(COD_VALUTA,
           ID_YEAR,
           ID_SCEN,
           ID_SCEN_CLASS,
           ID_MESE_MAT = x,
           VAL_TASSO = y)

  return(curve_interpol)

}

#' .interpolazione_spline
#' @description
#' Esegue interpolazione spline.
#' @param .x vettore con le x-coordinate dei punti da interpolare
#' @param .y vettore con le y-coordinate dei punti da interpolare
#' @param .max_x numero dei valori che specifica dove positionare l'interpolazione
#' @return tibble con 2 variabili:
#' * x dbl: x-coordinate dei punti interpolati
#' * y dbl: y-coordinate dei punti interpolati
#' @export
.interpolazione_spline <- function(.x, .y, .max_x) {
  
  xout <- seq_len(.max_x)
  
  out <- spline(.x, .y, method = "natural", xout = xout)
  
  out <- tibble(x = xout, y = out$y)
  
  out
  
}

