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
#' @param .max_x int.
#' @param .n_core number of cores
#' @return tibble with 5 variables:
#' * COD_VALUTA chr,
#' * ID_MESE_MAT dbl,
#' * ID_YEAR dbl,
#' * ID_SCEN dbl,
#' * VAL_TASSO dbl.
#' @export
do_interpolazione_spline <- function(.curve_1y, .max_x, .n_core) {

  cl <- makeCluster(.n_core)
  curve_1y_interpol <- parLapply(cl, 
                                 .curve_1y %>% group_split(ID_SCEN_CLASS), 
                                 .do_interpolazione_spline,
                                 .max_x = .max_x) %>% bind_rows()
  stopCluster(cl) # kill cluster
  closeAllConnections()
  gc()

  return(curve_1y_interpol)

}


#' .do_interpolazione_spline
#' @description
#' Esegue interpolazione spline su VAL_TASSO, per ogni combinazione di ID_SCEN_CLASS
#' @param .curve tba
#' @param .max_x int.
#' @return a tibble tba
#' @export
.do_interpolazione_spline <- function(.curve, .max_x) {
  interpolazione <- .curve %>%
      group_by(ID_YEAR, COD_VALUTA, ID_SCEN_CLASS, ID_SCEN) %>%
      reframe(.interpolazione_spline(.x = ID_MESE_MAT, .y = VAL_TASSO, .max_x = .max_x), .groups='drop') %>%
      select(COD_VALUTA,
             ID_YEAR,
             ID_SCEN,
             ID_MESE_MAT = x,
             VAL_TASSO = y,
             ID_SCEN_CLASS)
  
  return(interpolazione)
  
}

