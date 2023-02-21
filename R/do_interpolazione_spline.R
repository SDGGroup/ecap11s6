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

  list_split <- .curve_1y %>%
    group_by(ID_SCEN_CLASS) %>%
    group_split()

  cl <- makeCluster(.n_core)
  clusterEvalQ(cl, {library(dplyr)})
  clusterExport(cl, c(".interpolazione_spline",'.max_x'),envir = environment())
  curve_1y_interpol <- parLapply(cl, list_split, function(.x) {
    .x %>%
      group_by(ID_YEAR, COD_VALUTA, ID_SCEN_CLASS, ID_SCEN) %>%
      reframe(.interpolazione_spline(.x = ID_MESE_MAT, .y = VAL_TASSO, .max_x = .max_x), .groups='drop') %>%
      select(COD_VALUTA,
             ID_YEAR,
             ID_SCEN,
             ID_MESE_MAT = x,
             VAL_TASSO = y,
             ID_SCEN_CLASS)
  })
  stopCluster(cl) # kill cluster
  closeAllConnections()
  gc()
  curve_1y_interpol <- map_dfr(curve_1y_interpol, bind_rows)

  return(curve_1y_interpol)

}
