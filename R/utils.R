#' .interpolazione_spline
#' @description
#' Esegue interpolazione spline.
#' @param .x vectors giving the x-coordinates of the points to be interpolated.
#' @param .y vectors giving the y-coordinates of the points to be interpolated.
#' @param .max_x set of values specifying where interpolation is to take place.
#' @return tibble with 2 variables:
#' * x dbl: x-coordinates of the interpolated points.
#' * y dbl: y-coordinates of the interpolated points.
#' @export
.interpolazione_spline <- function(.x, .y, .max_x) {

  xout <- seq_len(.max_x)

  out <- spline(.x, .y, method = "natural", xout = xout)

  out <- tibble(x = xout, y = out$y)

  out

}

#' .concorda_segno
#' @description
#' Restituisce +1 se il segno di x e y è concorde, -1 se non concorde.
#' @param .x dbl,
#' @param .y dbl,
#' @return +1 or -1.
#' @export
.concorda_segno <- function(x, y){

  sx <- sign(x)
  sx <- ifelse(sx == 0 , 1, sx)
  sy <- sign(y)
  sy <- ifelse(sy == 0 , 1, sy)
  sx * sy

}

#' .do_deltapv_gestionale
#' @description
#' Calcola DELTA_PV gestionale per ogni scenario contenuto in scenari, valuta ed
#' entity contenuta in notional.
#' @param .scenari tba
#' @param .notional tba
#' @param .curve_1y_interpol tba
#' @return a tibble tba
#' @export
.do_deltapv_gestionale <- function(.scenari, .notional, .curve_1y_interpol){

  scenari_notional <- .scenari %>%
    inner_join(.notional,
               by = c('COD_VALUTA' = 'COD_VALUTA_FINALE', 'DES_SHOCK_FINALE' = 'DES_SHOCK_FINALE')) %>%
    select(ID_YEAR, COD_VALUTA, ID_SCEN, DES_SHOCK_FINALE, COD_ENTITY, ID_MESE_MAT, VAL_NOTIONAL)
  # num = years*scenari*480*cod_entity

  scenari_notional_curve <- scenari_notional %>%
    left_join(.curve_1y_interpol, by = c("ID_YEAR", 'COD_VALUTA', 'ID_MESE_MAT', 'ID_SCEN')) %>%
    select(ID_YEAR, COD_VALUTA, ID_SCEN, DES_SHOCK_FINALE, COD_ENTITY, ID_MESE_MAT, VAL_NOTIONAL, DISCOUNT_FACTOR)

  discount_factor_0 <- .curve_1y_interpol %>%
    filter (ID_SCEN == 0) %>%
    select(ID_YEAR, COD_VALUTA,ID_MESE_MAT,DISCOUNT_FACTOR_0 = DISCOUNT_FACTOR)

  scenari_notional_curve_df0 <- scenari_notional_curve %>%
    left_join(discount_factor_0, by = c("ID_YEAR", 'COD_VALUTA','ID_MESE_MAT'))

  deltaPV <- scenari_notional_curve_df0 %>%
    mutate(PV_SIM = VAL_NOTIONAL * DISCOUNT_FACTOR,
           PV_0 = VAL_NOTIONAL * DISCOUNT_FACTOR_0) %>%
    group_by(ID_YEAR, COD_VALUTA , ID_SCEN , DES_SHOCK_FINALE,  COD_ENTITY ) %>%
    summarise(PV_SIM = sum(PV_SIM, na.rm  = TRUE),
              PV_0 = sum(PV_0),
              .groups = 'drop') %>%
    mutate(DELTA_PV = PV_SIM - PV_0) %>%
    select(ID_YEAR, COD_VALUTA, ID_SCEN, DES_SHOCK_FINALE, COD_ENTITY, DELTA_PV)

  return(deltaPV)
}

#' .do_deltapv_segnaletico
#' @description
#' Calcola DELTA_PV segnaletico per ogni scenario contenuto in scenari, valuta ed
#' entity contenuta in notional.
#' @param .scenari tba
#' @param .notional tba
#' @param .curve_1y_interpol tba
#' @return a tibble tba
#' @export
do_deltapv_segnaletico <- function(.scenari, .notional, .notional_base, .curve_1y_interpol){

  scenari_notional <- .scenari %>%
    inner_join(.notional,
               by = c('COD_VALUTA' = 'COD_VALUTA_FINALE', 'DES_SHOCK_FINALE' = 'DES_SHOCK_FINALE')) %>%
    select(ID_YEAR, COD_VALUTA, ID_SCEN, DES_SHOCK_FINALE, COD_ENTITY, ID_MESE_MAT, VAL_NOTIONAL) %>%
    left_join(.notional_base, by = c("COD_VALUTA" = "COD_VALUTA_FINALE", "COD_ENTITY", "ID_MESE_MAT")) %>%
    select(ID_YEAR, COD_VALUTA, ID_SCEN, DES_SHOCK_FINALE, COD_ENTITY, ID_MESE_MAT,
           VAL_NOTIONAL = VAL_NOTIONAL.x,
           VAL_NOTIONAL_BASE = VAL_NOTIONAL.y)

  scenari_notional_curve <- scenari_notional %>%
    left_join(.curve_1y_interpol, by = c("ID_YEAR", 'COD_VALUTA', 'ID_MESE_MAT', 'ID_SCEN')) %>%
    select(ID_YEAR, COD_VALUTA, ID_SCEN, DES_SHOCK_FINALE, COD_ENTITY, ID_MESE_MAT, VAL_NOTIONAL, VAL_NOTIONAL_BASE, DISCOUNT_FACTOR)

  discount_factor_0 <- .curve_1y_interpol %>%
    filter (ID_SCEN == 0) %>%
    select(ID_YEAR, COD_VALUTA,ID_MESE_MAT,DISCOUNT_FACTOR_0 = DISCOUNT_FACTOR)

  scenari_notional_curve_df0 <- scenari_notional_curve %>%
    left_join(discount_factor_0, by = c("ID_YEAR", 'COD_VALUTA','ID_MESE_MAT'))

  deltaPV <- scenari_notional_curve_df0 %>%
    mutate(PV_SIM = VAL_NOTIONAL * DISCOUNT_FACTOR,
           PV_0 = VAL_NOTIONAL_BASE * DISCOUNT_FACTOR_0) %>%
    group_by(ID_YEAR, COD_VALUTA , ID_SCEN , DES_SHOCK_FINALE,  COD_ENTITY ) %>%
    summarise(PV_SIM = sum(PV_SIM, na.rm  = TRUE),
              PV_0 = sum(PV_0),
              .groups = 'drop') %>%
    mutate(DELTA_PV = PV_SIM - PV_0) %>%
    select(ID_YEAR, COD_VALUTA, ID_SCEN, DES_SHOCK_FINALE, COD_ENTITY, DELTA_PV)

  return(deltaPV)
}
