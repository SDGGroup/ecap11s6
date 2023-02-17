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

#' .do_ecap_arricchito
#' @description
#' Calcolo ECAP arricchito
#' @param .deltaPV tibble object with 7 variables:
#' * ID_YEAR dbl,
#' * COD_VALUTA chr,
#' * ID_SCEN dbl,
#' * DES_SHOCK_FINALE chr,
#' * COD_ENTITY chr,
#' * DELTA_PV dbl,
#' * DES_PREPAYMENT chr.
#' @param .quantiles tba
#' @return a tibble tba
#' @export
.do_ecap_arricchito <- function(.deltapv, .quantiles){

  # Calcoliamo l'Ecap con i percentili
  ECAP <- .deltapv %>%
    group_by(ID_YEAR, COD_VALUTA, COD_ENTITY, DES_PREPAYMENT) %>%
    reframe(num_scen = n(),
              # quantile ordine 3: seleziona sempre un dato presente e non una sua
              # statistica. (Nearest even order statistic with type 3)
              ECAP = quantile(-DELTA_PV, .quantiles, type = 3),
              VAL_PERCENTILE = .quantiles,
              .groups = "drop") %>%
    mutate(VAL_DELTA_PV = -ECAP)

  # leghiamo anche lo scenario corrispondente all'ECAP e lo shock corrispondente
  # all'ECAP
  # (N:B ci poterbbero essere del DELTA_PV duplicati, gestiamo con slice)
  ECAP_arricchito <- ECAP %>%
    inner_join(.deltapv, by = c("ID_YEAR",
                               "COD_VALUTA",
                               "COD_ENTITY",
                               "DES_PREPAYMENT",
                               "VAL_DELTA_PV" = "DELTA_PV")
    ) %>%
    group_by(ID_YEAR, COD_VALUTA, COD_ENTITY, VAL_PERCENTILE, DES_PREPAYMENT) %>%
    slice(1) %>%
    ungroup() %>%
    select(ID_YEAR, COD_VALUTA, COD_ENTITY, ECAP, VAL_PERCENTILE, ID_SCEN, DES_SHOCK_FINALE, DES_PREPAYMENT) %>%
    mutate(COD_RIPARTIZIONE = 0)

  return(ECAP_arricchito)

}
