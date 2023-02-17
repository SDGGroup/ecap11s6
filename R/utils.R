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
