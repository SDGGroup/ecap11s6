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
