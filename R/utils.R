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

#' .create_split_var
#' @description
#' Crea ID_SCEN_CLASS, bin var over ID_YEAR, COD_VALUTA, ID_SCEN
#' @param .curve tba tibble con ID_YEAR, COD_VALUTA, ID_SCEN
#' @param .n_split number of final bins
#' @export
.create_split_var <- function(.curve, .n_split){
  .curve <- .curve %>%
    left_join(.curve %>%
                select(ID_YEAR, COD_VALUTA, ID_SCEN) %>%
                distinct() %>%
                mutate(key_split = 1:n()) %>%
                mutate(ID_SCEN_CLASS = cut(key_split,.n_split,labels = FALSE))
    ) %>%
    select(-key_split)
  return(.curve)
}

