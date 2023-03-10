#' do_notional_prep_noprep
#' @description
#' Suddivide notional in notional_prep e notional_noprep.
#' @param .notional tibble con 5 variabili:
#' * COD_ENTITY chr,
#' * DES_SHOCK_FINALE chr,
#' * ID_MESE_MAT int,
#' * COD_VALUTA_FINALE chr,
#' * VAL_NOTIONAL dbl.
#' @return list con 2 tibbles, ognuno con 5 variabili:
#' * COD_VALUTA_FINALE chr,
#' * COD_ENTITY chr,
#' * ID_MESE_MAT int,
#' * DES_SHOCK_FINALE chr,
#' * VAL_NOTIONAL dbl.
#' @export

do_notional_prep_noprep <- function(.notional) {

  entity_valuta_prep <- .notional  %>%
    group_by(COD_VALUTA_FINALE, COD_ENTITY) %>%
    summarise(nshock = n_distinct(DES_SHOCK_FINALE), .groups = "drop") %>%
    filter(nshock > 1)

  entity_valuta_noprep <- .notional  %>%
    group_by(COD_VALUTA_FINALE, COD_ENTITY) %>%
    summarise(nshock = n_distinct(DES_SHOCK_FINALE), .groups = "drop") %>%
    filter(nshock == 1)

  notional_noprep <- .notional %>%
    inner_join(entity_valuta_noprep, by = c("COD_VALUTA_FINALE","COD_ENTITY")) %>%
    select(COD_VALUTA_FINALE,
           COD_ENTITY,
           ID_MESE_MAT,
           DES_SHOCK_FINALE,
           VAL_NOTIONAL)

  notional_prep <- .notional %>%
    inner_join(entity_valuta_prep, by = c("COD_VALUTA_FINALE","COD_ENTITY")) %>%
    select(COD_VALUTA_FINALE,
           COD_ENTITY,
           ID_MESE_MAT,
           DES_SHOCK_FINALE,
           VAL_NOTIONAL)

  return(list(notional_noprep = notional_noprep, notional_prep = notional_prep))
}
