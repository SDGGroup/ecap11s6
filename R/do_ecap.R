#' do_ecap.R
#' @description
#' tba
#' @param .deltapv a tibble object with 7 variables:
#' * ID_YEAR dbl,
#' * COD_VALUTA chr,
#' * ID_SCEN dbl,
#' * DES_SHOCK_FINALE chr,
#' * COD_ENTITY chr,
#' * DELTA_PV dbl,
#' * DES_PREPAYMENT chr.
#' @param .mapping_entity
#' * COD_ENTITY chr,
#' * DES_ENTITY chr,
#' * COD_NU_TDB chr,
#' * COD_BU_RND chr,
#' * FLG_CAPOGRUPPO chr,
#' @param .quantiles vector of quantiles
#' @param .prepayment chr.
#' @return a tibble object with 9 variables:
#' * ID_YEAR dbl,
#' * COD_VALUTA chr,
#' * COD_ENTITY chr,
#' * ECAP dbl,
#' * VAL_PERCENTILE dbl,
#' * ID_SCEN dbl,
#' * DES_SHOCK_FINALE chr,
#' * DES_PREPAYMENT chr,
#' * COD_RIPARTIZIONE dbl.
#' @export

do_ecap <- function(.deltapv, .mapping_entity, .quantiles){

  ECAP <- .do_ecap_arricchito(.deltapv, .quantiles)

  ECAP_2 <- ECAP %>%
    group_by(ID_YEAR, COD_VALUTA, VAL_PERCENTILE, DES_PREPAYMENT) %>%
    mutate(ECAP_00001 = ECAP[COD_ENTITY == "00001"]) %>%
    filter(!COD_ENTITY %in% c("00001", "00005")) %>%
    mutate(peso = ECAP/sum(ECAP),
           ECAP = ECAP_00001*peso,
           COD_RIPARTIZIONE = 2) %>%
    ungroup() %>%
    select(ID_YEAR,
           COD_VALUTA,
           COD_ENTITY,
           ECAP,
           VAL_PERCENTILE,
           ID_SCEN,
           DES_SHOCK_FINALE,
           DES_PREPAYMENT,
           COD_RIPARTIZIONE)

  filiali_estere <- .mapping_entity %>%
    filter ( FLG_CAPOGRUPPO == 'N') %>%
    select (COD_ENTITY) %>%
    distinct() %>%
    pull()

  ECAP_1 <- ECAP %>%
    group_by(ID_YEAR, COD_VALUTA, VAL_PERCENTILE, DES_PREPAYMENT) %>%
    mutate(ECAP_00001 = ECAP[COD_ENTITY == "00001"]) %>%
    filter(COD_ENTITY %in% c("00005", filiali_estere)) %>%
    mutate(peso = ECAP/sum(ECAP),
           ECAP = ECAP_00001*peso,
           COD_RIPARTIZIONE = 1) %>%
    ungroup() %>%
    select(ID_YEAR,
           COD_VALUTA,
           COD_ENTITY,
           ECAP,
           VAL_PERCENTILE,
           ID_SCEN,
           DES_SHOCK_FINALE,
           DES_PREPAYMENT,
           COD_RIPARTIZIONE)

  ECAP <- ECAP %>%
    bind_rows(ECAP_1, ECAP_2)

  valuta_tot <- ECAP %>%
    group_by(ID_YEAR, COD_ENTITY, VAL_PERCENTILE, DES_SHOCK_FINALE, DES_PREPAYMENT, COD_RIPARTIZIONE) %>%
    summarise(ECAP = sum(ECAP)) %>%
    mutate(ID_SCEN = NA,
           COD_VALUTA = "TOT") %>%
    ungroup()

  ECAP <- ECAP %>%
    bind_rows(valuta_tot) %>%
    select(ID_YEAR,
           COD_VALUTA,
           COD_ENTITY,
           ECAP,
           VAL_PERCENTILE,
           ID_SCEN,
           DES_SHOCK_FINALE,
           DES_PREPAYMENT,
           COD_RIPARTIZIONE)

  return(ECAP)
}
