#' Get World Bank indicators by 100k population
#'
#' @param dat data frame with state column to be merged
#' @param indicator code of indicator
#' @param name name to replace on indicator
#' @param conver.names.fun function that receives country and maps to new name (present in state column of dat)
#'
#' @return data frame with world bank data and ratio do cases (and deaths) per the world bank indicator
#' @export
#'
#' @examples
#' wb.indicator('SH.MED.NUMW.P3')
wb.indicator <- function(dat, indicator, name, convert.names.fun = NULL) {
    renamed.100k <- glue::glue("{name} (per 100,000 people)")

    dta.original <- loose.rock::run.cache(wbstats::wb, indicator = indicator, show.message = FALSE)

    dta.norm <- dta.original %>%
        dplyr::mutate(value = value * 100, value,
               indicator = renamed.100k,
               state = country)

    if (!is.null(convert.names.fun)) {
        dta.norm <- dta.norm %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(state = convert.names.fun(state))    
    }
    
    dta <- dta.norm %>%
        dplyr::group_by(state) %>%
        dplyr::arrange(desc(date)) %>%
        dplyr::summarise(date = first(date),
                         value = first(value))

    dta.vs.cases <- dplyr::inner_join(dat, dta, by = 'state') %>%
        dplyr::mutate(ratio.confirmed = cases.confirmed / value) %>%
        dplyr::mutate(ratio.death = cases.death / value)

    return(dta.vs.cases)
}
