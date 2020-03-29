wb.indicator <- function(dat, indicator, name) {
    renamed.100k <- glue("{name} (per 100,000 people)")

    dta.original <- run.cache(wb, indicator = indicator, show.message = FALSE)

    dta.norm <- dta.original %>%
        mutate(value = value * 100, value,
               indicator = renamed.100k,
               state = country)

    for (ix.name in names(convert.names)) {
        dta.norm <- dta.norm %>%
            arrange(state) %>%
            ungroup %>%
            mutate(state = replace(state, state == ix.name, convert.names[[ix.name]]))
    }

    dta <- dta.norm %>%
        group_by(state) %>%
        arrange(desc(date)) %>%
        summarise(date = first(date),
                  value = first(value))

    dta.vs.cases <- inner_join(dat, dta, by = 'state') %>%
        mutate(ratio.confirmed = cases.confirmed / value) %>%
        mutate(ratio.death = cases.death / value)

    return(dta.vs.cases)
}
