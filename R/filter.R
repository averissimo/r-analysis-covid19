filter.after <- function(dat, death.start, confirmed.start) {
    dat %>%
        #
        dplyr::filter((type == 'death' & cumul >= death.start ) |
                   (type == 'confirmed' & cumul >= confirmed.start) |
                   (type == 'recovered' & cumul >= 0)) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(days.after.100 = as.numeric(difftime(date, min(date), units = 'days'))) %>%
        return
}

filter.last.days <- function(dat, days) {
    dat %>%
        dplyr::arrange(state) %>%
        dplyr::mutate(days.before.now = as.numeric(difftime(date, max(date), units = 'days'))) %>%
        dplyr::filter(days.before.now > -days) %>%
        dplyr::arrange(date) %>%
        dplyr::group_by(state, type) %>%
        dplyr::mutate(cumul = cumsum(cases)) %>%
        dplyr::arrange(desc(date), state) %>%
        return()
}

filter.last.days.cumulative <- function(dat, days = 4, fun = mean) {

    last.week.tmp <- dat %>%
        fill.missing() %>% 
        dplyr::group_by(state, type) %>%
        dplyr::arrange(desc(date)) %>%
        dplyr::ungroup() %>%
        dplyr::select(state, date, type, cases, population, state.code)

    last.week <-
        # Join confirmed and death cases
        left_join(last.week.tmp %>% dplyr::filter(type == 'confirmed') %>% dplyr::select(-type),
                  last.week.tmp %>% dplyr::filter(type == 'death') %>% dplyr::select(-type),
                  by = c('state', 'state.code', 'date', 'population'),
                  suffix = c('.confirmed', '.death')) %>%
        #
        # Replace any NA from join
        dplyr::mutate(cases.confirmed = replace(cases.confirmed, is.na(cases.confirmed), 0),
               cases.death = replace(cases.death, is.na(cases.death), 0)) %>%
        #
        # Calculate cumulative sum over date
        dplyr::arrange(date) %>%
        dplyr::group_by(state, state.code) %>%
        #
        # calculate cumulative cases (i.e. all cases reported up to that day)
        dplyr::mutate(cumul.death = cumsum(cases.death),
               cumul.confirmed = cumsum(cases.confirmed)) %>%
        dplyr::arrange(desc(date)) %>%
        dplyr::mutate(last.week.cases.confirmed = rollapply(cases.confirmed, days, fun, align = 'left', fill = c(0,0,0), partial = TRUE) %>%
                   round()) %>%
        dplyr::mutate(last.week.cases.death = rollapply(cases.death, days, fun, align = 'left', fill = c(0,0,0), partial = TRUE) %>%
                   round()) %>%
        #
        #
        #left_join(populations, by = 'state') %>%
        dplyr::group_by(state, state.code) %>%
        dplyr::arrange(date) %>%
        dplyr::filter(difftime(date, anytime::anydate(date()) - params$last.days, units = 'days') >= 0) %>%
        #
        dplyr::mutate(deaths.per.100k = last.week.cases.death / population * 100000,
               confirmed.per.100k = last.week.cases.confirmed / population * 100000,
               ratio.deaths.per.confirmed = last.week.cases.death / last.week.cases.confirmed)

    return(last.week)
}

filter.death.vs.cases <- function(dat) {
    dat %>%
        dplyr::group_by(state, population) %>%
        dplyr::summarise(ratio = max(cumul.death) / max(cumul.confirmed),
                  cases.death = max(cumul.death / population * 100000),
                  cases.confirmed = max(cumul.confirmed / population * 100000)) %>%
        dplyr::filter(cases.death > 0)
}
