filter.after <- function(dat, death.start, confirmed.start) {
    dat %>%
        #
        filter((type == 'death' & cumul >= death.start ) |
                   (type == 'confirmed' & cumul >= confirmed.start) |
                   (type == 'recovered' & cumul >= 0)) %>%
        arrange(date) %>%
        mutate(days.after.100 = as.numeric(difftime(date, min(date), units = 'days'))) %>%
        return
}

filter.last.days <- function(dat, days) {
    dat %>%
        arrange(state) %>%
        mutate(days.before.now = as.numeric(difftime(date, max(date), units = 'days'))) %>%
        filter(days.before.now > -days) %>%
        arrange(date) %>%
        group_by(state, type) %>%
        mutate(cumul = cumsum(cases)) %>%
        arrange(desc(date), state) %>%
        return()
}

filter.last.days.cumulative <- function(dat, days = 4) {

    last.week.tmp <- dat %>%
        group_by(state, type) %>%
        arrange(desc(date)) %>%
        ungroup() %>%
        select(state, date, type, cases, population)

    last.week <-
        # Join confirmed and death cases
        left_join(last.week.tmp %>% filter(type == 'confirmed') %>% select(-type),
                  last.week.tmp %>% filter(type == 'death') %>% select(-type),
                  by = c('state', 'date', 'population'),
                  suffix = c('.confirmed', '.death')) %>%
        #
        # Replace any NA from join
        mutate(cases.confirmed = replace(cases.confirmed, is.na(cases.confirmed), 0),
               cases.death = replace(cases.death, is.na(cases.death), 0)) %>%
        #
        # Calculate cumulative sum over date
        arrange(date) %>%
        group_by(state) %>%
        #
        # calculate cumulative cases (i.e. all cases reported up to that day)
        mutate(cumul.death = cumsum(cases.death),
               cumul.confirmed = cumsum(cases.confirmed)) %>%
        arrange(desc(date)) %>%
        mutate(last.week.cases.confirmed = rollapply(cases.confirmed, days, sum, align = 'left', fill = c(0,0,0), partial = TRUE)) %>%
        mutate(last.week.cases.death = rollapply(cases.death, days, sum, align = 'left', fill = c(0,0,0), partial = TRUE)) %>%
        #
        #
        #left_join(populations, by = 'state') %>%
        group_by(state) %>%
        arrange(date) %>%
        filter(difftime(date, '2020-03-01', units = 'days') >= 0) %>%
        #
        mutate(deaths.per.100k = last.week.cases.death / population * 100000,
               confirmed.per.100k = last.week.cases.confirmed / population * 100000,
               ratio.deaths.per.confirmed = last.week.cases.death / last.week.cases.confirmed)

    return(last.week)
}

filter.death.vs.cases <- function(dat) {
    dat %>%
        group_by(state, population) %>%
        summarise(ratio = max(cumul.death) / max(cumul.confirmed),
                  cases.death = max(cumul.death / population * 100000),
                  cases.confirmed = max(cumul.confirmed / population * 100000)) %>%
        filter(cases.death > 0)
}
