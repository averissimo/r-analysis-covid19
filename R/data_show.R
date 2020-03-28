top30.data <- function(dat, case.type) {
    dat %>%
        filter(type == case.type) %>%
        group_by(state, type) %>%
        summarise(cases = sum(cases)) %>%
        arrange(cases) %>%
        ungroup() %>%
        filter(cases > 0) %>%
        arrange(-cases) %>%
        mutate(cases = format(cases, big.mark = ',')) %>%
        select(!! region.code := state, 'Cases Confirmed' = cases) %>%
        return()
}


ommit.data <- function(dat, case.type, filter.states = c()) {
    if (!any(colnames(dat) == 'days.after.100')) {
        days.name <- 'Date'
        dat <- dat %>% mutate(days.after.100 = date)
    } else {
        days.name <- 'Days'
    }

    case.type.name <- proper.cases(case.type, capitalize = TRUE)
    dat %>%
        filter(length(filter.states) == 0 | state %in% filter.states) %>%
        filter(type %in% c(case.type)) %>%
        mutate(cumul.per.100k = cumul / population * 100000) %>%
        ungroup %>%
        arrange(desc(date), desc(cumul)) %>%
        mutate(cases.per.100k = round(cases / population * 100000, digits = 3),
               population = format(population, big.mark = ','),
               cases = format(cases, big.mark = ','),
               cumul = format(cumul, big.mark = ',')) %>%
        select(!! region.code := state,
               !! days.name := days.after.100,
                !! case.type.name := cases,
               'Sub-total' = cumul,
               'Sub-total per 100k' = cumul.per.100k,
               population = population) %>%
        return()
}

population.data <- function(dat, filter.states = c()) {
    dat %>%
        filter(length(filter.states) == 0 | state %in% filter.states) %>%
        ungroup %>%
        select(state, population) %>%
        distinct(state, .keep_all = TRUE) %>%
        arrange(desc(population)) %>%
        mutate(population = format(population, big.mark = ',', trim = FALSE)) %>%
        select(!! region.code := state, Population = population) %>%
        return()
}

last.days.data <- function(dat, case.type, filter.states = c()) {
    case.type.name <- proper.cases(case.type, capitalize = TRUE)

    dat %>%
        filter(state %in% filter.states) %>%
        filter(type == 'confirmed') %>%
        group_by(state) %>%
        arrange(desc(date), desc(cases), state) %>%
        mutate(!! region.code := state,
               !!case.type.name := format(cases, big.mark = ','),
               Date = format(date, '%B %d'),
               'Sub-total' = format(cumul, big.mark = ',')) %>%
        ungroup() %>%
        select(!! region.code, Date, !!case.type.name, 'Sub-total') %>%
        return()
}

cumulative.last.days.data <- function(dat, case.type, filter.states = c(), days) {
    cumul.name <- '{proper.cases(case.type, capitalize = TRUE)} in last {days} days' %>% glue
    value.name <- '{proper.cases(case.type, capitalize = TRUE)}' %>% glue
    if (case.type == 'confirmed') {
        dat.norm <- dat %>%
            mutate(last.week.var = last.week.cases.confirmed,
                   cases = cases.confirmed)
    } else {
        dat.norm <- dat %>%
            mutate(last.week.var = last.week.cases.death,
                   cases = cases.death)
    }

    dat.norm %>%
        filter(state %in% filter.states) %>%
        group_by(state) %>%
        arrange(desc(date), state) %>%
        mutate(date = format(date, '%B %d'),
               last.week.cases.confirmed = format(last.week.cases.confirmed, big.mark = ','),
               last.week.cases.death = format(last.week.cases.death, big.mark = ',')) %>%
        select(!! region.code := state,
               Date = date,
               !! value.name := cases,
                !! cumul.name := last.week.var) %>%
        return()
}

death.vs.cases.data <- function(dat) {
    dat %>%
        arrange(desc(ratio)) %>%
        mutate(ratio = percent(ratio, accuracy = .0001),
               death = format(cases.death / 100000 * population, big.mark = ','),
               confirmed = format(cases.confirmed / 100000 * population, big.mark = ','),
               cases.death = round(cases.death, digits = 4),
               cases.confirmed = round(cases.death, digits = 4),
               population = format(population, big.mark = ',')) %>%
        select(!! region.code := state,
               'Death / Confirmed' = ratio,
               'Deaths' = death,
               'Deaths per 100k' = cases.death,
               'Confirmed' = confirmed,
               'Confirmed per 100k' = cases.confirmed,
               Population = population) %>%
        return()
}

something.vs.cases.data <- function(dat, name, state.filter = c()) {
    name.per <- '{name} per 100k' %>% glue
    cases.per.name <- 'Confirmed cases per {name}' %>% glue
    deaths.per.name <- 'Deaths per {name}' %>% glue
    dat %>%
        filter(length(state.filter) == 0 | state %in% state.filter) %>%
        mutate(!! region.code := state,
               Population = format(population, big.mark = ','),
               'Deaths per 100k' = round(cases.death, digits = 2),
               'Cases per 100k' = round(cases.confirmed, digits = 2),
               '(source year)' = as.numeric(date),
               !! name.per := value,
               !! cases.per.name := percent(cases.confirmed / value, accuracy = .01),
               !! deaths.per.name := percent(cases.death / value, accuracy = .01)) %>%
        ungroup %>%
        arrange(desc(value)) %>%
        select(!! region.code,
               Population,
               !! name.per,
               '(source year)' ,
               'Cases per 100k',
               'Deaths per 100k',
               !! cases.per.name,
               !! deaths.per.name) %>%
        return()
}

region <- function(num = 1, lower = FALSE) {
    if (num > 1 && lower) {
        return(tolower(region.code.plural))
    }  else if (num > 1 && !lower) {
        return(region.code.plural)
    }  else if (num <= 1 && lower) {
        return(tolower(region.code))
    }  else if (num <= 1 && !lower) {
        return(region.code)
    }
}
