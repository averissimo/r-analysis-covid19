top30.data <- function(dat, case.type) {
    case.type.str <- proper.cases(case.type, capitalize.all = TRUE)
    dat %>%
        dplyr::filter(type == case.type) %>%
        dplyr::group_by(state, type) %>%
        dplyr::summarise(cases = sum(cases)) %>%
        dplyr::arrange(cases) %>%
        dplyr::ungroup() %>%
        dplyr::filter(cases > 0) %>%
        dplyr::arrange(-cases) %>%
        dplyr::mutate(cases = format(cases, big.mark = ',')) %>%
        dplyr::select(!! region.code := state, !! case.type.str := cases) %>%
        return()
}


ommit.data <- function(dat, case.type, filter.states = c()) {
    if (!any(colnames(dat) == 'days.after.100')) {
        days.name <- 'Date'
        dat <- dat %>% dplyr::mutate(days.after.100 = date)
    } else {
        days.name <- 'Days'
    }

    case.type.name <- proper.cases(case.type, capitalize = TRUE)
    dat %>%
        dplyr::filter(length(filter.states) == 0 | state %in% filter.states) %>%
        dplyr::filter(type %in% c(case.type)) %>%
        dplyr::mutate(cumul.per.100k = cumul / population * 100000) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(desc(date), desc(cumul)) %>%
        dplyr::mutate(cases.per.100k = round(cases / population * 100000, digits = 3),
               population = format(population, big.mark = ','),
               cases = format(cases, big.mark = ','),
               cumul = format(cumul, big.mark = ',')) %>%
        dplyr::select(!! region.code := state,
               !! days.name := days.after.100,
                !! case.type.name := cases,
               'Sub-total' = cumul,
               'Sub-total per 100k' = cumul.per.100k,
               population = population) %>%
        return()
}

population.data <- function(dat, filter.states = c()) {
    dat %>%
        dplyr::filter(length(filter.states) == 0 | state %in% filter.states) %>%
        dplyr::ungroup() %>%
        dplyr::select(state, population) %>%
        distinct(state, .keep_all = TRUE) %>%
        dplyr::arrange(desc(population)) %>%
        dplyr::mutate(population = format(population, big.mark = ',', trim = FALSE)) %>%
        dplyr::select(!! region.code := state, Population = population) %>%
        return()
}

last.days.data <- function(dat, case.type, filter.states = c()) {
    case.type.name <- proper.cases(case.type, capitalize = TRUE)

    dat %>%
        dplyr::filter(length(filter.states) == 0 | state %in% filter.states) %>%
        dplyr::filter(type == 'confirmed') %>%
        dplyr::group_by(state) %>%
        dplyr::arrange(desc(date), desc(cases), state) %>%
        dplyr::mutate(!! region.code := state,
               !!case.type.name := format(cases, big.mark = ','),
               Date = format(date, '%B %d'),
               'Sub-total' = format(cumul, big.mark = ',')) %>%
        dplyr::ungroup() %>%
        dplyr::select(!! region.code, Date, !!case.type.name, 'Sub-total') %>%
        return()
}

cumulative.last.days.data <- function(dat, case.type, filter.states = c(), days) {
    cumul.name <- '{proper.cases(case.type, capitalize = TRUE)} in last {days} days' %>% glue::glue()
    value.name <- '{proper.cases(case.type, capitalize = TRUE)}' %>% glue::glue()
    if (case.type == 'confirmed') {
        dat.norm <- dat %>%
            dplyr::mutate(last.week.var = last.week.cases.confirmed,
                   cases = cases.confirmed)
    } else {
        dat.norm <- dat %>%
            dplyr::mutate(last.week.var = last.week.cases.death,
                   cases = cases.death)
    }

    dat.norm %>%
        dplyr::filter(state %in% filter.states) %>%
        dplyr::group_by(state) %>%
        dplyr::arrange(desc(date), state) %>%
        dplyr::mutate(date = format(date, '%B %d'),
               last.week.cases.confirmed = format(last.week.cases.confirmed, big.mark = ','),
               last.week.cases.death = format(last.week.cases.death, big.mark = ',')) %>%
        dplyr::select(!! region.code := state,
               Date = date,
               !! value.name := cases,
                !! cumul.name := last.week.var) %>%
        return()
}

death.vs.cases.data <- function(dat) {
    dat %>%
        dplyr::arrange(desc(ratio)) %>%
        dplyr::mutate(ratio = scales::percent(ratio, accuracy = .0001),
               death = format(cases.death / 100000 * population, big.mark = ','),
               confirmed = format(cases.confirmed / 100000 * population, big.mark = ','),
               cases.death = round(cases.death, digits = 4),
               cases.confirmed = round(cases.death, digits = 4),
               population = format(population, big.mark = ',')) %>%
        dplyr::select(!! region.code := state,
               'Death / Confirmed' = ratio,
               'Deaths' = death,
               'Deaths per 100k' = cases.death,
               'Confirmed' = confirmed,
               'Confirmed per 100k' = cases.confirmed,
               Population = population) %>%
        return()
}

something.vs.cases.data <- function(dat, name, state.filter = c()) {
    name.per <- '{name} per 100k' %>% glue::glue()
    cases.per.name <- 'Confirmed cases per {name}' %>% glue::glue()
    deaths.per.name <- 'Deaths per {name}' %>% glue::glue()
    dat %>%
        dplyr::filter(length(state.filter) == 0 | state %in% state.filter) %>%
        dplyr::mutate(!! region.code := state,
               Population = format(population, big.mark = ','),
               'Deaths per 100k' = round(cases.death, digits = 2),
               'Cases per 100k' = round(cases.confirmed, digits = 2),
               '(source year)' = as.numeric(date),
               !! name.per := value,
               !! cases.per.name := scales::percent(cases.confirmed / value, accuracy = .01),
               !! deaths.per.name := scales::percent(cases.death / value, accuracy = .01)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(desc(value)) %>%
        dplyr::select(!! region.code,
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
