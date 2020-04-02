download.de.data <- function() {
    eu.de.raw <- read_csv('https://github.com/covid19-eu-zh/covid19-eu-data/raw/master/dataset/covid-19-de.csv')

    eu.de <- eu.de.raw %>%
        mutate(nuts_1 = gsub('[\u00ad]', '', nuts_1)) %>%
        filter(nuts_1 != 'Repatriierte') %>%
        select(state = nuts_1, cases, deaths, date = datetime) %>%
        group_by(state) %>%
        melt(id.vars = c('state', 'date'), variable.name = 'type', value.name = 'cases') %>%
        mutate(type = if_else(type == 'cases', 'confirmed', 'death')) %>%
        arrange(desc(date)) %>%
        group_by(state, type) %>%
        mutate(date = anydate(format(date, '%Y/%m/%d')),
               cumul = cases,
               # need to break down differences between days
               cases = rollapply(cases, 2, function(ix) { if(length(ix) <= 1) { return(ix) } else { ix[1] - sum(ix[-1]) } }, fill = c(0, 0, 0), align = 'left', partial = TRUE))

    return(list(data = eu.de, source = 'Robert Koch Institute'))
}

download.it.data <- function() {
    eu.raw <- read_csv('https://github.com/pcm-dpc/COVID-19/raw/master/dati-regioni/dpc-covid19-ita-regioni.csv')

    it.nuts.codici <- it.nuts.codici %>%
        filter(nuts_2 != 'ITH1')

    eu.data <- eu.raw %>%
        mutate(codice_regione = as.double(codice_regione)) %>%
        left_join(it.nuts.codici, by = 'codice_regione') %>%
        mutate(state = label_eurostat(nuts_2, dic = 'geo'),
               date =  anydate(data),
               state.code = 'ITA') %>%
        select(state,
               date,
               confirmed = totale_casi,
               death = deceduti,
               state.code) %>%
        group_by(state, state.code, date) %>%
        summarise(confirmed = sum(confirmed),
                  death = sum(death)) %>%
        melt(id.vars = c('state', 'state.code', 'date'), variable.name = 'type', value.name = 'cases', factorsAsStrings = FALSE) %>%
        as_tibble %>%
        #
        arrange(desc(date)) %>%
        group_by(state, type) %>%
        mutate(date = anydate(format(date, '%Y/%m/%d')),
               cumul = cases,
               cases = rollapply(cases, 2, function(ix) { if(length(ix) <= 1) { return(ix) } else { ix[1] - sum(ix[-1]) } }, fill = c(0, 0, 0), align = 'left', partial = TRUE))

    return(list(data = eu.data, source = 'Protezione Civile'))
}

download.eurostat.population.nuts_3 <- function(country.code, nuts = 3) {
    nuts_converter <- list('1' = 3, '2' = 4, '3' = 5)
    nuts.label <- 'nuts_{nuts}' %>% glue
    nuts.label.2 <- 'nuts_{nuts}_label' %>% glue
    eu.population.raw <- suppressMessages(get_eurostat('cens_11ag_r3')) %>% tibble

    eu.population.norm <- eu.population.raw %>%
        filter(grepl('^{country.code}[0-9A-Za-z]{paste0("{", nuts, "}")}' %>% glue, geo)) %>%
        filter(age == 'TOTAL' & sex == 'T') %>%
        mutate(nuts = substr(geo, 0, nuts_converter[[nuts]])) %>%
        mutate(nuts.label = label_eurostat(nuts, dic = 'geo', fix_duplicated = TRUE)) %>%
        group_by(nuts, nuts.label) %>%
        filter(time == max(time)) %>%
        summarize(values = sum(values))

    populations <- eu.population.norm %>%
        ungroup() %>%
        select(state = nuts.label, population = values) %>%
        return()
}

download.eurostat.population <- function(country.code, nuts = 1) {
    nuts_converter <- list('1' = 3, '2' = 4)

    eu.population.raw <- suppressMessages(get_eurostat('tgs00096'))

    eu.population.norm <- eu.population.raw %>%
        filter(grepl('^{country.code}' %>% glue, geo)) %>%
        mutate(nuts_1 = substr(geo, 0, nuts_converter[[nuts]]),
               nuts_1_label = label_eurostat(nuts_1, dic = 'geo')) %>%
        group_by(nuts_1, nuts_1_label) %>%
        filter(time == max(time)) %>%
        summarize(values = sum(values))

    populations <- eu.population.norm %>%
        ungroup() %>%
        select(state = nuts_1_label, population = values)
}


download.world.data <- function() {
    data('coronavirus')

    my.corona.original <- coronavirus %>% as_tibble() %>%
        mutate(state = Country.Region) %>%
        group_by(state, type, date) %>%
        summarise(cases = sum(cases)) %>%
        arrange(date) %>%
        mutate(cumul = cumsum(cases))

    return(list(data = my.corona.original, source = 'RamiKrispin/coronavirus'))
}

download.nl.data <- function() {
    eu.nl.raw <- read_csv('https://github.com/covid19-eu-zh/covid19-eu-data/raw/master/dataset/covid-19-nl.csv')

    eu.nl.state <- eu.nl.raw %>%
        select(country, state = lau, cases, deaths, date = datetime) %>%
        #    filter(!is.na(state)) %>%
        mutate(country = 'Netherlands') %>%
        group_by(country, state) %>%
        melt(id.vars = c('country', 'state', 'date'), variable.name = 'type', value.name = 'cases') %>%
        mutate(type = if_else(type == 'cases', 'confirmed', 'death')) %>%
        arrange(desc(date)) %>%
        group_by(country, state, type) %>%
        # need to break down differences between days
        mutate(cases = rollapply(cases, 2, function(ix) { if(length(ix) <= 1) { return(ix) } else { ix[1] - sum(ix[-1]) } }, fill = c(0, 0, 0), align = 'left', partial = TRUE)) %>%
        arrange(date) %>%
        mutate(cumul = cumsum(cases))

    eu.nl <- eu.nl.state %>%
        group_by(country, type, date) %>%
        summarize(cases = sum(cases), cumul = sum(cumul)) %>%
        arrange(date)

    list(data = eu.nl, source = '??nl??') %>%
        return()
}

download.eucdc.data <- function() {
    eu.data.raw <- read_csv('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv')

    eu.data <- eu.data.raw %>%
        mutate(date = anydate(glue('{year}/{month}/{day}')) - 1,
               state = countriesAndTerritories) %>%
        select(state, date, cases, deaths, popData2018, state.code = countryterritoryCode) %>%
        mutate(state = iconv(state, to = 'UTF-8')) %>%
        arrange(date) %>%
        melt(id.vars = c('state', 'state.code', 'popData2018', 'date'),
             variable.name = 'type',
             value.name = 'cases') %>%
        mutate(type = if_else(type == 'cases', 'confirmed', 'death')) %>%
        group_by(state, state.code, type, date, popData2018) %>%
        summarise(cases = sum(cases)) %>%
        group_by(state, type) %>%
        mutate(cumul = cumsum(cases)) %>%
        ungroup() %>%
        mutate(state = gsub('_', ' ', state)) %>%
        select(state, date, type, cases, cumul, population = popData2018, state.code)

    list(data = eu.data, source = 'EU CDC') %>%
        return()
}
