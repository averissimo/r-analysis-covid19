#' Download Germany data (DE)
#'
#' @return data frame with latest data
#' @export
#'
#' @examples
download.de.data <- function() {
    eu.de.raw <- read_csv('https://github.com/covid19-eu-zh/covid19-eu-data/raw/master/dataset/covid-19-de.csv')

    eu.de <- eu.de.raw %>%
        dplyr::mutate(nuts_1 = gsub('[\u00ad]', '', nuts_1)) %>%
        dplyr::filter(nuts_1 != 'Repatriierte') %>%
        dplyr::select(state = nuts_1, cases, deaths, date = datetime) %>%
        dplyr::group_by(state) %>%
        reshape2::melt(id.vars = c('state', 'date'), variable.name = 'type', value.name = 'cases') %>%
        dplyr::mutate(type = dplyr::if_else(type == 'cases', 'confirmed', 'death')) %>%
        dplyr::arrange(desc(date)) %>%
        dplyr::group_by(state, type) %>%
        dplyr::mutate(date = anytime::anydate(format(date, '%Y/%m/%d')),
               cumul = cases,
               state.code = 'DEU',
               # need to break down differences between days
               cases = zoo::rollapply(cases, 2, function(ix) { if(length(ix) <= 1) { return(ix) } else { ix[1] - sum(ix[-1]) } }, fill = c(0, 0, 0), align = 'left', partial = TRUE))

    return(list(data = eu.de, source = 'Robert Koch Institute'))
}

download.it.data <- function() {
    eu.raw <- readr::read_csv('https://github.com/pcm-dpc/COVID-19/raw/master/dati-regioni/dpc-covid19-ita-regioni.csv')

    it.nuts.codici <- it.nuts.codici %>%
        dplyr::filter(nuts_2 != 'ITH1')

    eu.data <- eu.raw %>%
        dplyr::mutate(codice_regione = as.double(codice_regione)) %>%
        left_join(it.nuts.codici, by = 'codice_regione') %>%
        dplyr::mutate(state = eurostat::label_eurostat(nuts_2, dic = 'geo'),
               date =  anytime::anydate(data),
               state.code = 'ITA') %>%
        dplyr::select(state,
               date,
               confirmed = totale_casi,
               death = deceduti,
               state.code) %>%
        dplyr::group_by(state, state.code, date) %>%
        dplyr::summarise(confirmed = sum(confirmed),
                  death = sum(death)) %>%
        reshape2::melt(id.vars = c('state', 'state.code', 'date'),
                       variable.name = 'type',
                       value.name = 'cases',
                       factorsAsStrings = FALSE) %>%
        tibble::tibble() %>%
        #
        dplyr::arrange(desc(date)) %>%
        dplyr::group_by(state, type) %>%
        dplyr::mutate(date = anytime::anydate(format(date, '%Y/%m/%d')),
               cumul = cases,
               cases = zoo::rollapply(cases, 2, function(ix) { if(length(ix) <= 1) { return(ix) } else { ix[1] - sum(ix[-1]) } }, fill = c(0, 0, 0), align = 'left', partial = TRUE))

    return(list(data = eu.data, source = 'IT PC'))
}

download.eurostat.population.nuts_3 <- function(country.code, nuts = 3) {
    nuts_converter <- list('1' = 3, '2' = 4, '3' = 5)
    nuts.label <- 'nuts_{nuts}' %>% glue::glue()
    nuts.label.2 <- 'nuts_{nuts}_label' %>% glue::glue()
    eu.population.raw <- suppressMessages(get_eurostat('cens_11ag_r3')) %>% tibble::tibble()

    eu.population.norm <- eu.population.raw %>%
        dplyr::filter(grepl('^{country.code}[0-9A-Za-z]{paste0("{", nuts, "}")}' %>% glue::glue(), geo)) %>%
        dplyr::filter(age == 'TOTAL' & sex == 'T') %>%
        dplyr::mutate(nuts = substr(geo, 0, nuts_converter[[nuts]])) %>%
        dplyr::mutate(nuts.label = eurostat::label_eurostat(nuts, dic = 'geo', fix_duplicated = TRUE)) %>%
        dplyr::group_by(nuts, nuts.label) %>%
        dplyr::filter(time == max(time)) %>%
        dplyr::summarise(values = sum(values))

    populations <- eu.population.norm %>%
        dplyr::ungroup() %>%
        dplyr::select(state = nuts.label, population = values) %>%
        return()
}

download.eurostat.population <- function(country.code, nuts = 1) {
    nuts_converter <- list('1' = 3, '2' = 4)

    eu.population.raw <- suppressMessages(eurostat::get_eurostat('tgs00096'))

    eu.population.norm <- eu.population.raw %>%
        dplyr::filter(grepl('^{country.code}' %>% glue::glue(), geo)) %>%
        dplyr::mutate(nuts_1 = substr(geo, 0, nuts_converter[[nuts]]),
               nuts_1_label = eurostat::label_eurostat(nuts_1, dic = 'geo')) %>%
        dplyr::group_by(nuts_1, nuts_1_label) %>%
        dplyr::filter(time == max(time)) %>%
        dplyr::summarise(values = sum(values))

    populations <- eu.population.norm %>%
        dplyr::ungroup() %>%
        dplyr::select(state = nuts_1_label, population = values)
}


download.world.data <- function() {
    dat <- NULL
    tryCatch(dat <- coronavirus::coronavirus, error = function(err) {})

    my.corona.original <- coronavirus %>% tibble::tibble() %>%
        dplyr::mutate(state = Country.Region) %>%
        dplyr::group_by(state, type, date) %>%
        dplyr::summarise(cases = sum(cases)) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(cumul = cumsum(cases))

    return(list(data = my.corona.original, source = 'RamiKrispin/coronavirus'))
}

download.nl.data <- function() {
    eu.nl.raw <- read_csv('https://github.com/covid19-eu-zh/covid19-eu-data/raw/master/dataset/covid-19-nl.csv')

    eu.nl.state <- eu.nl.raw %>%
        dplyr::select(country, state = lau, cases, deaths, date = datetime) %>%
        #    dplyr::filter(!is.na(state)) %>%
        dplyr::mutate(country = 'Netherlands') %>%
        dplyr::group_by(country, state) %>%
        reshape2::melt(id.vars = c('country', 'state', 'date'), variable.name = 'type', value.name = 'cases') %>%
        dplyr::mutate(type = dplyr::if_else(type == 'cases', 'confirmed', 'death')) %>%
        dplyr::arrange(desc(date)) %>%
        dplyr::group_by(country, state, type) %>%
        # need to break down differences between days
        dplyr::mutate(cases = rollapply(cases, 2, function(ix) { if(length(ix) <= 1) { return(ix) } else { ix[1] - sum(ix[-1]) } }, fill = c(0, 0, 0), align = 'left', partial = TRUE)) %>%
        dplyr::arrange(date) %>%
        dplyr::mutate(cumul = cumsum(cases))

    eu.nl <- eu.nl.state %>%
        dplyr::group_by(country, type, date) %>%
        dplyr::summarise(cases = sum(cases), cumul = sum(cumul)) %>%
        dplyr::arrange(date)

    list(data = eu.nl, source = '??nl??') %>%
        return()
}

download.pt.data <- function() {
    eu.data.raw <- covid19.pt.data::download.updated.pt()

    eu.data <- eu.data.raw$cdc.eu %>%
        dplyr::mutate(date = anytime::anydate(glue::glue('{year}/{month}/{day}')) - 1,
               state = countriesAndTerritories) %>%
        dplyr::select(state, date, cases, deaths, popData2018, state.code = countryterritoryCode) %>%
        dplyr::mutate(state = iconv(state, to = 'UTF-8')) %>%
        dplyr::arrange(date) %>%
        reshape2::melt(id.vars = c('state', 'state.code', 'popData2018', 'date'),
             variable.name = 'type',
             value.name = 'cases') %>%
        dplyr::mutate(type = dplyr::if_else(type == 'cases', 'confirmed', 'death')) %>%
        dplyr::group_by(state, state.code, type, date, popData2018) %>%
        dplyr::summarise(cases = sum(cases)) %>%
        dplyr::group_by(state, type) %>%
        dplyr::mutate(cumul = cumsum(cases)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(state = gsub('_', ' ', state)) %>%
        dplyr::select(state, date, type, cases, cumul, population = popData2018, state.code)

    list(data = eu.data, source = 'PT DGS') %>%
        return()
}

download.john.hopkins <- function() {
    base.url <- 'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series'
    jh.data.raw.deaths    <- glue::glue('{base.url}/time_series_covid19_deaths_global.csv') %>% read_csv()
    jh.data.raw.confirmed <- glue::glue('{base.url}/time_series_covid19_confirmed_global.csv')%>% read_csv()
    
    jh.data.deaths <- jh.data.raw.deaths %>%
        reshape2::melt(id.vars = c('Province/State', 'Country/Region', 'Lat', 'Long'),
                       variable.name = 'date',
                       value.name = 'cases') %>%
        tibble::tibble() %>%
        dplyr::mutate(type = 'deaths',
                      date = str_replace(date, '([0-9]+)/([0-9]+)/([0-9]+)', '20\\3-\\1-\\2'),
                      date = anytime::anydate(date))
    
    jh.data.confirmed <- jh.data.raw.confirmed %>%
        reshape2::melt(id.vars = c('Province/State', 'Country/Region', 'Lat', 'Long'),
                       variable.name = 'date',
                       value.name = 'cases') %>%
        tibble::tibble() %>%
        dplyr::mutate(type = 'confirmed',
                      date = str_replace(date, '([0-9]+)/([0-9]+)/([0-9]+)', '20\\3-\\1-\\2'),
                      date = anytime::anydate(date))
    
    jh.data <- bind_rows(jh.data.deaths, jh.data.confirmed) %>%
        dplyr::select(state = 'Country/Region', date, type, cases) %>%
        dplyr::group_by(state, date, type) %>%
        dplyr::summarise(cases = sum(cases)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(state = jh.convert.names(state)) %>%
        dplyr::mutate(state = eu.convert.names(state))
    
    pop.wb <- run.cache(wb, indicator = "SP.POP.TOTL", 
                        show.message = FALSE)
    
    if (pop.wb %>% nrow == 0) {
        pop.wb <- run.cache(wb, indicator = "SP.POP.TOTL", 
                            show.message = FALSE,
                            force.recalc = TRUE)
        if (pop.wb %>% nrow == 0) {
            stop('ERROR retrieving World Bank data, please run again.')
        }
    }
    
    pop.norm <- pop.wb %>%
        tibble::tibble() %>% 
        dplyr::group_by(iso3c) %>%
        dplyr::filter(date == max(date)) %>%
        dplyr::select(population = value, state = country, iso3c) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(state = eu.convert.names(state)) %>%
        dplyr::mutate(state = jh.convert.names(state)) %>%
        dplyr::arrange(state)
    
    jh.data.pop <- jh.data %>% left_join(pop.norm, by = 'state') %>%
        dplyr::group_by(state, type, population) %>%
        dplyr::arrange(desc(date)) %>%
        dplyr::mutate(cumul = cases) %>%
        dplyr::mutate(cases = zoo::rollapply(cases, 2, function(ix) { if(length(ix) <= 1) { return(ix) } else { ix[1] - sum(ix[-1]) } }, fill = c(0, 0, 0), align = 'left', partial = TRUE)) %>%
        dplyr::select(state, date, type, cases, cumul, population, state.code = iso3c)
    
    return(list(data = jh.data.pop, source = 'John Hopkins'))
}


download.eucdc.data <- function() {
    eu.data.raw <- read_csv('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv')

    eu.data <- eu.data.raw %>%
        dplyr::mutate(date = anytime::anydate(glue::glue('{year}/{month}/{day}')) - 1,
               state = countriesAndTerritories) %>%
        dplyr::select(state, date, cases, deaths, popData2018, state.code = countryterritoryCode) %>%
        dplyr::mutate(state = iconv(state, to = 'UTF-8')) %>%
        dplyr::arrange(date) %>%
        reshape2::melt(id.vars = c('state', 'state.code', 'popData2018', 'date'),
             variable.name = 'type',
             value.name = 'cases') %>%
        dplyr::mutate(type = dplyr::if_else(type == 'cases', 'confirmed', 'death')) %>%
        dplyr::group_by(state, state.code, type, date, popData2018) %>%
        dplyr::summarise(cases = sum(cases)) %>%
        dplyr::group_by(state, type) %>%
        dplyr::mutate(cumul = cumsum(cases)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(state = gsub('_', ' ', state)) %>%
        dplyr::select(state, date, type, cases, cumul, population = popData2018, state.code)

    list(data = eu.data, source = 'EU CDC') %>%
        return()
}
