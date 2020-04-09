download.us.data <- function(by.state = FALSE) {
    json_data <- httr::GET('https://covidtracking.com/api/us/daily') %>% 
        httr::content('text') %>% 
        rjson::fromJSON()
    
    us.data.raw <- tibble::tibble()
    for (ix in seq_along(json_data)) {
        for (ix.name in names(json_data[[ix]])) {
            if (is.null(json_data[[ix]][[ix.name]])) {
                json_data[[ix]][[ix.name]] <- NA
            }
        }
        us.data.raw <- us.data.raw %>% 
            bind_rows(json_data[[ix]])
    }
    
    pop.wb <- run.cache(wb, indicator = "SP.POP.TOTL", 
                        show.message = FALSE) %>% 
        filter(country == 'United States') %>% 
        top_n(1, date) %>% 
        pull(value) %>% 
        pluck(1)
    
    us.data <- us.data.raw %>% 
        mutate(date = anytime::anydate(as.character(date)),
               state = 'USA') %>% 
        select(state,
               date,
               death = deathIncrease,
               confirmed = positiveIncrease,
               recovered,
               hospitalized = hospitalizedCumulative) %>% 
        arrange(desc(date)) %>% 
        mutate(hospitalized = zoo::rollapply(hospitalized, 2, function(ix) { if(length(ix) <= 1) { return(ix) } else { ix[1] - sum(ix[-1]) } }, fill = c(0, 0, 0), align = 'left', partial = TRUE)) %>% 
        reshape2::melt(id.vars = c('state', 'date'), variable.name = 'type', value.name = 'cases') %>% 
        filter(!is.na(cases)) %>% 
        tibble() %>% 
        group_by(state, type) %>% 
        dplyr::arrange(date) %>% 
        dplyr::mutate(cumul = cumsum(cases),
                      state.code = state,
                      population = pop.wb)
    
    source.date <- format(max(us.data %>% dplyr::filter(cases > 0) %>% dplyr::pull(date) %>% max), '%Y/%m/%d')
    
    if(by.state){
        stop('We don\'t have data by state implemented')
    } else {
        
    }
    
    return(list(data = us.data, source = '{source.date} (USA CDC)' %>% glue::glue()))
}

#' Download Germany data (DE)
#'
#' @return data frame with latest data
#' @export
download.de.data <- function(by.federal.state = FALSE) {
    
    pop.raw <- download.eurostat.population('DE', 1)
    
    #
    ## Package covid19.de.data does not provide accurate last few days
    #
    eu.de.raw2 <- covid19.de.data::update_dataset.no.age()

    eu.data2 <- eu.de.raw2 %>%
        arrange(desc(date), state) %>%
        dplyr::group_by(state, date) %>%
        dplyr::summarise(cases = sum(cases.sum), 
                         deaths = sum(deaths.sum)) %>%
        arrange(desc(date), state) %>%
        select(state, date, cases, deaths) %>%
        arrange(date) %>%
        reshape2::melt(id.vars = c('state', 'date'), 
                       measure.vars = c('deaths', 'cases'), 
                       variable.name = 'type', 
                       value.name = 'cases') %>%
        tibble::tibble() %>%
        dplyr::mutate(type = dplyr::if_else(type == 'cases', 'confirmed', 'death'),
                      date = date - 1) %>%
        dplyr::mutate(state.code = state) %>%
        dplyr::group_by(state, type) %>%
        dplyr::arrange(state, desc(date)) %>% 
        dplyr::mutate(state.code = state,
                       # need to break down differences between days
                      cases = zoo::rollapply(cases, 2, function(ix) { if(length(ix) <= 1) { return(ix) } else { ix[1] - sum(ix[-1]) } }, fill = c(0, 0, 0), align = 'left', partial = TRUE)) %>% 
        dplyr::filter(cases > 0) %>% 
        dplyr::arrange(desc(date), state)

    eu.data2.pop <- eu.data2 %>% left_join(pop.raw, by = c('state')) %>% 
        mutate(nuts.code = nuts) %>% 
        select(-nuts)
    
    #
    #
    eu.de.raw1 <- readr::read_csv('https://github.com/covid19-eu-zh/covid19-eu-data/raw/master/dataset/covid-19-de.csv')
    
    eu.data1 <- eu.de.raw1 %>%
        dplyr::mutate(datetime = anytime::anydate(datetime) - 1,) %>% 
        dplyr::mutate(nuts_1 = gsub('[\u00ad]', '', nuts_1)) %>%
        dplyr::filter(nuts_1 != 'Repatriierte') %>%
        dplyr::select(state = nuts_1, cases, deaths, date = datetime) %>%
        dplyr::group_by(state) %>%
        reshape2::melt(id.vars = c('state', 'date'), variable.name = 'type', value.name = 'cases') %>%
        tibble::tibble() %>% 
        dplyr::mutate(type = dplyr::if_else(type == 'cases', 'confirmed', 'death')) %>%
        dplyr::arrange(desc(date)) %>%
        dplyr::group_by(state, type) %>%
        dplyr::mutate(state.code = state,
               # need to break down differences between days
               cases = zoo::rollapply(cases, 2, function(ix) { if(length(ix) <= 1) { return(ix) } else { ix[1] - sum(ix[-1]) } }, fill = c(0, 0, 0), align = 'left', partial = TRUE)) %>% 
        dplyr::filter(cases > 0) %>% 
        dplyr::arrange(state, desc(date))
    
    eu.data1.pop <- eu.data1 %>% left_join(pop.raw, by = c('state')) %>% 
        mutate(nuts.code = nuts) %>% 
        select(-nuts)
    
    max.2 <- max(eu.data2.pop %>% filter(cases != 0) %>% pull(date))
    max.1 <- max(eu.data1.pop %>% filter(cases != 0) %>% pull(date))
    if (max.2 >= max.1) {
        futile.logger::flog.info('DE: Using district data')
        eu.data <- eu.data2.pop
    } else {
        futile.logger::flog.info('DE: Using state data')
        eu.data <- eu.data1.pop
    }
    
    if (!by.federal.state) {
        eu.data.out <- eu.data %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(state = 'Germany', state.code = 'DEU') %>% 
            dplyr::mutate(population = sum(pop.raw$population))
    } else{
        eu.data.out <- eu.data
    }
    
    source.date <- format(max(eu.data.out$date), '%Y/%m/%d')
    
    eu.data.out.cumul <- eu.data.out %>% 
        dplyr::group_by(state, type, state.code, population, date) %>% 
        dplyr::summarise(cases = sum(cases)) %>% 
        #
        dplyr::group_by(state, type, state.code, population) %>% 
        dplyr::arrange(date) %>% 
        dplyr::mutate(cumul = cumsum(cases)) %>% 
        #
        dplyr::select(state, date, type, cases, cumul, population, state.code) %>% 
        dplyr::arrange(desc(date))
        
    
    return(list(data = eu.data.out.cumul, source = '{source.date} (DE RKI)' %>% glue::glue()))
}

#' Download world data from Spain's Ministerio de Sanidad
#'
#' @return a list with data and source string
#' @export
download.es.data <- function(by.state = FALSE) {
    es.raw <- readr::read_csv('https://covid19.isciii.es/resources/serie_historica_acumulados.csv') %>% 
        filter(!is.na(FECHA))
    
    es.data <- es.raw %>% 
        dplyr::mutate(date = stringr::str_replace(FECHA, '([0-9]+)/([0-9]+)/([0-9]+)', '\\3-\\2-\\1') %>% anytime::anydate() + 1,
                      confirmed = CASOS,
                      death = Fallecidos,
                      recovered = Recuperados,
                      icu = UCI,
                      hospitalized = Hospitalizados) %>% 
        dplyr::select(state = CCAA, date, confirmed, death, recovered, icu, hospitalized) %>% 
        dplyr::mutate_if(~any(is.na(.x)), ~dplyr::if_else(is.na(.x), 0, .x)) %>% # missing values
        reshape2::melt(id.vars = c('state', 'date'), variable.name = 'type', value.name = 'cases') %>% 
        dplyr::arrange(desc(date)) %>% 
        dplyr::group_by(state, type) %>% 
        dplyr::mutate(cases = zoo::rollapply(cases, 2, function(ix) { if(length(ix) <= 1) { return(ix) } else { ix[1] - sum(ix[-1]) } }, fill = c(0, 0, 0), align = 'left', partial = TRUE),
                      cumul = 0)
    
    source.date <- format(max(es.data %>% dplyr::filter(cases > 0) %>% dplyr::pull(date) %>% max), '%Y/%m/%d')
    
    pop.raw <- download.eurostat.population('ES', 2)
    
    if (by.state) {
        es.data.norm <- es.data %>% 
            dplyr::left_join(es.nuts.ccaa, by = c('state' = 'CCAA')) %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(state = nuts_label,
                   nuts = nuts_code,
                   state.code = state) %>% 
            dplyr::select(-nuts_label, -nuts_code) %>% 
            dplyr::left_join(pop.raw, by = c('state', 'nuts')) %>% 
            dplyr::select(state, date, type, cases, cumul, population, state.code, nuts)
    } else {
        pop <- pop.raw %>% dplyr::pull(population) %>% sum()
        es.data.norm <- es.data %>%
            dplyr::ungroup() %>% 
            mutate(state = 'Spain') %>% 
            dplyr::group_by(state, date, type) %>% 
            dplyr::arrange(date) %>% 
            dplyr::summarise(cases = sum(cases),
                             cumul = sum(cumul)) %>% 
            dplyr::mutate(population = pop,
                          state.code = 'ESP') %>% 
            dplyr::select(state, date, type, cases, cumul, population, state.code)
    }
    
    es.data.out <- es.data.norm %>% 
        dplyr::group_by(state, type) %>%
        dplyr::arrange(date) %>% 
        dplyr::mutate(cumul = cumsum(cases))
    
    return(list(data = es.data.out, source = '{source.date} (ES ISCIII)' %>% glue::glue()))
}

#' Download world data from Italy's Protezione Civile
#'
#' @return a list with data and source string
#' @export
download.it.data <- function(by.state = TRUE) {
    eu.raw <- readr::read_csv('https://github.com/pcm-dpc/COVID-19/raw/master/dati-regioni/dpc-covid19-ita-regioni.csv')

    it.nuts.codici <- it.nuts.codici %>%
        dplyr::filter(nuts_2 != 'ITH1')

    eu.data <- eu.raw %>%
        dplyr::mutate(codice_regione = as.double(codice_regione)) %>%
        left_join(it.nuts.codici, by = 'codice_regione') %>%
        dplyr::mutate(state = eurostat::label_eurostat(nuts_2, dic = 'geo'),
               date =  anytime::anydate(data)) %>%
        dplyr::select(state,
               date,
               confirmed = totale_casi,
               death = deceduti) %>%
        dplyr::group_by(state, date) %>%
        dplyr::summarise(confirmed = sum(confirmed),
                  death = sum(death)) %>%
        reshape2::melt(id.vars = c('state', 'date'),
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

    pop.raw <- download.eurostat.population('IT', 2)
    
    # see readme.. this is a problem between EU and IT mapping of regions
    problem.states <- c('Provincia Autonoma di Bolzano/Bozen', 'Provincia Autonoma di Trento')
    regions.sum <- pop.raw %>% filter(state %in% problem.states) %>% pull(population) %>% sum
    
    populations <- pop.raw %>%
        filter(!state %in% problem.states) %>% 
        add_row(state = 'Provincia Autonoma di Trento', population = regions.sum, nuts = 'ITH2')
    
    eu.data <- eu.data %>% left_join(populations, by = 'state')
    
    if (by.state) {
        eu.data <- eu.data %>% 
            mutate(state.code = state)
    } else {
        eu.data <- eu.data %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(state = 'Italy',
                          state.code = 'ITA') %>% 
            dplyr::group_by(state, state.code, type, date) %>% 
            dplyr::summarise(cases = sum(cases),
                             population = sum(population)) %>% 
            dplyr::arrange(date) %>% 
            dplyr::mutate(cumul = cumsum(cases))
    }
    
    source.date <- format(max(eu.data$date), '%Y/%m/%d')
    return(list(data = eu.data, source = '{source.date} (IT PC)' %>% glue::glue()))
}

#' Download world data from Portugal's Direção Geral da Saúde
#'
#' @return a list with data and source string
#' @export
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

    source.date <- format(max(eu.data$date), '%Y/%m/%d')
    return(list(data = eu.data, source = '{source.date} (PT DGS)' %>% glue::glue()))
}

#' Download world data from John Hopkin's University
#'
#' @return a list with data and source string
#' @export
download.john.hopkins <- function() {
    base.url <- 'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series'
    jh.data.raw.deaths    <- glue::glue('{base.url}/time_series_covid19_deaths_global.csv') %>% readr::read_csv()
    jh.data.raw.confirmed <- glue::glue('{base.url}/time_series_covid19_confirmed_global.csv')%>% readr::read_csv()
    
    jh.data.deaths <- jh.data.raw.deaths %>%
        reshape2::melt(id.vars = c('Province/State', 'Country/Region', 'Lat', 'Long'),
                       variable.name = 'date',
                       value.name = 'cases') %>%
        tibble::tibble() %>%
        dplyr::mutate(type = 'death',
                      date = stringr::str_replace(date, '([0-9]+)/([0-9]+)/([0-9]+)', '20\\3-\\1-\\2'),
                      date = anytime::anydate(date))
    
    jh.data.confirmed <- jh.data.raw.confirmed %>%
        reshape2::melt(id.vars = c('Province/State', 'Country/Region', 'Lat', 'Long'),
                       variable.name = 'date',
                       value.name = 'cases') %>%
        tibble::tibble() %>%
        dplyr::mutate(type = 'confirmed',
                      date = stringr::str_replace(date, '([0-9]+)/([0-9]+)/([0-9]+)', '20\\3-\\1-\\2'),
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
        dplyr::ungroup() %>%
        #
        add_row(state = 'Cases on an international conveyance Japan', population = 3711, iso3c = 'Cases on an international conveyance Japan') %>%
        add_row(state = 'Taiwan', population = 23603121, iso3c = 'TWN') %>%
        add_row(state = 'Holy See', population = 801, iso3c = 'VAT') %>%
        add_row(state = 'MS Zaandam', population = 1829, iso3c = 'MS Zaandam') %>%
        add_row(state = 'Western Sahara', population = 567402, iso3c = 'ESH') %>% 
        #
        dplyr::mutate(state = eu.convert.names(state)) %>%
        dplyr::mutate(state = jh.convert.names(state)) %>%
        dplyr::arrange(state)
    
    # aa <- jh.data$state %>% unique() %>% sort()
    # bb <- pop.norm$state %>% unique() %>% sort()
    # cc <- eucdc.dat$data$state %>% unique() %>% sort()
    
    # countries.extended[! countries.extended %in% aa]
    # countries.extended[! countries.extended %in% bb]
    # countries.extended[! countries.extended %in% cc]
    
    jh.data.pop <- jh.data %>% left_join(pop.norm, by = 'state') %>%
        dplyr::group_by(state, type, population) %>%
        dplyr::arrange(desc(date)) %>%
        dplyr::mutate(cumul = cases) %>%
        dplyr::mutate(cases = zoo::rollapply(cases, 2, function(ix) { if(length(ix) <= 1) { return(ix) } else { ix[1] - sum(ix[-1]) } }, fill = c(0, 0, 0), align = 'left', partial = TRUE)) %>%
        dplyr::select(state, date, type, cases, cumul, population, state.code = iso3c)
    
    source.date <- format(max(jh.data.pop$date), '%Y/%m/%d')
    return(list(data = jh.data.pop, source = '{source.date} (John Hopkins)' %>% glue::glue()))
}


#' Download data from EU CDC
#'
#' @return a list with data and source string
#' @export
download.eucdc.data <- function() {
    eu.data.raw <- readr::read_csv('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv')

    cz.pop <- download.eurostat.population('CZ') %>% dplyr::pull(population) %>% purrr::pluck(1)
    eu.data.raw <- eu.data.raw %>% 
        dplyr::mutate(popData2018 = dplyr::if_else(countriesAndTerritories == 'Czechia', cz.pop, popData2018),
                      countryterritoryCode = dplyr::if_else(countriesAndTerritories == 'Czechia', 'CZE', countryterritoryCode))
    
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
        dplyr::select(state, date, type, cases, cumul, population = popData2018, state.code) %>% 
        dplyr::mutate(state = eu.convert.names(state)) %>% 
        mutate(state = if_else(state == 'United States of America', 'USA', state))

    source.date <- format(max(eu.data$date), '%Y/%m/%d')
    list(data = eu.data, source = '{source.date} (EU CDC)' %>% glue::glue()) %>%
        return()
}

#' Download population for EU States (by NUTS)
#'
#' @param country.code 2 letter country code
#' @param nuts level (1, 2 or 3)
#'
#' @return a table with country, population and nuts
#' @export
#'
#' @examples
#' download.eurostat.population('PT')
#' download.eurostat.population('PT', 2)
#' download.eurostat.population('PT', 1)
download.eurostat.population <- function(country.code, nuts = 3) {
    country.code <- toupper(country.code)
    nuts_converter <- list('1' = 3, '2' = 4, '3' = 5)
    nuts.label <- 'nuts_{nuts}' %>% glue::glue()
    nuts.label.2 <- 'nuts_{nuts}_label' %>% glue::glue()
    eu.population.raw <- suppressMessages(eurostat::get_eurostat('cens_11ag_r3')) %>% tibble::tibble()
    
    eu.population.norm <- eu.population.raw %>%
        dplyr::filter(grepl('^{country.code}[0-9A-Za-z]{paste0("{", nuts, "}")}' %>% glue::glue(), geo)) %>%
        dplyr::filter(age == 'TOTAL' & sex == 'T' & stringr::str_length(geo) == nuts_converter[nuts]) %>%
        dplyr::mutate(nuts = substr(geo, 0, nuts_converter[[nuts]])) %>%
        dplyr::mutate(nuts.label = eurostat::label_eurostat(nuts, dic = 'geo', fix_duplicated = TRUE)) %>%
        dplyr::group_by(nuts, nuts.label) %>%
        dplyr::filter(time == max(time)) %>%
        dplyr::summarise(values = sum(values))
    
    eu.population.norm %>%
        dplyr::ungroup() %>%
        dplyr::select(state = nuts.label, nuts, population = values) %>%
        return()
}
