#' Download Germany data (DE)
#'
#' @return data frame with latest data
#' @export
#'
#' @examples
#' download.de.data()
download.de.data <- function(by.federal.state = FALSE) {
    #
    ## Package covid19.de.data does not provide accurate last few days
    #
    # eu.de.raw2 <- covid19.de.data::update_dataset() 
    # 
    # eu.data2 <- eu.de.raw2 %>% 
    #     arrange(desc(date), state) %>% 
    #     dplyr::group_by(state, date) %>% 
    #     dplyr::summarise(cases = sum(cases), deaths = sum(deaths)) %>% 
    #     arrange(desc(date), state) %>% 
    #     select(state, date, cases, deaths) %>% 
    #     arrange(date) %>% 
    #     reshape2::melt(id.vars = c('state', 'date'), variable.name = 'type', value.name = 'cases') %>% 
    #     dplyr::mutate(type = dplyr::if_else(type == 'cases', 'confirmed', 'death')) %>%
    #     dplyr::mutate(state.code = state) %>% 
    #     tibble::tibble() %>% 
    #     arrange(state, desc(date))
    
    
    eu.de.raw <- readr::read_csv('https://github.com/covid19-eu-zh/covid19-eu-data/raw/master/dataset/covid-19-de.csv')
    
    eu.data <- eu.de.raw %>%
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
        arrange(state, desc(date))
    
    # if (max(eu.data2$date) > max(eu.data1$date)) {
    #     futile.logger::flog.info('DE: Using district data')
    #     eu.de <- eu.data2
    # } else {
    #     eu.de <- eu.data1
    #     futile.logger::flog.info('DE: Using state data')
    # }

    if (!by.federal.state) {
        eu.data.out <- eu.data %>% 
            dplyr::ungroup() %>% 
            dplyr::mutate(state = 'Germany', state.code = 'DEU') %>%
            dplyr::group_by(state, date, type, state.code) %>% 
            dplyr::summarise(cases = sum(cases)) %>% 
            dplyr::group_by(state, type, state.code) %>% 
            dplyr::arrange(date) %>% 
            dplyr::mutate(cumul = cumsum(cases)) %>% 
            select(state, date, type, cases, cumul, state.code) %>% 
            arrange(desc(date))
    } else {
        eu.data.out <- eu.data %>% 
            dplyr::group_by(state, type, state.code) %>% 
            dplyr::arrange(date) %>% 
            dplyr::mutate(cumul = cumsum(cases)) %>% 
            select(state, date, type, cases, cumul, state.code) %>% 
            arrange(desc(date))
    }

    
    source.date <- format(max(eu.data.out$date), '%Y/%m/%d')
    return(list(data = eu.data.out, source = '{source.date} (RKI)' %>% glue::glue()))
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

    source.date <- format(max(eu.data$date), '%Y/%m/%d')
    return(list(data = eu.data, source = '{source.date} (IT PC)' %>% glue::glue()))
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
    eu.nl.raw <- readr::read_csv('https://github.com/covid19-eu-zh/covid19-eu-data/raw/master/dataset/covid-19-nl.csv')

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

    source.date <- format(max(eu.data$date), '%Y/%m/%d')
    return(list(data = eu.data, source = '{source.date} (PT DGS)' %>% glue::glue()))
}

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


download.eucdc.data <- function() {
    eu.data.raw <- readr::read_csv('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv')

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
        mutate(state = if_else(state == 'United States of America', 'USA', state))

    source.date <- format(max(eu.data$date), '%Y/%m/%d')
    list(data = eu.data, source = '{source.date} (EU CDC)' %>% glue::glue()) %>%
        return()
}

replace.country.data <- function(all.dat, state.new.dat, state.name) {
    dat <- all.dat$data
    dat.new <- state.new.dat$data

    new.max.date <- max(dat.new$date)
    old.max.date <- max(dat %>% filter(state == state.name) %>% top_n(1, date) %>% pull(date))
    
    if (new.max.date > old.max.date) {
        new.pop <- dat %>% filter(state == state.name) %>% top_n(1) %>% pull(population) %>% unique
        dat.new.norm <- dat.new %>% 
            group_by(date, type, state.code) %>% 
            summarise(cases = sum(cases)) %>% 
            group_by(type) %>% 
            arrange(date) %>% 
            mutate(cumul = cumsum(cases)) %>% 
            mutate(state = state.name, population = new.pop) %>% 
            select(state, date, type, cases, cumul, population, state.code)

        source.by = '{all.dat$source}, {state.new.dat$source}' %>% glue::glue()
        list(data = dat %>% filter(state != state.name) %>% bind_rows(dat.new.norm),
             source = source.by) %>% 
            return()
    } else {
        return(all.dat)
    }
}