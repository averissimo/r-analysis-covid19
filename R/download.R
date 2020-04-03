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
               state.code = 'DEU',
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
    dat <- NULL
    tryCatch(dat <- coronavirus::coronavirus, error = function(err) {})

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

download.pt.data <- function() {
    eu.data.raw <- covid19.pt.data::download.updated.pt()

    eu.data <- eu.data.raw$cdc.eu %>%
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

    list(data = eu.data, source = 'PT DGS') %>%
        return()
}

download.john.hopkins <- function() {
    jh.data.raw.deaths    <- 'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv' %>% read_csv()
    jh.data.raw.confirmed <- 'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'%>% read_csv()

    jh.data.deaths <- jh.data.raw.deaths %>%
        melt(id.vars = c('Province/State', 'Country/Region', 'Lat', 'Long'),
             variable.name = 'date',
             value.name = 'cases') %>%
        tibble() %>%
        mutate(type = 'deaths',
               date = str_replace(date, '([0-9]+)/([0-9]+)/([0-9]+)', '20\\3-\\1-\\2'),
               date = anydate(date))

    jh.data.confirmed <- jh.data.raw.confirmed %>%
        melt(id.vars = c('Province/State', 'Country/Region', 'Lat', 'Long'),
             variable.name = 'date',
             value.name = 'cases') %>%
        tibble ()%>%
        mutate(type = 'confirmed',
               date = str_replace(date, '([0-9]+)/([0-9]+)/([0-9]+)', '20\\3-\\1-\\2'),
               date = anydate(date))

    jh.data <- bind_rows(jh.data.deaths, jh.data.confirmed) %>%
        select(state = 'Country/Region', date, type, cases) %>%
        group_by(state, date, type) %>%
        summarise(cases = sum(cases)) %>%
        ungroup() %>%
        mutate(state = jh.convert.names(state)) %>%
        mutate(state = eu.convert.names(state))

    populations <- run.cache(wb, indicator = "SP.POP.TOTL", show.message = FALSE) %>%
        group_by(iso3c) %>%
        filter(date == max(date)) %>%
        select(population = value, state = country, iso3c) %>%
        ungroup() %>%
        #
        add_row(state = 'Diamond Princess', population = 3711, iso3c = 'Diamond Princess') %>%
        add_row(state = 'Taiwan', population = 23603121, iso3c = 'TWN') %>%
        add_row(state = 'Holy See', population = 801, iso3c = 'VAT') %>%
        add_row(state = 'MS Zaandam', population = 1829, iso3c = 'MS Zaandam') %>%
        mutate(state = eu.convert.names(state)) %>%
        mutate(state = jh.convert.names(state)) %>%
        arrange(state)

    jh.data.pop <- jh.data %>% left_join(populations, by = 'state') %>%
        group_by(state, type, population) %>%
        arrange(desc(date)) %>%
        mutate(cumul = cases) %>%
        mutate(cases = rollapply(cases, 2, function(ix) { if(length(ix) <= 1) { return(ix) } else { ix[1] - sum(ix[-1]) } }, fill = c(0, 0, 0), align = 'left', partial = TRUE)) %>%
        select(state, date, type, cases, cumul, population, state.code = iso3c)

    return(list(data = jh.data.pop, source = 'John Hopkins'))
}

jh.convert.names <- function(state) {
    convert.tmp <- list(
        'Brunei' = 'Brunei Darussalam',
        'Burma' = 'Myanmar',
        'Cabo Verde' = 'Cape Verde',
        'Congo (Brazzaville)' = 'Congo',
        'Congo (Kinshasa)' = 'Democratic Republic of the Congo',
        'Czechia' = 'Czech Republic',
        'Guinea-Bissau' = 'Guinea Bissau',
        'Korea, South' = 'Korea, Rep.',
        # 'Malawi' = 'Malawi',
        'Taiwan*' = 'Taiwan',
        'Tanzania' = 'United Republic of Tanzania',
        'Timor-Leste' = 'Timor Leste',
        'United States' = 'USA',
        'US' = 'USA',
        'Diamond Princess' = 'Cases on an international conveyance Japan',
        'West Bank and Gaza' = 'Palestine'
    )

    state[state %in% names(convert.tmp)] <- sapply(state[state %in% names(convert.tmp)], function(ix) { convert.tmp[[ix]] })
    return(state)
}

eu.convert.names <- function(state) {
    convert.tmp <- eu.convert.names.arr()
    state[state %in% names(convert.tmp)] <- sapply(state[state %in% names(convert.tmp)], function(ix) { convert.tmp[[ix]] })
    return(state)
}

eu.convert.names.arr <- function() {
    list(
        '404-1' =  'Anguilla',
        '404-2' =  'Cases on an international conveyance Japan',
        '404-2' =  'Guernsey',
        '404-3' =  'Jersey',
        '404-4' =  'Montserrat',
        '404-5' =  'Palestine',
        #
        'Bahamas, The' =  'Bahamas',
        'Cabo Verde' =  'Cape Verde',
        'Congo, Rep.' =  'Congo',
        'Cote d\'Ivoire' =  'Cote dIvoire',
        'Congo, Dem. Rep.' =  'Democratic Republic of the Congo',
        'Egypt, Arab Rep.' =  'Egypt',
        'Gambia, The' =  'Gambia',
        'Guinea-Bissau' =  'Guinea Bissau',
        'Iran, Islamic Rep.' =  'Iran',
        'Kyrgyz Republic' =  'Kyrgyzstan',
        'Lao PDR' =  'Laos',
        'Russian Federation' =  'Russia',
        'St. Kitts and Nevis' =  'Saint Kitts and Nevis',
        'St. Lucia' =  'Saint Lucia',
        'St. Martin' =  'Saint Vincent and the Grenadines',
        'Sint Maarten (Dutch part)' =  'Sint Maarten',
        'Slovak Republic' =  'Slovakia',
        'Korea, Rep.' =  'South Korea',
        'Syrian Arab Republic' =  'Syria',
        'Taiwan*' =  'Taiwan',
        'Timor-Leste' =  'Timor Leste',
        'Turks and Caicos Islands' =  'Turks and Caicos islands',
        'Tanzania' =  'United Republic of Tanzania',
        'United States' =  'USA',
        'Virgin Islands (U.S.)' =  'United States Virgin Islands',
        'Venezuela, RB' =  'Venezuela',
        #
        'St. Vincent and the Grenadines' =  'Saint Vincent and the Grenadines'
    ) %>%
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
