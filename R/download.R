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

download.eurostat.population <- function(country.code) {
    eu.population.raw <- suppressMessages(get_eurostat('tgs00096'))

    eu.population.norm <- eu.population.raw %>%
        filter(grepl('^{country.code}' %>% glue, geo)) %>%
        mutate(nuts_1 = substr(geo, 0, 3),
               nuts_1_label = label_eurostat(nuts_1, dic = 'geo')) %>%
        group_by(nuts_1, nuts_1_label) %>%
        filter(time == max(time)) %>%
        summarize(values = sum(values))

    populations <- eu.population.norm %>%
        ungroup() %>%
        select(state = nuts_1_label, population = values)
}
