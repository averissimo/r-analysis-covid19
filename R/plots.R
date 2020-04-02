doubling.every <- function(date.vector, days) { return(function(a) {return(1/days * as.numeric(difftime(a, min(date.vector), units = 'days')))})}

top30 <- function(dat, case.type, n = 30) {
    my.plot <- dat %>%
        filter(type == case.type) %>%
        group_by(state, type) %>%
        summarise(cases = sum(cases)) %>%
        arrange(cases) %>%
        ungroup() %>%
        mutate(state = paste0(state, ' (', format(cases, big.mark = ',', trim = TRUE), ')')) %>%
        mutate(state = factor(state, levels = unique(.$state))) %>%
        top_n(n) %>%
        ggplot() +
        geom_bar(aes(state, cases, fill = state), stat = 'identity') +
        # scale_y_continuous(trans = 'log10') +
        coord_flip() +
        labs(x = '{proper.cases(case.type, capitalize = TRUE)}' %>% glue,
             y = region.code,
             title = '\'{proper.cases(case.type, capitalize.all = TRUE)}\' by {region.code}' %>% glue ,
             caption = last.date.string) +
        theme_minimal() +
        theme(legend.position = 'none')

    return(my.plot)
}


ommit.start <- function(dat, case.type, start_of, filter.states = c(), log2.flag = FALSE, per.100k.flag = FALSE, double.every = NULL, digits = 2, state.code.flag = TRUE) {
    lab.y <- proper.cases(case.type, capitalize = TRUE)
    lab.t <- '\'{proper.cases(case.type, capitalize = TRUE)}\' over time' %>% glue
    lab.s <- 'Only showing after each {region.code} had more than {start_of} {proper.cases(case.type)}' %>% glue
    lab.x <- 'Number of days since ≥ {start_of} {proper.cases(case.type)}' %>% glue

    if (per.100k.flag) {
        lab.y <- paste0(lab.y, ' -- per 100k population')
        lab.t <- paste0(lab.t, ' -- per 100k population')
    }

    if (!any(colnames(dat) == 'days.after.100')) {
        lab.s <- 'Showing cases since start of epidemic'
        lab.x <- 'Day'
        dat <- dat %>% mutate(days.after.100 = date)
    }

    if (case.type == 'all') {
        dat <- dat %>%
            ungroup() %>%
            mutate(state = paste0(state, ' - ', type))
    }

    my.plot.data <- dat %>%
        filter(cumul > 0) %>%
        filter(length(filter.states) == 0 | state %in% filter.states) %>%
        filter(case.type == 'all' | type == case.type) %>%
        group_by(state, type) %>%
        mutate(cumul = if_else(rep(per.100k.flag, length(cumul)),
                               cumul * 100000 / population,
                               as.double(cumul)),
               cumul.round = round(cumul, digits = digits)) %>%
        mutate(state.code.var = ifelse(state.code.flag, state.code, state)) %>%
        group_by(state, state.code.var) %>%
        arrange(-cases) %>% {
            tmp <- summarise(., cases = max(cumul.round))
            tmp <- arrange(tmp, cases)
            tmp <- mutate(tmp,
                          cases.str = format(cases, trim = TRUE, big.mark = ','),
                          state.data = paste0(state.code.var, ' (', cases, ')'),
                          state.data.str = paste0(state.code.var, ' (', cases.str, ')'))

            mutate(., state.data = factor(paste0(state.code.var, ' (', max(cumul.round), ')'),
                                          levels = rev(pull(tmp, state.data)),
                                          labels = rev(pull(tmp, state.data.str))))
        } %>%
        mutate(label = if_else(cumul.round == max(cumul.round), as.character(state.data), NA_character_)) %>%
        ungroup

    my.plot <- my.plot.data %>%
        ggplot(aes(x = days.after.100, y = cumul, color = state.data)) +

        geom_line(size = 1.2) +
        geom_point(size = 1.2) +

        geom_label_repel(aes(label = label,
                             fill = state.data),
                         na.rm = TRUE,
                         color = 'white',
                         size = 3.5,
                         min.segment.length = 0,
                         alpha = .9,
                         segment.alpha = .4,
                         segment.colour = 'black') +

        labs(y = lab.y,
             x = lab.x,
             title = lab.t,
             subtitle = lab.s,
             caption = last.date.string) +
        scale_color_viridis(discrete = TRUE, end = .85, option = 'A') +
        scale_fill_viridis(discrete = TRUE, end = .85, option = 'A') +
        theme_minimal() +
        theme(legend.position = 'none', legend.title = element_blank())

    if(log2.flag && !per.100k.flag) {
        my.plot <- my.plot +
            scale_y_continuous('{lab.y} (log2 scale)' %>% glue, trans = 'log2' %>% glue)
    } else if (log2.flag) {
        my.plot <- my.plot +
            scale_y_continuous('{lab.y} (log2 scale)' %>% glue, trans = 'log2' %>% glue, labels = function(val) { round(val, digits = 6) })
    }


    if (is.null(double.every)) {
        return(my.plot)
    } else {
        my.plot +
            stat_function(fun = doubling.every(my.plot.data$date, double.every),
                          color = 'gray',
                          linetype = 'dashed') +
            geom_label_repel(data = tibble(days.after.100 = median(my.plot.data$date),
                                           cumul = doubling.every(my.plot.data$date, 1)(median(my.plot.data$date)),
                                           label = 'Gray line doubles every 3 days',
                                           state.data = 'gray'),
                             mapping = aes(x = days.after.100, y = cumul, label = label),
                             color = 'white', fill = 'gray') %>%
            return()
    }
}

last.days <- function(dat, case.type, days, filter.states = c(), log2.flag = FALSE, per.100k.flag = FALSE, new.flag = FALSE, digits = 2, state.code.flag = TRUE) {
    lab.y <- proper.cases(case.type, capitalize = TRUE)
    lab.t <- '\'{proper.cases(case.type, capitalize = TRUE)}\' of the last {days} days' %>% glue

    if (new.flag) {
        lab.t <- 'New {lab.t}' %>% glue
        dat <- dat %>% mutate(cumul = cases)
    }

    if (per.100k.flag) {
        lab.y <- paste0(lab.y, ' -- per 100k population')
        lab.t <- paste0(lab.t, ' -- per 100k population')
    }

    if (case.type == 'all') {
        dat <- dat %>%
            ungroup() %>%
            mutate(state = paste0(state, ' - ', type))
    }

    my.plot.data <- dat %>%
        filter(length(filter.states) == 0 | state %in% filter.states) %>%
        filter(case.type == 'all' | type == case.type) %>%
        mutate(cumul = if_else(rep(per.100k.flag, length(cumul)),
                               cumul / population * 100000,
                               as.double(cumul)),
               cumul.round = round(cumul, digits = digits)) %>%
        mutate(state.code.var = ifelse(state.code.flag, state.code, state)) %>%
        group_by(state, state.code.var, type) %>% {
            tmp <- summarise(., cases = max(cumul.round))
            tmp <- arrange(tmp, cases)
            tmp <- mutate(tmp,
                          cases.str = format(cases, trim = TRUE, big.mark = ','),
                          state.data = paste0(state.code.var, ' (', cases, ')'),
                          state.data.str = paste0(state.code.var, ' (', cases.str, ')'))

            mutate(., state.data = factor(paste0(state.code.var, ' (', max(cumul.round), ')'),
                                          levels = rev(pull(tmp, state.data)),
                                          labels = rev(pull(tmp, state.data.str))))
            } %>%
        mutate(label = if_else(cumul.round == max(cumul.round), format(state.data, trim = TRUE, big.mark = ','), NA_character_)) %>%
        filter(cumul > 0) %>%
        ungroup

    my.plot <- my.plot.data %>%
        ggplot(aes(x = days.before.now, y = cumul, color = state.data)) +
            geom_line(size = 1.2) +
            geom_point(size = 1.2) +
            geom_label_repel(aes(label = label,
                                     fill = state.data),
                             na.rm = TRUE,
                             color = 'white',
                             size = 3.5,
                             min.segment.length = 0,
                             segment.colour = 'black',
                             segment.alpha = .4) +
            labs(x = 'Last {days} days' %>% glue,
                 y = lab.y,
                 title = lab.t,
                 caption = last.date.string) +
            scale_color_viridis(discrete = TRUE, end = .85, option = 'A') +
            scale_fill_viridis(discrete = TRUE, end = .85, option = 'A') +
            #
            theme_minimal() +
            theme(legend.position = 'none', legend.title = element_blank())

    if(log2.flag && !per.100k.flag) {
        my.plot <- my.plot +
            scale_y_continuous('{lab.y} (log2 scale)' %>% glue, trans = 'log2' %>% glue)
    } else if (log2.flag) {
        my.plot <- my.plot +
            scale_y_continuous('{lab.y} (log2 scale)' %>% glue, trans = 'log2' %>% glue, labels = function(val) { round(val, digits = 6) })
    }

    return(my.plot)
}


last.week.cumulative <- function(dat, case.type, days, filter.states = c(), log2.flag = FALSE, per.100k.flag = FALSE, digits = 2, state.code.flag = TRUE) {
    lab.y <- 'Number of cases confirmed in previous {days} days' %>% glue
    lab.t <- 'Rolling Average \'{proper.cases(case.type, capitalize = TRUE)}\' for the last {days} days' %>% glue
    lab.s <- 'WARNING:: Each point is an average from previous {days} days (only showing data from {format(min(dat$date), \'%B %d\')})' %>% glue

    if (per.100k.flag) {
        lab.y <- paste0(lab.y, ' -- per 100k population')
        lab.t <- paste0(lab.t, ' -- per 100k population')
    }

    dat.norm <- if (case.type == 'confirmed') {
        dat %>%
            mutate(last.week.var = last.week.cases.confirmed)
    } else if (case.type == 'death') {
        dat %>%
            mutate(last.week.var = last.week.cases.death)
    } else {
        dat %>%
            melt(id.vars = c('state', 'state.code', 'date', 'population'),
                 measure.vars = c('last.week.cases.confirmed', 'last.week.cases.death'),
                 variable.name = 'type', value.name = 'last.week.var') %>%
            mutate(type.aux = if_else(type == 'last.week.cases.confirmed', 'confirmed', 'death'),
                   state = paste0(state, ' - ', proper.cases(type.aux, capitalize = TRUE)))
    }

    my.plot.data <- dat.norm %>%
        filter(length(filter.states) == 0 | state %in% filter.states) %>%
        mutate(last.week.var = if_else(rep(per.100k.flag, length(last.week.var)),
                                       last.week.var / population * 100000,
                                       as.double(last.week.var)),
               last.week.var.round = round(last.week.var, digits = digits)) %>%
        mutate(state.code.var = ifelse(state.code.flag, state.code, state)) %>%
        group_by(state, state.code.var) %>% {
            tmp <- summarise(., cases = max(last.week.var.round))
            tmp <- arrange(tmp, cases)
            tmp <- mutate(tmp,
                          cases.str = format(cases, trim = TRUE, big.mark = ','),
                          state.data = paste0(state.code.var, ' (', cases, ')'),
                          state.data.str = paste0(state.code.var, ' (', cases.str, ')'))

            mutate(., state.data = factor(paste0(state.code.var, ' (', max(last.week.var.round), ')'),
                                          levels = rev(pull(tmp, state.data)),
                                          labels = rev(pull(tmp, state.data.str))))
        } %>%
        mutate(label = if_else(last.week.var.round == max(last.week.var.round), format(state.data, trim = TRUE, big.mark = ','), NA_character_))

    my.plot <- my.plot.data %>%
        #
        ggplot(aes(x = date, y = last.week.var, color = state.data)) +
            geom_line(size = 1.2) +
            geom_point(size = 1.2) +
                    geom_label_repel(aes(label = label,
                                         fill = state.data),
                                     na.rm = TRUE,
                                     color = 'white',
                                     size = 3.5,
                                     alpha = .9,
                                     segment.alpha = .4,
                                     segment.colour = 'black',
                                     min.segment.length = 0) +
            labs(title = lab.t,
                 subtitle = lab.s,
                 y = lab.y,
                 x = 'Date',
                 caption = last.date.string) +
            scale_color_viridis(discrete = TRUE, end = .85, option = 'A') +
            scale_fill_viridis(discrete = TRUE, end = .85, option = 'A') +
            theme_minimal() +
            theme(legend.position = 'none') +
            if(log2.flag) { scale_y_continuous('{lab.y} (log2 scale)' %>% glue, trans = 'log2' %>% glue) } else { scale_y_continuous() }

    return(my.plot)
}

death.vs.cases.plot <- function(dat, state.filter = c(), always.include = c()) {
    return(dat %>%
        filter(length(state.filter) == 0  | state %in% (c(always.include, state.filter) %>% unique)) %>%
        ggplot(aes(x = cases.confirmed, y = cases.death, color = state)) +
            geom_point(aes(size = population), alpha = .4) +
            geom_label_repel(aes(label = paste0(state, ' (', percent(ratio, accuracy = .1), ')'),
                             fill = state),
                             na.rm = TRUE,
                             alpha = .8,
                             color = 'white',
                             size = 3,
                             min.segment.length = 0,
                             alpha = .9,
                             segment.alpha = .4,
                             segment.colour = 'black',
                             force = 2) +
            expand_limits(x = ceiling(max(dat %>% pull(cases.confirmed))),
                          y = ceiling(max(dat %>% pull(cases.death)))) +
            labs(x = 'Confirmed Cases per 100k population',
                 y = 'Deaths per 100k population',
                 title = 'Deaths vs. Cases per 100k population',
                 subtitle = 'percentage is the death rate per confirmed cases and size represents size of {region.code} by population' %>% glue,
                 caption = last.date.string) +
            theme_minimal() +
            theme(legend.position = 'none'))
}

cases.plot <- function(dat, case.type, filter.states = c()) {
    my.plot <- dat %>%
        filter(length(filter.states) == 0 | state %in% filter.states) %>%
        filter(type == case.type) %>%
        group_by(state, type) %>%
        summarise(cases = sum(cases)) %>%
        arrange(cases) %>%
        ungroup() %>%
        mutate(state = paste0(state, ' (', cases, ')')) %>%
        mutate(state = factor(state, levels = unique(.$state))) %>%
        ggplot() +
            geom_bar(aes(state, cases, fill = state), stat = 'identity') +
            # scale_y_continuous(trans = 'log10') +
            coord_flip() +
            labs(x = region.code,
                 y = proper.cases(case.type, capitalize = TRUE),
                 title = '{proper.cases(case.type, capitalize = TRUE)} by {region.code}' %>% glue,
                 caption = last.date.string) +
            theme_minimal() +
            theme(legend.position = 'none')

    return(my.plot)
}

plot.what.vs.cases <- function(data,
                               state.filter = c(),
                               always.include = c(),
                               what = 'Hospital beds') {
    my.tbl <- data %>%
        filter(length(always.include) == 0 | state %in% (c(always.include, state.filter) %>% unique))
    return(
        my.tbl %>%
            ggplot(aes(x = cases.confirmed, y = value, color = state)) +
            geom_point(aes(size = ratio.confirmed), alpha = .4) +
            geom_label_repel(aes(label = paste0(state,
                                                ' (', percent(ratio.confirmed, accuracy = .1), '/',
                                                percent(ratio.death, accuracy = .1), ')'),
                                 fill = state),
                             na.rm = TRUE,
                             alpha = .8,
                             color = 'white',
                             size = 3,
                             min.segment.length = 0,
                             alpha = .9,
                             segment.alpha = .4,
                             segment.colour = 'black',
                             force = 2) +
            expand_limits(x = ceiling(max(my.tbl %>% pull(cases.confirmed))),
                          y = ceiling(max(my.tbl %>% pull(value)))) +
            scale_color_viridis(discrete = TRUE, end = .85) +
            scale_fill_viridis(discrete = TRUE, end = .85) +
            labs(x = 'Confirmed Cases per 100k population',
                 y = glue('{what} per 100k population'),
                 title = glue('{what} vs. Cases per 100k population'),
                 subtitle = glue('percentage shows the {tolower(what)} per confirmed cases and deaths, respectively'),
                 caption = last.date.string) +
            theme_minimal() +
            theme(legend.position = 'none')
    )
}
