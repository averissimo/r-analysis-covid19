doubling.every <- function(date.vector, days) { return(function(a) {return(1/days * as.numeric(difftime(a, min(date.vector), units = 'days')))})}

top30 <- function(dat, case.type, n = 30) {
    my.plot <- dat %>%
        filter(type == case.type) %>%
        group_by(state, type) %>%
        summarise(cases = sum(cases)) %>%
        arrange(cases) %>%
        ungroup() %>%
        mutate(state = paste0(state, ' (', cases, ')')) %>%
        mutate(state = factor(state, levels = unique(.$state))) %>%
        top_n(n) %>%
        ggplot() +
        geom_bar(aes(state, cases, fill = state), stat = 'identity') +
        # scale_y_continuous(trans = 'log10') +
        coord_flip() +
        labs(x = 'Confirmed Cases',
             y = region.code,
             title = 'Confirmed Cases by {region.code}' %>% glue ,
             caption = last.date.string) +
        theme_minimal() +
        theme(legend.position = 'none')

    return(my.plot)
}


ommit.start <- function(dat, case.type, start_of, filter.states = c(), log2.flag = FALSE, per.100k.flag = FALSE) {
    lab.y <- proper.cases(case.type, capitalize = TRUE)
    lab.t <- glue('{proper.cases(case.type, capitalize = TRUE)} over time')
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



    my.plot.data <- dat %>%
        filter(cumul > 0) %>%
        filter(length(filter.states) == 0 | state %in% filter.states) %>%
        filter(type == case.type) %>%
        ungroup(state) %>%
        mutate(cumul = if_else(rep(per.100k.flag, length(cumul)),
                               round(cumul * 100000 / population, digits = 1),
                               as.double(cumul))) %>%
        group_by(state) %>%
        arrange(-cases) %>% {
            tmp <- summarise(., cases = max(cumul))
            tmp <- arrange(tmp, cases)
            tmp <- mutate(tmp,
                          cases.str = format(cases, big.mark = ','),
                          state.data = paste0(state, ' (', cases, ')'),
                          state.data.str = paste0(state, ' (', cases.str, ')'))

            mutate(., state.data = factor(paste0(state, ' (', max(cumul), ')'),
                                          levels = rev(pull(tmp, state.data)),
                                          labels = rev(pull(tmp, state.data.str))))
        } %>%
        mutate(label = if_else(cumul == max(cumul), as.character(state.data), NA_character_)) %>%
        ungroup

    my.plot <- my.plot.data %>%
        ggplot(aes(x = days.after.100, y = cumul, color = state.data)) +

        #stat_function(fun = doubling.every(my.plot.data$date, 3),
        ##              color = 'gray',
        #              linetype = 'dashed') +

        geom_line(size = 1.2) +

        #geom_label_repel(data = tibble(days.after.100 = median(my.plot.data$date),
        #                               cumul = doubling.every(my.plot.data$date, 1)(median(my.plot.data$date)),
        #                               label = 'Gray line doubles every 3 days',
        #                               state.data = 'gray'),
        #                 mapping = aes(x = days.after.100, y = cumul, label = label),
        #                 color = 'white', fill = 'gray') +

        geom_label_repel(aes(label = label,
                             fill = state.data),
                         na.rm = TRUE,
                         color = 'white',
                         size = 3.5,
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
        theme(legend.position = 'none', legend.title = element_blank()) +
        if(log2.flag) { scale_y_continuous('{lab.y} (log2 scale)' %>% glue, trans = 'log2' %>% glue) } else { scale_y_continuous() }

     return(my.plot)
}

last.days <- function(dat, case.type, days, filter.states = c(), log2.flag = FALSE, per.100k.flag = FALSE) {
    lab.y <- proper.cases(case.type, capitalize = TRUE)
    lab.t <- glue('{proper.cases(case.type, capitalize = TRUE)} over time')

    if (per.100k.flag) {
        lab.y <- paste0(lab.y, ' -- per 100k population')
        lab.t <- paste0(lab.t, ' -- per 100k population')
    }

    my.plot <- dat %>%
        filter(type == case.type & state %in% filter.states) %>%
        mutate(cumul = if_else(rep(per.100k.flag, length(cumul)),
                               round(cumul / population * 100000, digits = 1),
                               as.double(cumul))) %>%
        group_by(state) %>% {
            tmp <- summarise(., cases = max(cumul))
            tmp <- arrange(tmp, cases)
            tmp <- mutate(tmp,
                          cases.str = format(cases, big.mark = ','),
                          state.data = paste0(state, ' (', cases, ')'),
                          state.data.str = paste0(state, ' (', cases.str, ')'))

            mutate(., state.data = factor(paste0(state, ' (', max(cumul), ')'),
                                          levels = rev(pull(tmp, state.data)),
                                          labels = rev(pull(tmp, state.data.str))))
            } %>%
        mutate(label = if_else(cumul == max(cumul), format(state.data, big.mark = ','), NA_character_)) %>%
        filter(cumul > 0) %>%
        ungroup %>%
        ggplot(aes(x = days.before.now, y = cumul, color = state.data)) +
            geom_line(size = 1.2) +
            geom_label_repel(aes(label = label,
                                     fill = state.data),
                                 na.rm = TRUE,
                                 color = 'white',
                                 size = 3.5,
                                 segment.colour = 'black',
                                 segment.alpha = .4) +
            labs(x = 'Last {days} days' %>% glue,
                 y = lab.y,
                 title = '{proper.cases(case.type, capitalize = TRUE)} in the last {days} days' %>% glue,
                 caption = last.date.string) +
            scale_color_viridis(discrete = TRUE, end = .85, option = 'A') +
            scale_fill_viridis(discrete = TRUE, end = .85, option = 'A') +
            #
            theme_minimal() +
            theme(legend.position = 'none', legend.title = element_blank()) +
            if(log2.flag) { scale_y_continuous('{lab.y} (log2 scale)' %>% glue, trans = 'log2' %>% glue) } else { scale_y_continuous() }

    return(my.plot)
}


last.week.cumulative <- function(dat, case.type, days, filter.states = c(), log2.flag = FALSE, per.100k.flag = FALSE) {
    lab.y <- 'Number of cases confirmed in previous {days} days' %>% glue
    lab.t <- 'Cumulative {proper.cases(case.type, capitalize = TRUE)} for the last {days} days' %>% glue
    lab.s <- 'WARNING:: Each point is a sum from previous 4 days *(only showing data from {format(min(dat$date), \'%B %d\')})' %>% glue

    if (per.100k.flag) {
        lab.y <- paste0(lab.y, ' -- per 100k population')
        lab.t <- paste0(lab.t, ' -- per 100k population')
    }

    dat.norm <- if (case.type == 'confirmed') {
        dat %>%
            mutate(last.week.var = last.week.cases.confirmed)
    } else {
        dat %>%
            mutate(last.week.var = last.week.cases.death)
    }

    my.plot.data <- dat.norm %>%
        filter(state %in% filter.states) %>%
        mutate(last.week.var = if_else(rep(per.100k.flag, length(last.week.var)),
                                       round(last.week.var / population * 100000, digits = 1),
                                       as.double(last.week.var))) %>%
        group_by(state) %>% {
            tmp <- summarise(., cases = last(last.week.var))
            tmp <- arrange(tmp, cases)
            tmp <- mutate(tmp,
                          cases.str = format(cases, big.mark = ','),
                          state.data = paste0(state, ' (', cases, ')'),
                          state.data.str = paste0(state, ' (', cases.str, ')'))

            mutate(., state.data = factor(paste0(state, ' (', last(last.week.var), ')'),
                                          levels = rev(pull(tmp, state.data)),
                                          labels = rev(pull(tmp, state.data.str))))
        } %>%
        mutate(label = if_else(date == max(date), format(state.data, big.mark = ','), NA_character_))

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
                                     segment.alpha = .4,
                                     segment.colour = 'black') +
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
