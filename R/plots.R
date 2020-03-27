top30 <- function(dat, type) {
    my.plot <- dat %>%
        filter(type == 'confirmed') %>%
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
        labs(x = 'Confirmed Cases',
             y = region.code,
             title = 'Confirmed Cases by {region.code}' %>% glue ,
             caption = last.date.string) +
        theme_minimal() +
        theme(legend.position = 'none')

    return(my.plot)
}


ommit.start <- function(dat, case.type, start_of, filter.countries = c(), log2.flag = FALSE, per.100k.flag = FALSE) {
    lab.y <- proper.cases(case.type, capitalize = TRUE)
    lab.t <- glue('{proper.cases(case.type, capitalize = TRUE)} over time')

    if (per.100k.flag) {
        lab.y <- paste0(lab.y, ' -- per 100k population')
        lab.t <- paste0(lab.t, ' -- per 100k population')
    }

     my.plot <- dat %>%
        filter(state %in% filter.countries) %>%
        filter(type == case.type) %>%
        ungroup(state) %>%
        group_by(state) %>%
        mutate(cumul = if_else(rep(per.100k.flag, length(cumul)), round(cumul / population * 100000, digits = 3), cumul)) %>%
        arrange(-cases) %>% {
            tmp <- summarise(., cases = max(cumul))
            tmp <- arrange(tmp, cases)
            tmp <- mutate(tmp, state.data = paste0(state, ' (', cases, ')'))

            mutate(., state.data = factor(paste0(state, ' (', max(cumul), ')'),
                                          levels = rev(pull(tmp, state.data))))
        } %>%
        mutate(label = if_else(cumul == max(cumul), as.character(state.data), NA_character_)) %>%
        ungroup %>%
        ggplot(aes(x = days.after.100, y = cumul, color = state.data)) +
        geom_line(size = 1.2) +
        geom_label_repel(aes(label = label,
                             fill = state.data),
                         na.rm = TRUE,
                         color = 'white',
                         size = 3.5,
                         segment.alpha = .4,
                         segment.colour = 'black') +
        labs(y = lab.y,
             x = 'Number of days since ≥ {start_of} {proper.cases(case.type)}' %>% glue,
             title = lab.t,
             subtitle = 'Only showing after each {region.code} had more than {start_of} {proper.cases(case.type)}' %>% glue,
             caption = last.date.string) +
        scale_color_viridis(discrete = TRUE, end = .85, option = 'A') +
        scale_fill_viridis(discrete = TRUE, end = .85, option = 'A') +
        theme_minimal() +
        theme(legend.position = 'none', legend.title = element_blank()) +
        if(log2.flag) { scale_y_continuous('{lab.y} (log2 scale)' %>% glue, trans = 'log2' %>% glue) } else { scale_y_continuous() }

     return(my.plot)
}

last.days <- function(dat, case.type, days, filter.countries = c(), log2.flag = FALSE, per.100k.flag = FALSE) {
    lab.y <- proper.cases(case.type, capitalize = TRUE)
    lab.t <- glue('{proper.cases(case.type, capitalize = TRUE)} over time')

    if (per.100k.flag) {
        lab.y <- paste0(lab.y, ' -- per 100k population')
        lab.t <- paste0(lab.t, ' -- per 100k population')
    }

    my.plot <- dat %>%
        filter(type == case.type & state %in% filter.countries) %>%
        mutate(cumul = if_else(rep(per.100k.flag, length(cumul)), round(cumul / population * 100000, digits = 3), cumul)) %>%
        group_by(state) %>% {
            tmp <- summarise(., cases = max(cumul))
            tmp <- arrange(tmp, cases)
            tmp <- mutate(tmp, state.data = paste0(state, ' (', cases, ')'))

            mutate(., state.data = factor(paste0(state, ' (', max(cumul), ')'),
                                            levels = rev(pull(tmp, state.data))))
            } %>%
        mutate(label = if_else(cumul == max(cumul), as.character(state.data), NA_character_)) %>%
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
                 title = 'Confirmend Cases in the last {days} days' %>% glue,
                 caption = last.date.string) +
            scale_color_viridis(discrete = TRUE, end = .85, option = 'A') +
            scale_fill_viridis(discrete = TRUE, end = .85, option = 'A') +
            #
            theme_minimal() +
            theme(legend.position = 'none', legend.title = element_blank()) +
            if(log2.flag) { scale_y_continuous('{lab.y} (log2 scale)' %>% glue, trans = 'log2' %>% glue) } else { scale_y_continuous() }

    return(my.plot)
}


last.week.cumulative <- function(dat, case.type, days, filter.countries = c(), log2.flag = FALSE, per.100k.flag = FALSE) {
    lab.y <- 'Number of cases confirmed in previous {days} days' %>% glue
    lab.t <- 'Cumulative {proper.cases(case.type, capitalize = TRUE)} for the last {days} days' %>% glue
    lab.s <- 'WARNING:: Each point is a sum from previous 4 days' %>% glue

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

    my.plot <- dat.norm %>%
        filter(state %in% filter.countries) %>%
        mutate(last.week.var = if_else(rep(per.100k.flag, length(last.week.var)), round(last.week.var / population * 100000, digits = 3), last.week.var)) %>%
        group_by(state) %>% {
            tmp <- summarise(., cases = last(last.week.var))
            tmp <- arrange(tmp, cases)
            tmp <- mutate(tmp, state.data = paste0(state, ' (', cases, ')'))

            mutate(., state.data = factor(paste0(state, ' (', last(last.week.var), ')'),
                                            levels = rev(pull(tmp, state.data))))
        } %>%
        mutate(label = if_else(date == max(date), as.character(state.data), NA_character_)) %>%
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

cases.plot <- function(dat, case.type, filter.countries = c()) {
    my.plot <- dat %>%
        filter(length(filter.countries) == 0 | state %in% filter.countries) %>%
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
