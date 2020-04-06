plot.options <- futile.options::OptionsManager('logger.options')
plot.options(point.size = 2, 
             line.size = 2, 
             segment.size = .5, 
             segment.alpha = .8, 
             label.alpha = .9,
             segment.color = 'black',
             label.size = 3.5,
             min.segment.length = 0,
             label.force = 2)

doubling.every <- function(date.vector, days) { return(function(a) {return(1/days * as.numeric(difftime(a, min(date.vector), units = 'days')))})}

top30 <- function(dat, case.type, n = 30) {
    my.plot <- dat %>%
        dplyr::filter(type == case.type) %>%
        dplyr::group_by(state, type) %>%
        dplyr::summarise(cases = sum(cases)) %>%
        dplyr::arrange(cases) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(state = paste0(state, ' (', format(cases, big.mark = ',', trim = TRUE), ')')) %>%
        dplyr::mutate(state = factor(state, levels = unique(.$state))) %>%
        top_n(n) %>%
        ggplot2::ggplot() +
        ggplot2::geom_bar(ggplot2::aes(state, cases, fill = state), stat = 'identity') +
        # scale_y_continuous(trans = 'log10') +
        ggplot2::coord_flip() +
        ggplot2::labs(x = '{proper.cases(case.type, capitalize = TRUE)}' %>% glue::glue(),
             y = region.code,
             title = '\'{proper.cases(case.type, capitalize.all = TRUE)}\' by {region.code}' %>% glue::glue() ,
             caption = last.date.string) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = 'none')

    return(my.plot)
}

# dat <- my.world$main
# case.type <- 'all'
# case.type <- 'confirmed'
# start_of <- 100 ; filter.states = c() ; log2.flag = FALSE ; per.100k.flag = FALSE ; double.every = NULLdigits = 2 ; state.code.flag = TRUE
ommit.start <- function(dat, case.type, start_of, filter.states = c(), log2.flag = FALSE, per.100k.flag = FALSE, double.every = NULL, digits = 2, state.code.flag = TRUE) {
    lab.y <- proper.cases(case.type, capitalize = TRUE)
    lab.t <- '\'{proper.cases(case.type, capitalize = TRUE)}\' over time' %>% glue::glue()
    lab.s <- 'Only showing after each {region.code} had more than {start_of} {proper.cases(case.type)}' %>% glue::glue()
    lab.x <- 'Number of days since \u2265 {start_of} {proper.cases(case.type)}' %>% glue::glue()

    if (per.100k.flag) {
        lab.y <- paste0(lab.y, ' -- per 100k population')
        lab.t <- paste0(lab.t, ' -- per 100k population')
    }

    if (!any(colnames(dat) == 'days.after.100')) {
        lab.s <- 'Showing cases since start of epidemic'
        lab.x <- 'Day'
        dat <- dat %>% dplyr::mutate(days.after.100 = date)
    }

    if (case.type == 'all') {
        dat <- dat %>%
            dplyr::ungroup() %>%
            dplyr::mutate(state = paste0(state, ' - ', proper.cases(type)),
                          state.code = paste0(state.code, ' - ', proper.cases(type)))
    }

    my.plot.data <- dat %>%
        dplyr::filter(cumul > 0) %>%
        dplyr::filter(length(filter.states) == 0 | state %in% filter.states) %>%
        dplyr::filter(case.type == 'all' | type == case.type) %>%
        dplyr::group_by(state, type) %>%
        dplyr::mutate(cumul = dplyr::if_else(rep(per.100k.flag, length(cumul)),
                               cumul * 100000 / population,
                               as.double(cumul))) %>%
        dplyr::mutate(state.code.var = if_else(rep(state.code.flag, state.code %>% length), state.code, state)) %>%
        dplyr::group_by(state, state.code.var) %>%
        dplyr::arrange(-cases) %>% 
        build.labels('cumul', digits = digits) %>% 
        ungroup

    my.plot <- my.plot.data %>%
        ggplot2::ggplot(ggplot2::aes(x = days.after.100, y = cumul, color = state.data)) +

        ggplot2::geom_point(size = plot.options('point.size')) +
        ggplot2::geom_line(size = plot.options('line.size')) +

        ggrepel::geom_label_repel(ggplot2::aes(label = label,
                             fill = state.data),
                         na.rm = TRUE,
                         color = 'white',
                         size = 3.5,
                         segment.size = 1.5,
                         min.segment.length = 0,
                         alpha = .9,
                         segment.alpha = .6,
                         segment.colour = 'black') +

        ggplot2::labs(y = lab.y,
             x = lab.x,
             title = lab.t,
             subtitle = lab.s,
             caption = last.date.string) +
        ggplot2::scale_color_viridis_d(end = .85, option = 'A') +
        ggplot2::scale_fill_viridis_d(end = .85, option = 'A') +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = 'none', legend.title = element_blank())

    if(log2.flag && !per.100k.flag) {
        my.plot <- my.plot +
            scale_y_continuous('{lab.y} (log2 scale)' %>% glue::glue(), trans = 'log2' %>% glue::glue())
    } else if (log2.flag) {
        my.plot <- my.plot +
            scale_y_continuous('{lab.y} (log2 scale)' %>% glue::glue(), trans = 'log2' %>% glue::glue(), labels = function(val) { round(val, digits = 6) })
    }


    if (is.null(double.every)) {
        return(my.plot)
    } else {
        my.plot +
            stat_function(fun = doubling.every(my.plot.data$date, double.every),
                          color = 'gray',
                          linetype = 'dashed') +
            ggrepel::geom_label_repel(data = tibble(days.after.100 = median(my.plot.data$date),
                                           cumul = doubling.every(my.plot.data$date, 1)(median(my.plot.data$date)),
                                           label = 'Gray line doubles every 3 days',
                                           state.data = 'gray'),
                             mapping = ggplot2::aes(x = days.after.100, y = cumul, label = label),
                             color = 'white', fill = 'gray') %>%
            return()
    }
}

last.days <- function(dat, case.type, days, filter.states = c(), log2.flag = FALSE, per.100k.flag = FALSE, new.flag = FALSE, digits = 2, state.code.flag = TRUE) {
    lab.y <- proper.cases(case.type, capitalize = TRUE)
    lab.t <- '\'{proper.cases(case.type, capitalize = TRUE)}\' of the last {days} days' %>% glue::glue()

    if (new.flag) {
        lab.t <- 'New {lab.t}' %>% glue::glue()
        dat.norm <- dat %>% dplyr::mutate(cumul = cases)
    } else {
        dat.norm <- dat
    }

    if (per.100k.flag) {
        lab.y <- paste0(lab.y, ' -- per 100k population')
        lab.t <- paste0(lab.t, ' -- per 100k population')
    }

    if (case.type == 'all') {
        dat.norm <- dat.norm %>%
            dplyr::ungroup() %>%
            dplyr::mutate(state = paste0(state, ' - ', proper.cases(type)),
                          state.code = paste0(state.code, ' - ', proper.cases(type)))
    }

    my.plot.data <- dat.norm %>%
        dplyr::filter(length(filter.states) == 0 | state %in% filter.states) %>%
        dplyr::filter(case.type == 'all' | type == case.type) %>%
        dplyr::mutate(cumul = dplyr::if_else(rep(per.100k.flag, length(cumul)),
                               cumul / population * 100000,
                               as.double(cumul))) %>%
        dplyr::mutate(state.code.var = ifelse(state.code.flag, state.code, state)) %>%
        dplyr::group_by(state, state.code.var, type) %>% 
        build.labels('cumul', digits = digits) %>% 
        dplyr::filter(cumul > 0) %>%
        ungroup

    my.plot <- my.plot.data %>%
        ggplot2::ggplot(ggplot2::aes(x = days.before.now, y = cumul, color = state.data)) +
            
            ggplot2::geom_point(size = plot.options('point.size')) +
            ggplot2::geom_line(size = plot.options('line.size')) +
        
            ggrepel::geom_label_repel(ggplot2::aes(label = label,
                                     fill = state.data),
                             na.rm = TRUE,
                             color = 'white',
                             size = 3.5,
                             segment.size = 1.5,
                             min.segment.length = 0,
                             segment.colour = 'black',
                             segment.alpha = .6) +
            ggplot2::labs(x = 'Last {days} days' %>% glue::glue(),
                 y = lab.y,
                 title = lab.t,
                 caption = last.date.string) +
            ggplot2::scale_color_viridis_d(end = .85, option = 'A') +
            ggplot2::scale_fill_viridis_d(end = .85, option = 'A') +
            #
            ggplot2::theme_minimal() +
            ggplot2::theme(legend.position = 'none', legend.title = element_blank())

    if(log2.flag && !per.100k.flag) {
        my.plot <- my.plot +
            scale_y_continuous('{lab.y} (log2 scale)' %>% glue::glue(), trans = 'log2' %>% glue::glue())
    } else if (log2.flag) {
        my.plot <- my.plot +
            scale_y_continuous('{lab.y} (log2 scale)' %>% glue::glue(), trans = 'log2' %>% glue::glue(), labels = function(val) { round(val, digits = 6) })
    }

    return(my.plot)
}

build.labels <- function(input, order.by, digits = 2) {
    my.format <- function(val) {
        val %>% 
            round(digits = digits) %>% 
            format(big.mark = ',', trim = FALSE) %>% 
            return()
    }
    
    tmp <- input %>% 
        dplyr::group_by(state) %>% 
        dplyr::arrange(desc(date), desc(!!as.name(order.by))) %>% 
        dplyr::mutate(state.data = state.code.var,
               state.data.order = !!as.name(order.by),
               state.data.val.max = max(!!as.name(order.by)),
               state.data.val.last = first(!!as.name(order.by))) %>% 
        dplyr::mutate(state.data.label = if_else(state.data.val.max == state.data.val.last,
                                          paste0(state.code.var, 
                                                 ': ', 
                                                 state.data.val.max %>% my.format),
                                          paste0(state.code.var, 
                                                 ': ', 
                                                 state.data.val.max %>% my.format, 
                                                 ' (today: ', 
                                                 state.data.val.last %>% my.format,
                                                 ')')))
    
    tmp %>% 
        dplyr::mutate(label = dplyr::if_else(state.data.order == max(state.data.order), state.data.label, NA_character_),
               state.data = factor(state.data,
                                   levels = .$state.data %>% unique)) %>% 
        dplyr::select(-state.data.label, state.data.val.last, state.data.val.max, state.data.order) %>% 
        return()
}

last.week.cumulative <- function(dat, case.type, days, filter.states = c(), log2.flag = FALSE, per.100k.flag = FALSE, digits = 2, state.code.flag = TRUE) {
    lab.y <- 'Number of cases confirmed in previous {days} days' %>% glue::glue()
    lab.t <- 'Rolling Average \'{proper.cases(case.type, capitalize = TRUE)}\' for the last {days} days' %>% glue::glue()
    lab.s <- 'WARNING:: Each point is an average from previous {days} days (only showing data from {format(min(dat$date), \'%B %d\')})' %>% glue::glue()

    if (per.100k.flag) {
        lab.y <- paste0(lab.y, ' -- per 100k population')
        lab.t <- paste0(lab.t, ' -- per 100k population')
    }
    
    dat.norm <- if (case.type == 'confirmed') {
        dat %>%
            dplyr::mutate(last.week.var = last.week.cases.confirmed)
    } else if (case.type == 'death') {
        dat %>%
            dplyr::mutate(last.week.var = last.week.cases.death)
    } else {
        dat %>%
            reshape2::melt(id.vars = c('state', 'state.code', 'date', 'population'),
                 measure.vars = c('last.week.cases.confirmed', 'last.week.cases.death'),
                 variable.name = 'type', value.name = 'last.week.var') %>%
            dplyr::mutate(type.aux = dplyr::if_else(type == 'last.week.cases.confirmed', 'confirmed', 'death'),
                          state = paste0(state, ' - ', proper.cases(type.aux, capitalize = TRUE)),
                          state.code = paste0(state.code, ' - ', proper.cases(type.aux, capitalize = TRUE)))
    }

    my.plot.data <- dat.norm %>%
        dplyr::filter(length(filter.states) == 0 | state %in% filter.states) %>%
        dplyr::mutate(last.week.var = dplyr::if_else(rep(per.100k.flag, length(last.week.var)),
                                                     last.week.var / population * 100000,
                                                     as.double(last.week.var))) %>%
        dplyr::mutate(state.code.var = if_else(rep(state.code.flag, length(state.code)), state.code, state)) %>%
        dplyr::group_by(state, state.code.var) %>% 
        build.labels('last.week.var', digits = digits)
    
    my.plot <- my.plot.data %>%
        #
        ggplot2::ggplot(ggplot2::aes(x = date, y = last.week.var, color = state.data)) +
            ggplot2::geom_point(size = plot.options('point.size')) +
            ggplot2::geom_line(size = plot.options('line.size')) +
                    ggrepel::geom_label_repel(ggplot2::aes(label = label,
                                         fill = state.data),
                                     na.rm = TRUE,
                                     color = 'white',
                                     size = plot.options('label.size'),
                                     segment.size = plot.options('segment.size'),
                                     alpha = plot.options('label.alpha'),
                                     segment.alpha = plot.options('segment.alha'),
                                     segment.colour = plot.options('segment.color'),
                                     min.segment.length = plot.options('min.segment.length'),
                                     force = plot.options('label.force')) +
            ggplot2::labs(title = lab.t,
                 subtitle = lab.s,
                 y = lab.y,
                 x = 'Date',
                 caption = last.date.string) +
            ggplot2::scale_color_viridis_d(end = .85, option = 'A') +
            ggplot2::scale_fill_viridis_d(end = .85, option = 'A') +
            ggplot2::theme_minimal() +
            ggplot2::theme(legend.position = 'none') +
            if(log2.flag) { scale_y_continuous('{lab.y} (log2 scale)' %>% glue::glue(), trans = 'log2' %>% glue::glue()) } else { scale_y_continuous() }

    return(my.plot)
}

death.vs.cases.plot <- function(dat, state.filter = c(), always.include = c(), expand = TRUE) {
    my.plot <- dat %>%
        dplyr::filter(length(state.filter) == 0  | state %in% (c(always.include, state.filter) %>% unique)) %>%
        ggplot2::ggplot(ggplot2::aes(x = cases.confirmed, y = cases.death, color = state)) +
            ggplot2::geom_point(ggplot2::aes(size = population), alpha = .4) +
            ggrepel::geom_label_repel(ggplot2::aes(label = paste0(state, ' (', scales::percent(ratio, accuracy = .1), ')'),
                                      fill = state),
                                      na.rm = TRUE,
                                      color = 'white',
                                      size = plot.options('label.size'),
                                      segment.size = plot.options('segment.size'),
                                      alpha = plot.options('label.alpha'),
                                      segment.alpha = plot.options('segment.alha'),
                                      segment.colour = plot.options('segment.color'),
                                      min.segment.length = plot.options('min.segment.length'),
                                      force = plot.options('label.force')) +
            
            ggplot2::labs(x = 'Confirmed Cases per 100k population',
                 y = 'Deaths per 100k population',
                 title = 'Deaths vs. Cases per 100k population',
                 subtitle = 'Percentage shown is the death rate per confirmed cases, while size represents population of {region.code}' %>% glue::glue(),
                 caption = last.date.string) +
            ggplot2::theme_minimal() +
            ggplot2::theme(legend.position = 'none')
    
    if (expand) {
        my.plot <- my.plot + ggplot2::expand_limits(x = ceiling(max(dat %>% dplyr::pull(cases.confirmed))),
                                                    y = ceiling(max(dat %>% dplyr::pull(cases.death))))
    }
    return(my.plot)
}

cases.plot <- function(dat, case.type, filter.states = c()) {
    my.plot <- dat %>%
        dplyr::filter(length(filter.states) == 0 | state %in% filter.states) %>%
        dplyr::filter(type == case.type) %>%
        dplyr::group_by(state, type) %>%
        dplyr::summarise(cases = sum(cases)) %>%
        dplyr::arrange(cases) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(state = paste0(state, ' (', cases, ')')) %>%
        dplyr::mutate(state = factor(state, levels = unique(.$state))) %>%
        ggplot2::ggplot() +
            ggplot2::geom_bar(ggplot2::aes(state, cases, fill = state), stat = 'identity') +
            # scale_y_continuous(trans = 'log10') +
            ggplot2::coord_flip() +
            ggplot2::labs(x = region.code,
                 y = proper.cases(case.type, capitalize = TRUE),
                 title = '{proper.cases(case.type, capitalize = TRUE)} by {region.code}' %>% glue::glue(),
                 caption = last.date.string) +
            ggplot2::theme_minimal() +
            ggplot2::theme(legend.position = 'none')

    return(my.plot)
}

plot.what.vs.cases <- function(data,
                               state.filter = c(),
                               always.include = c(),
                               what = 'Hospital beds') {
    my.tbl <- data %>%
        dplyr::filter(length(always.include) == 0 | state %in% (c(always.include, state.filter) %>% unique))
    return(
        my.tbl %>%
            ggplot2::ggplot(ggplot2::aes(x = cases.confirmed, y = value, color = state)) +
            ggplot2::geom_point(ggplot2::aes(size = ratio.confirmed), alpha = .4) +
            ggrepel::geom_label_repel(ggplot2::aes(label = paste0(state,
                                                ' (', scales::percent(ratio.confirmed, accuracy = .1), '/',
                                                scales::percent(ratio.death, accuracy = .1), ')'),
                                 fill = state),
                             na.rm = TRUE,
                             color = 'white',
                             size = plot.options('label.size'),
                             segment.size = plot.options('segment.size'),
                             alpha = plot.options('label.alpha'),
                             segment.alpha = plot.options('segment.alha'),
                             segment.colour = plot.options('segment.color'),
                             min.segment.length = plot.options('min.segment.length'),
                             force = plot.options('label.force')) +
            ggplot2::expand_limits(x = ceiling(max(my.tbl %>% dplyr::pull(cases.confirmed))),
                          y = ceiling(max(my.tbl %>% dplyr::pull(value)))) +
            ggplot2::scale_color_viridis_d(end = .85) +
            ggplot2::scale_fill_viridis_d(end = .85) +
            ggplot2::labs(x = 'Confirmed Cases per 100k population',
                 y = '{what} per 100k population' %>% glue::glue(),
                 title = '{what} vs. Cases per 100k population' %>% glue::glue(),
                 subtitle = 'scales::percentage shows the {tolower(what)} per confirmed cases and deaths, respectively' %>% glue::glue(),
                 caption = last.date.string) +
            ggplot2::theme_minimal() +
            ggplot2::theme(legend.position = 'none')
    )
}
