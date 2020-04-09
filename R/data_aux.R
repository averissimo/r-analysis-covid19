fill.missing <- function(dat) {
  days <- tibble::tibble(date = seq.Date(from = min(dat$date %>% unique %>% sort), 
                                         to = max(dat$date %>% unique %>% sort),
                                         by = 'day'),
                         join = 1)
  states <- dat %>% 
    dplyr::ungroup() %>% 
    dplyr::distinct(state, state.code, population) %>% 
    dplyr::mutate(join = 1)
  
  case.types <- tibble(type = dat$type %>% unique() %>% sort(), join = 1)
  
  index.tbl <- full_join(days, states, by = 'join') %>% 
    dplyr::full_join(case.types, by = 'join') %>% 
    dplyr::select(-join)
  
  new.dat <- dplyr::left_join(index.tbl, dat, by = c('date', 'state', 'state.code', 'type', 'population'))
  
  new.dat %>% 
    dplyr::mutate(cases = if_else(is.na(cases), 0 , cases)) %>% 
    dplyr::arrange(date) %>% 
    dplyr::group_by(state, type) %>% 
    # build cumul
    dplyr::mutate(cumul = cumsum(cases)) %>% 
    # remove latest day from countries that don't have data
    dplyr::arrange(desc(date)) %>% 
    dplyr::mutate(rev.cumul = cumsum(cases)) %>% 
    dplyr::filter(rev.cumul > 0) %>% 
    dplyr::select(-rev.cumul) %>% 
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