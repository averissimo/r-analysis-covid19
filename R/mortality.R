#' Download report
#'
#' @param index index of report to extract (1 is the latest)
#' @param only.date filter out if date isn't the same (NULL for keep whatever report it is)
#'
#' @return report contents
#' @export
download.evm <- function() {
  url <- 'https://evm.min-saude.pt/table?t=geral&s=0'

  #Reading the HTML code from the website
  webpage <- xml2::read_html(url)

  json_text <- rvest::html_nodes(webpage, 'script') %>%
      rvest::html_text()

  return(json_text)
}


#' @examples 
#' show.year.mortality(mortality.with.stats, seq(2019, 2020))
  show.year.mortality <- function(dat, year = NULL) {
  my.plot <- dat %>%
    melt(id = c('day', 'mean', 'meanPre2020', 'sd', 'sdPre2020', 'max', 'maxPre2020', 'min', 'minPre2020')) %>%
    filter(is.null(year) | variable %in% year) %>%
    ggplot() +
    geom_crossbar(aes(x = day, y = meanPre2020, ymin=minPre2020, ymax=maxPre2020, fill = 'Min/Max from 2009-19'), alpha = 0, size = .1, width = 1) +
    geom_crossbar(aes(x = day, y = meanPre2020, ymin=minPre2020, ymax=maxPre2020, color = 'Min/Max from 2009-19', fill = 'Min/Max from 2009-19'), alpha = 1, size = .1, width = 1, show.legend = FALSE) +
    #
    geom_crossbar(aes(x = day, y = meanPre2020, ymin=meanPre2020 - sdPre2020, ymax=meanPre2020 + sdPre2020, fill = '2009-19'), alpha = 0, size = .1, width = 1) +
    geom_crossbar(aes(x = day, y = meanPre2020, ymin=meanPre2020 - sdPre2020, ymax=meanPre2020 + sdPre2020, color = '2009-19', fill = '2009-19'), alpha = .4, size = .1, width = 1, show.legend = FALSE) +
    #
    geom_smooth(aes(x = day, y = value, color = 'Smooth regression from 2009-19', group = NA), span = 0.1, method = 'loess', formula = y ~ x, na.rm = TRUE, se = FALSE) +
    geom_smooth(aes(x = day, y = meanPre2020, color = 'Smooth regression from 2009-19', group = NA), span = 0.1, method = 'loess', formula = y ~ x, na.rm = TRUE, show.legend = FALSE) +
    #
    geom_line(aes(x = day, y = value, group = variable, color = 'Year'), size = 1, alpha = 1, na.rm = TRUE) +
    #
    geom_smooth(aes(x = day, y = value, group = variable, color = 'Smooth regression from Year'), span = 0.1, method = 'loess', formula = y ~ x, na.rm = TRUE, se = FALSE) +
    geom_smooth(aes(x = day, y = value, group = variable, color = 'Smooth regression from Year'), span = 0.1, method = 'loess', formula = y ~ x, na.rm = TRUE, show.legend = FALSE) +
    #
    geom_blank(data = data.frame(x = rep(5, 5), y = 0.5, 
                                 group = c("Min/Max from 2009-19", "2009-19", "Smooth regression from 2009-19", "Year", "Smooth regression from Year")), 
               aes(y = y, color = group, fill = group),
               show.legend = FALSE) +
    scale_x_discrete(breaks = function(ix) {
      ix[c(FALSE, TRUE, TRUE, TRUE, TRUE)] <- FALSE
      return(ix)
    }) +
    scale_color_manual("", values = c("darkgreen", "darkgray", "green", "red", 'darkred'), breaks=c("2009-19", "Min/Max from 2009-19", "Smooth regression from 2009-19", "Year", "Smooth regression from Year")) +
    scale_fill_manual("Legend_no", values = c("darkgreen", "darkgray", "green", "red", 'darkred'), guide=FALSE, breaks=c("2009-19", "Min/Max from 2009-19", "Smooth regression from 2009-19", "Year", "Smooth regression from Year")) +
    facet_wrap( ~ variable, ncol = 1) +
    theme_minimal() +
    
    theme(legend.key = element_rect(fill = "white", colour = "white"), legend.position = 'top', axis.text.x = element_text(angle = 45, hjust = 1, size = 5))

  return(my.plot)
}