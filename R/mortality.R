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


show.year.mortality <- function(dat, year = NULL) {
  my.plot <- dat %>%
    melt(id = c('day', 'mean', 'meanPre2020', 'sd', 'sdPre2020', 'max', 'maxPre2020', 'min', 'minPre2020')) %>%
    filter(is.null(year) | variable %in% year) %>%
    ggplot() +
    geom_crossbar(aes(x = day, y = meanPre2020, ymin=minPre2020, ymax=maxPre2020), color = NA, fill = "red", alpha = .5, size = .1, width = 1) +
    geom_crossbar(aes(x = day, y = meanPre2020, ymin=meanPre2020-sdPre2020, ymax=meanPre2020+sdPre2020), color = 'orange', fill = "blue", alpha = .7, width = 1) +
    geom_smooth(aes(x = day, y = meanPre2020, group = NA), span = 0.1, color = 'green', method = 'loess', formula = y ~ x, na.rm = TRUE) +
    geom_line(aes(x = day, y = value, group = variable, alpha = .7), na.rm = TRUE) +
    geom_smooth(aes(x = day, y = value, group = variable), span = 0.1, method = 'loess', formula = y ~ x, na.rm = TRUE) +
    scale_x_discrete(breaks = function(ix) {
      ix[c(FALSE, TRUE, TRUE, TRUE, TRUE)] <- FALSE
      return(ix)
    }) +
    facet_wrap( ~ variable, ncol = 1) +
    labs(caption = 'red and orange background bars are error plots between 2009-2019.\ngreen line represents a smooth regression of mean cases between 2009-2019.') +
    theme_minimal() +
    theme(legend.position = 'none', axis.text.x = element_text(angle = 45, hjust = 1, size = 5))

  return(my.plot)
}
