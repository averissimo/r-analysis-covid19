#' Maps type of case
#'
#' @param value one of 3, 'confirmed', 'death', 'all'
#' @param capitalize only first word
#' @param capitalize.all all words
#'
#' @return a formatted version of value
#' @export
#'
#' @examples
#' proper.cases(c('death', 'confirmed'))
#' proper.cases(c('death', 'confirmed'), capitalize = TRUE)
proper.cases <- function(value, capitalize = FALSE, capitalize.all = FALSE) {
    val = (dplyr::if_else(value == 'confirmed', 'confirmed cases', dplyr::if_else(value == 'death', 'deaths', dplyr::if_else(value == 'all', 'cases', value))))
    if (capitalize.all) {
        return(loose.rock::proper(val))
    } else if (capitalize) {
        substring(val, 1, 1) = toupper(substring(val, 1, 1))
        return(val)
    } else {
        return(val)
    }
}
