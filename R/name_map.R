proper.cases <- function(value, capitalize = FALSE, capitalize.all = FALSE) {
    val = if (value == 'confirmed') {
        'confirmed cases'
    } else if (value == 'death') {
        'deaths'
    } else {
        return(value)
    }
    if (capitalize.all) {
        return(loose::rock(proper(val)))
    } else if (capitalize) {
        substring(val, 1, 1) = toupper(substring(val, 1, 1))
        return(val)
    } else {
        return(val)
    }
}
