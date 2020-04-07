#' Convert country names in John Hopkins dataset
#'
#' @param state what
#'
#' @return
jh.convert.names <- function(val) {
  convert.tmp <- list(
    'Brunei' = 'Brunei Darussalam',
    'Burma' = 'Myanmar',
    'Cabo Verde' = 'Cape Verde',
    'Congo (Brazzaville)' = 'Congo',
    'Congo (Kinshasa)' = 'Democratic Republic of the Congo',
    'Czechia' = 'Czech Republic',
    'Diamond Princess' = 'Cases on an international conveyance Japan',
    'Guinea-Bissau' = 'Guinea Bissau',
    'Korea, South' = 'Korea, Rep.',
    # 'Malawi' = 'Malawi',
    'Taiwan*' = 'Taiwan',
    'Tanzania' = 'United Republic of Tanzania',
    'Timor-Leste' = 'Timor Leste',
    'United States' = 'USA',
    'US' = 'USA',
    'West Bank and Gaza' = 'Palestine'
  )
  
  val[val %in% names(convert.tmp)] <- sapply(val[val %in% names(convert.tmp)], function(ix) { convert.tmp[[ix]] })
  return(val)
}

#' Convert country names in EU CDC dataset
#'
#' @param state what
#'
#' @return
eu.convert.names <- function(val) {
  convert.tmp <- list(
    # Unconvertables
    '404-1' =  'Anguilla',
    '404-2' =  'Cases on an international conveyance Japan',
    '404-2' =  'Guernsey',
    '404-3' =  'Jersey',
    '404-4' =  'Montserrat',
    '404-5' =  'Palestine',
    #
    'Bahamas, The' =  'Bahamas',
    'Cabo Verde' =  'Cape Verde',
    'Congo, Rep.' =  'Congo',
    'Cote d\'Ivoire' =  'Cote dIvoire',
    'Congo, Dem. Rep.' =  'Democratic Republic of the Congo',
    'Czechia' = 'Czech Republic',
    'Egypt, Arab Rep.' =  'Egypt',
    'Gambia, The' =  'Gambia',
    'Guinea-Bissau' =  'Guinea Bissau',
    'Iran, Islamic Rep.' =  'Iran',
    'Kyrgyz Republic' =  'Kyrgyzstan',
    'Lao PDR' =  'Laos',
    'Russian Federation' =  'Russia',
    'St. Kitts and Nevis' =  'Saint Kitts and Nevis',
    'St. Lucia' =  'Saint Lucia',
    'St. Martin' =  'Saint Vincent and the Grenadines',
    'Sint Maarten (Dutch part)' =  'Sint Maarten',
    'Slovak Republic' =  'Slovakia',
    'Korea, Rep.' =  'South Korea',
    'Syrian Arab Republic' =  'Syria',
    'Taiwan*' =  'Taiwan',
    'Timor-Leste' =  'Timor Leste',
    'Turks and Caicos Islands' =  'Turks and Caicos islands',
    'Tanzania' =  'United Republic of Tanzania',
    'United States' =  'USA',
    'Virgin Islands (U.S.)' =  'United States Virgin Islands',
    'Venezuela, RB' =  'Venezuela',
    #
    'St. Vincent and the Grenadines' =  'Saint Vincent and the Grenadines'
  )
  val[val %in% names(convert.tmp)] <- sapply(val[val %in% names(convert.tmp)], function(ix) { convert.tmp[[ix]] })
  return(val)
}

