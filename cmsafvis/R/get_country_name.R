#' Get the country name for a iso country code
#'
#' @param country_code three-letter iso country code, all capitalized.
#' @param language either "eng" or "deu".
#'
#' @return the name of the country in the chosen language.
#'
#' @examples
#'get_country_name(c("DEU", "GBR", "TOT", "USA", "AFR", "EUR", "S_A"))
#'get_country_name(c("DEU", "GBR", "TOT", "USA", "AFR", "EUR", "S_A"),
#'  language = "deu")
#'
#' @importFrom assertthat assert_that is.string
#' @noRd
get_country_name <- function(country_code, language = "eng") {
  assert_that(is.string(language))
  assert_that(language %in% c("eng", "deu"))
  assert_that(is.character(country_code))
  assert_that(all(nchar(country_code) == 3))
  assert_that(all(is.upper(country_code)))

  if (language == "eng") {
    destination <- "country.name.en"
  } else if (language == "deu") {
    destination <- "country.name.de"
  }

  origin <- "iso3c"
  country_name <- countrycode::countrycode(
    country_code,
    origin,
    destination,
    # This is generated in data-raw/generate_internal_data.R
    custom_dict = country_codes
  )

  return(country_name)
}
