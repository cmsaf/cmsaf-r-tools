#' Is country
#'
#' Determines whether a given region is a country or not.
#'
#' @param region_code the three-letter code, all capitalized.
#'
#' @return \code{TRUE} if it is a country, \code{FALSE} otherwise.
#' @importFrom assertthat assert_that
#'
#' @examples
#' is_country(c("DEU", "GBR", "TOT", "AFR", "ESP", "EUR", "S_A", "USA", "123"))
#'
#' @noRd
is_country <- function(region_code) {
  assert_that(is.character(region_code))
  assert_that(all(nchar(region_code) == 3))
  assert_that(all(is.upper(region_code)))

  origin <- "iso3c"
  destination <- "is.country"
  is_country <- countrycode::countrycode(
    region_code,
    origin,
    destination,
    custom_dict = country_codes,
    warn = FALSE
  )

  is_country[is.na(is_country)] <- FALSE

  return(as.logical(is_country))
}
