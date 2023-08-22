#' Set time locale
#'
#' Uses [Sys.setlocale()] under the hood.
#'
#' Be careful to reset the locale in order to not change the user's environment..
#'
#' @param language_code one of "eng", "deu", ""
#'
#' @return the new locale (from [Sys.setlocale()])
#' @noRd
#' @importFrom assertthat is.string
#'
#' @examples
#' set_time_locale("eng")
#' set_time_locale("")  # reset to default
set_time_locale <- function(language_code) {
  assert_that(is.string(language_code))
  assert_that(
    language_code %in% c("eng", "deu", ""),
    msg = "Only 'eng' and 'deu' are supported languages. Or use '' to reset."
  )

  # See documentation at `?locales`
  german <- c(
    "de",
    "de_DE",
    "de_DE.UTF-8",
    "de_DE.utf8",
    "German"
  )

  english <- c(
    "en",
    "en_US",
    "en_US.UTF-8",
    "en_US.utf8",
    "English"
  )

  language <- switch(
    language_code,
    "eng" = english,
    "deu" = german
  )

  new  <- ""
  for (lang_format in language) {
    if (new != "") break
    new <- suppressWarnings(Sys.setlocale("LC_TIME", lang_format))
  }

  # Locale couldn't be set
  if (language_code != "" && new == "") {
    stop(paste0(
      "Couldn't set locale to '",
      language_code,
      "'. Are you sure it's installed on the system?"
    ))
  }
  return(new)
}
