get_title <- function(variable, plot_type = NULL, language, year = NULL) {
  translations <- get_translations()

  assert_that(toupper(variable) %in% dimnames(translations)[[1]])
  assert_that(language %in% c("deu", "eng"))

  if (is.null(plot_type)) {
    assert_that(assertthat::is.string(year))
    assert_that(nchar(year) == 4)
    title <- paste(year, "vs.", switch(
      language,
      "eng" = "Climatology",
      "deu" = "Klimatologie",
      stop("unknown language")
    ))
    return(title)
  }

  assert_that(plot_type %in% c(
    "absolute_map",
    "anomaly_map",
    "climatology_map"
  ))
  translation <- translations[toupper(variable), "title", language]
  if (language == "deu") {
    if (plot_type == "absolute" && toupper(variable) == "CTP") {
      # Special case
      return(paste("absoluter", translation))
    }
    title <- switch(
      plot_type,
      "absolute_map" = paste("absolute", translation),
      "anomaly_map" = paste(translation, "Anomalie"),
      "climatology_map" = paste(translation, "Klimatologie"),
      stop(paste("no title for plot_type", plot_type))
    )
  } else if (language == "eng") {
    title <- switch(
      plot_type,
      "absolute_map" = paste("absolute", translation),
      "anomaly_map" = paste(translation, "anomaly"),
      "climatology_map" = paste("mean annual", translation),
      stop(paste("no title for plot_type", plot_type))
    )
  }

  return(title)
}

get_unit <- function(variable, language) {
  translations <- get_translations()

  assert_that(toupper(variable) %in% dimnames(translations)[[1]])
  assert_that(language %in% c("deu", "eng"))

  unit <- translations[toupper(variable), "unit", language]
  return(unit)
}

get_climatology_word <- function(language) {
  assert_that(language %in% c("eng", "deu"))

  word <- switch(
    language,
    "eng" = "Climatology",
    "deu" = "Klimatologie",
    stop("unknown language")
  )

  return(word)
}

get_translation_duration <- function(language) {
  assert_that(language %in% c("eng", "deu"))

  word <- switch(
    language,
    "eng" = "Time span",
    "deu" = "Zeitraum",
    stop("unknown language")
  )

  return(word)
}

get_axis_label <- function(variable, language) {
  translations <- get_translations()

  assert_that(toupper(variable) %in% dimnames(translations)[[1]])
  assert_that(language %in% c("deu", "eng"))

  label <- paste0(
    translations[toupper(variable), "title", language],
    " [",
    translations[toupper(variable), "unit", language],
    "]"
  )
  return(label)
}

get_translations <- function() {
  variable_titles_english <- c(
    "Sunshine Duration",
    "Surface incoming shortwave radiation",
    "Fractional cloud cover",
    "Direct Normalised Irradiance",
    "Surface incoming direct radiation",
    "Cloud Top Temperature",
    "Cloud Top Height",
    "Cloud Top Pressure"
  )

  variable_units_english <- c(
    "hours",
    "W/m^2",
    "percent",
    "W/m^2",
    "W/m^2",
    "K",
    "m",
    "hPa"
  )

  variable_titles_german <- c(
    "Sonnenscheindauer",
    "Globalstrahlung",
    "Wolkenbedeckung",
    "Normalisierte Direktstrahlung",
    "Kurzwellige Direktstrahlung",
    "Wolkenoberkantentemperatur",
    "Wolkenoberkantenh\u00f6he",
    "Wolkenoberkantendruck"
  )

  variable_units_german <- c(
    "Stunden",
    "W/m^2",
    "Prozent",
    "W/m^2",
    "W/m^2",
    "K",
    "m",
    "hPa"
  )

  translations <- array(
    data = cbind(
      variable_titles_english, variable_units_english,
      variable_titles_german, variable_units_german
    ),
    dim = c(
      length(variable_titles_english),
      2,
      2
    ),
    dimnames = list(
      c("SDU", "SIS", "CFC", "DNI", "SID", "CTT", "CTH", "CTP"),
      c("title", "unit"),
      c("eng", "deu")
    )
  )

  return(translations)
}
