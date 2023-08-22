get_continental_coordinates <- function(country_code) {
  if (country_code == "AFR") {
    return(
      list(
        lon_min = -25,
        lon_max = 60,
        lat_min = -45,
        lat_max = 42
      )
    )
  }

  if (country_code == "EUR") {
    return(
      list(
        lon_min = -25,
        lon_max = 35,
        lat_min = 30,
        lat_max = 65
      )
    )
  }

  if (country_code == "TOT") {
    return(
      list(
        lon_min = -65,
        lon_max = 65,
        lat_min = -65,
        lat_max = 65
      )
    )
  }
}
