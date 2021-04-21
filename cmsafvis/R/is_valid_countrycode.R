is_valid_regioncode <- function(code) {
  if (is_country(code)) {
    return(TRUE)
  }

  if (code %in% c("EUR", "AFR", "S_A", "TOT")) {
    return(TRUE)
  }

  return(FALSE)
}
