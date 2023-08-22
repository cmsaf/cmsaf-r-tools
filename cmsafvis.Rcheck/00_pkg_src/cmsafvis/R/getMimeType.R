getMimeType <- function(filename) {
  assertthat::assert_that(is.character(filename))

  allowedFiles <- c(".png", ".tif", ".jpeg", ".jpg", ".pdf", ".kml")

  assertthat::assert_that(!all(!endsWith(filename, allowedFiles)))
  if (endsWith(filename, ".png")) {
    return("image/png")
  } else if (endsWith(filename, ".tif")) {
    return("image/tiff")
  } else if (endsWith(filename, ".jpeg") || endsWith(filename, ".jpg")) {
    return("image/jpeg")
  } else if (endsWith(filename, ".pdf")) {
    return("application/pdf")
  } else if (endsWith(filename, ".kml")) {
    return("application/vnd.google-earth.kml+xml")
  }
}
