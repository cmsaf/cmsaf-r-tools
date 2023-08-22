#' Is uppercase
#'
#' @param text is the text to check.
#'
#' @return \code{TRUE} if text is completely upper case, \code{FALSE} otherwise.
#' @importFrom assertthat assert_that
#'
#' @examples
#' is.upper(c("YES", "no"))
#'
#' @noRd
is.upper <- function(text) {
  assert_that(is.character(text))
  return(text == toupper(text))
}


#' Suffix a file name with an extension
#'
#' @param filename the filename to extend.
#'
#' @return the filename with the given extension.
#' @importFrom assertthat assert_that is.string
#'
#' @examples
#' add_suffix("SDU_2019_ESP_mask", ".nc")
#' add_suffix("filename", ".png")
#'
#' @noRd
add_suffix <- function(filename, suffix) {
  assert_that(is.character(filename))
  assert_that(is.string(suffix))

  return(paste0(filename, suffix))
}

add_ncdf_ext <- function(filename) {
  return(add_suffix(filename, ".nc"))
}

add_image_ext <- function(filename) {
  return(add_suffix(filename, ".png"))
}

add_video_ext <- function(filename) {
  return(add_suffix(filename, ".mp4"))
}

#' Construct a filename
#'
#' @param ... the parts of the filename.
#' @param sep the separator to use for the parts.
#'
#' @return the concatenated parts of the filename.
#'
#' @examples
#' construct_filename("SDU", "accumulated", "2019")
#'
#' @noRd
construct_filename <- function(..., sep = "_") {
  return(paste(..., sep = sep))
}
