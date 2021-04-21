#' Designed for the CM SAF R Toolbox.
#'
#' This function checks the nc version.
#' 
#' @param nc34 (numeric)
#' @export
get_nc_version <- function(nc34) {
  if (nc34 == 4) {
    nc_format <- TRUE
    compression <- 4
  } else {
    nc_format <- FALSE
    compression <- NA
  }

  return(list(force_v4 = nc_format, compression = compression))
}
