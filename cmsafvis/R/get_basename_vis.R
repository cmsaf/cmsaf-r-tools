#' Determine the basename of a NetCDF file
#'
#' This function determines the basename of either a file/URL path or an 'nc'
#' object (using nc$filename).
#'
#' When the origin of the file path is a local .nc file then
#' \code{get_basename_vis()} is equivalent to \code{base::basename()}.
#'
#' \code{get_basename_vis()} also handles the case of \code{infile}/\code{nc}
#' originating from a URL. 
#' 
#' The value of \code{get_basename_vis()} always ends in ".nc".
#'
#' If both \code{infile} and \code{nc} are specified, \code{infile} is ignored.
#' 
#'
#' @param infile Filename of input NetCDF file. This may include the directory
#'  (character).
#' @param nc Alternatively to \code{infile} you can specify the input as an
#'  object of class `ncdf4` (as returned from \code{ncdf4::nc_open}).
#'
#' @return A character string giving the basename.
#' @export
#'
get_basename_vis <- function(infile, nc = NULL) {
  if (!is.null(nc)) {
    # Get the basename of the filename of the nc object
    b <- basename(nc$filename)
  } else b <- basename(infile)
  # If the filename is a URL it may not end in ".nc" e.g. it may include a query at the end
  # If so, this extracts the part of the basename before ".nc"
  if (!endsWith(b, ".nc")) b <- paste0(sub(".nc.*", "", b), ".nc")
  return(b)
}
