#' Add grid info
#'
#' Adds a standard longitude/latitude grid to a file which is based on a different grid.
#'
#' No existing data is changed. The additional grid info is added as two
#'   additional variables (lon and lat).
#'
#' @param infile Character containing file name or path of input file.
#' @param auxfile Character containing file name or path of auxiliary file.
#' @param outfile Character containing file name or path of output file. If
#'   NULL, the input file is directly edited instead.
#' @param overwrite Logical; should existing output file be overwritten? If
#'   outfile is NULL, this parameter is ignored.
#' @param verbose logical; if TRUE, progress messages are shown
#'
#' @export
#'
#'@family data manipulation functions
add_grid_info <- function(infile, auxfile, outfile, overwrite = FALSE, verbose = FALSE) {
# TODO: add examples to documentation
  check_infile(infile)
  check_infile(auxfile)
  stopifnot(length(outfile) <= 1, is.character(outfile) | is.null(outfile))

  if (!is.null(outfile)) {
    outfile <- correct_filename(outfile)
    check_overwrite(outfile, overwrite)
  }

  calc_time_start <- Sys.time()

  # Decide whether to edit the infile or create a new one to edit.
  if (is.null(outfile)) {
    file_to_change <- infile
  } else {
    stopifnot(file.copy(infile, outfile, overwrite = overwrite))
    file_to_change <- outfile
  }

  # Get data from aux file.
  nc_aux <- nc_open(auxfile)


  lon <- ncvar_get(nc_aux, "lon")  #TODO be more resistant to different spellings
  lon_list <- nc_aux$var[["lon"]]

  lat <- ncvar_get(nc_aux, "lat")
  lat_list <- nc_aux$var[["lat"]]
  nc_close(nc_aux)

  # Add variables to existing file.
  nc <- nc_open(file_to_change, write = TRUE)

  dimnames   <- names(nc$dim)
  varnames <- names(nc$var)

  if ("lon" %in% c(dimnames, varnames)) {
    stop("infile already contains lon/lat data")
  }

  dim_x <- nc$dim[["x"]]
  dim_y <- nc$dim[["y"]]
  # Parameter 'shuffle' is ignored because it can only be used for values of
  # type 'integer', but lon and lat values are always of type 'double'.
  lon_new <- ncvar_def(name = lon_list$name,
                       units = lon_list$units,
                       dim = list(dim_x, dim_y),
                       missval = lon_list$missval,
                       longname = lon_list$longname,
                       prec = lon_list$prec,
                       compression = lon_list$compression,
                       chunksizes = lon_list$chunksizes
  )
  lat_new <- ncvar_def(name = lat_list$name,
                       units = lat_list$units,
                       dim = list(dim_x, dim_y),
                       missval = lat_list$missval,
                       longname = lat_list$longname,
                       prec = lat_list$prec,
                       compression = lat_list$compression,
                       chunksizes = lat_list$chunksizes
  )
  nc <- ncvar_add(nc, lon_new)
  ncvar_put(nc, lon_list$name, lon)
  nc <- ncvar_add(nc, lat_new)
  ncvar_put(nc, lat_list$name, lat)
  nc_close(nc)

  calc_time_end <- Sys.time()

  if (verbose) {
    message(get_processing_time_string(calc_time_start, calc_time_end))
  }
}
