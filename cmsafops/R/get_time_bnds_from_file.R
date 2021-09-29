get_time_bounds_from_file <- function(infile, nc = NULL) {
  nc_in <- nc_open(infile)
  time_bnds <- ncvar_get(nc_in, TIME_BOUNDS_NAMES$DEFAULT,
                         collapse_degen = FALSE)
  if (is.null(nc)) nc_close(nc_in)

  return(time_bnds)
}
