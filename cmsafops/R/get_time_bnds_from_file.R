get_time_bounds_from_file <- function(infile) {
  nc_in <- nc_open(infile)
  time_bnds <- ncvar_get(nc_in, TIME_BOUNDS_NAMES$DEFAULT,
                         collapse_degen = FALSE)
  nc_close(nc_in)

  return(time_bnds)
}
