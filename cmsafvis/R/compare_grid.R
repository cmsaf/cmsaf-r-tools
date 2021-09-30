# Function for comparing the spatial range of infile1 with passed lon/lat range
# while the grid distance between lons and lats are compared to infile2
compare_grid <- function(
  infile1,
  infile2,
  lon_min,
  lon_max,
  lat_min,
  lat_max,
  nc1 = NULL,
  nc2 = NULL
) {
  if (!is.null(nc1)) nc <- nc1
  else nc <- ncdf4::nc_open(infile1)

  # Retrieve the grid and the dimensions of the existing outfile
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  lon_range <- range(lon)
  lat_range <- range(lat)
  dx <- lon[2] - lon[1]
  dy <- lat[2] - lat[1]

  # Close the file
  if (is.null(nc1)) ncdf4::nc_close(nc)

  if (!is.null(nc2)) nc_dims <- nc2
  else nc_dims <- ncdf4::nc_open(infile2)
  # Retrieve the grid and the dimensions of the existing outfile
  lon_steps <- ncdf4::ncvar_get(nc_dims, "lon")
  lat_steps <- ncdf4::ncvar_get(nc_dims, "lat")
  dx_steps <- lon_steps[2] - lon_steps[1]
  dy_steps <- lat_steps[2] - lat_steps[1]

  # Close the file
  if (is.null(nc2)) ncdf4::nc_close(nc_dims)

  # Check area and step lengths
  return(
    lon_min == lon_range[1] &&
      lon_max == lon_range[2] &&
      lat_min == lat_range[1] &&
      lat_max == lat_range[2] &&
      dx == dx_steps &&
      dy == dy_steps
  )
}
