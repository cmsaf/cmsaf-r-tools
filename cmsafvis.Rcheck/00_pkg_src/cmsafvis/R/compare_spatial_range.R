# Function for comparing full spatial ranges of two files
compare_spatial_range <- function(
  file1,
  file2,
  nc_file1 = NULL,
  nc_file2 = NULL
) {
  if (!is.null(nc_file1)) nc1 <- nc_file1
  else nc1 <- ncdf4::nc_open(file1)

  # Retrieve the grid and the dimensions of the existing outfile
  lon1 <- ncdf4::ncvar_get(nc1, "lon")
  lat1 <- ncdf4::ncvar_get(nc1, "lat")
  lon1_range <- range(lon1)
  lat1_range <- range(lat1)
  nx1 <- lon1[2] - lon1[1]
  ny1 <- lat1[2] - lat1[1]

  # Close the file
  if (is.null(nc_file1)) ncdf4::nc_close(nc1)

  if (!is.null(nc_file2)) nc2 <- nc_file2
  else nc2 <- ncdf4::nc_open(file2)

  # Retrieve the grid and the dimensions of the existing outfile
  lon2 <- ncdf4::ncvar_get(nc2, "lon")
  lat2 <- ncdf4::ncvar_get(nc2, "lat")
  lon2_range <- range(lon2)
  lat2_range <- range(lat2)
  nx2 <- lon2[2] - lon2[1]
  ny2 <- lat2[2] - lat2[1]

  # Close the file
  if (is.null(nc_file2)) ncdf4::nc_close(nc2)

  return(
    lon1_range[1] == lon2_range[1] &&
      lon1_range[2] == lon2_range[2] &&
      lat1_range[1] == lat2_range[1] &&
      lat1_range[2] == lat2_range[2] &&
      nx1 == nx2 &&
      ny1 == ny2
  )
}
