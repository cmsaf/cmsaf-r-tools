#' Create country mask
#'
#' Create mask file for a country
#'
#' @noRd
create_country_mask <- function(infile,
                        temp_dir,
                        country_code,
                        states,
                        verbose,
                        nc = NULL) {

  # This function extracts a region defined by a polygon from a gridded data set
  if (missing(infile) && is.null(nc)) {
    stop("Please specify an infile or nc object.")
  }

  if (missing(country_code)) {
    stop("Please specify a country code (or use 'S_A' instead).")
  }

  # Create outfile name
  outfile <- add_ncdf_ext(construct_filename("Country_mask", country_code))
  outfile <- file.path(temp_dir, outfile)

  # If mask exists, check spatial range and decide if we can re-use it
  if (file.exists(outfile)) {
    reuse_mask <- compare_spatial_range(
      outfile,
      infile,
      nc_file2 = nc
    )

    # Check area and step lengths
    if (reuse_mask) {
      if (verbose) {
        message(
          paste("Re-use country mask:", normalizePath(outfile))
        )
      }
      return(outfile)
    }
  }

  country_name <- get_country_name(country_code)

  countriesHigh <- numeric(0)  # Hack to prevent IDE warning in second next line (see https://stackoverflow.com/questions/62091444/how-to-load-data-from-other-package-in-my-package)
  utils::data("countriesHigh", package = "rworldxtra", envir = environment())

  # extract the Polygon from the file
  idx <- which(countriesHigh$ISO3.1 == country_code)
  poly <- countriesHigh[idx, ]
  poly <- methods::as(poly, "SpatialPolygons")

  # Open the netcdf-file
  if (!is.null(nc)) nc_in <- nc
  else nc_in <- ncdf4::nc_open(infile)

  # Retrieve the grid and the dimensions
  lon <- ncdf4::ncvar_get(nc_in, "lon")
  lat <- ncdf4::ncvar_get(nc_in, "lat")
  nx <- nc_in$dim$lon$len
  ny <- nc_in$dim$lat$len

  londim <- nc_in$dim[["lon"]]
  latdim <- nc_in$dim[["lat"]]

  # Close the file
  if (is.null(nc)) ncdf4::nc_close(nc_in)

  # Define the grid
  lonmin <- lon[1]
  latmin <- lat[1]
  dlon <- lon[2] - lon[1]
  dlat <- lat[2] - lat[1]
  nlon <- nx
  nlat <- ny

  # Set the grid
  grd <- terra::rast(xmin = lonmin, xmax = (lonmin + (dlon*nlon)), 
                     ymin = latmin, ymax = (latmin + (dlat*nlat)), 
                     nrow = nlon, ncol = nlat)
  terra::crs(grd) <- "epsg:4326"
  grd <- terra::init(grd, 1)
  poly <- terra::vect(poly)

  # Determine the pixels inside the Polygon
  dummy <- terra::extract(grd, poly, cells=TRUE)
  
  idx <- rep(NA, nlon * nlat)
  idx[dummy$cell] <- 1

  #Define the mask and invert the latitudes
  Poly.grid <- matrix(idx, nrow = nlon, ncol = nlat)
  Poly.grid <- Poly.grid[, nlat:1]

  nc.dimlon <- ncdf4::ncdim_def(londim$name, londim$units, londim$vals)
  nc.dimlat <- ncdf4::ncdim_def(latdim$name, latdim$units, latdim$vals)
  nc.dimtim <- ncdf4::ncdim_def(name = "time", units = "days since 1970-01-01 00:00:00", vals = 1, unlim = TRUE)
  nc.Polygon <- ncdf4::ncvar_def(country_name, "-", list(nc.dimlon, nc.dimlat, nc.dimtim), -999, prec = "short")

  ncnew <- ncdf4::nc_create(outfile, nc.Polygon)

  # write the data to netcdf
  ncdf4::ncvar_put(ncnew,
                   nc.Polygon,
                   Poly.grid,
                   start = c(1, 1, 1),
                   count = c(-1, -1, -1))

  ncdf4::nc_close(ncnew)

  # Check if creation was successful and throw error if failed.
  if (!file.exists(outfile)) {
    stop("Failed to generate country mask")
  }

  return(outfile)
}

#' Create final country mask
#'
#' Create country mask with specific coordinates
#'
#' @noRd
create_country_mask_final <- function(mask_infile,
                                      temp_dir,
                                      lon_min,
                                      lon_max,
                                      lat_min,
                                      lat_max,
                                      country_code,
                                      verbose) {
  outfile <- add_ncdf_ext(construct_filename("Country_mask", country_code, "final"))
  outfile <- file.path(temp_dir, outfile)

  if (!file.exists(mask_infile)) {
    stop("Mask infile does not exist. Make sure that create_country_mask is called before this function.")
  }

  country_name <- get_country_name(country_code)

  cmsafops::sellonlatbox(
    var = country_name,
    infile = mask_infile,
    outfile = outfile,
    lon1 = lon_min,
    lon2 = lon_max,
    lat1 = lat_min,
    lat2 = lat_max,
    overwrite = TRUE
  )

  x <- ncdf4::nc_open(outfile)
  if (all(is.na(ncdf4::ncvar_get(x, country_name)))) {
    ncdf4::nc_close(x)
    stop(
      paste0(
        "The chosen country (",
        country_name,
        ") and the selected area, (lon=(",
        lon_min, ", ", lon_max,
        "), lat=(",
        lat_min, ", ", lat_max,
        ")) have no spatial overlap!"))
  }
  ncdf4::nc_close(x)

  return(outfile)
}

