#' Select one or more countries from a CM SAF NetCDF file
#'
#' This function masks a CM SAF NetCDF file by one or more selected countries
#' and writes the result to a new NetCDF file. The country polygons are taken
#' from `rworldxtra::countriesHigh`. Values outside the selected countries are
#' set to missing values by adding a one-timestep mask to the input data.
#'
#' @param var Name of NetCDF variable (character).
#' @param infile Filename of input NetCDF file. This may include the directory
#'   (character).
#' @param outfile Filename of output NetCDF file. This may include the directory
#'   (character).
#' @param country_code One or more three-letter ISO3 country codes, all
#'   capitalized or convertible to uppercase (character).
#' @param crop Logical; if TRUE, the input file is first cropped to the bounding
#'   box of the selected country or countries before the mask is applied.
#' @param overwrite Logical; should an existing output file be overwritten?
#' @param states Logical; reserved for future support of sub-national regions.
#'   Currently only country polygons are supported.
#' @param verbose Logical; if TRUE, progress messages are shown.
#' @param nc Alternatively to `infile` you can specify the input as an object of
#'   class `ncdf4` as returned from `ncdf4::nc_open`.
#'
#' @return A NetCDF file containing the selected country or countries is written.
#'   The return value is the normalized output filename, invisibly.
#' @export
#' @family selection and removal functions
#'
#' @examples
#' \dontrun{
#' selcountry(
#'   var = "SIS",
#'   infile = "SIS_input.nc",
#'   outfile = "SIS_Germany.nc",
#'   country_code = "DEU",
#'   crop = TRUE,
#'   overwrite = TRUE
#' )
#'
#' selcountry(
#'   var = "SIS",
#'   infile = "SIS_input.nc",
#'   outfile = "SIS_DACH.nc",
#'   country_code = c("DEU", "AUT", "CHE"),
#'   crop = TRUE,
#'   overwrite = TRUE
#' )
#' }
selcountry <- function(var,
                       infile,
                       outfile,
                       country_code,
                       crop = TRUE,
                       overwrite = FALSE,
                       states = FALSE,
                       verbose = FALSE,
                       nc = NULL) {
  check_variable(var)
  
  if (missing(infile) && is.null(nc)) {
    stop("Please specify an infile or nc object.")
  }
  if (is.null(nc)) {
    check_infile(infile)
  }
  
  check_outfile(outfile)
  outfile <- correct_filename(outfile)
  check_overwrite(outfile, overwrite)
  
  if (missing(country_code) || !is.character(country_code) || !length(country_code)) {
    stop("Please specify at least one ISO3 country code.")
  }
  
  country_code <- unique(toupper(country_code))
  if (!all(nchar(country_code) == 3)) {
    stop("All country codes must be three-letter ISO3 codes.")
  }
  
  if (!is.logical(crop) || length(crop) != 1) {
    stop("crop must be TRUE or FALSE.")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1) {
    stop("overwrite must be TRUE or FALSE.")
  }
  if (!is.logical(states) || length(states) != 1) {
    stop("states must be TRUE or FALSE.")
  }
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("verbose must be TRUE or FALSE.")
  }
  
  if (states) {
    warning(
      "The states argument is currently ignored. Only country polygons are supported.",
      call. = FALSE
    )
  }
  
  calc_time_start <- Sys.time()
  
  country_poly <- selcountry_get_country_polygons(country_code)
  grid_info <- selcountry_get_regular_lonlat(infile = infile, nc = nc)
  
  temp_files <- character(0)
  on.exit({
    if (length(temp_files)) {
      unlink(temp_files[file.exists(temp_files)], force = TRUE)
    }
  }, add = TRUE)
  
  work_file <- infile
  work_nc <- nc
  
  if (crop) {
    bbox <- selcountry_get_bbox(country_poly, grid_info)
    
    cropped_file <- tempfile(
      pattern = paste0(var, "_", paste(country_code, collapse = "_"), "_bbox_"),
      fileext = ".nc"
    )
    temp_files <- c(temp_files, cropped_file)
    
    if (verbose) {
      message(
        "Cropping input file to country bounding box: lon=(",
        signif(bbox$lon_min, 7), ", ", signif(bbox$lon_max, 7),
        "), lat=(", signif(bbox$lat_min, 7), ", ", signif(bbox$lat_max, 7), ")."
      )
    }
    
    sellonlatbox(
      var = var,
      infile = infile,
      outfile = cropped_file,
      lon1 = bbox$lon_min,
      lon2 = bbox$lon_max,
      lat1 = bbox$lat_min,
      lat2 = bbox$lat_max,
      overwrite = TRUE,
      verbose = verbose,
      nc = nc
    )
    
    work_file <- cropped_file
    work_nc <- NULL
  }
  
  mask_file <- tempfile(
    pattern = paste0("Country_mask_", paste(country_code, collapse = "_"), "_"),
    fileext = ".nc"
  )
  temp_files <- c(temp_files, mask_file)
  
  selcountry_create_mask(
    infile = work_file,
    outfile = mask_file,
    country_poly = country_poly,
    nc = work_nc,
    verbose = verbose
  )
  
  if (verbose) {
    message("Applying country mask and writing output file.")
  }
  
  cmsaf.add(
    var1 = var,
    var2 = "country_mask",
    infile1 = work_file,
    infile2 = mask_file,
    outfile = outfile,
    overwrite = overwrite,
    verbose = verbose,
    nc1 = work_nc
  )
  
  calc_time_end <- Sys.time()
  if (verbose) {
    message("selcountry finished in ", round(difftime(calc_time_end, calc_time_start, units = "secs"), 2), " seconds.")
  }
  
  invisible(outfile)
}

#' Get country polygons for ISO3 country codes
#'
#' @noRd
selcountry_get_country_polygons <- function(country_code) {
  countriesHigh <- numeric(0)
  utils::data("countriesHigh", package = "rworldxtra", envir = environment())
  
  code_column <- NULL
  if ("ISO3.1" %in% names(countriesHigh)) {
    code_column <- "ISO3.1"
  } else if ("ADM0_A3" %in% names(countriesHigh)) {
    code_column <- "ADM0_A3"
  } else if ("SOV_A3" %in% names(countriesHigh)) {
    code_column <- "SOV_A3"
  }
  
  if (is.null(code_column)) {
    stop("Could not identify ISO3 country code column in rworldxtra::countriesHigh.")
  }
  
  available_codes <- as.character(countriesHigh@data[[code_column]])
  missing_codes <- setdiff(country_code, available_codes)
  
  if (length(missing_codes)) {
    stop(
      "The following country codes are not available as country polygons: ",
      paste(missing_codes, collapse = ", "),
      ". Only ISO3 country codes are supported; predefined regions such as EUR, AFR, TOT or S_A are not valid here."
    )
  }
  
  idx <- which(available_codes %in% country_code)
  methods::as(countriesHigh[idx, ], "SpatialPolygons")
}

#' Read regular lon-lat coordinates from a NetCDF file
#'
#' @noRd
selcountry_get_regular_lonlat <- function(infile, nc = NULL) {
  if (!is.null(nc)) {
    nc_in <- nc
    close_nc <- FALSE
  } else {
    nc_in <- ncdf4::nc_open(infile)
    close_nc <- TRUE
  }
  on.exit({
    if (close_nc) {
      ncdf4::nc_close(nc_in)
    }
  }, add = TRUE)
  
  if (!all(c("lon", "lat") %in% names(nc_in$dim))) {
    stop("The input file must contain lon and lat dimensions.")
  }
  
  lon_dim <- nc_in$dim[["lon"]]
  lat_dim <- nc_in$dim[["lat"]]
  
  # Prefer dimension values. Some CM SAF files provide lon/lat also as
  # two-dimensional coordinate variables although the data are stored on a
  # regular lon-lat grid. Reading ncvar_get(nc, "lon") can then return a matrix
  # and a simple dimensionality check would incorrectly reject the file.
  lon <- lon_dim$vals
  lat <- lat_dim$vals
  
  if (is.null(lon) || !length(lon) || all(is.na(lon))) {
    lon <- selcountry_get_coord_from_variable(nc_in, "lon", "x", lon_dim$len)
  }
  if (is.null(lat) || !length(lat) || all(is.na(lat))) {
    lat <- selcountry_get_coord_from_variable(nc_in, "lat", "y", lat_dim$len)
  }
  
  lon <- as.numeric(lon)
  lat <- as.numeric(lat)
  
  if (length(lon) < 2 || length(lat) < 2) {
    stop("The lon and lat dimensions must contain at least two values.")
  }
  if (length(lon) != lon_dim$len || length(lat) != lat_dim$len) {
    stop("Could not derive one-dimensional lon and lat coordinates from the input file.")
  }
  if (anyNA(lon) || anyNA(lat)) {
    stop("The lon and lat coordinates must not contain missing values.")
  }
  
  list(
    lon = lon,
    lat = lat,
    lon_dim = lon_dim,
    lat_dim = lat_dim
  )
}

#' Read a one-dimensional coordinate from a coordinate variable
#'
#' @noRd
selcountry_get_coord_from_variable <- function(nc_in, coord_name, axis, expected_len) {
  if (!coord_name %in% names(nc_in$var)) {
    stop("Could not find ", coord_name, " coordinate values in the input file.")
  }
  
  coord <- ncdf4::ncvar_get(nc_in, coord_name)
  coord <- selcountry_squeeze_singleton_dims(coord)
  
  if (is.null(dim(coord))) {
    coord <- as.numeric(coord)
    if (length(coord) == expected_len) {
      return(coord)
    }
    stop(
      "Could not derive one-dimensional ", coord_name,
      " coordinates from the input file. Expected ", expected_len,
      " values but found ", length(coord), "."
    )
  }
  
  if (length(dim(coord)) == 2) {
    candidates <- list(
      as.numeric(coord[, 1]),
      as.numeric(coord[1, ])
    )
    candidates <- candidates[vapply(candidates, length, integer(1)) == expected_len]
    
    if (length(candidates) >= 1) {
      return(candidates[[1]])
    }
  }
  
  dim_names <- names(dim(coord))
  if (is.null(dim_names)) {
    dim_names <- vapply(
      nc_in$var[[coord_name]]$dim,
      function(x) x$name,
      character(1)
    )
  }
  
  if (!is.null(dim_names) && axis == "x") {
    lon_index <- which(dim_names == "lon")
    if (length(lon_index) == 1 && dim(coord)[lon_index] == expected_len) {
      return(as.numeric(selcountry_slice_first(coord, lon_index)))
    }
  }
  
  if (!is.null(dim_names) && axis == "y") {
    lat_index <- which(dim_names == "lat")
    if (length(lat_index) == 1 && dim(coord)[lat_index] == expected_len) {
      return(as.numeric(selcountry_slice_first(coord, lat_index)))
    }
  }
  
  stop("selcountry currently supports regular lon-lat grids only.")
}

#' Remove singleton dimensions from a coordinate array
#'
#' @noRd
selcountry_squeeze_singleton_dims <- function(x) {
  dims <- dim(x)
  if (is.null(dims)) {
    return(x)
  }
  
  non_singleton <- dims > 1
  if (!any(non_singleton)) {
    return(as.numeric(x))
  }
  
  if (sum(non_singleton) == 1) {
    return(as.numeric(x))
  }
  
  x
}

#' Extract the first coordinate line along a selected dimension
#'
#' @noRd
selcountry_slice_first <- function(x, keep_dim) {
  idx <- rep(list(1), length(dim(x)))
  idx[[keep_dim]] <- TRUE
  do.call("[", c(list(x), idx, list(drop = TRUE)))
}

#' Get a country bounding box expanded by approximately one grid cell
#'
#' @noRd
selcountry_get_bbox <- function(country_poly, grid_info) {
  bbox <- sp::bbox(country_poly)
  
  lon <- grid_info$lon
  lat <- grid_info$lat
  dlon <- min(abs(diff(sort(unique(lon)))), na.rm = TRUE)
  dlat <- min(abs(diff(sort(unique(lat)))), na.rm = TRUE)
  
  lon_range <- range(lon, na.rm = TRUE)
  lat_range <- range(lat, na.rm = TRUE)
  
  list(
    lon_min = max(lon_range[1], bbox["x", "min"] - dlon),
    lon_max = min(lon_range[2], bbox["x", "max"] + dlon),
    lat_min = max(lat_range[1], bbox["y", "min"] - dlat),
    lat_max = min(lat_range[2], bbox["y", "max"] + dlat)
  )
}

#' Create a one-timestep country mask on the input grid
#'
#' The mask contains 0 inside the selected country polygons and missing values
#' outside. Adding this mask to the input data therefore keeps values inside the
#' countries unchanged and sets values outside the countries to missing values.
#'
#' @noRd
selcountry_create_mask <- function(infile,
                                   outfile,
                                   country_poly,
                                   nc = NULL,
                                   verbose = TRUE) {
  if (!is.null(nc)) {
    nc_in <- nc
    close_nc <- FALSE
  } else {
    nc_in <- ncdf4::nc_open(infile)
    close_nc <- TRUE
  }
  on.exit({
    if (close_nc) {
      ncdf4::nc_close(nc_in)
    }
  }, add = TRUE)
  
  grid_info <- selcountry_get_regular_lonlat(infile = infile, nc = nc_in)
  lon <- grid_info$lon
  lat <- grid_info$lat
  nx <- length(lon)
  ny <- length(lat)
  
  londim <- grid_info$lon_dim
  latdim <- grid_info$lat_dim
  
  lonmin <- lon[1]
  latmin <- lat[1]
  dlon <- lon[2] - lon[1]
  dlat <- lat[2] - lat[1]
  
  grid_topology <- sp::GridTopology(
    cellcentre.offset = c(lonmin, latmin),
    cellsize = c(dlon, dlat),
    cells.dim = c(nx, ny)
  )
  grid <- sp::SpatialGrid(grid_topology, proj4string = sp::proj4string(country_poly))
  
  inside <- !is.na(sp::over(grid, country_poly))
  mask <- matrix(ifelse(inside, 0, NA_real_), nrow = nx, ncol = ny)
  
  # Keep the orientation consistent with the existing country-mask workflow.
  mask <- mask[, ny:1]
  
  if (all(is.na(mask))) {
    stop("The selected country polygons do not overlap with the input grid.")
  }
  
  nc_dim_lon <- ncdf4::ncdim_def(londim$name, londim$units, londim$vals)
  nc_dim_lat <- ncdf4::ncdim_def(latdim$name, latdim$units, latdim$vals)
  nc_dim_time <- ncdf4::ncdim_def(
    name = "time",
    units = "days since 1970-01-01 00:00:00",
    vals = 1,
    unlim = TRUE
  )
  
  nc_mask_var <- ncdf4::ncvar_def(
    name = "country_mask",
    units = "-",
    dim = list(nc_dim_lon, nc_dim_lat, nc_dim_time),
    missval = -999,
    prec = "short"
  )
  
  if (file.exists(outfile)) {
    file.remove(outfile)
  }
  
  nc_out <- ncdf4::nc_create(outfile, nc_mask_var)
  ncdf4::ncvar_put(
    nc_out,
    nc_mask_var,
    mask,
    start = c(1, 1, 1),
    count = c(-1, -1, -1)
  )
  ncdf4::nc_close(nc_out)
  
  if (!file.exists(outfile)) {
    stop("Failed to create country mask file.")
  }
  
  if (verbose) {
    message("Created country mask: ", normalizePath(outfile))
  }
  
  invisible(outfile)
}