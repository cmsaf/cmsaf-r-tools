#' Calculate climatology
#'
#' Calculate climatology or reuse existing one.
#'
#' @noRd
calculate_climatology <- function(
  variable,
  climate_year_start,
  climate_year_end,
  start_date,
  end_date,
  country_code,
  climate_dir,
  infile,
  lon_min,
  lon_max,
  lat_min,
  lat_max,
  accumulate,
  verbose,
  nc = NULL
) {

  acc_str <- ""
  if (accumulate) {
    acc_str <- "accumulated_"
  }
  # climatology destination filename
  climatology_file <- add_ncdf_ext(construct_filename(
    variable,
    "climatology",
    paste0(climate_year_start, "-", climate_year_end),
    paste0(acc_str, country_code)
  ))
  climatology_file <- file.path(climate_dir, climatology_file)

  climate_tot_file <- merge_climatology(
    variable = variable,
    infile = infile,
    climate_dir = climate_dir,
    climate_year_start = climate_year_start,
    climate_year_end = climate_year_end,
    start_date = start_date,
    end_date = end_date,
    accumulate = accumulate,
    verbose = verbose,
    nc = nc
  )

  # outfile: [variable]_climatology_[climate_year_start-climate_year_end]_[acc_str]_[country_code]
  cut_climate(
    variable = variable,
    outfile = climatology_file,
    infile = infile,
    country_code = country_code,
    climate_dir = climate_dir,
    climate_tot_file = climate_tot_file,
    climate_year_start = climate_year_start,
    climate_year_end = climate_year_end,
    lon_min = lon_min,
    lon_max = lon_max,
    lat_min = lat_min,
    lat_max = lat_max,
    accumulate = accumulate,
    verbose = verbose,
    nc = nc
  )

  return(climatology_file)
}

merge_climatology <- function(
  variable,
  infile,
  climate_dir,
  climate_year_start,
  climate_year_end,
  start_date,
  end_date,
  accumulate,
  verbose,
  nc = NULL
) {
  acc_str <- ""
  if (accumulate) {
    acc_str <- "accumulated_"
  }
  # climatology file name
  outfile <- add_ncdf_ext(construct_filename(
    variable,
    "climatology",
    paste0(climate_year_start, "-", climate_year_end),
    paste0(acc_str, "TOT")
  ))
  outfile <- file.path(climate_dir, outfile)

  if (file.exists(outfile)) {
    reuse_tot_climatology <- compare_spatial_range(
      file1 = outfile,
      file2 = infile,
      nc_file2 = nc
    )

    if (reuse_tot_climatology) {
      if (verbose) {
        message(
          paste("Re-use climatology:", normalizePath(outfile))
        )
      }
      return(outfile)
    }
  }

  # Get full time range of infile
  id <- ncdf4::nc_open(infile)
    date_time <- as.Date(cmsafops::get_time(ncdf4::ncatt_get(id, "time", "units")$value, ncdf4::ncvar_get(id, "time")))
    firstyear <- format(min(date_time), "%Y")
    lastyear  <- format(max(date_time), "%Y")
    lastyear  <- as.character(as.numeric(lastyear) - 1)
  ncdf4::nc_close(id)
  
  # Need to extract yearly files if climatology does not exist
  extract_climate_files(
    variable = variable,
    infile = infile,
    climate_dir = climate_dir,
    climate_year_start = firstyear,
    climate_year_end = lastyear,
    accumulate = accumulate,
    verbose = verbose,
    nc = nc
  )

  for (climate_year in climate_year_start:climate_year_end) {
    if (accumulate) {
      file1 <- add_ncdf_ext(construct_filename(variable,
                                               climate_year,
                                               "timsum"))
      file1 <- file.path(climate_dir, file1)
    } else {
      file1 <- add_ncdf_ext(construct_filename(variable,
                                               climate_year))
      file1 <- file.path(climate_dir, file1)
    }

    file2 <- add_ncdf_ext(construct_filename(variable,
                                             climate_year,
                                             "mergetime"))
    file2 <- file.path(climate_dir, file2)

    # Leap dates have been removed in extract_climate_files
    if (!file.copy(file1, file2, overwrite = TRUE)) {
      stop(paste("Failed to copy", file1, "to", file2))
    }

    nc <- ncdf4::nc_open(file2)
    times <- ncdf4::ncvar_get(nc, "time")
    units <- ncdf4::ncatt_get(nc, "time", "units")$value
    ncdf4::nc_close(nc)

    date_times <- as.Date(range(cmsafops::get_time(units, times)))
    start_date_year <- as.Date(paste0(climate_year, format(start_date, format = "-%m-%d")))
    end_date_year <- as.Date(paste0(climate_year, format(end_date, format = "-%m-%d")))

    # Check if file has January 1st and December 31st as dates
    if (date_times[1] > start_date_year) {
      stop(paste0("Failed to compute climatology: Missing dates between ", start_date_year, " and ", date_times[1], " detected."))
    }
    if (date_times[2] < end_date_year) {
      stop(paste0("Failed to compute climatology: Missing dates between ", date_times[2], " and ", end_date_year, " detected."))
    }
  }

  tmpfile <- add_ncdf_ext(construct_filename(
    variable,
    "climatology",
    paste0(climate_year_start, "-", climate_year_end),
    "broad"
  ))
  tmpfile <- file.path(climate_dir, tmpfile)

  if (verbose) {
    message("Merge climate files")
  }

  cmsafops::box_mergetime(
    var = variable,
    path = climate_dir,
    pattern = paste0(variable, ".*", "mergetime"),
    outfile = tmpfile,
    overwrite = TRUE
  )

  # Calculate ydaymean
  if (verbose) {
    message("Calculate ydaymean")
  }

  cmsafops::ydaymean(
    var = variable,
    infile = tmpfile,
    outfile = outfile,
    overwrite = TRUE)

  # Clean up
  file.remove(tmpfile)
  for (climate_year in climate_year_start:climate_year_end) {
    residual <- add_ncdf_ext(construct_filename(variable,
                                                climate_year,
                                                "mergetime"))
    residual <- file.path(climate_dir, residual)
    file.remove(residual)
  }

  return(outfile)
}

# This function is ALWAYS CALLED AFTER merge_climatology
cut_climate <- function(
  variable,
  outfile,
  infile,
  country_code,
  climate_dir,
  climate_tot_file,
  climate_year_start,
  climate_year_end,
  lon_min,
  lon_max,
  lat_min,
  lat_max,
  accumulate,
  verbose,
  nc = NULL
  ) {
  acc_str <- ""
  if (accumulate) {
    acc_str <- "accumulated_"
  }

  # Build climate infile name
  climate_tot_file <- add_ncdf_ext(construct_filename(
    variable,
    "climatology",
    paste0(climate_year_start, "-", climate_year_end),
    paste0(acc_str, "TOT")
  ))
  climate_tot_file <- file.path(climate_dir, climate_tot_file)

  # Only do cutting process when sub area selected
  if (country_code == "TOT") {
    return(climate_tot_file)
  }

  # If this file exists in temp dir, no need to create it again
  if (file.exists(outfile)) {
    reuse_climatology <- compare_grid(
      infile1 = outfile,
      infile2 = infile,
      lon_min = lon_min,
      lon_max = lon_max,
      lat_min = lat_min,
      lat_max = lat_max,
      nc2 = nc
    )

    if (reuse_climatology) {
      if (verbose) {
        message(
          paste("Re-use climatology:", normalizePath(outfile))
        )
      }
      return(outfile)
    }
  }

  if (!file.exists(climate_tot_file)) {
    stop(paste0("Required climatology file ", climate_tot_file, " is missing."))
  }

  cmsafops::sellonlatbox(
    var = variable,
    infile = climate_tot_file,
    outfile = outfile,
    lon1 = lon_min,
    lon2 = lon_max,
    lat1 = lat_min,
    lat2 = lat_max,
    overwrite = TRUE
  )

  return(outfile)
}
