apply_mask_current <- function(variable,
                               temp_dir,
                               infile,
                               mask_file_final,
                               climatology_file,
                               country_code,
                               lon_min,
                               lon_max,
                               lat_min,
                               lat_max,
                               climate_year_start,
                               climate_year_end,
                               start_date,
                               end_date,
                               accumulate = FALSE,
                               mean_value = FALSE,
                               relative = FALSE) {
  start_doy <- format(start_date, format = "%j")
  finish_doy <- format(end_date, format = "%j")

  # output file name
  if(accumulate){
    outfile <- add_ncdf_ext(
      construct_filename(
        variable,
        "Diffclim_accumulated",
        country_code,
        format(end_date, "%Y"),
        "until",
        finish_doy,
        "mask"
      )
    )
  }else if (mean_value){
    outfile <- add_ncdf_ext(
      construct_filename(
        variable,
        "Diffclim_averaged",
        country_code,
        format(end_date, "%Y"),
        "until",
        finish_doy,
        "mask"
      )
    )
  }
  
  outfile <- file.path(temp_dir, outfile)

  # Apply lon/lat values to infile.
  infile_lonlatrange <- add_ncdf_ext(
    construct_filename(variable,
                       format(end_date, "%Y-%m%-d"),
                       "lonlat",
                       country_code)
  )
  infile_lonlatrange <- file.path(temp_dir, infile_lonlatrange)

  cmsafops::sellonlatbox(
    var = variable,
    infile = infile,
    outfile = infile_lonlatrange,
    lon1 = lon_min,
    lon2 = lon_max,
    lat1 = lat_min,
    lat2 = lat_max,
    overwrite = TRUE
  )

  # Apply date period to infile.
  infile_lonlattimerange <- add_ncdf_ext(
    construct_filename(variable, format(end_date, "%Y-%m-%d"), country_code)
  )
  infile_lonlattimerange <- file.path(temp_dir, infile_lonlattimerange)

  cmsafops::selperiod(
    var = variable,
    start = start_date,
    end = end_date,
    infile = infile_lonlatrange,
    outfile = infile_lonlattimerange,
    overwrite = TRUE
  )

  # Apply date period to climatology file.
  climatology_file_timerange <- add_ncdf_ext(
    construct_filename(
      tools::file_path_sans_ext(basename(climatology_file)),
      start_doy,
      finish_doy
    )
  )
  climatology_file_timerange <- file.path(temp_dir, climatology_file_timerange)

  # We need the year of the climatology time steps in order to cut them.
  # TODO Move to function in operator package? Toolbox needs similar functionality.
  climatology_nc <- ncdf4::nc_open(climatology_file)
  unit_string <- ncdf4::ncatt_get(climatology_nc, "time", "units")$value
  time_data <- ncdf4::ncvar_get(climatology_nc, "time")

  date_time <- as.Date(cmsafops::get_time(ncdf4::ncatt_get(climatology_nc, "time", "units")$value, 
                                          ncdf4::ncvar_get(climatology_nc, "time")))
  firstyear <- format(min(date_time), "%Y")
  lastyear  <- format(max(date_time), "%Y")
  lastyear  <- as.character(as.numeric(lastyear))

  ncdf4::nc_close(climatology_nc)

  # Get relevant dates
  climatology_dates <- as.Date(cmsafops::get_time(unit_string, time_data))
  # compare_start_date <- format(paste0(climate_year_start, format(start_date, format = "-%m-%d")))
  # compare_end_date <- format(paste0(climate_year_start, format(end_date, format = "-%m-%d")))
  compare_start_date <- format(paste0(firstyear, format(start_date, format = "-%m-%d")))
  compare_end_date <- format(paste0(lastyear, format(end_date, format = "-%m-%d")))
  climatology_dates <- climatology_dates[((compare_start_date <= climatology_dates) & (climatology_dates <= compare_end_date))]

  if (length(climatology_dates) == 0) {
    stop(
      paste0(
        "Climatology file is incomplete. Please try deleting: ",
        normalizePath(climatology_file),
        " and restarting the process."
      )
    )
  }

  years_unique <- unique(format(climatology_dates, "%Y"))
  if (length(years_unique) > 1) {
    start_short <- format(start_date, format = "%m-%d")
    end_short <- format(end_date, format = "%m-%d")
    stop(
      paste0("Climatology file '", climatology_file, "' contains more than one year: ", paste(years_unique, collapse = ", "),
             "\nPlease make sure that the input file contains data for each climatology year of the given time range (", start_short, " - ", end_short, ").")
    )
  }

  climate_select_start_date <- paste0(years_unique, format(start_date, "-%m-%d"))
  climate_select_end_date <- paste0(years_unique, format(end_date, "-%m-%d"))

  cmsafops::selperiod(
    var = variable,
    start = climate_select_start_date,
    end = climate_select_end_date,
    infile = climatology_file,
    outfile = climatology_file_timerange,
    overwrite = TRUE
  )

  # Subtract climatology from infile.
  if(accumulate){
    diff_climate_file <- add_ncdf_ext(
      construct_filename(
        variable,
        "Diffclim_accumulated",
        country_code,
        format(end_date, "%Y"),
        "until",
        finish_doy
      )
    )
  } else if (mean_value){
    diff_climate_file <- add_ncdf_ext(
      construct_filename(
        variable,
        "Diffclim_averaged",
        country_code,
        format(end_date, "%Y"),
        "until",
        finish_doy
      )
    )
  }
  
  diff_climate_file <- file.path(temp_dir, diff_climate_file)
  
  if (relative) {
    cmsafops::cmsaf.sub.rel(
      var1 = variable,
      var2 = variable,
      infile1 = infile_lonlattimerange,
      infile2 = climatology_file_timerange,
      outfile = diff_climate_file,
      overwrite = TRUE
    )
  } else {
    cmsafops::cmsaf.sub(
      var1 = variable,
      var2 = variable,
      infile1 = infile_lonlattimerange,
      infile2 = climatology_file_timerange,
      outfile = diff_climate_file,
      overwrite = TRUE
    )
  }
  
  region_name <- get_country_name(country_code)

  adjust_location(
    variable = variable,
    variable_mask = region_name,
    is_country = is_country(country_code),
    mask_file = mask_file_final,
    var_file = diff_climate_file,
    outfile = outfile)


  # Remove non-reusable files
  if (file.exists(infile_lonlatrange)) { file.remove(infile_lonlatrange) }
  if (file.exists(infile_lonlattimerange)) { file.remove(infile_lonlattimerange) }
  if (file.exists(diff_climate_file)) { file.remove(diff_climate_file) }
  if (file.exists(climatology_file_timerange)) { file.remove(climatology_file_timerange) }

  return(outfile)
}
