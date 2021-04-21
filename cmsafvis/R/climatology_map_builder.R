climatology_map_builder <- function(variable,
                                    mask_file_final,
                                    climatology_file,
                                    temp_dir,
                                    climate_year_start,
                                    climate_year_end,
                                    country_code,
                                    lon_min,
                                    lon_max,
                                    lat_min,
                                    lat_max,
                                    start_date,
                                    end_date) {

  outfile <- add_ncdf_ext(construct_filename(
    variable,
    "climatology",
    paste0(climate_year_start, "-", climate_year_end),
    country_code,
    "mask"
  ))
  outfile <- file.path(temp_dir, outfile)


  tmpfile <- add_ncdf_ext(construct_filename(variable,
                                             "clim",
                                             country_code))
  tmpfile <- file.path(temp_dir, tmpfile)

  # Could not use inpath = outpath in selperiod operator
  tmpfile2 <- add_ncdf_ext(construct_filename("TMP",
                                              variable,
                                              "clim",
                                              country_code))
  tmpfile2 <- file.path(temp_dir, tmpfile2)

  tryCatch({
    cmsafops::sellonlatbox(
      var = variable,
      infile = climatology_file,
      outfile = tmpfile,
      lon1 = lon_min,
      lon2 = lon_max,
      lat1 = lat_min,
      lat2 = lat_max,
      overwrite = TRUE
    )}, error = function(cond) {
      stop(paste0("An error occured while selecting the given spatial range."))
    })

  start_date <- paste(unlist(strsplit(unlist(strsplit(basename(climatology_file), "_"))[grepl("-",unlist(strsplit(basename(climatology_file), "_")))], "-"))[1], format(start_date, format = "%m-%d"), sep = "-")
  end_date <- paste(unlist(strsplit(unlist(strsplit(basename(climatology_file), "_"))[grepl("-",unlist(strsplit(basename(climatology_file), "_")))], "-"))[1], format(end_date, format = "%m-%d"), sep = "-")

  tryCatch({
    cmsafops::selperiod(
      var = variable,
      start = start_date,
      end = end_date,
      infile = tmpfile,
      outfile = tmpfile2,
      overwrite = TRUE
    )
  }, error = function(cond) {
    stop(paste0("An error occured while selecting the given period (", start_date, "-", end_date, "). Please make sure that your file contains data for this range."))
  })



  variable_mask <- get_country_name(country_code)

  adjust_location(
    variable = variable,
    variable_mask = variable_mask,
    is_country = is_country(country_code),
    mask_file = mask_file_final,
    var_file = tmpfile2,
    outfile = outfile)

  # Remove files that are always created
  if (file.exists(tmpfile)) { file.remove(tmpfile) }
  if (file.exists(tmpfile2)) { file.remove(tmpfile2) }

  return(outfile)

}
