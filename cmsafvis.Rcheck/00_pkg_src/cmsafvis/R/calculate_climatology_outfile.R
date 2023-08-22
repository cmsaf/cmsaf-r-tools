# This function is used for monitor climate plots ("warming_stripes_plot", "time_series_plot", "trend_plot")
calculate_climatology_outfile <- function(variable,
                                          climate_year_start,
                                          climate_year_end,
                                          country_code,
                                          climate_dir,
                                          infile,
                                          lon_min,
                                          lon_max,
                                          lat_min,
                                          lat_max,
                                          selected_number,
                                          analyze_method,
                                          verbose,
                                          nc = NULL
)
{
  if (verbose) {
    message("Calculate climatology")
  }
  climate_year_start = paste0(climate_year_start, "-01-01")
  climate_year_end = paste0(climate_year_end, "-12-01")
  
  selperiod_tmp <- file.path(tempdir(), get_basename_vis(infile, nc =nc))
  if(file.exists(selperiod_tmp)){
    unlink(selperiod_tmp)
  }
  tryCatch({
    cmsafops::selperiod(var = variable, start = climate_year_start, end = climate_year_end, infile = infile, outfile = selperiod_tmp, overwrite = TRUE, nc = nc)
  }, error = function(e) {
    stop(paste0("An error occured while extracting data. ","cmsafops::selperiod"))
  })
  
  if (analyze_method == "mean") {
    str <- "averaged_"
  }
  if (analyze_method == "accumulate") {
    str <- "accumulated_"
  }
  
  
  # climatology destination filename
  climatology_file <- add_ncdf_ext(construct_filename(
    variable,
    "climatology",
    paste0(climate_year_start, "-", climate_year_end),
    paste0(str, country_code)
  ))
  climatology_file <- file.path(climate_dir, climatology_file)
  if(file.exists(climatology_file)){
    unlink(climatology_file)
  }
  
  climate_tmp_file <- file.path(tempdir(), "tmp_ymonmean.nc")
  if(file.exists(climate_tmp_file)){
    unlink(climate_tmp_file)
  }
  
  cmsafops::ymonmean(
    var = variable,
    infile = selperiod_tmp,
    outfile = climate_tmp_file,
    overwrite = TRUE
  )
  
  cmsafops::sellonlatbox(
    var = variable,
    infile = climate_tmp_file,
    outfile = climatology_file,
    lon1 = lon_min,
    lon2 = lon_max,
    lat1 = lat_min,
    lat2 = lat_max,
    overwrite = TRUE
  )
  
  # remove temp files
  if (file.exists(climate_tmp_file)) { file.remove(climate_tmp_file) }
  
  return(climatology_file)
}