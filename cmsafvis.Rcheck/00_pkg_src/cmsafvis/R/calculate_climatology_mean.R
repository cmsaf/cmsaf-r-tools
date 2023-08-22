calculate_climatology_mean <- function(
  variable,
  climate_year_start ,
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
  mean_value,
  verbose,
  nc = NULL
)
{
  if (mean_value) {
    mean_str <- "averaged_"
  }
  # climatology destination filename
  climatology_file <- add_ncdf_ext(construct_filename(
    variable,
    "climatology",
    paste0(climate_year_start, "-", climate_year_end),
    paste0(mean_str, country_code)
  ))
  climatology_file <- file.path(climate_dir, climatology_file)
  
  # If this file exists in temp dir, no need to create it again
  # if (file.exists(climatology_file)) {
  #   reuse_climatology <- compare_grid(
  #     infile1 = climatology_file,
  #     infile2 = infile,
  #     lon_min = lon_min,
  #     lon_max = lon_max,
  #     lat_min = lat_min,
  #     lat_max = lat_max
  #   )
  #   
  #   if (reuse_climatology) {
  #     if (verbose) {
  #       message(
  #         paste("Re-use climatology:", normalizePath(outfile))
  #       )
  #     }
  #     return(outfile)
  #   }
  # }
  
  climate_tmp_file <- file.path(tempdir(), "tmp_ymonmean.nc")
  cmsafops::ymonmean(
    var = variable,
    infile = infile,
    outfile = climate_tmp_file,
    overwrite = TRUE,
    nc = nc
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
  
  return(climatology_file)
}