# This function is used for monitor climate plots ("warming_stripes_plot", "time_series_plot", "trend_plot")
apply_mask_final <- function(variable,
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
                             selected_number = 1) {
  # output file name
  if(accumulate){
    outfile <- add_ncdf_ext(
      construct_filename(
        variable,
        "Diffclim_all_accumulated",
        country_code,
        climate_year_start,
        "until",
        format(end_date, "%Y"),
        "mask"
      )
    )
  }else if (mean_value){
    outfile <- add_ncdf_ext(
      construct_filename(
        variable,
        "Diffclim_all_averaged",
        country_code,
        climate_year_start,
        "until",
        format(end_date, "%Y"),
        "mask"
      )
    )
  }
  outfile <- file.path(temp_dir, outfile)
  
  # Apply lon/lat values to infile.
  infile_lonlatrange <- add_ncdf_ext(
    construct_filename(variable,
                       "infile",
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

  # Subtract climatology from infile.
  if(accumulate){
    diff_climate_file <- add_ncdf_ext(
      construct_filename(
        variable,
        "Diffclim_all_accumulated",
        country_code,
        climate_year_start,
        "until",
        format(end_date, "%Y")
      )
    )
  } else if (mean_value){
    diff_climate_file <- add_ncdf_ext(
      construct_filename(
        variable,
        "Diffclim_all_averaged",
        country_code,
        climate_year_start,
        "until",
        format(end_date, "%Y")
      )
    )
  }
  
  monitor_climate_temp_dir <- file.path(tempdir(), "monitor_climate_temp")
  # remove if it exists
  if (dir.exists(monitor_climate_temp_dir)) {
    unlink(monitor_climate_temp_dir, recursive = TRUE)
  }
  # create new temp dic
  if (!dir.exists(monitor_climate_temp_dir)) {
    dir.create(monitor_climate_temp_dir)
  }
  
  diff_climate_temp_dir <- file.path(tempdir(), "diff_climate_temp")
  # remove if it exists
  if (dir.exists(diff_climate_temp_dir)) {
    unlink(diff_climate_temp_dir, recursive = TRUE)
  }
  # create new temp dic
  if (!dir.exists(diff_climate_temp_dir)) {
    dir.create(diff_climate_temp_dir)
  }
  
  file_data <- cmsafops::read_file(infile_lonlatrange, variable)
  file_data$variable$prec <- "float"
  years_all <- cmsafops::get_date_time(file_data$dimension_data$t, file_data$time_info$units)$years
  years_unique <- sort(unique(years_all))
  
  # if(selected_number == 1)
  # {
    diff_climate_file <- file.path(temp_dir, diff_climate_file)
    if (file.exists(diff_climate_file)) { file.remove(diff_climate_file) }
    
    anomaly_tmp_file <- file.path(monitor_climate_temp_dir, paste0("anomaly_tmp_file.nc"))
    
    cmsafops::mon.anomaly.climatology(variable, infile_lonlatrange, anomaly_tmp_file, climatology_file, overwrite = TRUE)
    
    if(mean_value == TRUE)
    {
      cmsafops::timselmean(var = variable, nts = selected_number, infile = anomaly_tmp_file, outfile = diff_climate_file, overwrite = TRUE)
    }

    if(accumulate == TRUE)
    {
      cmsafops::timselsum(var = variable, nts = selected_number, infile = anomaly_tmp_file, outfile = diff_climate_file, overwrite = TRUE)
    }
    
  # }else{
  # 
  #   for(i in 1:length(years_unique)){
  #     selyear_file_tmp <- file.path(monitor_climate_temp_dir, paste0("selyear_tmp", years_unique[i],".nc"))
  #     cmsafops::selyear(variable, c(years_unique[i]), infile_lonlatrange, selyear_file_tmp)
  #     
  #     timsel_file_tmp <- file.path(monitor_climate_temp_dir, paste0("timsel_tmp", years_unique[i],".nc"))
  #     clim_file_tmp <- file.path(monitor_climate_temp_dir, paste0("clim_tmp", years_unique[i],".nc"))
  # 
  #     if(mean_value == TRUE)
  #     {
  #       cmsafops::timselmean(var = variable, nts = selected_number, infile = selyear_file_tmp, outfile = timsel_file_tmp, overwrite = TRUE)
  #       cmsafops::timselmean(var = variable, nts = selected_number, infile = climatology_file, outfile = clim_file_tmp, overwrite = TRUE)
  #     }
  #       
  #     if(accumulate == TRUE)
  #     {
  #       cmsafops::timselsum(var = variable, nts = selected_number, infile = selyear_file_tmp, outfile = timsel_file_tmp, overwrite = TRUE)
  #       cmsafops::timselsum(var = variable, nts = selected_number, infile = climatology_file, outfile = clim_file_tmp, overwrite = TRUE)
  #     }
  #     
  #     diff_climate_file_tmp <- file.path(diff_climate_temp_dir, paste0("diff_climate_file_tmp", years_unique[i],".nc"))
  # 
  #     cmsafops::cmsaf.sub(
  #       var1 = variable,
  #       var2 = variable,
  #       infile1 = timsel_file_tmp,
  #       infile2 = clim_file_tmp,
  #       outfile = diff_climate_file_tmp,
  #       overwrite = TRUE
  #     )
  #   }
  #   
  #   diff_climate_file <- file.path(temp_dir, diff_climate_file)
  #   
  #   # merge all diff climate files
  #   cmsafops::box_mergetime(variable,
  #                           path = diff_climate_temp_dir,
  #                           pattern = "diff_climate",
  #                           outfile = diff_climate_file,
  #                           lon1 = -180,
  #                           lon2 = 180,
  #                           lat1 = -90,
  #                           lat2 = 90,
  #                           nc34 = 4,
  #                           overwrite = TRUE)
  # }
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
  
  # remove if it exists
  if (dir.exists(monitor_climate_temp_dir)) {
    unlink(monitor_climate_temp_dir, recursive = TRUE)
  }
  if (dir.exists(diff_climate_temp_dir)) {
    unlink(diff_climate_temp_dir, recursive = TRUE)
  }
  
  return(outfile)
  
}