# This function is used for monitor climate plots ("warming_stripes_plot", "time_series_plot", "trend_plot")
# Extract selected period and prepare the data (accumulate or mean).
extractOutfile <- function(variable,
                           infile,
                           selected_number,
                           analyze_method,
                           temp_dir,
                           verbose,
                           nc = NULL) {
  if (verbose) {
    message("Prepare infile")
  }
    
  outfile <- file.path(temp_dir, get_basename_vis(infile = infile, nc = nc))
  
  if(analyze_method == "accumulate"){
    tryCatch({
      cmsafops::monsum(var = variable, infile = infile, outfile = outfile, overwrite = TRUE, nc = nc)
    }, error = function(e) {
      stop(paste0("An error occured while extracting data. ","cmsafops::monsum"))
    })

  }else if(analyze_method == "mean"){
    tryCatch({
      cmsafops::monmean(var = variable, infile = infile, outfile = outfile, overwrite = TRUE, nc = nc)
    }, error = function(e) {
      stop(paste0("An error occured while extracting data. ","cmsafops::monmean"))
    })
  }
  
  # check file
  file_data <- cmsafops::read_file(outfile, variable)
  file_data$variable$prec <- "float"
  years_all <- cmsafops::get_date_time(file_data$dimension_data$t, file_data$time_info$units)$years
  years_unique <- sort(unique(years_all))
  
  #lookup_table_years <- as.data.frame(table(years_all))
  #temp_lookup <- subset(lookup_table_years, subset = Freq < 12)
  #row.names(temp_lookup) <- NULL

  # if(selected_number != 1){
  #   if(nrow(temp_lookup) >= 1)
  #   {
  #     stop("The file must contain all months of a year")
  #   }
  # }
  
  return(outfile)
}
