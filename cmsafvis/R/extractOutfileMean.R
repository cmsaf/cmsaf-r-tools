# This function is used for monitor climate plots ("anomaly_map", "absolute_map", "climatology_map)
# Extract selected period and prepare the data.
extractOutfileMean <- function(variable,
                           infile,
                           start_date,
                           end_date,
                           mean_value = FALSE,
                           temp_dir,
                           verbose,
                           nc = NULL) {
  if (verbose) {
    message("Prepare infile")
  }
  infile_basename <- get_basename_vis(infile = infile, nc = nc)
  selperiod_tmp <- file.path(tempdir(), infile_basename)
  if(file.exists(selperiod_tmp)){
    unlink(selperiod_tmp)
  }
  tryCatch({
    cmsafops::selperiod(var = variable, start = start_date, end = end_date, infile = infile, outfile = selperiod_tmp, overwrite = TRUE, nc = nc)
  }, error = function(e) {
    stop(paste0("An error occured while extracting data. ","cmsafops::selperiod"))
  })
  
  outfile <- file.path(temp_dir, infile_basename)
  
  tryCatch({
    if(mean_value)
    {
      year_to_analyze <- format(start_date, "%Y")
      outfile <- add_ncdf_ext(
        construct_filename(
          tools::file_path_sans_ext(infile_basename),
          year_to_analyze,
          "timmean"
        ))
      outfile <- file.path(temp_dir, outfile)
      cmsafops::timmean(var = variable, infile = selperiod_tmp, outfile = outfile, overwrite = TRUE, nc = nc)
    }
  }, error = function(e) {
    stop(paste0("An error occured while extracting data. ", "cmsafops::timmean"))
  })
  
  return(outfile)
}
