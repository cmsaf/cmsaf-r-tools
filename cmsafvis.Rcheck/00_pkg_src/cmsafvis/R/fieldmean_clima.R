# requires cut_climate
fieldmean_climate <- function(
  variable,
  temp_dir,
  climate_infile,
  climate_year_start,
  climate_year_end,
  country_code,
  accumulate,
  nc = NULL) {

  acc_string <- ""
  #mean_string <- ""
  if (accumulate) {
    acc_string <- "accumulated_"
  } 
  # else if (mean_value)
  # {
  #   mean_string <- "averaged_"
  # }

  #if (accumulate) {
    outfile <- add_ncdf_ext(
      construct_filename(
        variable,
        "climatology",
        paste0(climate_year_start, "-", climate_year_end),
        paste0(acc_string, country_code),
        "fldmean"
      )
    )
  # } else if (mean_value)
  # {
  #   outfile <- add_ncdf_ext(
  #     construct_filename(
  #       variable,
  #       "climatology",
  #       paste0(climate_year_start, "-", climate_year_end),
  #       paste0(mean_string, country_code),
  #       "fldmean"
  #     )
  #   )
  # }
  
  outfile <- file.path(temp_dir, outfile)

  cmsafops::fldmean(
    var = variable,
    infile = climate_infile,
    outfile = outfile,
    overwrite = TRUE,
    nc = nc)

  return(outfile)
}
