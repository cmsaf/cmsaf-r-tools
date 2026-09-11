fieldmean_current <- function(variable,
                              temp_dir,
                              infile,
                              mask_file_final,
                              country_code,
                              lon_min,
                              lon_max,
                              lat_min,
                              lat_max,
                              climate_year_start,
                              climate_year_end,
                              start_date,
                              end_date,
                              nc = NULL) {
  tmpfile <- add_ncdf_ext(construct_filename(variable,
                                          format(end_date, "%Y"),
                                          country_code))
  tmpfile <- file.path(temp_dir, tmpfile)

  cmsafops::sellonlatbox(
    var = variable,
    infile = infile,
    outfile = tmpfile,
    lon1 = lon_min,
    lon2 = lon_max,
    lat1 = lat_min,
    lat2 = lat_max,
    overwrite = TRUE,
    nc = nc
  )

  tmpfile2 <- add_ncdf_ext(construct_filename(variable,
                                           format(end_date, "%Y"),
                                           country_code,
                                           "seltimestep"))
  tmpfile2 <- file.path(temp_dir, tmpfile2)

  cmsafops::selperiod(
    var = variable,
    start = start_date,
    end = end_date,
    infile = tmpfile,
    outfile = tmpfile2,
    overwrite = TRUE
  )

  variable_mask <- get_country_name(country_code)
  var_masked_file <- add_ncdf_ext(construct_filename(variable,
                                             format(end_date, "%Y"),
                                             country_code,
                                             "mask"))
  var_masked_file <- file.path(temp_dir, var_masked_file)

  adjust_location(
    variable = variable,
    variable_mask = variable_mask,
    is_country = is_country(country_code),
    mask_file = mask_file_final,
    var_file = tmpfile2,
    outfile = var_masked_file
  )

  outfile <- add_ncdf_ext(construct_filename(variable,
                                             format(end_date, "%Y"),
                                             "analyzed",
                                             country_code,
                                             "fldmean"))
  outfile <- file.path(temp_dir, outfile)

  cmsafops::fldmean(
    var = variable,
    infile = var_masked_file,
    outfile = outfile,
    overwrite = TRUE)

  # Remove non-reusable files
  if (file.exists(tmpfile)) { file.remove(tmpfile) }
  if (file.exists(tmpfile2)) { file.remove(tmpfile2) }
  if (file.exists(var_masked_file)) { file.remove(var_masked_file) }

  return(outfile)
}
