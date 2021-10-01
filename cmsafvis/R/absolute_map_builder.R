absolute_map_builder <- function(variable,
                                 temp_dir,
                                 infile,
                                 mask_file_final,
                                 country_code,
                                 lon_min,
                                 lon_max,
                                 lat_min,
                                 lat_max,
                                 start_date,
                                 end_date,
                                 nc = NULL) {

  # outfile name
  outfile <- add_ncdf_ext(construct_filename(variable,
                                          format(end_date, "%Y"),
                                          country_code,
                                          "mask"))
  outfile <- file.path(temp_dir, outfile)


  tmpfile <- add_ncdf_ext(construct_filename(variable,
                                          format(end_date, "%Y"),
                                          country_code))
  tmpfile <- file.path(temp_dir, tmpfile)

  # Could not use inpath = outpath in selperiod operator
  tmpfile2 <- add_ncdf_ext(construct_filename("TMP",
                                           variable,
                                           format(end_date, "%Y"),
                                           country_code))
  tmpfile2 <- file.path(temp_dir, tmpfile2)

  tryCatch({
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
    )}, error = function(cond) {
      stop(paste0("An error occured while selecting the given spatial range."))
    })

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
