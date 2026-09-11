apply_mask_clima <-
  function(variable,
           mask_file_final,
           climatology_file,
           temp_dir,
           country_code,
           climate_year_start,
           climate_year_end,
           accumulate) {

    acc_string <- ""
    #mean_string <- ""
    if (accumulate) 
    {
      acc_string <- "accumulated_"
    } 
    # else if (mean_value)
    # {
    #   mean_string <- "averaged_"
    # }

    # Build outfile path
    #if (accumulate) 
    #{
      outfile <- add_ncdf_ext(
        construct_filename(
          variable,
          "climatology",
          paste0(climate_year_start, "-", climate_year_end),
          paste0(acc_string, country_code),
          "mask"
        )
      )
    #} 
    # else if (mean_value)
    # {
    #   outfile <- add_ncdf_ext(
    #     construct_filename(
    #       variable,
    #       "climatology",
    #       paste0(climate_year_start, "-", climate_year_end),
    #       paste0(mean_string, country_code),
    #       "mask"
    #     )
    #   )
    # }
    
    outfile <- file.path(temp_dir, outfile)

    if (file.exists(mask_file_final)) {
      # Use add operator
      tryCatch({
        cmsafops::cmsaf.add(
          var1 = variable,
          var2 = get_country_name(country_code),
          infile1 = climatology_file,
          infile2 = mask_file_final,
          outfile = outfile,
          overwrite = TRUE
        )}, error = function(cond) {
          stop(paste0("An error occured while applying final mask file to climatology file."))
        })
      return(outfile)
    } else {
      stop(paste("Applying mask file to climatology file failed: final mask file: ", mask_file_final, " does not exist!"))
    }
}
