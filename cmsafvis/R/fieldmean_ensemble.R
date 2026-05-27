fieldmean_ensemble <-
  function(variable,
           infile,
           mask_file_final,
           temp_dir,
           climate_dir,
           country_code,
           lon_min,
           lon_max,
           lat_min,
           lat_max,
           climate_year_start,
           climate_year_end,
           accumulate,
           verbose,
           keep_files,
           nc = NULL) {

    # Keep the original input file separate from the yearly climate files.
    # The previous implementation overwrote `infile` inside the loop, which
    # meant that missing yearly files could trigger extract_climate_files()
    # with a missing file as input.
    source_infile <- infile
    
    # Get full time range of infile.
    id <- ncdf4::nc_open(source_infile)
    date_time <- as.Date(cmsafops::get_time(ncdf4::ncatt_get(id, "time", "units")$value,
                                                     +                                            ncdf4::ncvar_get(id, "time")))
    firstyear <- as.numeric(format(min(date_time), "%Y"))
    lastyear <- as.numeric(format(max(date_time), "%Y")) - 1
    ncdf4::nc_close(id)
    
    if (lastyear < firstyear) {
      stop("The input file does not contain enough years to build an ensemble fieldmean plot.")
    }
    
    comparison_years <- firstyear:lastyear

    if (verbose) {
      pb <- progress::progress_bar$new(
        format = "Computing field mean for :year [:bar] :percent eta: :eta",
        total = length(comparison_years),
        clear = TRUE,
        callback = function(x) {message("Computed field means")},
        show_after = 0
      )
    }

    # Compute field mean for each year.
	   for (climate_year in comparison_years) {
       if (verbose) {
		    if (climate_year == firstyear) {
          pb$tick(0, tokens = list(year = climate_year))
        } else {
          pb$tick(tokens = list(year = climate_year))
        }
      }

      # Build file paths
      tmpfile <- add_ncdf_ext(construct_filename(variable,
                                                 climate_year,
                                                 country_code,
                                                 "mask"))
      tmpfile <- file.path(temp_dir, tmpfile)
	  
      if (file.exists(tmpfile)) {
        reuse_file <- compare_grid(
          infile1 = tmpfile,
          infile2 = infile,
          lon_min = lon_min,
          lon_max = lon_max,
          lat_min = lat_min,
          lat_max = lat_max,
          nc2 = nc
        )
      } else {
        reuse_file <- FALSE
      }

      if (!reuse_file) {
        # These files are created by extract_climate_files()
        if (accumulate) {
          year_file_name <- add_ncdf_ext(
            construct_filename(
              variable,
              climate_year,
              "timsum"))
        } 
        else {
          year_file_name <- add_ncdf_ext(construct_filename(variable, climate_year))
        }
        
        year_infile <- file.path(climate_dir, year_file_name)
        
        # Need to extract yearly files if climate files do not exist. Use the
        # original source input, not the yearly file path that is missing.
        if (!file.exists(year_infile)) {
          extract_climate_files(
            # Question: How does this work if infile doesn't exist?
            variable = variable,
            infile = source_infile,
            climate_dir = climate_dir,
			      climate_year_start = firstyear,
            climate_year_end = lastyear,
            accumulate = accumulate,
            verbose = verbose,
			      nc = nc
          )
        }
        
        if (!file.exists(year_infile)) {
          stop(paste("Expected yearly climate file was not created:", year_infile))
        }
        
        var_file <- add_ncdf_ext(
          construct_filename(
            variable,
            climate_year,
            country_code))
        var_file <- file.path(temp_dir, var_file)

        cmsafops::sellonlatbox(
          var = variable,
          infile = year_infile,
          outfile = var_file,
          lon1 = lon_min,
          lon2 = lon_max,
          lat1 = lat_min,
          lat2 = lat_max,
          overwrite = TRUE,
          nc = nc
        )

        adjust_location(
          variable = variable,
          variable_mask = get_country_name(country_code),
          is_country = is_country(country_code),
          mask_file = mask_file_final,
          var_file = var_file,
          outfile = tmpfile)

       # var_file is only an intermediate subset. adjust_location() either
       # creates a masked file for countries or copies the subset for larger
       # regions, so var_file can be removed in both cases.
       if (file.exists(var_file)) {
         file.remove(var_file)
        }
      }

      outfile <- add_ncdf_ext(construct_filename(variable,
                                                 climate_year,
                                                 country_code,
                                                 "fldmean"))
      outfile <- file.path(temp_dir, outfile)

      # Call fieldmean for each year
      cmsafops::fldmean(
        var = variable,
        infile = tmpfile,
        outfile = outfile,
        overwrite = TRUE)

      # Remove auxiliar file
      if (!keep_files && file.exists(tmpfile)) {
        file.remove(tmpfile)
      }
    }
    if (verbose) pb$update(1)  # Finishes the progress bar

    # These files are only used in the plotting process and the return value of
    # this function call is currently not caught by the caller.
    return(outfile)
  }
