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

	# Get full time range of infile
	id <- ncdf4::nc_open(infile)
	  date_time <- as.Date(cmsafops::get_time(ncdf4::ncatt_get(id, "time", "units")$value, ncdf4::ncvar_get(id, "time")))
	  firstyear <- format(min(date_time), "%Y")
	  lastyear  <- format(max(date_time), "%Y")
	  lastyear  <- as.character(as.numeric(lastyear) - 1)
	ncdf4::nc_close(id)

    if (verbose) {
      pb <- progress::progress_bar$new(
        format = "Computing field mean for :year [:bar] :percent eta: :eta",
		total = length(firstyear:lastyear),
        clear = TRUE,
        callback = function(x) {message("Computed field means")},
        show_after = 0
      )
    }

    # Compute field mean for each year.
	for (climate_year in firstyear:lastyear) {
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
        # These files are created in extractClimaFiles.R
        if (accumulate) {
          infile <- add_ncdf_ext(
            construct_filename(
              variable,
              climate_year,
              "timsum"))
        } 
        # else if (mean_value)
        # {
        #   infile <- add_ncdf_ext(
        #     construct_filename(
        #       variable,
        #       climate_year,
        #       "timmean"))
        # } 
        else {
          infile <- add_ncdf_ext(construct_filename(variable,
                                                    climate_year))
        }
        infile <- file.path(climate_dir, infile)
		
        # Need to extract yearly files if climate files do not exist
        if (!file.exists(infile)) {
          extract_climate_files(
            # Question: How does this work if infile doesn't exist?
            variable = variable,
            infile = infile,
            climate_dir = climate_dir,
			      climate_year_start = firstyear,
            climate_year_end = lastyear,
            accumulate = accumulate,
            #mean_value = mean_value,
            verbose = verbose
          )
        }

        var_file <- add_ncdf_ext(
          construct_filename(
            variable,
            climate_year,
            country_code))
        var_file <- file.path(temp_dir, var_file)

        cmsafops::sellonlatbox(
          var = variable,
          infile = infile,
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

        # If not Europe, Africa, Totaldisc, or arbitrary, also remove var_file
        if (is_country(country_code) && file.exists(var_file)) {
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
      if (!keep_files) {
        file.remove(tmpfile)
      }
    }
    if (verbose) pb$update(1)  # Finishes the progress bar

    # Will return the outfile from the latest year... Not sure if this is what we want...
    # These files are only used in the plotting process (and the return value of this function call is never caught).
    return(outfile)
  }
