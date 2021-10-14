# This function extracts all years in the climate range and does the accumulation process.
extract_climate_files <- function(
  variable,
  infile,
  climate_dir,
  climate_year_start,
  climate_year_end,
  accumulate,
  #mean_value = FALSE,
  verbose,
  nc = NULL) {
  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "Extracting climate file for :year [:bar] :percent eta: :eta",
      total = length(climate_year_start:climate_year_end),
      clear = TRUE,
      callback = function(x) {message("Extracted climate files")},
      show_after = 0
    )
  }

  # Loop through climate years
  for (year in climate_year_start:climate_year_end) {
    if (verbose) {
      if (year == climate_year_start) {
        pb$tick(0, tokens = list(year = year))
      } else {
        pb$tick(tokens = list(year = year))
      }
    }

    # Filename constructions
    yearlyFileRaw <- add_ncdf_ext(
      construct_filename(
        variable,
        year))
    yearlyFileRaw <- file.path(climate_dir, yearlyFileRaw)

    if (accumulate) {
      outfile <- add_ncdf_ext(
        construct_filename(
          variable,
          year,
          "timsum"))
      outfile <- file.path(climate_dir, outfile)
    } 
    # else if (mean_value)
    # {
    #   outfile <- add_ncdf_ext(
    #     construct_filename(
    #       variable,
    #       year,
    #       "timmean"))
    #   outfile <- file.path(climate_dir, outfile)
    # }
    else {
      outfile <- yearlyFileRaw
    }

    if (file.exists(outfile)) {
      if (compare_spatial_range(outfile, infile, nc_file2 = nc)) {
        next()
      }
    }

    #if(!mean_value){  # if == TRUE: only do this with daily files 
      yearlyFileRaw_with_leap <- add_ncdf_ext(
        construct_filename(
          variable,
          year,
          "with_leap"))
      yearlyFileRaw_with_leap <- file.path(climate_dir, yearlyFileRaw_with_leap)
  
      # First extract all climate years
      tryCatch({
        cmsafops::selyear(
          var = variable,
          year = year,
          infile = infile,
          outfile = yearlyFileRaw_with_leap,
          overwrite = TRUE,
          nc = nc
        )
      }, error = function(e) {
        if (startsWith(deparse(sys.calls()[[sys.nframe() - 5]])[[1]], "merge_climatology")) {
          if (!is.null(nc)) {
            stop(paste0("An error occured while extracting data for the year ", year, ".\nPlease make sure that the input nc object contains the required data or provide a climatology file for the selected region."))
          } else {
            stop(paste0("An error occured while extracting data for the year ", year, ".\nPlease make sure that the input file ", infile, " contains the required data or provide a climatology file for the selected region."))
          }
        } else {
          if (!is.null(nc)) {
            stop(paste0("An error occured while extracting data for the year ", year, ".\nPlease make sure that the input nc object contains the required data."))
          } else {
            stop(paste0("An error occured while extracting data for the year ", year, ".\nPlease make sure that the input file ", infile, " contains the required data."))
          }
        }
      })

      if (is_leap_year(year)) {
        # Remove leap year dates
        tryCatch({
          cmsafops::extract.period(
            var = variable,
            start = paste0(year, "-02-29"),
            end = paste0(year, "-02-29"),
            infile = yearlyFileRaw_with_leap,
            outfile = yearlyFileRaw,
            overwrite = TRUE)
        }, error = function(e) {
          stop(paste0("An error occured while removing date ", year, "-02-29."))
        })
  
        # Clean up non reusable file
        if (file.exists(yearlyFileRaw_with_leap)) { file.remove(yearlyFileRaw_with_leap) }
      } else {
        if (!file.copy(from = yearlyFileRaw_with_leap, to = yearlyFileRaw, overwrite = TRUE)) {
          stop(paste("Failed to copy", yearlyFileRaw_with_leap, "to", yearlyFileRaw))
        }
        file.remove(yearlyFileRaw_with_leap)
      }
    

      # Require that this file has 365 unique dates
      nc_check <- ncdf4::nc_open(yearlyFileRaw)
      unit_string <- ncdf4::ncatt_get(nc_check, "time", "units")$value
      time_data <- ncdf4::ncvar_get(nc_check, "time")
      ncdf4::nc_close(nc_check)
  
      # Get relevant dates
      check_dates <- as.Date(unique(cmsafops::get_time(unit_string, time_data)))
  
      if (length(check_dates) != 365) {
        stop(
          paste0(
            "Input file is missing dates of the year ", year, ".\n",
            "It must contain every day of every year from ", climate_year_start, " to ", climate_year_end, "!"
            )
          )
      }
    #}
    
    if (accumulate) {
      tryCatch({
        cmsafops::timcumsum(
          var = variable,
          infile = yearlyFileRaw,
          outfile = outfile,
          overwrite = TRUE
        )
      }, error = function(e) {
        if (file.exists(outfile)) { try(file.remove(outfile)) }
        if (file.exists(yearlyFileRaw)) { try(file.remove(yearlyFileRaw)) }
        stop(paste0("An error occured while accumulating climate file for year ", year, "."))
      })

      # Remove non-reusable files
      if (file.exists(yearlyFileRaw)) { file.remove(yearlyFileRaw) }
    }
    
    # if(mean_value)
    # {
    #   # TODO
    # }
  }
  if (verbose) pb$update(1)  # Finishes the progress bar
}
