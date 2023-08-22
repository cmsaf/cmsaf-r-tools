# This function extracts the year to be analyzed and does the accumulation process.
# It returns the modified infile.
extractFinalOutfile <- function(variable,
                               infile,
                               start_date,
                               end_date,
                               accumulate,
                               temp_dir,
                               verbose,
                               nc = NULL) {
  if (verbose) {
    message("Prepare infile")
  }

  year_to_analyze <- format(start_date, "%Y")

  infile_basename <- get_basename_vis(infile = infile, nc = nc)
  selyearInfile <- add_ncdf_ext(
    construct_filename(
      tools::file_path_sans_ext(infile_basename),
      year_to_analyze
    )
  )
  selyearInfile <- file.path(temp_dir, selyearInfile)

  tryCatch({
    cmsafops::selyear(
      var = variable,
      year = year_to_analyze,
      infile = infile,
      outfile = selyearInfile,
      overwrite = TRUE,
      nc = nc
    )
  }, error = function(e) {
    if (!is.null(nc)) {
      stop(paste0("An error occured while extracting data for the year ", year_to_analyze, ".\nPlease make sure that the input nc object contains the required data."))
    } else {
      stop(paste0("An error occured while extracting data for the year ", year_to_analyze, ".\nPlease make sure that the input file ", infile, " contains the required data."))
    }
  })

  # Remove leap date if included in selected period
  no_leap_year_file <- add_ncdf_ext(construct_filename(tools::file_path_sans_ext(selyearInfile), "no_leap"))
  no_leap_year_file <- file.path(temp_dir, basename(no_leap_year_file))
  if (is_leap_year(as.numeric(year_to_analyze))) {
    leapDate <- paste0(year_to_analyze, "-02-29")
    if (start_date <= as.Date(leapDate) && as.Date(leapDate) <= end_date) {
      # Remove leap year dates
      tryCatch({
        cmsafops::extract.period(
          var = variable,
          start = leapDate,
          end = leapDate,
          infile = selyearInfile,
          outfile = no_leap_year_file,
          overwrite = TRUE)
      }, error = function(e) {
        stop(paste0("An error occured while removing date ", year_to_analyze, "-02-29."))
      })
    } else {
      no_leap_year_file <- selyearInfile
    }
  } else {
    no_leap_year_file <- selyearInfile
  }

  # First extract the year to be analyzed
  # If user wants to accumulate the file, we do so here.
  if (accumulate) {
    outfile <- add_ncdf_ext(
      construct_filename(
        tools::file_path_sans_ext(infile_basename),
        year_to_analyze,
        "timsum"
      )
    )
    outfile <- file.path(temp_dir, outfile)
    
    tryCatch({
      cmsafops::timcumsum(
        var = variable,
        infile = no_leap_year_file,
        outfile = outfile,
        overwrite = TRUE
      )
    }, error = function(e) {
      stop("An error occured while accumulating infile.")
    })

    # Remove non reusable files
    if (file.exists(no_leap_year_file)) { file.remove(no_leap_year_file) }
  } else {
    # Assuming we have one timestep we can just continue with the given infile
    outfile <- no_leap_year_file
  }

  # Remove non-reusable files
  if (file.exists(selyearInfile) && outfile != selyearInfile) { file.remove(selyearInfile) }

  return(outfile)
}
