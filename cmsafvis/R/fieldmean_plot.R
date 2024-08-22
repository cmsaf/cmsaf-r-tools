#' A 'cmsaf' extension for creating spatial mean plots.
#'
#' This plotting routine generates a graph showing the evolution of
#' the spatial mean of a given variable within the given time range 
#' and area. The intended application is for daily accumulated data, 
#' such as sunshine duration.
#' Dependent on the output format a PNG or MP4 is created.
#'
#' You can pass a YAML config file and/or specify the arguments directly.
#' Argument prioritization is done in the following way:
#' Direct argument > config argument > default argument.
#' Thus, if you pass a existing config file but also want to modify a specific argument you can do that easily.
#'
#' @inheritParams monitor_climate
#'
#' @export
#' @importFrom assertthat assert_that is.date is.dir is.flag is.number is.readable is.string is.writeable
fieldmean_plot <- function(config = NULL,
                           variable = NULL,
                           accumulate = FALSE,
                           infile = NULL,
                           temp_dir = tempdir(),
                           out_dir = getwd(),
                           climate_dir = NULL,
                           climate_year_start = 1983,
                           climate_year_end = 2018,
                           show_extreme_climate_years = NULL,
                           climatology_until_eoy = FALSE,
                           start_date = NULL,
                           end_date = NULL,
                           country_code = "S_A",
                           lon_min = NULL,
                           lon_max = NULL,
                           lat_min = NULL,
                           lat_max = NULL,
                           outfile_name = NULL,
                           output_format = "animation",
                           animation_pace = 0.07,
                           freeze_animation = FALSE,
                           language = "eng",
                           keep_files = TRUE,
                           states = FALSE,
                           attach = FALSE,
                           infile_attach = "auto",
                           dwd_logo = FALSE,
                           verbose = TRUE,
                           nc = NULL) {
  # Call central argument parser
  arguments_necessary <- methods::formalArgs(parse_arguments)
  arguments <- as.list(match.call())
  arguments[[1]] <- "fieldmean_plot"
  names(arguments)[1] <- "plot_type"
  arguments <- arguments[stats::na.omit(match(arguments_necessary, names(arguments)))]
  parsedArguments <- do.call(parse_arguments, arguments, envir = parent.frame())

  # Use parsed arguments
  variable <- parsedArguments$variable
  accumulate <- parsedArguments$accumulate
  temp_dir <- parsedArguments$temp_dir
  infile <- parsedArguments$infile
  climate_dir <- parsedArguments$climate_dir
  climate_year_start <- parsedArguments$climate_year_start
  climate_year_end <- parsedArguments$climate_year_end
  show_extreme_climate_years <- parsedArguments$show_extreme_climate_years
  climatology_until_eoy <- parsedArguments$climatology_until_eoy
  country_code <- parsedArguments$country_code
  out_dir <- parsedArguments$out_dir
  lon_min <- parsedArguments$lon_min
  lon_max <- parsedArguments$lon_max
  lat_min <- parsedArguments$lat_min
  lat_max <- parsedArguments$lat_max
  end_date <- parsedArguments$end_date
  animation_pace <- parsedArguments$animation_pace
  output_format <- parsedArguments$output_format
  language <- parsedArguments$language
  start_date <- parsedArguments$start_date
  freeze_animation <- parsedArguments$freeze_animation
  outfile_name <- parsedArguments$outfile_name
  keep_files <- parsedArguments$keep_files
  states <- parsedArguments$states
  attach <- parsedArguments$attach
  infile_attach <- parsedArguments$infile_attach
  new_infile <- parsedArguments$new_infile
  dwd_logo <- parsedArguments$dwd_logo
  verbose <- parsedArguments$verbose
  nc <- parsedArguments$nc
  
  # check_infile_monitor_climate
  # if(accumulate == FALSE)
  #   stop("Creating a multi day graphic of non accumulated data is not possible. Please choose to accumulate the infile, or plot format 'animation', or select a single day in the date range")
  
  if (attach) {
    attach_file(variable = variable,
                infile = infile,
                infile_attach = infile_attach,
                new_infile = new_infile,
                temp_dir = temp_dir,
                verbose = verbose
    )
    infile <- new_infile
  }

    # If user wants to accumulate the file, we do so here.
    finalInfile <- extractFinalOutfile(
      variable = variable,
      infile = infile,
      start_date = start_date,
      end_date = end_date,
      accumulate = accumulate,
      temp_dir = temp_dir,
      verbose = verbose,
      nc = nc
    )

    climatology_file <- calculate_climatology(
      variable = variable,
      climate_year_start = climate_year_start,
      climate_year_end = climate_year_end,
      start_date = start_date,
      end_date = end_date,
      country_code = country_code,
      climate_dir = climate_dir,
      infile = infile,
      lon_min = lon_min,
      lon_max = lon_max,
      lat_min = lat_min,
      lat_max = lat_max,
      accumulate = accumulate,
      verbose = verbose,
      nc = nc
    )

  if (is_country(country_code)) {
    mask_file <- create_country_mask(
      infile = finalInfile,
      temp_dir = temp_dir,
      country_code = country_code,
      states = states,
      verbose = verbose
    )

    mask_file_final <- create_country_mask_final(
      mask_infile = mask_file,
      temp_dir = temp_dir,
      country_code = country_code,
      lon_min = lon_min,
      lon_max = lon_max,
      lat_min = lat_min,
      lat_max = lat_max,
      verbose = verbose
    )

    # Remove reusable file if desired
    if (!keep_files & file.exists(mask_file)) {
      file.remove(mask_file)
    }

    # Apply final mask file to climatology file
    climate_masked_file <- apply_mask_clima(
      variable = variable,
      mask_file_final = mask_file_final,
      climatology_file = climatology_file,
      temp_dir = temp_dir,
      country_code = country_code,
      climate_year_start = climate_year_start,
      climate_year_end = climate_year_end,
      accumulate = accumulate
    )
  } else {
    climate_masked_file <- climatology_file
  }

  var_climatology_accumulated_fldmean <- fieldmean_climate(
    variable = variable,
    temp_dir = temp_dir,
    climate_infile = climate_masked_file,
    climate_year_start = climate_year_start,
    climate_year_end = climate_year_end,
    country_code = country_code,
    accumulate = accumulate
  )

  infile_current <- fieldmean_current(
    variable = variable,
    temp_dir = temp_dir,
    infile = finalInfile,
    mask_file_final = mask_file_final,
    country_code = country_code,
    lon_min = lon_min,
    lon_max = lon_max,
    lat_min = lat_min,
    lat_max = lat_max,
    climate_year_start = climate_year_start,
    climate_year_end = climate_year_end,
    start_date = start_date,
    end_date = end_date
  )

  fieldmean_ensemble(
    variable = variable,
    infile = infile,
    mask_file_final = mask_file_final,
    temp_dir = temp_dir,
    climate_dir = climate_dir,
    country_code = country_code,
    lon_min = lon_min,
    lon_max = lon_max,
    lat_min = lat_min,
    lat_max = lat_max,
    climate_year_start = climate_year_start,
    climate_year_end = climate_year_end,
    accumulate = accumulate,
    verbose = verbose,
    keep_files = keep_files,
    nc = nc
  )

  # Remove reusable files if desired
  if (!keep_files) {
    if (file.exists(climate_masked_file)) { file.remove(climate_masked_file) }
    if (is_country(country_code) && file.exists(mask_file_final)) { file.remove(mask_file_final) }
  }

  plot_fieldmean(
    variable = variable,
    infile = var_climatology_accumulated_fldmean,
    infile2 = infile_current,
	  infile3 = infile,								# added to plot all years
    country_code = country_code,
    climate_year_start = climate_year_start,
    climate_year_end = climate_year_end,
    show_extreme_climate_years = show_extreme_climate_years,
    climatology_until_eoy = climatology_until_eoy,
    animation_pace = animation_pace,
    output_format = output_format,
    language = language,
    temp_dir = temp_dir,
    out_dir = out_dir,
    start_date = start_date,
    end_date = end_date,
    freeze_animation = freeze_animation,
    outfile_name = outfile_name,
    adjustAccumulation = accumulate,
    dwd_logo = dwd_logo,
    verbose = verbose
  )
}
