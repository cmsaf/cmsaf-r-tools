#' A 'cmsaf' extension for creating an anomaly map.
#'
#' This plotting routine generates a graph showing the anomaly of a 
#' given variable within the given time range and area. The intended 
#' application is for daily accumulated data, such as sunshine duration.
#' Dependent on the output format a PNG or MP4 is created.
#'
#' You can pass a YAML config file and / or specify the arguments directly.
#' Argument prioritization is done in the following way:
#' Direct argument > config argument > default argument.
#' Thus, if you pass a existing config file but also want to modify a specific 
#' argument you can do that easily.
#'
#' @inheritParams monitor_climate
#'
#' @export
#' @importFrom assertthat assert_that is.date is.dir is.flag is.number is.readable is.string is.writeable
anomaly_map <- function(config = NULL,
                        variable = NULL,
                        accumulate = FALSE,
                        mean_value = FALSE,
                        infile = NULL,
                        temp_dir = tempdir(),
                        out_dir = getwd(),
                        climate_dir = NULL,
                        climate_year_start = 1983,
                        climate_year_end = 2018,
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
                        min_value = NULL,
                        max_value = NULL,
						            color_pal = 1,
						            relative = FALSE,
                        nbreaks = NULL,
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
  arguments[[1]] <- "anomaly_map"
  names(arguments)[1] <- "plot_type"
  arguments <- arguments[stats::na.omit(match(arguments_necessary, names(arguments)))]
  parsedArguments <- do.call(parse_arguments, arguments, envir = parent.frame())

  # Use parsed arguments
  variable <- parsedArguments$variable
  accumulate <- parsedArguments$accumulate
  mean_value <- parsedArguments$mean_value
  temp_dir <- parsedArguments$temp_dir
  climate_dir <- parsedArguments$climate_dir
  climate_year_start <- parsedArguments$climate_year_start
  climate_year_end <- parsedArguments$climate_year_end
  country_code <- parsedArguments$country_code
  out_dir <- parsedArguments$out_dir
  lon_min <- parsedArguments$lon_min
  lon_max <- parsedArguments$lon_max
  lat_min <- parsedArguments$lat_min
  lat_max <- parsedArguments$lat_max
  infile <- parsedArguments$infile
  start_date <- parsedArguments$start_date
  end_date <- parsedArguments$end_date
  language <- parsedArguments$language
  freeze_animation <- parsedArguments$freeze_animation
  animation_pace <- parsedArguments$animation_pace
  output_format <- parsedArguments$output_format
  min_value <- parsedArguments$min_value
  max_value <- parsedArguments$max_value
  nbreaks <- parsedArguments$nbreaks
  color_pal <- parsedArguments$color_pal
  relative <- parsedArguments$relative
  outfile_name <- parsedArguments$outfile_name
  keep_files <- parsedArguments$keep_files
  states <- parsedArguments$states
  attach <- parsedArguments$attach
  infile_attach <- parsedArguments$infile_attach
  new_infile <- parsedArguments$new_infile
  dwd_logo <- parsedArguments$dwd_logo
  verbose <- parsedArguments$verbose
  nc <- parsedArguments$nc
  
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

  if(mean_value){
    finalInfile <- extractOutfileMean(
      variable = variable,
      infile = infile,
      start_date = start_date,
      end_date = end_date,
      mean_value = mean_value,
      temp_dir = temp_dir,
      verbose = verbose,
      nc = nc
    )
  } else
  {
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
  }
 
  if(mean_value)
  {
    climatology_file <- calculate_climatology_mean(
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
      mean_value = mean_value,
      verbose = verbose,
      nc = nc
    )
  } else {
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
  }
  
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
  }

  var_masked_infile <- apply_mask_current(
    variable = variable,
    temp_dir = temp_dir,
    infile = finalInfile,
    mask_file_final = mask_file_final,
    climatology_file = climatology_file,
    country_code = country_code,
    lon_min = lon_min,
    lon_max = lon_max,
    lat_min = lat_min,
    lat_max = lat_max,
    climate_year_start = climate_year_start,
    climate_year_end = climate_year_end,
    start_date = start_date,
    end_date = end_date,
    accumulate = accumulate,
    mean_value = mean_value,
    relative = relative
  )

  # Remove reusable file if desired
  if (!keep_files) {
    if (file.exists(climatology_file)) { file.remove(climatology_file) }
    if (is_country(country_code) && file.exists(mask_file_final)) { file.remove(mask_file_final) }
  }
  
  if(mean_value){
    plot_abs_map_mean(
      variable = variable,
      country_code = country_code,
      climate_year_start = climate_year_start,
      climate_year_end = climate_year_end,
      infile = var_masked_infile,
      climatology_file = climatology_file,
      out_dir = out_dir,
      start_date = start_date,
      end_date = end_date,
      language = language,
      plot_type = "anomaly_map",
      animation_pace = animation_pace,
      output_format = output_format,
      min_value = min_value,
      max_value = max_value,
	    color_pal = color_pal,
	    relative = relative,
      nbreaks = nbreaks,
      freeze_animation = freeze_animation,
      outfile_name = outfile_name,
      accumulate = accumulate,
      mean_value = mean_value,
      states = states,
      dwd_logo = dwd_logo,
      verbose = verbose
    )
  } else
  {
    plot_abs_map(
      variable = variable,
      country_code = country_code,
      climate_year_start = climate_year_start,
      climate_year_end = climate_year_end,
      infile = var_masked_infile,
      climatology_file = climatology_file,
      out_dir = out_dir,
      start_date = start_date,
      end_date = end_date,
      language = language,
      plot_type = "anomaly_map",
      animation_pace = animation_pace,
      output_format = output_format,
      min_value = min_value,
      max_value = max_value,
	    color_pal = color_pal,
      relative = relative,
      nbreaks = nbreaks,
      freeze_animation = freeze_animation,
      outfile_name = outfile_name,
      accumulate = accumulate,
      states = states,
      dwd_logo = dwd_logo,
      verbose = verbose
    )
  }
  
}
