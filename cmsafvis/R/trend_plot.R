#' A 'cmsaf' extension for creating trend plots.
#'
#' This plotting routine generates graphical output for
#' the given variable within the given time range and area.
#' Dependent on the output format a PNG is created.
#'
#' @inheritParams monitor_climate
#'
#' @export
#' @importFrom assertthat assert_that is.date is.dir is.flag is.number is.readable is.string is.writeable

trend_plot <- function(variable = NULL,
                       infile = NULL,
                       selected_number = 1,
                       analyze_method = TRUE,
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
                       output_format = "graphic",
                       language = "eng",
                       keep_files = TRUE,
                       states = FALSE,
                       attach = FALSE,
                       infile_attach = "auto",
                       verbose = TRUE,
                       nc = NULL)
{
  # Call central argument parser
  arguments_necessary <- methods::formalArgs(parse_arguments)
  arguments <- as.list(match.call())
  arguments[[1]] <- "trend_plot"
  names(arguments)[1] <- "plot_type"
  arguments <- arguments[stats::na.omit(match(arguments_necessary, names(arguments)))]
  parsedArguments <- do.call(parse_arguments, arguments, envir = parent.frame())
  
  # Use parsed arguments
  variable <- parsedArguments$variable

  temp_dir <- parsedArguments$temp_dir
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
  
  output_format <- parsedArguments$output_format
  
  outfile_name <- parsedArguments$outfile_name
  
  #keep_files <- parsedArguments$keep_files
  states <- parsedArguments$states
  attach <- parsedArguments$attach
  infile_attach <- parsedArguments$infile_attach
  new_infile <- parsedArguments$new_infile
  verbose <- parsedArguments$verbose
  nc <- parsedArguments$nc
  
  # convert, because we need another data type
  if(analyze_method == FALSE)
    analyze_method = "mean"
  else if(analyze_method == TRUE)
    analyze_method = "accumulate"
  
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
  
  finalInfile <- extractOutfile(
    variable = variable,
    infile = infile,
    selected_number = selected_number,
    analyze_method = analyze_method,
    temp_dir = temp_dir,
    verbose = verbose,
    nc = nc
  )

  climatology_file <- calculate_climatology_outfile(
    variable = variable,
    climate_year_start = climate_year_start,
    climate_year_end = climate_year_end,
    country_code = country_code,
    climate_dir = climate_dir,
    infile = infile,
    lon_min = lon_min,
    lon_max = lon_max,
    lat_min = lat_min,
    lat_max = lat_max,
    selected_number = selected_number,
    analyze_method = analyze_method,
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
  }

  # convert, because we need another data type
  mean_value <- FALSE
  accumulate = FALSE
  if(analyze_method == "mean")
    mean_value = TRUE
  else if(analyze_method == "accumulate")
    accumulate = TRUE

  var_masked_file <- apply_mask_final(
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
    selected_number = selected_number
  )

  # Remove reusable file if desired
  if (!keep_files) {
    if (is_country(country_code) && file.exists(mask_file_final)) { file.remove(mask_file_final) }
  }
  
  country_name <- get_country_name(country_code)
  title <- paste0("Trend Plot: ", country_name, " (", climate_year_start, " - ", substring(end_date,1,4), ")")

  plot_trend(
    variable = variable,
    infile = var_masked_file,
    climatology_file = climatology_file,
    out_dir = out_dir,
    climate_year_start = climate_year_start,
    climate_year_end = climate_year_end,
    start_date = start_date,
    end_date = end_date,
    country_code = country_code,
    outfile_name = outfile_name,
    language = language,
    title = title,
    subtitle = "", 
    trend_line = TRUE, 
    verbose = TRUE)
}