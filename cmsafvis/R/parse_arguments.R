#' Parse arguments
#'
#' @importFrom assertthat assert_that is.dir is.flag is.number is.readable is.string is.writeable
#'
#' @noRd
parse_arguments <- function(plot_type,
                            config = NULL,
                            variable = NULL,
                            accumulate = FALSE,
                            mean_value = FALSE,
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
  #### Deal with config ####
  if (is.null(config)) {
    configRead <- FALSE
  } else {
    assert_that(is.string(config))
    assert_that(file.exists(normalizePath(config, mustWork = FALSE)))
    assert_that(!is.dir(config))
    assert_that(is.readable(config))

    configParams <- yaml::read_yaml(config)

    configRead <- TRUE
  }
  #### All non optional parameters (either direct argument pass or via config file) ####
  #### infile ####
  if (missing(infile) && missing(nc)) {
    if (configRead) {
      infile <- tryCatch(
        configParams$infile,
        error = function(cond) {
          stop(
            paste0(
              "Can't read 'infile' from config file: ",
              normalizePath(config)
            )
          )
        }
      )
    }
    if (!configRead || is.null(infile)) {
      stop(c(
        "Required argument 'infile' is missing.\n",
        "Specify it directly or as 'nc' or in a config file."
      ))
    }
  }

  if (is.null(nc)) {
    assert_that(is.string(infile))
    assert_that(file.exists(normalizePath(infile, mustWork = FALSE)))
    assert_that(!is.dir(infile))
    assert_that(is.readable(infile))
  }
  #TODO Add check on nc object too.
  
  # Evaluate all default user values extractable from infile or nc
  defaultValues <- getUserOptions(infile = infile, nc = nc)

  #### Check all optional parameters ####
  #### temp_dir ####
  if (missing(temp_dir) && configRead) {
    temp_dir_config <- tryCatch(
      configParams$temp_dir,
      error = function(cond) {
        stop(paste0(
          "Can't read 'temp_dir' from config file: ",
          normalizePath(config)
        ))
      }
    )
    if (!is.null(temp_dir_config)) {
      temp_dir <- temp_dir_config
    }
  }

  assert_that(is.string(temp_dir))
  assert_that(is.dir(temp_dir))
  assert_that(dir.exists(temp_dir))
  assert_that(is.writeable(temp_dir))

  #### climate_dir ####
  if (missing(climate_dir) && configRead) {
    out_dir_config <- tryCatch(
      configParams$climate_dir,
      error = function(cond) {
        stop(paste0(
          "Can't read 'climate_dir' from config file: ",
          normalizePath(config)
        ))
      }
    )
    if (!is.null(out_dir_config)) {
      climate_dir <- out_dir_config
    }
  }
  if (is.null(climate_dir)) {
    climate_dir <- temp_dir
  }
  assert_that(is.string(climate_dir))
  assert_that(is.dir(climate_dir))
  assert_that(dir.exists(climate_dir))
  assert_that(is.writeable(climate_dir))

  #### out_dir ####
  if (missing(out_dir) && configRead) {
    out_dir_config <- tryCatch(
      configParams$out_dir,
      error = function(cond) {
        stop(paste0(
          "Can't read 'out_dir' from config file: ",
          normalizePath(config)
        ))
      }
    )
    if (!is.null(out_dir_config)) {
      out_dir <- out_dir_config
    }
  }

  assert_that(is.string(out_dir))
  assert_that(is.dir(out_dir))
  assert_that(dir.exists(out_dir))
  assert_that(is.writeable(out_dir))

  #### climate_year_start ####
  if (missing(climate_year_start) && configRead) {
    climate_year_start_config <- tryCatch(
      configParams$climate_year_start,
      error = function(cond) {
        stop(paste0(
          "Can't read 'climate_year_start' from config file: ",
          normalizePath(config)
        ))
      }
    )
    if (!is.null(climate_year_start_config)) {
      climate_year_start <- climate_year_start_config
    }
  }

  assert_that(is.number(climate_year_start))

  #### climate_year_end ####
  if (missing(climate_year_end) && configRead) {
    climate_year_end_config <- tryCatch(
      configParams$climate_year_end,
      error = function(cond) {
        stop(paste0(
          "Can't read 'climate_year_end' from config file: ",
          normalizePath(config)
        ))
      }
    )
    if (!is.null(climate_year_end_config)) {
      climate_year_end <- climate_year_end_config
    }
  }

  assert_that(is.number(climate_year_end))
  assert_that(climate_year_start <= climate_year_end)

  #### accumulate ####
  if (missing(accumulate)) {
    if (configRead) {
      accumulate <- tryCatch(
        configParams$accumulate,
        error = function(cond) {
          stop(paste0(
            "Can't read 'accumulate' from config file: ",
            normalizePath(config)
          ))
        }
      )
    }
    if (!configRead || is.null(accumulate)) {
      accumulate <- FALSE
    }
  }

  assert_that(is.flag(accumulate))

  #### show_extreme_climate_years ####
  if (missing(show_extreme_climate_years) && configRead) {
    show_extreme_climate_years_config <- tryCatch(
      configParams$show_extreme_climate_years,
      error = function(cond) {
        stop(paste0(
          "Can't read 'show_extreme_climate_years' from config file: ",
          normalizePath(config)
        ))
      }
    )
    if (!is.null(show_extreme_climate_years_config)) {
      show_extreme_climate_years <- show_extreme_climate_years_config
    }
  }

  if (is.null(show_extreme_climate_years)) {
    show_extreme_climate_years <- accumulate
  }

  if (plot_type != "absolute_map") {
    assert_that(is.flag(show_extreme_climate_years))
  }

  #### climatology_until_eoy ####
  if (missing(climatology_until_eoy)) {
    if (configRead) {
      climatology_until_eoy <- tryCatch(
        configParams$climatology_until_eoy,
        error = function(cond) {
          stop(paste0(
            "Can't read 'climatology_until_eoy' from config file: ",
            normalizePath(config)
          ))
        }
      )
    }
    if (!configRead || is.null(climatology_until_eoy)) {
      climatology_until_eoy <- FALSE
    }
  }

  assert_that(is.flag(climatology_until_eoy))

  #### language ####
  if (missing(language) && configRead) {
    language_config <- tryCatch(
      configParams$language,
      error = function(cond) {
        stop(paste0(
          "Can't read 'language' from config file: ",
          normalizePath(config)
        ))
      }
    )
    if (!is.null(language_config)) {
      language <- language_config
    }
  }

  assert_that(is.string(language))
  assert_that(nchar(language) == 3)
  assert_that(language %in% c("eng", "deu"))

  #### animation_pace ####
  if (missing(animation_pace) && configRead) {
    animation_pace_config <- tryCatch(
      configParams$animation_pace,
      error = function(cond) {
        stop(paste0(
          "Can't read 'animation_pace' from config file: ",
          normalizePath(config)
        ))
      }
    )
    if (!is.null(animation_pace_config)) {
      animation_pace <- animation_pace_config
    }
  }

  assert_that(is.number(animation_pace))
  assert_that(animation_pace > 0)

  #### freeze_animation ####
  if (missing(freeze_animation) && configRead) {
    freeze_animation_config <- tryCatch(
      configParams$freeze_animation,
      error = function(cond) {
        stop(paste0(
          "Can't read 'freeze_animation' from config file: ",
          normalizePath(config)
        ))
      }
    )
    if (!is.null(freeze_animation_config)) {
      freeze_animation <- freeze_animation_config
    }
  }

  assert_that(is.flag(freeze_animation))

  #### min_value ####
  if (missing(min_value) && configRead) {
    min_value_config <- tryCatch(
      configParams$min_value,
      error = function(cond) {
        stop(paste0(
          "Can't read 'min_value' from config file: ",
          normalizePath(config)
        ))
      }
    )
    if (!is.null(min_value_config)) {
      min_value <- min_value_config
    }
  }

  assert_that(is.null(min_value) || is.number(min_value))

  #### max_value ####
  if (missing(max_value) && configRead) {
    max_value_config <- tryCatch(
      configParams$max_value,
      error = function(cond) {
        stop(paste0(
          "Can't read 'max_value' from config file: ",
          normalizePath(config)
        ))
      }
    )
    if (!is.null(max_value_config)) {
      max_value <- max_value_config
    }
  }

  assert_that(is.null(max_value) || is.number(max_value))
  if (is.number(min_value) && is.number(max_value)) {
    assert_that(min_value <= max_value)
  }
  
  #### color_pal ####
  if (missing(color_pal) && configRead) {
    color_pal_config <- tryCatch(
      configParams$color_pal,
      error = function(cond) {
        stop(paste0(
          "Can't read 'color_pal' from config file: ",
          normalizePath(config)
        ))
      }
    )
    if (!is.null(color_pal_config)) {
      color_pal <- color_pal_config
    }
  }
  
  assert_that(is.number(color_pal))
  
  #### relative ####
  if (missing(relative) && configRead) {
    relative_config <- tryCatch(
      configParams$relative,
      error = function(cond) {
        stop(paste0(
          "Can't read 'relative' from config file: ",
          normalizePath(config)
        ))
      }
    )
    if (!is.null(relative_config)) {
      relative <- relative_config
    }
  }
  
  assert_that(is.logical(relative))

  #### nbreaks ####
  if (missing(nbreaks) && configRead) {
    nbreaks_config <- tryCatch(
      configParams$nbreaks,
      error = function(cond) {
        stop(paste0(
          "Can't read 'nbreaks' from config file: ",
          normalizePath(config)
        ))
      }
    )
    if (!is.null(nbreaks_config)) {
      nbreaks <- nbreaks_config
    }
  }

  assert_that(is.null(nbreaks) || (is.numeric(nbreaks) && nbreaks > 0))
  if (is.numeric(nbreaks)) {
    nbreaks <- as.integer(nbreaks)
    assert_that(is.integer(nbreaks))
  }

  #### variable ####
  if (missing(variable)) {
    if (configRead) {
      variable <- tryCatch(
        configParams$variable,
        error = function(cond) {
          stop(paste0(
            "Can't read 'variable' from config file: ",
            normalizePath(config)
          ))
        }
      )
    }
    if (!configRead || is.null(variable)) {
      variable <- defaultValues$variables[1]
    }
  }

  assert_that(is.string(variable))
  if (!(variable %in% defaultValues$variables)) {
    if (!is.null(nc)) stop(paste0("The given nc object does not contain the variable ", variable, "."))
    else stop(paste0("The given infile ", infile, " does not contain the variable ", variable, "."))
  }

  #### keep_files ####
  if (missing(keep_files)) {
    if (configRead) {
      keep_files <- tryCatch(
        configParams$keep_files,
        error = function(cond) {
          stop(paste0(
            "Can't read 'keep_files' from config file: ",
            normalizePath(config)
          ))
        }
      )
    }
    if (!configRead || is.null(keep_files)) {
      keep_files <- TRUE
    }
  }

  assert_that(is.flag(keep_files))

  #### country_code ####
  if (missing(country_code)) {
    if (configRead) {
      country_code_config <- tryCatch(
        configParams$country_code,
        error = function(cond) {
          stop(paste0(
            "Can't read 'country_code' from config file: ",
            normalizePath(config)
          ))
        }
      )
    }

    if (!is.null(country_code_config)) {
      country_code <- country_code_config
    }
  }

  assert_that(is.string(country_code))
  assert_that(is.upper(country_code))
  assert_that(nchar(country_code) == 3)
  assert_that(is_valid_regioncode(country_code))

  #### lon_min ####
  if (missing(lon_min)) {
    if (configRead) {
      lon_min <- tryCatch(
        configParams$lon_min,
        error = function(cond) {
          stop(
            paste0(
              "Can't read 'lon_min' from config file: ",
              normalizePath(config)
            )
          )
        }
      )
    }
  }

  if (is.null(lon_min)) {
    lon_min <- defaultValues$lon_range[1]
  }

  assert_that(is.number(lon_min))

  #### lon_max ####
  if (missing(lon_max)) {
    if (configRead) {
      lon_max <- tryCatch(
        configParams$lon_max,
        error = function(cond) {
          stop(
            paste0(
              "Can't read 'lon_max' from config file: ",
              normalizePath(config)
            )
          )
        }
      )
    }
  }

  if (is.null(lon_max)) {
    lon_max <- defaultValues$lon_range[2]
  }

  assert_that(is.number(lon_max))
  assert_that(lon_max >= lon_min)
  assert_that(lon_max <= 180)
  assert_that(lon_min >= -180)

  #### lat_min ####
  if (missing(lat_min)) {
    if (configRead) {
      lat_min <- tryCatch(
        configParams$lat_min,
        error = function(cond) {
          stop(
            paste0(
              "Can't read 'lat_min' from config file: ",
              normalizePath(config)
            )
          )
        }
      )
    }
  }

  if (is.null(lat_min)) {
    lat_min <- defaultValues$lat_range[1]
  }

  assert_that(is.number(lat_min))

  #### lat_max ####
  if (missing(lat_max)) {
    if (configRead) {
      lat_max <- tryCatch(
        configParams$lat_max,
        error = function(cond) {
          stop(
            paste0(
              "Can't read 'lat_max' from config file: ",
              normalizePath(config)
            )
          )
        }
      )
    }
  }

  if (is.null(lat_max)) {
    lat_max <- defaultValues$lat_range[2]
  }

  assert_that(is.number(lat_max))
  assert_that(lat_max >= lat_min)
  assert_that(lat_max <= 90)
  assert_that(lat_min >= -90)

  # If country code continental, get coordinates
  if (country_code %in% c("AFR", "EUR", "TOT")) {
    if (verbose) {
      message(paste0("Overwriting longitude and latitude range according to country code: ", country_code, "."))
    }

    area <- get_continental_coordinates(country_code)
    lon_min <- area$lon_min
    lon_max <- area$lon_max
    lat_min <- area$lat_min
    lat_max <- area$lat_max
  }


  #### start_date ####
  if (missing(start_date)) {
    if (configRead) {
      start_date <- tryCatch(
        configParams$start_date,
        error = function(cond) {
          stop(paste0(
            "Couldn't read 'start_date' from config file: ",
            normalizePath(config)
          ))
        }
      )
    }
    if (!configRead || is.null(start_date)) {
      start_date <- defaultValues$time_range[1]
    }
  }
  # Accept a date or parse a string.
  assert_that(is.date(start_date) || is.string(start_date))
  if (is.character(start_date)) {
    start_date <- as.Date(start_date)
  }

  #### end_date ####
  if (missing(end_date)) {
    if (configRead) {
      end_date <- tryCatch(
        configParams$end_date,
        error = function(cond) {
          stop(paste0(
            "Can't read 'end_date' from config file: ",
            normalizePath(config)
          ))
        }
      )
    }
    if (!configRead || is.null(end_date)) {
      end_date <- defaultValues$time_range[2]
    }
  }
  # Accept a date or parse a string.
  assert_that(is.date(end_date) || is.string(end_date))
  if (is.character(end_date)) {
    end_date <- as.Date(end_date)
  }

  if(plot_type %in% c("warming_stripes_plot", "time_series_plot", "trend_plot")){
    assert_that(end_date >= start_date)

    start_year <- format(start_date, "%Y")
    end_year <- format(end_date, "%Y")
  }else{
    assert_that(end_date >= start_date)
    
    start_year <- format(start_date, "%Y")
    end_year <- format(end_date, "%Y")
    assert_that(start_year == end_year,
                msg = "Year of start_date not equal to year of end_date")
  }
  
  if (!(accumulate || mean_value)) {
    if (output_format == "graphic") {
      if (start_date != end_date) {
        if (plot_type != "fieldmean_plot") {
          if(!(plot_type %in% c("warming_stripes_plot", "time_series_plot", "trend_plot"))){
            stop("You are trying to create a multi-day graphic of non accumulated data. This is not allowed. Either accumulate the data or chose start_date equal to end_date.")
          }
        }
      }
    }
  }

  # Leap year check
  start_doy <- as.numeric(format(start_date, format = "%j"))
  finish_doy <- as.numeric(format(end_date, format = "%j"))
  if (is_leap_year(start_year)) {
    if (start_doy == 60 && finish_doy == 60) {
      stop("You chose to only analyze a leap year date but these dates are removed in this operator. Please choose another time range.")
    }

    if (start_doy == 60) {
      warning("Leap year dates are removed in this operator. The given start date will be adjusted to March first.")
      start_date <- start_date + 1
    }

    if (finish_doy == 60) {
      warning("Leap year dates are removed in this operator. The given end date will be adjusted to February 28th.")
      end_date <- end_date - 1
    }
  }

  #### states ####
  if (missing(states)) {
    if (configRead) {
      states <- tryCatch(
        configParams$states,
        error = function(cond) {
          stop(paste0(
            "Can't read 'states' from config file: ",
            normalizePath(config)
          ))
        }
      )
    }
    if (!configRead || is.null(states)) {
      states <- FALSE
    }
  }

  assert_that(is.flag(states))

  if (states && !is_country(country_code)) {
    stop(
      "State borders can only be plotted for countries.\n",
      "Either set \"states = FALSE\" or set \"country_code\" to that of a country."
    )
  }

  #### dwd_logo ####
  if (missing(dwd_logo)) {
    if (configRead) {
      dwd_logo <- tryCatch(
        configParams$states,
        error = function(cond) {
          stop(paste0(
            "Can't read 'dwd_logo' from config file: ",
            normalizePath(config)
          ))
        }
      )
    }
    if (!configRead || is.null(dwd_logo)) {
      dwd_logo <- FALSE
    }
  }
  
  assert_that(is.flag(dwd_logo))
  
  #### verbose ####
  if (missing(verbose)) {
    if (configRead) {
      verbose <- tryCatch(
        configParams$verbose,
        error = function(cond) {
          stop(paste0(
            "Can't read 'verbose' from config file: ",
            normalizePath(config)
          ))
        }
      )
    }
    if (!configRead || is.null(verbose)) {
      verbose <- TRUE
    }
  }

  assert_that(is.flag(verbose))

  #### output_format ####
  if (missing(output_format)) {
    if (configRead) {
      config_output_format <- tryCatch(
        configParams$output_format,
        error = function(cond) {
          stop(paste0(
            "Can't read 'output_format' from config file: ",
            normalizePath(config)
          ))
        }
      )

      if (!is.null(config_output_format)) {
        output_format <- config_output_format
      }
    }
  }

  assert_that(is.string(output_format))
  assert_that(output_format %in% c("graphic", "animation"))

  #### outfile_name ####
  if (missing(outfile_name)) {
    if (configRead) {
      outfile_name_config <- tryCatch(
        configParams$outfile_name,
        error = function(cond) {
          stop(paste0(
            "Can't read 'outfile_name' from config file: ",
            normalizePath(config)
          ))
        }
      )

      if (!is.null(outfile_name_config)) {
        outfile_name <- outfile_name_config
      }
    }
  }

  # Build outfile_name from params
  if (is.null(outfile_name)) {
    outfile_name <- default_out_name(
      output_format = output_format,
      plot_type = plot_type,
      variable = variable,
      country_code = country_code,
      start_date = start_date,
      end_date = end_date,
      climate_year_start = climate_year_start,
      climate_year_end = climate_year_end,
      language = language
    )
  }

  assert_that(is.character(outfile_name))
  if (output_format == "animation") {
    if (!endsWith(outfile_name, ".mp4")) {
      outfile_name <- add_video_ext(outfile_name)
    }
  } else {
    if (!endsWith(outfile_name, ".png")) {
      outfile_name <- add_image_ext(outfile_name)
    }
  }

  # Check consistency
  if (output_format == "graphic") {
    if (!(accumulate || mean_value)) {
      if (start_date != end_date) {
        if (!(plot_type %in% c("fieldmean_plot", "warming_stripes_plot", "time_series_plot", "trend_plot")))
        stop("Creating graphics of non-accumulated data can only be done for a single day! Please choose start_date == end_date or select output_format = animation.")
      }
    }
  }

  if (attach) {
      tryCatch({
      #TODO Is it possible to get attach to work with an nc input as well?
      if (!is.null(nc)) stop("'infile' must be provided instead of 'nc' to use the attach option")
      first_day_in <- substr(basename(infile), 5, 14)
      last_day_in <- substr(basename(infile), 16, 25)

      if (infile_attach == "auto") {
      first_day_attach <- format(paste(climate_year_start, "01", "01", sep = "-"))
      last_day_attach <- as.character(as.Date(first_day_in) - 1)
      infile_attach <- add_ncdf_ext(
        construct_filename(
          variable,
          paste0(first_day_attach, "-", last_day_attach)
        )
      )
      if (!file.exists(infile_attach)) {
        stop("No unambiguous file to attach to")
      }
      } else {
        first_day_attach <- substr(basename(infile_attach), 5, 14)
        last_day_attach <- substr(basename(infile_attach), 16, 25)
      }
      }, error = function(cond) {
        stop(paste("Could not find a file to attach the data to.
                   Maybe the file does not have a canonical filename."))
      })
      new_infile <- add_ncdf_ext(
        construct_filename(
          variable,
          paste(first_day_attach, last_day_in, sep = "-")
        )
      )
      new_infile <- file.path(temp_dir, new_infile)
  } else {
      new_infile <- infile
    }

  result <- list(
    variable = variable,
    accumulate = accumulate,
    mean_value = mean_value,
    temp_dir = temp_dir,
    infile = infile,
    climate_dir = climate_dir,
    climate_year_start = climate_year_start,
    climate_year_end = climate_year_end,
    show_extreme_climate_years = show_extreme_climate_years,
    climatology_until_eoy = climatology_until_eoy,
    country_code = country_code,
    out_dir = out_dir,
    lon_min = lon_min,
    lon_max = lon_max,
    lat_min = lat_min,
    lat_max = lat_max,
    start_date = start_date,
    end_date = end_date,
    language = language,
    animation_pace = animation_pace,
    output_format = output_format,
    min_value = min_value,
    max_value = max_value,
    color_pal = color_pal,
    relative = relative,
    nbreaks = nbreaks,
    freeze_animation = freeze_animation,
    outfile_name = outfile_name,
    keep_files = keep_files,
    states = states,
    attach = attach,
    infile_attach = infile_attach,
    new_infile = new_infile,
    dwd_logo = dwd_logo,
    verbose = verbose,
    nc = nc
  )

  if (verbose) {
    original_options <- options(useFancyQuotes = FALSE)
    result_output <- result
    for (i in seq_along(result_output)) {
      current <- result_output[[i]]
      if (assertthat::is.date(current)) {
        current <- as.character(current)
      }
      if (is.character(current)) {
        result_output[[i]] <- sQuote(current)
      }
    }
    options(original_options)

    message(
      paste(
        "Using the following arguments:",
        paste(names(result), result_output, sep = "=", collapse = ", ")
      )
    )
  }

  return(result)
}

default_out_name <- function(output_format,
                             plot_type,
                             variable,
                             country_code,
                             start_date,
                             end_date,
                             climate_year_start = NULL,
                             climate_year_end = NULL,
                             language) {

  if (plot_type == "climatology_map") {
    # Compute some parameters from arguments
    start_date_short <- format(start_date, "%m%d")
    end_date_short <- format(end_date, "%m%d")

    # climatology has different style
    formattedTimeRange <- paste0(climate_year_start, "-", climate_year_end, "_", start_date_short, "-", end_date_short)
  } else {
    # absolute, anomaly, fieldmean, fieldmean_and_anomaly
    formattedTimeRange <- paste0(format(start_date, "%Y%m%d"), "-", format(end_date, "%Y%m%d"))
  }

  if (output_format == "graphic") {
    return(
      add_image_ext(
        construct_filename(
          variable,
          plot_type,
          country_code,
          formattedTimeRange,
          language
        )
      )
    )
  } else {
    return(
      add_video_ext(
        construct_filename(
          variable,
          plot_type,
          country_code,
          formattedTimeRange,
          language
        )
      )
    )
  }
}
