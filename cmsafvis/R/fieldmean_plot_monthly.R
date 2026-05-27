#' Detect the temporal resolution used by fieldmean_plot.
#'
#' @noRd
detect_fieldmean_time_resolution <- function(variable,
                                             infile,
                                             nc = NULL) {
  if (!is.null(nc)) {
    nc_in <- nc
    close_nc <- FALSE
  } else {
    nc_in <- ncdf4::nc_open(infile)
    close_nc <- TRUE
  }
  
  if (close_nc) {
    on.exit(ncdf4::nc_close(nc_in), add = TRUE)
  }
  
  time_values <- ncdf4::ncvar_get(nc_in, "time")
  time_units <- ncdf4::ncatt_get(nc_in, "time", "units")$value
  date_time <- as.Date(cmsafops::get_time(time_units, time_values))
  
  if (length(date_time) < 2) {
    stop("Cannot detect the temporal resolution from fewer than two time steps. Please set time_resolution explicitly.")
  }
  
  unique_dates <- sort(unique(date_time))
  median_step_days <- stats::median(as.numeric(diff(unique_dates)))
  steps_per_year <- table(format(date_time, "%Y"))
  max_steps <- max(as.integer(steps_per_year))
  
  if (max_steps <= 12 && median_step_days >= 25) {
    return("monthly")
  }
  
  if (max_steps >= 300 || median_step_days <= 2) {
    return("daily")
  }
  
  stop(
    paste0(
      "Could not detect temporal resolution automatically. ",
      "Please use time_resolution = 'daily' or time_resolution = 'monthly'."
    )
  )
}

#' Create a robust temporary filename for the monthly fieldmean workflow.
#'
#' Avoid construct_filename() here because it is built around the historic
#' daily/yearly workflow and may interpret additional components as a year.
#'
#' @noRd
fieldmean_monthly_tempfile <- function(variable, country_code, step, temp_dir) {
  safe_part <- function(x) {
    x <- as.character(x)
    x <- gsub("[^A-Za-z0-9_-]+", "_", x)
    x
  }
  
  file.path(
    temp_dir,
    paste0(
      safe_part(variable),
      "_monthly_",
      safe_part(country_code),
      "_",
      safe_part(step),
      ".nc"
    )
  )
}

#' Create fieldmean plots from monthly data.
#'
#' This is the monthly time-series path for fieldmean_plot(). It first reduces
#' the spatial data to one regional fieldmean time series and then computes the
#' current year, ensemble years and climatology from a small year-month matrix.
#'
#' @noRd
fieldmean_plot_monthly <- function(variable,
                                   accumulate,
                                   infile,
                                   temp_dir,
                                   out_dir,
                                   climate_year_start,
                                   climate_year_end,
                                   show_extreme_climate_years,
                                   start_date,
                                   end_date,
                                   country_code,
                                   lon_min,
                                   lon_max,
                                   lat_min,
                                   lat_max,
                                   outfile_name,
                                   output_format,
                                   animation_pace,
                                   freeze_animation,
                                   language,
                                   keep_files,
                                   states,
                                   dwd_logo,
                                   verbose,
                                   nc = NULL) {
  if (!is.null(nc)) {
    stop("Monthly fieldmean plots currently require an input file path. The 'nc' object interface is not supported for this path yet.")
  }
  
  time_series <- prepare_monthly_fieldmean_timeseries(
    variable = variable,
    infile = infile,
    temp_dir = temp_dir,
    country_code = country_code,
    lon_min = lon_min,
    lon_max = lon_max,
    lat_min = lat_min,
    lat_max = lat_max,
    keep_files = keep_files,
    states = states,
    verbose = verbose
  )
  
  plot_data <- build_monthly_fieldmean_plot_data(
    time_series = time_series,
    climate_year_start = climate_year_start,
    climate_year_end = climate_year_end,
    start_date = start_date,
    end_date = end_date,
    accumulate = accumulate
  )
  
  draw_monthly_fieldmean_plot(
    variable = variable,
    infile = infile,
    plot_data = plot_data,
    country_code = country_code,
    climate_year_start = climate_year_start,
    climate_year_end = climate_year_end,
    show_extreme_climate_years = show_extreme_climate_years,
    animation_pace = animation_pace,
    output_format = output_format,
    language = language,
    out_dir = out_dir,
    outfile_name = outfile_name,
    freeze_animation = freeze_animation,
    dwd_logo = dwd_logo,
    verbose = verbose
  )
  
  invisible(plot_data)
}

#' Compute one monthly fieldmean time series for the selected region.
#'
#' @noRd
prepare_monthly_fieldmean_timeseries <- function(variable,
                                                 infile,
                                                 temp_dir,
                                                 country_code,
                                                 lon_min,
                                                 lon_max,
                                                 lat_min,
                                                 lat_max,
                                                 keep_files,
                                                 states,
                                                 verbose) {
  if (verbose) {
    message("Preparing monthly regional fieldmean time series.")
  }
  
  region_file <- fieldmean_monthly_tempfile(
    variable = variable,
    country_code = country_code,
    step = "region",
    temp_dir = temp_dir
  )
  
  cmsafops::sellonlatbox(
    var = variable,
    infile = infile,
    outfile = region_file,
    lon1 = lon_min,
    lon2 = lon_max,
    lat1 = lat_min,
    lat2 = lat_max,
    overwrite = TRUE
  )
  
  fieldmean_input <- region_file
  files_to_remove <- region_file
  
  if (is_country(country_code)) {
    mask_file <- create_country_mask(
      infile = infile,
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
    
    masked_file <- fieldmean_monthly_tempfile(
      variable = variable,
      country_code = country_code,
      step = "mask",
      temp_dir = temp_dir
    )
    
    adjust_location(
      variable = variable,
      variable_mask = get_country_name(country_code),
      is_country = TRUE,
      mask_file = mask_file_final,
      var_file = region_file,
      outfile = masked_file
    )
    
    fieldmean_input <- masked_file
    files_to_remove <- c(files_to_remove, masked_file)
    
    if (!keep_files) {
      files_to_remove <- c(files_to_remove, mask_file, mask_file_final)
    }
  }
  
  fieldmean_file <- fieldmean_monthly_tempfile(
    variable = variable,
    country_code = country_code,
    step = "fldmean",
    temp_dir = temp_dir
  )
  
  cmsafops::fldmean(
    var = variable,
    infile = fieldmean_input,
    outfile = fieldmean_file,
    overwrite = TRUE
  )
  
  time_series <- read_monthly_fieldmean_timeseries(
    variable = variable,
    infile = fieldmean_file
  )
  
  if (!keep_files) {
    for (file in unique(files_to_remove)) {
      if (file.exists(file)) {
        file.remove(file)
      }
    }
  }
  
  time_series
}

#' Read a fieldmean NetCDF file as a data frame.
#'
#' @noRd
read_monthly_fieldmean_timeseries <- function(variable,
                                              infile) {
  nc_in <- ncdf4::nc_open(infile)
  on.exit(ncdf4::nc_close(nc_in), add = TRUE)
  
  time_values <- ncdf4::ncvar_get(nc_in, "time")
  time_units <- ncdf4::ncatt_get(nc_in, "time", "units")$value
  date_time <- as.Date(cmsafops::get_time(time_units, time_values))
  
  values <- as.numeric(ncdf4::ncvar_get(nc_in, variable))
  
  fill_value <- ncdf4::ncatt_get(nc_in, variable, "_FillValue")$value
  missing_value <- ncdf4::ncatt_get(nc_in, variable, "missing_value")$value
  invalid_values <- c(fill_value, missing_value)
  invalid_values <- invalid_values[!is.null(invalid_values) & !is.na(invalid_values)]
  
  if (length(invalid_values) > 0) {
    values[values %in% invalid_values] <- NA_real_
  }
  
  data.frame(
    date = date_time,
    year = as.integer(format(date_time, "%Y")),
    month = as.integer(format(date_time, "%m")),
    value = values
  )
}

#' Build plot-ready monthly matrices and vectors.
#'
#' @noRd
build_monthly_fieldmean_plot_data <- function(time_series,
                                              climate_year_start,
                                              climate_year_end,
                                              start_date,
                                              end_date,
                                              accumulate) {
  current_year <- as.integer(format(end_date, "%Y"))
  start_month <- as.integer(format(start_date, "%m"))
  end_month <- as.integer(format(end_date, "%m"))
  
  if (start_month > end_month) {
    stop("Monthly fieldmean plots require start_date and end_date to be in the same year and in chronological month order.")
  }
  
  selected_months <- start_month:end_month
  
  mean_or_na <- function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    }
    mean(x, na.rm = TRUE)
  }
  
  time_series <- time_series[time_series$month %in% selected_months, ]
  monthly_values <- stats::aggregate(
    value ~ year + month,
    data = time_series,
    FUN = mean_or_na,
    na.action = stats::na.pass
  )
  
  years <- sort(unique(monthly_values$year))
  if (!current_year %in% years) {
    stop(paste("The selected current year", current_year, "is not available in the monthly input data."))
  }
  
  value_matrix <- matrix(
    NA_real_,
    nrow = length(years),
    ncol = length(selected_months),
    dimnames = list(as.character(years), sprintf("%02d", selected_months))
  )
  
  for (i in seq_len(nrow(monthly_values))) {
    year_index <- as.character(monthly_values$year[i])
    month_index <- sprintf("%02d", monthly_values$month[i])
    value_matrix[year_index, month_index] <- monthly_values$value[i]
  }
  
  if (accumulate) {
    value_matrix <- t(apply(value_matrix, 1, function(x) {
      if (all(is.na(x))) {
        return(rep(NA_real_, length(x)))
      }
      cumsum(x)
    }))
    colnames(value_matrix) <- sprintf("%02d", selected_months)
  }
  
  climatology_years <- as.character(climate_year_start:climate_year_end)
  climatology_years <- climatology_years[climatology_years %in% rownames(value_matrix)]
  
  if (length(climatology_years) == 0) {
    stop("None of the requested climatology years are available in the monthly input data.")
  }
  
  current <- as.numeric(value_matrix[as.character(current_year), ])
  climatology <- apply(value_matrix[climatology_years, , drop = FALSE], 2, mean_or_na)
  
  comparison_years <- as.integer(rownames(value_matrix))
  comparison_years <- comparison_years[comparison_years < current_year]
  
  if (length(comparison_years) == 0) {
    stop("The monthly input data do not contain any comparison years before the selected current year.")
  }
  
  ensemble <- value_matrix[as.character(comparison_years), , drop = FALSE]
  
  incomplete_climatology <- rowSums(is.na(value_matrix[climatology_years, , drop = FALSE])) > 0
  if (any(incomplete_climatology)) {
    warning("Some climatology years have missing monthly values. The climatology is computed with na.rm = TRUE.")
  }
  
  list(
    current_year = current_year,
    selected_months = selected_months,
    current = current,
    climatology = as.numeric(climatology),
    ensemble = ensemble,
    comparison_years = comparison_years,
    climatology_years = as.integer(climatology_years),
    accumulate = accumulate
  )
}

#' Draw monthly fieldmean graphics or animations.
#'
#' @noRd
draw_monthly_fieldmean_plot <- function(variable,
                                        infile,
                                        plot_data,
                                        country_code,
                                        climate_year_start,
                                        climate_year_end,
                                        show_extreme_climate_years,
                                        animation_pace,
                                        output_format,
                                        language,
                                        out_dir,
                                        outfile_name,
                                        freeze_animation,
                                        dwd_logo,
                                        verbose) {
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(suppressWarnings(graphics::par(oldpar)), add = TRUE)
  
  pic.width <- 500
  pic.height <- 500
  logo.size <- 0.4
  logo.x <- 0.81
  logo.y <- 0.06
  
  logo_cmsaf_path <- system.file(
    "extdata",
    "CMSAF_logo.png",
    package = "cmsafvis",
    mustWork = TRUE
  )
  logo_cmsaf <- png::readPNG(logo_cmsaf_path)
  dims <- dim(logo_cmsaf)[1:2]
  AR <- dims[1] / dims[2] * pic.width / pic.height
  
  if (dwd_logo) {
    logo.size2 <- 0.1
    logo.x2 <- 0.92
    logo.y2 <- 0.22
    
    logo_dwd_path <- system.file(
      "extdata",
      "DWD_logo.png",
      package = "cmsafvis",
      mustWork = TRUE
    )
    logo_dwd <- png::readPNG(logo_dwd_path)
    dims2 <- dim(logo_dwd)[1:2]
    AR2 <- dims2[1] / dims2[2] * pic.width / pic.height
  }
  
  country_name <- get_country_name(country_code, language = language)
  plot_title <- get_title(
    variable = variable,
    language = language,
    year = as.character(plot_data$current_year)
  )
  plot_title <- paste0(plot_title, " (", country_name, ")")
  ylab_text <- get_axis_label(variable, language)
  
  nc_in <- ncdf4::nc_open(infile)
  var_unit <- ncdf4::ncatt_get(nc_in, variable, "units")$value
  ncdf4::nc_close(nc_in)
  
  if (!is.null(var_unit) && !is.na(var_unit) && (grepl("(neu)", var_unit, fixed = TRUE) || grepl("(new)", var_unit, fixed = TRUE))) {
    remove_substrings <- c("\\(neu\\)", "\\(new\\)")
    for (substring in remove_substrings) {
      var_unit <- gsub(substring, "", var_unit)
    }
    var_unit <- trimws(var_unit)
    pattern <- "\\[.*?\\]"
    ylab_text <- gsub(pattern, paste0("[", var_unit, "]"), ylab_text)
  }
  
  legend_text <- paste0(
    get_climatology_word(language),
    " (",
    climate_year_start,
    "-",
    climate_year_end,
    ")"
  )
  
  x_dates <- as.Date(sprintf(
    "%s-%02d-15",
    plot_data$current_year,
    plot_data$selected_months
  ))
  
  all_values <- c(
    as.numeric(plot_data$ensemble),
    plot_data$climatology,
    plot_data$current
  )
  all_values <- all_values[is.finite(all_values)]
  
  if (length(all_values) == 0) {
    stop("No finite monthly fieldmean values are available for plotting.")
  }
  
  y_min <- min(0, min(all_values, na.rm = TRUE))
  y_max <- max(all_values, na.rm = TRUE)
  limit_y <- signif(ceiling(y_max + abs(y_max) / 15), digits = 2)
  if (limit_y <= y_min) {
    limit_y <- y_min + 1
  }
  
  draw_one_frame <- function(n_current) {
    graphics::par(
      cex = 1.2,
      oma = c(0, 0, 0, 0),
      mar = c(2.2, 4, 3.5, 2),
      mgp = c(3, 1, 0)
    )
    
    current_to_plot <- plot_data$current
    if (n_current < length(current_to_plot)) {
      current_to_plot[(n_current + 1):length(current_to_plot)] <- NA_real_
    }
    
    set_time_locale(language)
    tryCatch(
      graphics::plot(
        x_dates,
        plot_data$climatology,
        type = "l",
        lwd = 5,
        xlab = "",
        ylab = ylab_text,
        main = plot_title,
        cex.lab = 1.2,
        ylim = c(y_min, limit_y),
        cex.main = 1.2
      ),
      finally = {set_time_locale("")}
    )
    
    year_max_end_value <- -Inf
    year_max_end <- NULL
    year_min_end_value <- Inf
    year_min_end <- NULL
    year_min_pos_value <- Inf
    year_min_pos_index <- max(1, round(length(x_dates) * 0.9))
    
    for (i in seq_len(nrow(plot_data$ensemble))) {
      dat <- as.numeric(plot_data$ensemble[i, ])
      graphics::lines(x_dates, dat, col = "grey", lwd = 2)
      
      final_value <- dat[length(dat)]
      if (!is.na(final_value) && final_value > year_max_end_value) {
        year_max_end_value <- final_value
        year_max_end <- rownames(plot_data$ensemble)[i]
      }
      if (!is.na(final_value) && final_value < year_min_end_value) {
        year_min_end_value <- final_value
        year_min_end <- rownames(plot_data$ensemble)[i]
      }
      if (!is.na(dat[year_min_pos_index]) && dat[year_min_pos_index] < year_min_pos_value) {
        year_min_pos_value <- dat[year_min_pos_index]
      }
    }
    
    graphics::lines(x_dates, plot_data$climatology, col = "black", lwd = 5)
    graphics::lines(x_dates, current_to_plot, col = "red", lwd = 5)
    
    if (output_format == "animation") {
      current_label <- format(x_dates[n_current], ifelse(language == "deu", "%m.%Y", "%Y-%m"))
    } else {
      current_label <- as.character(plot_data$current_year)
    }
    
    graphics::legend(
      "topleft",
      legend = c(legend_text, current_label),
      col = c("black", "red"),
      lwd = 4,
      cex = 1.0
    )
    
    graphics::par(usr = c(0, 1, 0, 1))
    graphics::rasterImage(
      logo_cmsaf,
      logo.x - (logo.size / 2),
      logo.y - (AR * logo.size / 2),
      logo.x + (logo.size / 2),
      logo.y + (AR * logo.size / 2),
      interpolate = TRUE
    )
    
    if (dwd_logo) {
      graphics::rasterImage(
        logo_dwd,
        logo.x2 - (logo.size2 / 2),
        logo.y2 - (AR2 * logo.size2 / 2),
        logo.x2 + (logo.size2 / 2),
        logo.y2 + (AR2 * logo.size2 / 2),
        interpolate = TRUE
      )
    }
    
    if (show_extreme_climate_years && plot_data$accumulate) {
      if (!is.null(year_max_end) && is.finite(year_max_end_value)) {
        graphics::text(x = 0.9, y = year_max_end_value / limit_y, labels = year_max_end, col = "darkgrey")
      }
      if (!is.null(year_min_end) && is.finite(year_min_pos_value)) {
        graphics::text(x = 0.9, y = year_min_pos_value / limit_y, labels = year_min_end, pos = 1, col = "darkgrey")
      }
    }
  }
  
  if (output_format == "graphic") {
    picout <- file.path(out_dir, outfile_name)
    grDevices::png(
      picout,
      width = pic.width,
      height = pic.height,
      units = "px",
      pointsize = 12
    )
    draw_one_frame(length(x_dates))
    grDevices::dev.off()
    
    if (verbose) {
      message(paste("Image has been created at", normalizePath(picout)))
    }
  }
  
  if (output_format == "animation") {
    vidout <- file.path(out_dir, outfile_name)
    nr_frozen_frames <- ifelse(freeze_animation, 100, 0)
    
    if (verbose) {
      pb <- progress::progress_bar$new(
        format = "Creating monthly animation [:bar] :percent eta: :eta",
        total = length(x_dates) + nr_frozen_frames,
        clear = TRUE,
        callback = function(x) {message("Created monthly animation")}
      )
    }
    
    animation::saveVideo(
      video.name = vidout,
      img.name = "Rplot",
      other.opts = "-pix_fmt yuv420p -loglevel warning",
      interval = animation_pace,
      ani.height = pic.height,
      ani.width = pic.width,
      units = "px",
      autobrowse = FALSE,
      verbose = FALSE,
      expr = {
        for (i in seq_along(x_dates)) {
          draw_one_frame(i)
          if (verbose) {
            pb$tick()
          }
        }
        if (freeze_animation) {
          for (i in seq_len(nr_frozen_frames)) {
            draw_one_frame(length(x_dates))
            if (verbose) {
              pb$tick()
            }
          }
        }
      }
    )
  }
  
  if (verbose) {
    final_index <- length(plot_data$current)
    ensemble_final <- plot_data$ensemble[, final_index]
    year_max <- names(which.max(ensemble_final))
    year_min <- names(which.min(ensemble_final))
    
    titles <- c("Analyzed year", "Climatology", "Maximum valued year", "Minimum valued year")
    standout_years <- c(
      plot_data$current_year,
      paste(climate_year_start, climate_year_end, sep = " - "),
      year_max,
      year_min
    )
    standout_values <- c(
      plot_data$current[final_index],
      plot_data$climatology[final_index],
      ensemble_final[year_max],
      ensemble_final[year_min]
    )
    
    final_values <- data.frame(
      title = titles,
      years = standout_years,
      value = as.numeric(standout_values)
    )
    
    ranking_values <- ranking(
      out_dir = out_dir,
      var = variable,
      country_code = country_code,
      climate_year_start = min(plot_data$comparison_years),
      climate_year_end = max(plot_data$comparison_years),
      doy = final_index,
      years = plot_data$comparison_years,
      values = as.numeric(ensemble_final)
    )
    
    calc.parameters.monitor.climate(final_values, ranking_values)
    
    if (plot_data$accumulate) {
      message("Significant values at the final month:")
      print(final_values)
    }
  }
}