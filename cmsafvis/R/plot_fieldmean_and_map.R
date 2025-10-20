# This should animate the Sunshine Duration Analysis of the year 2018
plot_fieldmean_and_map <- function(variable,
                                   country_code,
                                   climate_year_start,
                                   climate_year_end,
                                   show_extreme_climate_years,
                                   climatology_until_eoy,
                                   infile,
                                   infile2,
                                   infile_map,
                                   temp_dir,
                                   out_dir,
                                   start_date,
                                   end_date,
                                   language,
                                   animation_pace,
                                   output_format,
                                   min_value,
                                   max_value,
                                   color_pal = 1,
                                   nbreaks = NULL,
                                   freeze_animation,
                                   outfile_name,
                                   adjustAccumulation,
                                   states,
                                   dwd_logo,
                                   verbose) {
  # Make sure that any user settings are reset when the function exits
  # This is a requirement by CRAN
  oldpar <- graphics::par(no.readonly = TRUE)
  # Warning: In graphics::par(oldpar) : par(new) ohne Plot aufgerufen
  on.exit(suppressWarnings(graphics::par(oldpar)))

  # Dimensions of pic
  pic.width <- 900
  pic.height <- 500

  # Compute some parameters from arguments
  start_doy <- as.numeric(format(start_date, format = "%j"))
  finish_doy <- as.numeric(format(end_date, format = "%j"))
  if (is_leap_year(as.numeric(format(as.Date(start_date),"%Y")))) {
    start_doy <- ifelse(start_doy >= 60, start_doy - 1, start_doy)
  }
  if (is_leap_year(as.numeric(format(as.Date(end_date),"%Y")))) {
    finish_doy <- ifelse(finish_doy >= 60, finish_doy - 1, finish_doy)
  }
  country_name <- get_country_name(country_code, language = language)
  country_name_en <- get_country_name(country_code)

  # CMSAF logo
  # Size and location of the logo
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
  AR <- dims[1] / dims[2]  *  5 / 5.5


  # DWD Logo
  if (dwd_logo){
    # Size and location of the logo
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
    AR2 <- dims2[1] / dims2[2]  * 5 / 5.5
  }
  
  # line thickness
  lwd1 <- 5
  lwd2 <- 2

  if (states) {
    check_state_dependencies()

    statesHigh <- rnaturalearth::ne_states(country_name_en)
    state_lines <- methods::as(statesHigh, "SpatialLines")
  }

  countriesHigh <- numeric(0)  # Hack to prevent IDE warning in second next line (see https://stackoverflow.com/questions/62091444/how-to-load-data-from-other-package-in-my-package)
  utils::data("countriesHigh", package = "rworldxtra", envir = environment())
  world_countries <- methods::as(countriesHigh, "SpatialLines")

  # read in netcdf data
  nc_map <- ncdf4::nc_open(infile_map)
    field_source <- ncdf4::ncvar_get(nc_map, variable)
    lon <- ncdf4::ncvar_get(nc_map, "lon")
    lat <- ncdf4::ncvar_get(nc_map, "lat")
    var_unit  <- ncdf4::ncatt_get(nc_map, variable, "units")$value
  ncdf4::nc_close(nc_map)

  duration <- length(field_source[1, 1, ])

  # set language
  data_year <- format(end_date, "%Y")
  map_title <- get_title(
    variable = variable,
    language = language,
    plot_type = "anomaly_map"
  )
  map_title <- paste0(map_title, " (", country_name, ")")
  plot_title <- get_title(
    variable = variable,
    language = language,
    year = data_year
  )
  units_text <- paste0(
    "[",
    get_unit(variable = variable, language = language),
    "]"
  )
  ylab_text <- get_axis_label(variable, language)
  
  # Check if the unit should be replaced
  if (grepl("(neu)", var_unit) || grepl("(new)", var_unit)) {
    remove_substrings <- c("\\(neu\\)", "\\(new\\)")
    # Remove (neu) or (new)
    for (substring in remove_substrings) {
      var_unit <- gsub(substring, "", var_unit)
    }
    # Trim any extra whitespace that may be left
    var_unit <- trimws(var_unit)
    
    # Replace the old substring with the new substring
    pattern <- "\\[.*?\\]"
    # Replace the content within square brackets with the new content
    ylab_text <- gsub(pattern, paste0("[", var_unit, "]"), ylab_text)
  } 

  date_format_string <- ifelse(language == "deu", "%d.%m.%Y", "%Y-%m-%d")

  legend_text <- paste0(
    get_climatology_word(language),
    " (",
    climate_year_start,
    "-",
    climate_year_end,
    ")"
  )

  nx <- length(lon)
  ny <- length(lat)

  data_start <- 1
  data_stop <- finish_doy - start_doy + 1

  field_to_plot <- field_source[, , data_stop]
  field_for_setup <- field_source[, , data_start:data_stop]

  # define colors
  bwr_col <-
    grDevices::colorRampPalette(
      c(
        "lightgreen",
        "green",
        "orange",
        "orangered1",
        "red",
        "magenta"
      )
    )

  duration <- length(field_for_setup[1, 1, ])
  limit_lower <- min(field_for_setup[, , duration], na.rm = TRUE)
  limit_lower_z <- signif(floor(limit_lower - 50), digits = 1)

  limit_upper <- max(field_for_setup, na.rm = TRUE)
  limit_upper_z <- signif(ceiling(limit_upper + 50), digits = 2)


  #select the range of the color bar
  min_value_default <- 0
  max_value_default <- limit_upper_z
  freq.ticks <- 1

  if (abs(max(field_for_setup[, , duration], na.rm = TRUE)) < abs(min(field_for_setup[, , duration], na.rm = TRUE))) {
    limit_lower <- min(field_for_setup[, , duration], na.rm = TRUE)
    limit_lower_z <- signif(floor(limit_lower - 50), digits = 1)
    min_value_default <- limit_lower_z
    max_value_default <- (-1) * min_value_default
  }
  else{
    limit_upper <- max(field_for_setup, na.rm = TRUE)
    limit_upper_z <- signif(ceiling(limit_upper + 50), digits = 1)
    max_value_default <- limit_upper_z
    min_value_default <- (-1) * max_value_default
  }
  bwr_col <-
    grDevices::colorRampPalette(c(
      "darkblue",
      "blue",
      "lightskyblue",
      "orangered1",
      "red",
      "red4"
    ))
	
    if (color_pal == 2){bwr_col <- 
      grDevices::colorRampPalette(c("#474747", "#7a7a7a", "#a8a8a8", "#cdcdcd",
                                    "#e2e2e2", "#f9f9f9", "#fdf3db", "#fee8b2",
                                    "#fedf8c", "#fed66c", "#fdcf45", "#fac631"))
    }
    
    if (color_pal == 3){bwr_col <- 
      grDevices::colorRampPalette(c("#5c3209", "#96560e", "#b27028", "#d1a759",
                                    "#dfc07a", "#f5e5bf", "#fefefe","#b0dfda", 
                                    "#6fc0b8", "#389c94", "#078470", "#045f5a", 
                                    "#0f3c33"))
    }

  # Overwrite color arguments with default values if non are specified
  if (is.null(min_value)) {
    min_value <- min_value_default
  }

  if (is.null(max_value)) {
    max_value <- max_value_default
  }

  coldiff <- max_value - min_value

  ####################
  
  palettes <- GetPaletteConfig(gui = FALSE)
  colors <- getColors(PAL = color_pal, palettes = palettes, num_brk = ncol - 1, reverse = FALSE)

  # Set the colors and the color bar
  k <- 8:12
  diffs <- min_value + k*ceiling(coldiff/10) - max_value
  seq_max <- max_value + min(diffs[diffs >= 0])
  # Set the colors and the color bar
  if (is.null(nbreaks)) {
    col.breaks <- seq(min_value, seq_max, by = ceiling(coldiff/10))
    ncol <- length(col.breaks)
  } else {
    col.breaks <- seq(min_value, max_value, length.out = nbreaks)
    col.breaks <- round(col.breaks, digits = 0)
    ncol <- nbreaks
  }

  at.ticks <- seq(1, ncol, freq.ticks)
  names.ticks <- col.breaks[at.ticks]
  zlim <- c(1, ncol)

  ncol <- max(2, ncol)

  colors <- bwr_col(ncol - 1)

  # read in netcdf data
  # read in annual mean
  nc_mean <- ncdf4::nc_open(infile)
  var_clima <- ncdf4::ncvar_get(nc_mean, variable)
  ncdf4::nc_close(nc_mean)

  # read in current data
  nc_current <- ncdf4::nc_open(infile2)
  var_current <- ncdf4::ncvar_get(nc_current, variable)
  ncdf4::nc_close(nc_current)

  dates <- seq(
    as.Date(paste(data_year, 1, 1, sep = "-")),
    as.Date(paste(data_year, 12, 31, sep = "-")),
    by = "day"
  )
  # Remove Feb 29 in leap year.
  if (length(dates) == 366) {
    dates <- dates[-60]
  }

  # Preparing data
  if (start_doy > 1 || !climatology_until_eoy) {
    plot_stop <- finish_doy
  } else {
    plot_stop <- 365
  }

  # --- Safe slicing (identical logic to plot_fieldmean) ---
  duration <- finish_doy - start_doy + 1
  climate_duration <- duration
  
  len_curr <- length(var_current)
  len_clim <- length(var_clima)
  
  # Current: if already windowed (length equals duration), do NOT slice again.
  if (len_curr == duration) {
    var_current_to_plot <- as.numeric(var_current)
  } else {
    s_idx <- max(1, start_doy)
    e_idx <- min(finish_doy, len_curr)
    var_current_to_plot <- as.numeric(var_current[s_idx:e_idx])
  }
  
  # Climatology: usually full year, but still clamp to be safe.
  s_idx_clim <- max(1, start_doy)
  e_idx_clim <- min(finish_doy, len_clim)
  var_climate_to_plot <- as.numeric(var_clima[s_idx_clim:e_idx_clim])
  
  # De-accumulate relative to in-window start if requested.
  if (start_doy > 1 && adjustAccumulation) {
    off_curr <- var_current_to_plot[1]; if (is.na(off_curr)) off_curr <- 0
    off_clim <- var_climate_to_plot[1]; if (is.na(off_clim)) off_clim <- 0
    var_current_to_plot  <- var_current_to_plot  - off_curr
    var_climate_to_plot  <- var_climate_to_plot - off_clim
  }
  
  # Convenience: build x-index for current series length (avoid using finish_doy directly)
  idx_seq_curr <- seq.int(from = start_doy, length.out = length(var_current_to_plot))
  idx_seq_clim <- seq.int(from = start_doy, length.out = length(var_climate_to_plot))
  plot_length   <- min(length(idx_seq_curr), length(idx_seq_clim))
  
  ############################################################################################
  # plot graphic


  if (output_format == "graphic") {

    limit <- -Inf

    for (climate_year in seq(from = climate_year_start, to = climate_year_end, by = 1)) {
      input <- add_ncdf_ext(construct_filename(variable,
                                               climate_year,
                                               country_code,
                                               "fldmean"))
      input <- file.path(temp_dir, input)
      nc_fieldmean <- ncdf4::nc_open(input)
      var_ensemble <- ncdf4::ncvar_get(nc_fieldmean, variable)
      ncdf4::nc_close(nc_fieldmean)

      # Cut the data
      var_ensemble <- var_ensemble[start_doy:finish_doy]

      # Remove accumulation of previous dates
      if (start_doy > 1 && adjustAccumulation) {
        var_ensemble <- var_ensemble - rep(var_ensemble[1], climate_duration)
      }

      assign(paste0(variable, "_acc_", climate_year), var_ensemble)

      # Compute y limit dependent on accumulation process
      if (adjustAccumulation) {
        limit <- max(limit, var_ensemble[length(var_ensemble)])
      } else {
        limit <- max(limit, max(var_ensemble, na.rm = TRUE))
      }
    }

    limit_y <- signif(ceiling(limit + limit/15), digits = 2)

    picpath <- out_dir
    picout <- file.path(picpath, outfile_name)

    grDevices::png(
      picout,
      width = pic.width,
      height = pic.height,
      units = "px",
      pointsize = 12
    )

    graphics::par(mfrow = c(1, 2))

    # plot_length <- length(start_doy:plot_stop)

    graphics::par(
      cex = 1.2,
      oma = c(0, 0, 0, 0),
      mar = c(2.2, 4, 3.5, 2),
      mgp = c(3, 1, 0)
    )

    set_time_locale(language)
    tryCatch(
      graphics::plot(
        dates[start_doy:plot_stop],
        var_climate_to_plot,
        type = "l",
        lwd = lwd1,
        xlab = "",
        ylab = ylab_text,
        main = plot_title,
        cex.lab = 1.2,
        ylim = c(0, limit_y),
        cex.main = 1.2
      ),
      finally = {set_time_locale("")}
    )

    # Variables for adding max/min end value to plot
    yearMaxEndValue <- -Inf
    yearMaxEnd <- NULL
    yearMinEndValue <- Inf
    yearMinEnd <- NULL

    # For the min year y position take the min value at 90%
    yearMinPosIndex <- round(plot_length*0.9)
    yearMinPosValue <- Inf

    # start loop to plot all years of climatology
    for (climate_year in seq(from = climate_year_start, to = climate_year_end, by = 1)) {
      dat <- get(paste0(variable, "_acc_", climate_year))

      set_time_locale(language)
      tryCatch(
        graphics::lines(
          dates[start_doy:plot_stop],
          dat,
          col = "grey",
          lwd = lwd2
        ),
        finally = {set_time_locale("")}
      )

      # Determine min and max end values
      if (dat[plot_length] > yearMaxEndValue) {
        yearMaxEndValue <- dat[plot_length]
        yearMaxEnd <- climate_year
      }
      if (dat[plot_length] < yearMinEndValue) {
        yearMinEndValue <- dat[plot_length]
        yearMinEnd <- climate_year
      }
      if (dat[yearMinPosIndex] < yearMinPosValue) {
        yearMinPosValue <- dat[yearMinPosIndex]
      }
    }

    set_time_locale(language)
    tryCatch({
      graphics::lines(
        dates[start_doy:plot_stop],
        var_climate_to_plot,
        col = "black",
        lwd = lwd1
      )
      graphics::lines(
        dates[idx_seq_curr],
        var_current_to_plot,
        col = "red",
        lwd =  lwd1
      )},
      finally = {set_time_locale("")}
    )

    graphics::legend(
      "topleft",
      legend = c(legend_text, data_year),
      col = c("black", "red"),
      lwd = 4,
      cex = 1.0
    )

    graphics::par(usr = c(0, 1, 0, 1))
    size <- logo.size
    x <- logo.x
    y <- logo.y
    graphics::rasterImage(logo_cmsaf,
                          x - (size / 2),
                          y - (AR * size / 2),
                          x + (size / 2),
                          y + (AR * size / 2),
                          interpolate = TRUE)

    if (dwd_logo){
      size <- logo.size2
      x <- logo.x2
      y <- logo.y2
      graphics::rasterImage(logo_dwd,
                          x - (size / 2),
                          y - (AR2 * size / 2),
                          x + (size / 2),
                          y + (AR2 * size / 2),
                          interpolate = TRUE)
    }

    if (show_extreme_climate_years && adjustAccumulation) {
      y_val_max <- yearMaxEndValue/limit_y
      y_val_min <- yearMinPosValue/limit_y

      graphics::text(x = 0.9, y = y_val_max, labels = yearMaxEnd, col = "darkgrey")
      graphics::text(x = 0.9, y = y_val_min, labels = yearMinEnd, pos = 1,  col = "darkgrey")
    }

    ################################################################################################################
    # plot var Anomaly map

    # To plot a meaningful color bar the data need to be transfered to numbers from 1.1 to nticks + 0.1
    field.plot_freeze <- matrix(ncol = ny, nrow = nx)
    for (l in seq(1, ncol - 1)) {
      idx <- which(field_to_plot >= col.breaks[l] &
                     field_to_plot <= col.breaks[l + 1],
                   arr.ind = TRUE)
      field.plot_freeze[idx] <- l + 0.1
    }

    lg_mar <- 4.5 + floor(log10(max(abs(names.ticks))))

    # Start animation
    graphics::par(
      cex = 1.3,
      mgp = c(2, 1, 0),
      mar = c(2, 1, 3, 4.5) + 0.1
    )

    fields::image.plot(
      lon,
      lat,
      field.plot_freeze,
      xlab = "",
      ylab = "",
      col = colors,
      xaxt = 'n',
      yaxt = 'n',
      main = map_title,
      zlim = zlim,
      breaks = c(1:ncol),
      nlevel = ncol - 1,
      legend.mar = lg_mar,
      axis.args = list(at = at.ticks, labels = names.ticks),
      cex.main = 1.1
    )

    # State and/or country borders
    if (states) {
      raster::plot(state_lines, add = TRUE, lwd = 0.75)
      raster::plot(world_countries, add = TRUE, lwd = 1.5)
    } else {
      raster::plot(world_countries, add = TRUE, lwd = 1)
    }

    graphics::par(usr = c(0, 1, 0, 1))
    size <- logo.size
    x <- logo.x
    y <- logo.y
    graphics::rasterImage(logo_cmsaf,
                          x - (size / 2),
                          y - (AR * size / 2),
                          x + (size / 2),
                          y + (AR * size / 2),
                          interpolate = TRUE)

    if (dwd_logo){
      size <- logo.size2
      x <- logo.x2
      y <- logo.y2
      graphics::rasterImage(logo_dwd,
                          x - (size / 2),
                          y - (AR2 * size / 2),
                          x + (size / 2),
                          y + (AR2 * size / 2),
                          interpolate = TRUE)
    }

    if (start_doy == 1 && climatology_until_eoy) {
      box_dates <- format(dates[finish_doy], format = date_format_string)
    } else {
      box_dates <- paste(
        format(dates[start_doy], format = date_format_string),
        "-",
        format(dates[finish_doy], format = date_format_string)
      )
    }

    graphics::legend("topright", legend = box_dates, cex = 1.12)

    graphics::mtext(
      paste(units_text),
      side = 1,
      line = -2.9,
      at = 0.958,
      outer = TRUE,
      cex = 1.3
    ) # default: at = 0.958

    grDevices::dev.off()

    if (verbose) {
      message(paste("Image has been created at", normalizePath(picout)))
    }
  }
  ######################################################################################################

  if (output_format == "animation") {
    viddir <- out_dir
    vidout <- file.path(viddir, outfile_name)

    nr_frozen_frames <- 100

    if (verbose) {
      pb <- progress::progress_bar$new(
        format = "Creating animation [:bar] :percent eta: :eta",
        total = duration + ifelse(freeze_animation, nr_frozen_frames, 0),
        clear = TRUE,
        callback = function(x) {message("Created animation")}
      )
    }

    animation::saveVideo(
      video.name = vidout,
      img.name = "Rplot",
      other.opts = "-pix_fmt yuv420p -loglevel warning",  # See 'other.opts' of ?animation::saveVideo
      interval = animation_pace,
      ani.height = pic.height,
      ani.width = pic.width,
      units = "px",
      autobrowse = FALSE,  # means that the output file isn't opened automatically
      verbose = FALSE,
      expr = {
        for (i in 1:duration) {
          # plot var graphic
          limit <- -Inf

          # start loop over all years and read in netcdf data for annual sum of var
          for (climate_year in seq(from = climate_year_start,
                                   to = climate_year_end,
                                   by = 1)) {
            input <- add_ncdf_ext(construct_filename(variable,
                                                     climate_year,
                                                     country_code,
                                                     "fldmean"))
            input <- file.path(temp_dir, input)
            nc_fieldmean <- ncdf4::nc_open(input)
            var_ensemble <- ncdf4::ncvar_get(nc_fieldmean, variable)
            ncdf4::nc_close(nc_fieldmean)

            # Cut the data
            var_ensemble <- var_ensemble[start_doy:finish_doy]

            # Remove accumulation of previous dates
            if (start_doy > 1 && adjustAccumulation) {
              var_ensemble <- var_ensemble - rep(var_ensemble[1], climate_duration)
            }

            assign(paste0(variable, "_acc_", climate_year), var_ensemble)

            # Compute y limit dependent on accumulation process
            if (adjustAccumulation) {
              limit <- max(limit, var_ensemble[length(var_ensemble)])
            } else {
              limit <- max(limit, max(var_ensemble, na.rm = TRUE))
            }
          }

          limit_y <- signif(ceiling(limit + limit/15), digits = 2)

          # plot_length <- plot_stop - start_doy + 1
          corr_date <- start_doy + i - 1

          graphics::par(mfrow = c(1, 2))

          graphics::par(
            cex = 1.2,
            oma = c(0, 0, 0, 0),
            mar = c(2.2, 4, 3.5, 2),
            mgp = c(3, 1, 0)
          )

          set_time_locale(language)
          tryCatch(
            graphics::plot(
              dates[start_doy:plot_stop],
              var_climate_to_plot,
              type = "l",
              lwd = lwd1,
              xlab = "",
              ylab = ylab_text,
              main = plot_title,
              cex.lab = 1.2,
              ylim = c(0, limit_y),
              cex.main = 1.2
            ),
            finally = {set_time_locale("")}
          )

          # Variables for adding max/min end value to plot
          yearMaxEndValue <- -Inf
          yearMaxEnd <- NULL
          yearMinEndValue <- Inf
          yearMinEnd <- NULL

          # For the min year y position take the min value at 90%
          yearMinPosIndex <- round(plot_length*0.9)
          yearMinPosValue <- Inf

          # start loop to plot all years of climatology
          for (climate_year in seq(from = climate_year_start,
                                   to = climate_year_end,
                                   by = 1)) {
            dat <- get(paste0(variable, "_acc_", climate_year))

            set_time_locale(language)
            tryCatch(
              graphics::lines(
                dates[start_doy:plot_stop],
                dat,
                col = "grey",
                lwd = lwd2
              ),
              finally = {set_time_locale("")}
            )

            # Determine min and max end values
            if (dat[plot_length] > yearMaxEndValue) {
              yearMaxEndValue <- dat[plot_length]
              yearMaxEnd <- climate_year
            }
            if (dat[plot_length] < yearMinEndValue) {
              yearMinEndValue <- dat[plot_length]
              yearMinEnd <- climate_year
            }
            if (dat[yearMinPosIndex] < yearMinPosValue) {
              yearMinPosValue <- dat[yearMinPosIndex]
            }
          }

          # ensure we do not run past the available current series
          i_curr <- min(i, length(var_current_to_plot))
          var_current_seq <- var_current_to_plot[1:i_curr]
          
          corr_date_idx <- idx_seq_curr[i_curr]  # last plotted date index for "current"
          
          set_time_locale(language)
          tryCatch({
            graphics::lines(
              dates[start_doy:plot_stop],
              var_climate_to_plot,
              col = "black",
              lwd = lwd1
            )
            graphics::lines(
              dates[start_doy:corr_date_idx],
              var_current_seq,
              col = "red",
              lwd = lwd1
            )},
            finally = {set_time_locale("")}
          )

          gaa <- format(dates[corr_date], format = date_format_string)
          graphics::legend(
            "topleft",
            legend = c(legend_text, gaa),
            col = c("black", "red"),
            lwd = 4,
            cex = 1.0
          )


          graphics::par(usr = c(0, 1, 0, 1))
          size <- logo.size
          x <- logo.x
          y <- logo.y
          graphics::rasterImage(logo_cmsaf,
                                x - (size / 2),
                                y - (AR * size / 2),
                                x + (size / 2),
                                y + (AR * size / 2),
                                interpolate = TRUE)

          if (dwd_logo){
            size <- logo.size2
            x <- logo.x2
            y <- logo.y2
            graphics::rasterImage(logo_dwd,
                                x - (size / 2),
                                y - (AR2 * size / 2),
                                x + (size / 2),
                                y + (AR2 * size / 2),
                                interpolate = TRUE)
          }

          if (show_extreme_climate_years && adjustAccumulation) {
            y_val_max <- yearMaxEndValue/limit_y
            y_val_min <- yearMinPosValue/limit_y

            graphics::text(x = 0.9, y = y_val_max, labels = yearMaxEnd, col = "darkgrey")
            graphics::text(x = 0.9, y = y_val_min, labels = yearMinEnd, pos = 1,  col = "darkgrey")
          }

          ################################################################################################################
          # plot var_2018 Anomaly map
          field_to_animate <- field_for_setup[, , i]

          # To plot a meaningful color bar the data need to be transfered to numbers from 1.1 to nticks + 0.1
          field.plot_freeze <- matrix(ncol = ny, nrow = nx)
          for (l in seq(1, ncol - 1)) {
            idx <-
              which(((field_to_animate >= col.breaks[l]) &
                       (field_to_animate <= col.breaks[l + 1])
              ), arr.ind = TRUE)
            field.plot_freeze[idx] <- l + 0.1
          }


          # Start animation
          if (verbose) {
            pb$tick()
          }


          lg_mar <- 4.5 + floor(log10(max(abs(names.ticks))))

          graphics::par(
            cex = 1.3,
            mgp = c(2, 1, 0),
            mar = c(2, 1, 3, 4.5) + 0.1
          )

          fields::image.plot(
            lon,
            lat,
            field.plot_freeze,
            xlab = "",
            ylab = "",
            col = colors,
            xaxt = 'n',
            yaxt = 'n',
            main = map_title,
            zlim = zlim,
            breaks = c(1:ncol),
            nlevel = ncol - 1,
            legend.mar = lg_mar,
            axis.args = list(at = at.ticks, labels = names.ticks),
            cex.main = 1.1
          )

          # State and/or country borders
          if (states) {
            raster::plot(state_lines, add = TRUE, lwd = 0.75)
            raster::plot(world_countries, add = TRUE, lwd = 1.5)
          } else {
            raster::plot(world_countries, add = TRUE, lwd = 1)
          }

          graphics::par(usr = c(0, 1, 0, 1))
          size <- logo.size
          x <- logo.x
          y <- logo.y
          graphics::rasterImage(logo_cmsaf,
                                x - (size / 2),
                                y - (AR * size / 2),
                                x + (size / 2),
                                y + (AR * size / 2),
                                interpolate = TRUE)

          if (dwd_logo){
            size <- logo.size2
            x <- logo.x2
            y <- logo.y2
            graphics::rasterImage(logo_dwd,
                                x - (size / 2),
                                y - (AR2 * size / 2),
                                x + (size / 2),
                                y + (AR2 * size / 2),
                                interpolate = TRUE)
          }

          if (start_doy == 1 && climatology_until_eoy) {
            box_dates <- format(dates[i], format = date_format_string)
          } else {
            box_dates <- paste(
              format(dates[start_doy], format = date_format_string),
              "-",
              format(dates[corr_date], format = date_format_string)
            )
          }

          graphics::legend("topright",
                           legend = box_dates,
                           cex = 1.12)

          graphics::mtext(
            paste(units_text),
            side = 1,
            line = -2.9,
            at = 0.958,
            outer = TRUE,
            cex = 1.3
          ) # default: at = 0.958

        } #end of "daily" loop

        ###############################################################################################################

        if (freeze_animation) {
          for (k in 1:nr_frozen_frames) {
            # plot var_graphic
            limit <- -Inf

            # start loop over all years and read in netcdf data for annual sum of var
            for (climate_year in seq(from = climate_year_start,
                                     to = climate_year_end,
                                     by = 1)) {
              input <- add_ncdf_ext(construct_filename(variable,
                                                       climate_year,
                                                       country_code,
                                                       "fldmean"))
              input <- file.path(temp_dir, input)

              nc_fieldmean <- ncdf4::nc_open(input)
              var_ensemble <- ncdf4::ncvar_get(nc_fieldmean, variable)
              ncdf4::nc_close(nc_fieldmean)

              # Cut the data
              var_ensemble <- var_ensemble[start_doy:finish_doy]

              # Remove accumulation of previous dates
              if (start_doy > 1 && adjustAccumulation) {
                var_ensemble <- var_ensemble - rep(var_ensemble[1], climate_duration)
              }

              assign(paste0(variable, "_acc_", climate_year), var_ensemble)

              # Compute y limit dependent on accumulation process
              if (adjustAccumulation) {
                limit <- max(limit, var_ensemble[length(var_ensemble)])
              } else {
                limit <- max(limit, max(var_ensemble, na.rm = TRUE))
              }
            }

            limit_y <- signif(ceiling(limit + limit/15), digits = 2)

            # plot_length <- plot_stop - start_doy

            graphics::par(mfrow = c(1, 2))

            graphics::par(
              cex = 1.2,
              oma = c(0, 0, 0, 0),
              mar = c(2.2, 4, 3.5, 2),
              mgp = c(3, 1, 0)
            )

            set_time_locale(language)
            tryCatch(
              graphics::plot(
                dates[start_doy:plot_stop],
                var_climate_to_plot,
                type = "l",
                lwd = lwd1,
                xlab = "",
                ylab = ylab_text,
                main = plot_title,
                cex.lab = 1.2,
                ylim = c(0, limit_y),
                cex.main = 1.2
              ),
              finally = {set_time_locale("")}
            )

            # start loop to plot all years of climatology
            for (climate_year in seq(from = climate_year_start,
                                     to = climate_year_end,
                                     by = 1)) {
              dat <- get(paste0(variable, "_acc_", climate_year))

              set_time_locale(language)
              tryCatch(
                graphics::lines(
                  dates[start_doy:plot_stop],
                  dat,
                  col = "grey",
                  lwd = lwd2
                ),
                finally = {set_time_locale("")}
              )
            }

            set_time_locale(language)
            tryCatch({
              graphics::lines(
                dates[start_doy:plot_stop],
                var_climate_to_plot,
                col = "black",
                lwd = lwd1
              )
              graphics::lines(
                dates[start_doy:corr_date],
                var_current_to_plot,
                col = "red",
                lwd = lwd1
              )},
              finally = {set_time_locale("")}
            )

            graphics::legend(
              "topleft",
              legend = c(legend_text, data_year),
              col = c("black", "red"),
              lwd = 4,
              cex = 1.0
            )


            graphics::par(usr = c(0, 1, 0, 1))
            size <- logo.size
            x <- logo.x
            y <- logo.y
            graphics::rasterImage(logo_cmsaf,
                                  x - (size / 2),
                                  y - (AR * size / 2),
                                  x + (size / 2),
                                  y + (AR * size / 2),
                                  interpolate = TRUE)

            if (dwd_logo){
              size <- logo.size2
              x <- logo.x2
              y <- logo.y2
              graphics::rasterImage(
                logo_dwd,
                x - (size / 2),
                y - (AR2 * size / 2),
                x + (size / 2),
                y + (AR2 * size / 2),
                interpolate = TRUE
              )
            }

            if (show_extreme_climate_years && adjustAccumulation) {
              y_val_max <- yearMaxEndValue/limit_y
              y_val_min <- yearMinPosValue/limit_y

              graphics::text(x = 0.9, y = y_val_max, labels = yearMaxEnd, col = "darkgrey")
              graphics::text(x = 0.9, y = y_val_min, labels = yearMinEnd, pos = 1,  col = "darkgrey")
            }

            ################################################################################################################

            field_to_animate <- field_for_setup[, , duration]

            # To plot a meaningful color bar the data need to be transfered to numbers from 1.1 to nticks + 0.1
            field.plot_freeze <- matrix(ncol = ny, nrow = nx)
            for (l in seq(1, ncol - 1)) {
              idx <- which(
                field_to_animate >= col.breaks[l] &
                  field_to_animate <= col.breaks[l + 1],
                arr.ind = TRUE
              )
              field.plot_freeze[idx] <- l + 0.1
            }


            # Start animation
            if (verbose) {
              pb$tick()
            }


            lg_mar <- 4.5 + floor(log10(max(abs(names.ticks))))

            graphics::par(
              cex = 1.3,
              mgp = c(2, 1, 0),
              mar = c(2, 1, 3, 4.5) + 0.1
            )

            fields::image.plot(
              lon,
              lat,
              field.plot_freeze,
              xlab = "",
              ylab = "",
              col = colors,
              xaxt = 'n',
              yaxt = 'n',
              main = map_title,
              zlim = zlim,
              breaks = c(1:ncol),
              nlevel = ncol - 1,
              legend.mar = lg_mar,
              axis.args = list(at = at.ticks, labels = names.ticks),
              cex.main = 1.1
            )

            # State and/or country borders
            if (states) {
              raster::plot(state_lines, add = TRUE, lwd = 0.75)
              raster::plot(world_countries, add = TRUE, lwd = 1.5)
            } else {
              raster::plot(world_countries, add = TRUE, lwd = 1)
            }

            graphics::par(usr = c(0, 1, 0, 1))
            size <- logo.size
            x <- logo.x
            y <- logo.y
            graphics::rasterImage(logo_cmsaf,
                                  x - (size / 2),
                                  y - (AR * size / 2),
                                  x + (size / 2),
                                  y + (AR * size / 2),
                                  interpolate = TRUE)

            if (dwd_logo){
              size <- logo.size2
              x <- logo.x2
              y <- logo.y2
              graphics::rasterImage(
                logo_dwd,
                x - (size / 2),
                y - (AR2 * size / 2),
                x + (size / 2),
                y + (AR2 * size / 2),
                interpolate = TRUE
              )
            }

            box_dates <- paste(
              format(dates[start_doy], format = date_format_string),
              "-",
              format(dates[corr_date], format = date_format_string)
            )

            graphics::legend("topright",
                             legend = box_dates,
                             cex = 1.12)

            graphics::mtext(
              paste(units_text),
              side = 1,
              line = -2.9,
              at = 0.958,
              outer = TRUE,
              cex = 1.3
            ) # default: at = 0.958
          } # end of for frames
        } # end of if freeze_animation
      } # end of saveVideo expr
    ) # end of saveVideo
  } #end of if

  # Print stats
  # --- Print stats (am Ende) ---
if (isTRUE(verbose)) {
  # Jahre-Vector konsistent zur Klimatologie
  years_all <- as.integer(climate_year_start):as.integer(climate_year_end)

  # Werte je Jahr aus den bereits zugeschnittenen Reihen:
  values_all <- vapply(
    years_all,
    function(yy) {
      dat <- get(paste0(variable, "_acc_", yy))
      if (isTRUE(adjustAccumulation)) utils::tail(dat, 1) else mean(dat, na.rm = TRUE)
    },
    numeric(1)
  )

  # max/min Jahr aus den Werten ableiten
  imax <- which.max(values_all); imin <- which.min(values_all)
  yearMaxEnd <- years_all[imax];  yearMinEnd <- years_all[imin]

  dat_max <- get(paste0(variable, "_acc_", yearMaxEnd))
  dat_min <- get(paste0(variable, "_acc_", yearMinEnd))

  final_values <- data.frame(
    title = c("Analyzed year","Climatology","Maximum valued year","Minimum valued year"),
    years = c(format(start_date, "%Y"),
              paste(climate_year_start, climate_year_end, sep=" - "),
              yearMaxEnd, yearMinEnd),
    value = c(
      utils::tail(var_current_to_plot, 1),
      utils::tail(var_climate_to_plot, 1),
      utils::tail(dat_max, 1),
      utils::tail(dat_min, 1)
    )
  )

  ranking.values <- ranking(
    out_dir, variable, country_code, climate_year_start, climate_year_end, finish_doy,
    years = years_all, values = values_all
  )

  calc.parameters.monitor.climate(final_values, ranking.values)

  if (isTRUE(adjustAccumulation)) {
    message("Significant values at the final time period:")
    print(final_values)
  }
}

}
