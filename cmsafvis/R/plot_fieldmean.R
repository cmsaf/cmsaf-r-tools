# plotting function for fieldmean_plot
plot_fieldmean <- function(variable,
                           infile,
                           infile2,
						               infile3,
                           country_code,
                           climate_year_start,
                           climate_year_end,
                           show_extreme_climate_years,
                           climatology_until_eoy,
                           animation_pace,
                           output_format,
                           language,
                           out_dir,
                           temp_dir,
                           start_date,
                           end_date,
                           freeze_animation,
                           outfile_name,
                           adjustAccumulation,
                           dwd_logo,
                           verbose) {
  # Make sure that any user settings are reset when the function exits
  # This is a requirement by CRAN
  oldpar <- graphics::par(no.readonly = TRUE)
  # Warning: In graphics::par(oldpar) : par(new) ohne Plot aufgerufen
  on.exit(suppressWarnings(graphics::par(oldpar)))

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

  # Dimensions of pic
  pic.width <- 500
  pic.height <- 500

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
  AR <- dims[1] / dims[2]  * pic.width / pic.height


  # DWD Logo
  if(dwd_logo){
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
    AR2 <- dims2[1] / dims2[2]  * pic.width / pic.height
  }
  
  # line thickness
  lwd1 <- 5
  lwd2 <- 2

  temp_dir_acc <- temp_dir

  #set language
  data_year <- format(end_date, "%Y")

  plot_title <- get_title(
    variable = variable,
    language = language,
    year = data_year
  )
  plot_title <- paste0(plot_title, " (", country_name, ")")
  ylab_text <- get_axis_label(variable, language)

  date_format_string <- ifelse(language == "deu", "%d.%m.%Y", "%Y-%m-%d")

  legend_text <- paste0(
    get_climatology_word(language),
    " (",
    climate_year_start,
    "-",
    climate_year_end,
    ")"
  )

  input <- infile
  input2 <- infile2
  
  # Get full time range of infile
  id <- ncdf4::nc_open(infile3)
    date_time <- as.Date(cmsafops::get_time(ncdf4::ncatt_get(id, "time", "units")$value, ncdf4::ncvar_get(id, "time")))
    firstyear <- format(min(date_time), "%Y")
    lastyear  <- format(max(date_time), "%Y")
	  lastyear  <- as.character(as.numeric(lastyear) - 1)
	  var_unit  <- ncdf4::ncatt_get(id, variable, "units")$value
  ncdf4::nc_close(id)
  
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

  # read in netcdf data
  # read in annual mean
  nc_mean <- ncdf4::nc_open(input)
  var_clima <- ncdf4::ncvar_get(nc_mean, variable)
  ncdf4::nc_close(nc_mean)

  # read in current data
  nc_current <- ncdf4::nc_open(input2)
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
  duration <- finish_doy - start_doy + 1
  climate_duration <- duration
  
  len_curr <- length(var_current)
  len_clim <- length(var_clima)
  
  # --- Slice current safely ---
  if (len_curr == duration) {
    # current is already windowed (e.g., 02-Mar .. 03-Oct), do not slice again
    var_current_to_plot <- as.numeric(var_current)
  } else {
    # slice by DOY but clamp to available range
    s_idx <- max(1, start_doy)
    e_idx <- min(finish_doy, len_curr)
    var_current_to_plot <- as.numeric(var_current[s_idx:e_idx])
  }
  
  # --- Slice climatology safely (usually full year) ---
  s_idx_clim <- max(1, start_doy)
  e_idx_clim <- min(finish_doy, len_clim)
  var_climate_to_plot <- as.numeric(var_clima[s_idx_clim:e_idx_clim])
  
  # --- De-accumulate relative to the in-window start (if requested) ---
  if (start_doy > 1 && adjustAccumulation) {
    # Use first in-window values as offsets; robust against NA at Jan 1
    off_curr <- var_current_to_plot[1]
    if (is.na(off_curr)) off_curr <- 0
    var_current_to_plot <- var_current_to_plot - off_curr
    
    off_clim <- var_climate_to_plot[1]
    if (is.na(off_clim)) off_clim <- 0
    var_climate_to_plot <- var_climate_to_plot - off_clim
  }
  
  # Keep a consistent plot length downstream
  plot_length <- min(length(var_current_to_plot), length(var_climate_to_plot))
  
  #### DEBUGGING
  message(sprintf("[DBG] curr_len=%d clim_len=%d duration=%d tail(curr)=%s",
                  length(var_current_to_plot), length(var_climate_to_plot), duration,
                  as.character(utils::tail(var_current_to_plot,1))))
  
  ############################################################################################
  # plot var_2018_graphic

  if (output_format == "graphic") {

    picpath <- out_dir
    picout <- file.path(picpath, outfile_name)

    limit <- -Inf

    # start loop over all years and read in netcdf data for annual sum of var
	for (climate_year in seq(from = firstyear, to = lastyear, by = 1)) {
      input <- add_ncdf_ext(construct_filename(variable,
                                               climate_year,
                                               country_code,
                                               "fldmean"))
      input <- file.path(temp_dir_acc, input)
      nc_fieldmean <- ncdf4::nc_open(input)
      var_ensemble <- ncdf4::ncvar_get(nc_fieldmean, variable)
      ncdf4::nc_close(nc_fieldmean)

      # Cut the data
      var_ensemble <- var_ensemble[start_doy:finish_doy]

      # Remove accumulation of previous dates
      if (start_doy > 1 && adjustAccumulation) {
        var_ensemble <- var_ensemble - rep(var_ensemble[1], climate_duration)
      }

      assign(paste0(variable, "_acc_", climate_year), var_ensemble)  # TODO use matrix: tmp = matrix(nrow = length(climate_year_end:climate_year_end), ncol = climate_duration, dimnames = list(climate_year_start:climate_year_end))

      # Compute y limit dependent on accumulation process
      if (adjustAccumulation) {
        limit <- max(limit, c(var_ensemble[length(var_ensemble)], var_current[length(var_current)]))
      } else {
        limit <- max(limit, max(c(var_ensemble, var_current), na.rm = TRUE))
      }
    }

    limit_y <- signif(ceiling(limit + limit/15), digits = 2)

    grDevices::png(
      picout,
      width = pic.width,
      height = pic.height,
      units = "px",
      pointsize = 12
    )

    # plot_length <- finish_doy - start_doy + 1

    graphics::par(
      cex = 1.2,
      oma = c(0, 0, 0, 0),
      mar = c(2.2, 4, 3.5, 2),
      mgp = c(3, 1, 0)
    )

    set_time_locale(language)
    tryCatch(
      graphics::plot(
        dates[start_doy:finish_doy],
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
	for (climate_year in seq(from = firstyear, to = lastyear, by = 1)) {
      dat <- get(paste0(variable, "_acc_", climate_year))

      set_time_locale(language)
      tryCatch(
        graphics::lines(
          dates[start_doy:finish_doy],
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
        dates[start_doy:finish_doy],
        var_climate_to_plot,
        col = "black",
        lwd = lwd1
      )
      graphics::lines(
        dates[start_doy:finish_doy],
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
      graphics::text(x = 0.9, y = y_val_min, labels = yearMinEnd, pos = 1, col = "darkgrey")
    }


    grDevices::dev.off()

    if (verbose) {
      message(paste("Image has been created at", normalizePath(picout)))
    }
  }

  ###########################################################################################################################

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

          limit <- -Inf

          # start loop over all years and read in netcdf data for annual sum of var
		  for (climate_year in seq(from = firstyear,
                                   to = lastyear,
                                   by = 1)) {
            input <- add_ncdf_ext(construct_filename(variable,
                                                     climate_year,
                                                     country_code,
                                                     "fldmean"))
            input <- file.path(temp_dir_acc, input)

            nc_fieldmean <- ncdf4::nc_open(input)
            var_ensemble <- ncdf4::ncvar_get(nc_fieldmean, variable)
            ncdf4::nc_close(nc_fieldmean)

            # Cut data
            var_ensemble <- var_ensemble[start_doy:finish_doy]

            # Remove previous accumulation if required
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
          # plot_length <- finish_doy - start_doy + 1

          corr_date <- start_doy + i - 1

          graphics::par(
            cex = 1.2,
            oma = c(0, 0, 0, 0),
            mar = c(2.2, 4, 3.5, 2),
            mgp = c(3, 1, 0)
          )

          set_time_locale(language)
          tryCatch(
            graphics::plot(
              dates[start_doy:finish_doy],
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

		  for (climate_year in seq(from = firstyear,
                                   to = lastyear,
                                   by = 1)) {
            dat <- get(paste0(variable, "_acc_", climate_year))

            set_time_locale(language)
            tryCatch(
              graphics::lines(
                dates[start_doy:finish_doy],
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

          var_current_seq <- var_current_to_plot[1:i]

          set_time_locale(language)
          tryCatch({
            graphics::lines(
              dates[start_doy:finish_doy],
              var_climate_to_plot,
              col = "black",
              lwd = lwd1
            )
            graphics::lines(
              dates[start_doy:corr_date],
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

          if (verbose) {
            pb$tick()
          }
        } # end of "daily" loop
        if (freeze_animation) {
          for (k in 1:nr_frozen_frames) {

            limit <- -Inf

			for (climate_year in seq(from = firstyear,
                                     to = lastyear,
                                     by = 1)) {
              input <- add_ncdf_ext(construct_filename(variable,
                                                       climate_year,
                                                       country_code,
                                                       "fldmean"))
              input <- file.path(temp_dir_acc, input)
              nc_fieldmean <- ncdf4::nc_open(input)
              var_ensemble <- ncdf4::ncvar_get(nc_fieldmean, variable)
              ncdf4::nc_close(nc_fieldmean)

              # Cut the data
              var_ensemble <- var_ensemble[start_doy:finish_doy]

              # Remove previous accumulation if required
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

            graphics::par(
              cex = 1.2,
              oma = c(0, 0, 0, 0),
              mar = c(2.2, 4, 3.5, 2),
              mgp = c(3, 1, 0)
            )

            set_time_locale(language)
            tryCatch(
              graphics::plot(
                dates[start_doy:finish_doy],
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
			for (climate_year in seq(from = firstyear,
                                     to = lastyear,
                                     by = 1)) {
              dat <- get(paste0(variable, "_acc_", climate_year))

              set_time_locale(language)
              tryCatch(
                graphics::lines(
                  dates[start_doy:finish_doy],
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
                dates[start_doy:finish_doy],
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
              graphics::text(x = 0.9, y = y_val_min, labels = yearMinEnd, pos = 1, col = "darkgrey")
            }

            if (verbose) {
              pb$tick()
            }
          } # end of for frames
        } # end of if freeze_animation
      } # end of saveVideo expr
    ) # end of saveVideo
  } #end of if

  # Print stats
  if (verbose) {
    dat_max <- get(paste0(variable, "_acc_", yearMaxEnd))
    dat_min <- get(paste0(variable, "_acc_", yearMinEnd))
    titles <- c("Analyzed year", "Climatology", "Maximum valued year", "Minimum valued year")

    standout_years <- c(format(start_date, format = "%Y"),
                        paste(climate_year_start, climate_year_end, sep = " - "),
                        yearMaxEnd,
                        yearMinEnd)

    standout_values <- c(var_current_to_plot[length(var_current_to_plot)],
                         var_climate_to_plot[length(var_current_to_plot)],
                         dat_max[length(var_current_to_plot)],
                         dat_min[length(var_current_to_plot)])

    final_values <- data.frame(title = titles, years = standout_years, value = standout_values)
    
    # --- Build consistent ranking values based on plotted data ---
    
    # Vector of years corresponding to climatology files
    years_all <- as.integer(firstyear):as.integer(lastyear)
    
    # Derive one representative value per year
    # If accumulation is active: take the final day value (sum)
    # Otherwise: use the mean over the selected period
    values_all <- vapply(
      years_all,
      function(yy) {
        dat <- get(paste0(variable, "_acc_", yy))  # already cropped & adjusted in plot loop
        if (adjustAccumulation) {
          utils::tail(dat, 1)  # final accumulated value
        } else {
          mean(dat, na.rm = TRUE)  # or sum(dat, na.rm = TRUE) if desired
        }
      },
      numeric(1)
    )
    
    # Create ranking dataframe directly from these values
    ranking.values <- ranking(
      out_dir, variable, country_code, firstyear, lastyear, finish_doy,
      years = years_all, values = values_all
    )
    
    # Pass to existing output routine
    calc.parameters.monitor.climate(final_values, ranking.values)
    
    # Print message
    if (adjustAccumulation) {
      message("Significant values at the final time period:")
      print(final_values)
    }
  }
}
