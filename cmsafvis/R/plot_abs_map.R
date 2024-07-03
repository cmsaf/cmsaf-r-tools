plot_abs_map <- function(variable,
                         country_code,
                         climate_year_start = NULL,
                         climate_year_end = NULL,
                         infile,
                         climatology_file = NULL,
                         out_dir,
                         start_date,
                         end_date,
                         language,
                         plot_type,
                         animation_pace,
                         output_format,
                         min_value,
                         max_value,
                         lon_min,
                         lon_max,
                         lat_min,
                         lat_max,
						             color_pal,
						             relative,
                         nbreaks,
                         freeze_animation,
                         outfile_name,
                         accumulate,
                         states,
                         dwd_logo,
                         verbose,
                         nc = NULL) {
  # Make sure that any user settings are reset when the function exits
  # This is a requirement by CRAN
  oldpar <- graphics::par(no.readonly = TRUE)
  # Warning: In graphics::par(oldpar) : par(new) ohne Plot aufgerufen
  on.exit(suppressWarnings(graphics::par(oldpar)))

  countriesHigh <- numeric(0)  # Hack to prevent IDE warning in second next line (see https://stackoverflow.com/questions/62091444/how-to-load-data-from-other-package-in-my-package)
  utils::data("countriesHigh", package = "rworldxtra", envir = environment())
  world_countries <- methods::as(countriesHigh, "SpatialLines")

  # Compute some parameters from arguments
  start_date_short <- format(start_date, "%m%d")
  end_date_short <- format(end_date, "%m%d")
  start_doy <- as.numeric(format(start_date, format = "%j"))
  finish_doy <- as.numeric(format(end_date, format = "%j"))
  if (is_leap_year(as.numeric(format(as.Date(start_date),"%Y")))) {
    start_doy <- ifelse(start_doy >= 60, start_doy - 1, start_doy)
  }
  if (is_leap_year(as.numeric(format(as.Date(end_date),"%Y")))) {
    finish_doy <- ifelse(finish_doy >= 60, finish_doy - 1, finish_doy)
  }
  year <- format(end_date, "%Y")

  set_time <- start_doy > 1
  country_name <- get_country_name(country_code, language = language)
  country_name_en <- get_country_name(country_code)

  if (states) {
    check_state_dependencies()

    statesHigh <- rnaturalearth::ne_states(country_name_en)
    state_lines <- methods::as(statesHigh, "SpatialLines")
  }

  map_title <- get_title(
    variable = variable,
    plot_type = plot_type,
    language = language
  )
  map_title <- paste0(map_title, " (", country_name, ")")
  units_text <- paste0(
    "[",
    get_unit(variable = variable, language = language),
    "]"
  )
  if (relative) units_text <- "percent"
  box_text <- paste0(get_climatology_word(language), ": ")
  time_box <- paste0(get_translation_duration(language), ":")

  date_format_string <- ifelse(language == "deu", "%d.%m.%Y", "%Y-%m-%d")
  date_format_string_short <- ifelse(language == "deu", "%d.%m.", "%m-%d")
  
  # Functions to calculate aspect ratio (code parts from ggplot2)
  map_aspect = function(x, y) {
    x.center <- sum(range(x)) / 2
    y.center <- sum(range(y)) / 2
    x.dist <- dist_central_angle(x.center + c(-0.5, 0.5), rep(y.center, 2))
    y.dist <- dist_central_angle(rep(x.center, 2), y.center + c(-0.5, 0.5))
    y.dist / x.dist
  }
  
  dist_central_angle <- function(lon, lat) {
    # Convert to radians
    lat <- lat * pi / 180
    lon <- lon * pi / 180
    
    hav <- function(x) sin(x / 2) ^ 2
    ahav <- function(x) 2 * asin(x)
    
    n <- length(lat)
    ahav(sqrt(hav(diff(lat)) + cos(lat[-n]) * cos(lat[-1]) * hav(diff(lon))))
  }

  # input
  if (!is.null(nc)) opennc <- nc
  else opennc <- ncdf4::nc_open(infile)
  field_source <- ncdf4::ncvar_get(opennc, variable)
  lon <- ncdf4::ncvar_get(opennc, "lon")
  lat <- ncdf4::ncvar_get(opennc, "lat")
  if (is.null(nc)) ncdf4::nc_close(opennc)
  
  nx <- length(lon)
  ny <- length(lat)
  
  if (is.null(lon_min)) {
    lon_min <- min(lon, na.rm =TRUE)
    lat_min <- min(lat, na.rm =TRUE)
    lon_max <- max(lon, na.rm =TRUE)
    lat_max <- max(lat, na.rm =TRUE)
  }

  if (is_leap_year(year) && finish_doy > 60 && start_doy < 60) {
    # We remove the leap year date in this case so we actually have one day less.
    # Subtracting one from finish doy will do the job.
    finish_doy <- finish_doy - 1
  }

  duration <- finish_doy - start_doy + 1
  
  if (start_doy == finish_doy) {
    field_to_plot <- field_source
    field_for_setup <- field_source
    field_for_setup_last <- field_source
  } else {
    field_to_plot <- field_source[, , duration]
    field_for_setup <- field_source[, , 1:duration]
    field_for_setup_last <- field_source[, , duration]
  }

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

  limit_lower <- min(field_for_setup_last, na.rm = TRUE)
  limit_upper <- max(field_for_setup_last, na.rm = TRUE)

  col_margin <- (limit_upper - limit_lower)/20

  limit_lower_z <- floor(limit_lower - col_margin)
  limit_upper_z <- ceiling(limit_upper + col_margin)

  # select the range of the color bar
  min_value_default <- 0
  max_value_default <- limit_upper_z
  freq.ticks <- 1

  if (plot_type == "absolute_map") {
    min_value_default <- limit_lower_z
    bwr_col <-
      grDevices::colorRampPalette(c(
        "lightgreen",
        "green",
        "orange",
        "orangered1",
        "red",
        "magenta"
      ))
  }

  if (plot_type == "climatology_map") {
    min_value_default <- limit_lower_z
    bwr_col <-
      grDevices::colorRampPalette(c(
        "lightgreen",
        "green",
        "orange",
        "orangered1",
        "red",
        "magenta"
      ))
    coldiff <- max_value_default - min_value_default
  }

  if (plot_type == "anomaly_map") {
    if (abs(max(field_for_setup_last, na.rm = TRUE)) < abs(min(field_for_setup_last, na.rm = TRUE))) {
      limit_lower <- min(field_for_setup_last, na.rm = TRUE)
      limit_lower_z <- signif(floor(limit_lower - 0), digits = 2)
      min_value_default <- limit_lower_z
      max_value_default <- (-1) * min_value_default
    }
    else{
      limit_upper <- max(field_for_setup, na.rm = TRUE)
      limit_upper_z <- signif(ceiling(limit_upper + 0), digits = 2)
      max_value_default <- limit_upper_z
      min_value_default <- (-1) * max_value_default
    }
    bwr_col <-
      grDevices::colorRampPalette(
        c(
          "darkblue",
          "blue",
          "lightskyblue",
          "orangered1",
          "red",
          "red4"
        )
      )
	  
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
  }

  if (output_format == "animation") {

    viddir <- out_dir
    vidout <- file.path(viddir, outfile_name)

    if (plot_type == "climatology_map") {
      min_value_default <- 0
      bwr_col <-
        grDevices::colorRampPalette(c(
          "lightgreen",
          "green",
          "orange",
          "orangered1",
          "red",
          "magenta"
        ))
    }
    if (plot_type == "absolute_map") {
      min_value_default <- 0
      bwr_col <-
        grDevices::colorRampPalette(c(
          "lightgreen",
          "green",
          "orange",
          "orangered1",
          "red",
          "magenta"
        ))
    }
  }

  if (output_format == "graphic") {
    if (plot_type != "anomaly_map") {
      if (min_value_default < 0) {
        min_value_default <- 0
        bwr_col <-
          grDevices::colorRampPalette(c(
            "lightgreen",
            "green",
            "orange",
            "orangered1",
            "red",
            "magenta"
          ))
      }
    }
  }

  # Check if need to assign default min_value and max_value values (computed above)
  if (is.null(min_value)) {
    min_value <- min_value_default
  }
  if (is.null(max_value)) {
    max_value <- max_value_default
  }

  coldiff <- max_value - min_value
  
  # Get aspect ratio
  aspect_ratio <- map_aspect(c(lon_min, lon_max), c(lat_min, lat_max))
   
  # pic size
  pic.height <- 830
  pic.width <-  round((((nx / ny) / aspect_ratio) * pic.height) * 1.09)
  if (pic.width < 710) {
    pic.width <- 830
    pic.height <- round(pic.width * ((ny * aspect_ratio) / (nx * 1.09)))
  }

  if (country_code == "TOT") {
    pic.width <-  830
    pic.height <- 830
  }

  if (country_code == "EUR") {
    pic.width <-  1300
    pic.height <- 900
  }

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

  # Never let ncol fall below 2
  ncol <- max(2, ncol)

  colors <- bwr_col(ncol - 1)

  # Size and location of the logo
  logo.size <- 0.26
  logo.x <- 0.88
  logo.y <- 0.06

  if (country_code == "EUR") {
    logo.size <- 0.16
    logo.x <- 0.9
    logo.y <- 0.06
  }

  logo_cmsaf_path <- system.file(
    "extdata",
    "CMSAF_logo.png",
    package = "cmsafvis",
    mustWork = TRUE
  )
  logo_cmsaf <- png::readPNG(logo_cmsaf_path)
  dims <- dim(logo_cmsaf)[1:2]
  AR <- dims[1] / dims[2] * pic.width / pic.height

  # DWD Logo
  if (dwd_logo){
    logo.size2 <- 0.07
    logo.x2 <- 0.95
    logo.y2 <- 0.14

    # DWD Logo
    #logo.size2 <- 0.06
    #logo.x2 <- 0.95
    #logo.y2 <- 0.25

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
  
  if (country_code == "EUR") {
    logo.size2 <- 0.04
    logo.x2 <- 0.97
    logo.y2 <- 0.14
  }

  if (output_format == "graphic") {

    picpath <- out_dir
    picout <- file.path(picpath, outfile_name)

    # To plot a meaningful color bar the data need to be transfered to numbers from 1.1 to nticks + 0.1
    field.plot_freeze <- matrix(ncol = ny, nrow = nx)
    for (l in seq(1, ncol - 1)) {
      idx <- which(field_to_plot >= col.breaks[l] &
                     field_to_plot <= col.breaks[l + 1],
                   arr.ind = TRUE)
      field.plot_freeze[idx] <- l + 0.1
    }


    # Start animation
    grDevices::png(
      picout,
      width = pic.width,
      height = pic.height,
      units = "px",
      pointsize = 12
    )


    lg_mar <- 4 + floor(log10(max(abs(names.ticks))))

    graphics::par(cex = 1.3,
                  mgp = c(2, 1, 0),
                  mar = c(2, 1, 3, 4) + 0.1)
    fields::imagePlot(
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
      axis.args = list(at = at.ticks, labels = names.ticks),
      legend.mar = lg_mar,
      cex.main = 1.1,
      xlim = c(lon_min, lon_max),
      ylim = c(lat_min, lat_max),
      asp = aspect_ratio
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

    if (plot_type == "climatology_map" && set_time) {
      # Duration (time span) is only needed if not the entire year is used.
      set_time_locale(language)
      start_date_nice <- format(as.Date(start_date_short, format = "%m%d"), "%b")  # Depends on locale
      end_date_nice <- format(as.Date(end_date_short, format = "%m%d"), "%b")  # Depends on locale
      set_time_locale("")  # Reset locale

      graphics::legend("topright", legend = c(
        paste(time_box, start_date_nice, "-", end_date_nice)
      ), cex = 1.3)
    } else if (plot_type %in% c("absolute_map", "anomaly_map")) {
      # For absolute_map and anomaly_map, duration (time span) is always needed.
      box_dates <- paste(
        format(start_date, format = date_format_string),
        "-",
        format(end_date, format = date_format_string)
      )
      graphics::legend("topright", legend = c(paste(time_box, box_dates)), cex = 1.3)
    }

    if (plot_type %in% c("anomaly_map", "climatology_map")) {
      graphics::legend("topleft", legend = c(
        paste0(box_text, climate_year_start, "-", climate_year_end)
      ), cex = 1.3)
    }
    unit_location <- 0.928

    if (country_code == "EUR") {
      unit_location <- 0.958
    }

    graphics::mtext(
      paste(units_text),
      side = 1,
      line = -2.9,
      at = unit_location,
      outer = TRUE,
      cex = 1.3
    ) # default: at = 0.958

    grDevices::dev.off()

    if (verbose) {
      message(paste("Image has been created at", normalizePath(picout)))
    }
  }

  # Animation format
  if (output_format == "animation") {

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

          if (is_leap_year(year) && start_doy < 60 && finish_doy > 60) {
            dates <- seq(start_date, by = "day",length.out = duration + 1)
            dates <- dates[format(dates, "%j") != "60"]
          } else {
            dates <- seq(start_date, by = "day",length.out = duration)
          }

          lg_mar <- 4 + floor(log10(max(abs(names.ticks))))

          graphics::par(
            cex = 1.3,
            mgp = c(2, 1, 0),
            mar = c(2, 1, 3, 4) + 0.1
          )

          fields::imagePlot(
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
            cex.main = 1.1,
            xlim = c(lon_min, lon_max),
            ylim = c(lat_min, lat_max),
            asp = aspect_ratio
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

          # Legend for time span (duration)
          if (plot_type == "climatology_map") {
            current_date_short <- format(dates[i], format = date_format_string_short)
            start_date_nice <- format(
              as.Date(start_date_short, format = "%m%d"),
              format = date_format_string_short
            )

            if (accumulate) {
              time_box_text <- paste(time_box, start_date_nice, "-", current_date_short)
            } else {
              time_box_text <- paste(current_date_short)
            }
          } else {
            current_date_nice <- format(dates[i], format = date_format_string)
            if (accumulate) {
              start_date_nice <- format(start_date, format = date_format_string)
              time_box_text <- paste(
                time_box, start_date_nice, "-", current_date_nice
              )
            } else {
              time_box_text <- paste(current_date_nice)
            }
          }
          graphics::legend(
            "topright",
            legend = time_box_text,
            cex = 1.3
          )

          # Legend for climatology
          if (plot_type != "absolute_map") {
            graphics::legend("topleft",
                             legend = c(
                               paste0(
                                 box_text,
                                 climate_year_start,
                                 "-",
                                 climate_year_end
                               )
                             ),
                             cex = 1.3)
          }
          unit_location <- 0.928

          if (country_code == "EUR") {
            unit_location <- 0.958
          }
          graphics::mtext(
            paste(units_text),
            side = 1,
            line = -2.9,
            at = unit_location,
            outer = TRUE,
            cex = 1.3
          ) # default: at = 0.958

          if (verbose) {
            pb$tick()
          }
        } # end of "daily" loop
        # Frozen format
        if (freeze_animation) {
          for (k in 1:nr_frozen_frames) {
            field_to_animate <- field_for_setup_last

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

            if (is_leap_year(year)) {
              dates <- seq(start_date, by = "day",length.out = duration + 1)

              # We remove the leap year date if it is in the list of dates. Else we remove the last date
              if (60 %in% format(dates, "%j")) {
                dates <- dates[format(dates, "%j") != "60"]
              } else {
                dates <- dates[-length(dates)]
              }
            } else {
              dates <- seq(start_date, by = "day",length.out = duration)
            }

            lg_mar <- 4 + floor(log10(max(abs(names.ticks))))

            graphics::par(
              cex = 1.3,
              mgp = c(2, 1, 0),
              mar = c(2, 1, 3, 4) + 0.1
            )

            fields::imagePlot(
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
              cex.main = 1.1,
              xlim = c(lon_min, lon_max),
              ylim = c(lat_min, lat_max),
              asp = aspect_ratio
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

            # Legend for time span (duration)
            if (plot_type == "climatology_map") {
              if (accumulate) {
                time_box_text <- paste(time_box, start_date_nice, "-", current_date_short)
              } else {
                time_box_text <- paste(current_date_short)
              }
            } else {
              current_date_nice <- format(dates[i], format = date_format_string)
              if (accumulate) {
                start_date_nice <- format(start_date, format = date_format_string)
                time_box_text <- paste(
                  time_box, start_date_nice, "-", current_date_nice
                )
              } else {
                time_box_text <- paste(current_date_nice)
              }
            }
            graphics::legend(
              "topright",
              legend = time_box_text,
              cex = 1.3
            )

            # Legend for climatology
            if (plot_type != "absolute_map") {
              graphics::legend("topleft",
                               legend = c(
                                 paste0(
                                   box_text,
                                   climate_year_start,
                                   "-",
                                   climate_year_end
                                 )
                               ),
                               cex = 1.3)
            }
            unit_location <- 0.928

            if (country_code == "EUR") {
              unit_location <- 0.958
            }
            graphics::mtext(
              paste(units_text),
              side = 1,
              line = -2.9,
              at = unit_location,
              outer = TRUE,
              cex = 1.3
            ) # default: at = 0.958

            if (verbose) {
              pb$tick()
            }
          } # end of for frames
        } # end of if freeze_animation
      } # end of saveVideo expr
    ) # end of saveVideo
  } #end of if
  
  
  if(plot_type == "absolute_map")
  {
    # calc monitor climate parameters
    tmp_climate_dir <- file.path(tempdir(), "tmp_climate_dir")
    # remove if it exists
    if (dir.exists(tmp_climate_dir)) {
      unlink(tmp_climate_dir, recursive = TRUE)
    }
    # create new temp dic
    if (!dir.exists(tmp_climate_dir)) {
      dir.create(tmp_climate_dir)
    }
    
    # Ranking
    # tmp_ranking <- file.path(tmp_climate_dir, paste0("tmp_ranking.nc"))
    # cmsafops::monmean(var = variable, infile = infile, outfile = tmp_ranking, overwrite = TRUE)
    # tmp_ranking_1 <- file.path(tmp_climate_dir, paste0("tmp_ranking_1.nc"))
    # cmsafops::yearmean(var = variable, infile = tmp_ranking, outfile = tmp_ranking_1, overwrite = TRUE)
    # tmp_ranking_final <- file.path(tmp_climate_dir, paste0("tmp_ranking_final.nc"))
    # cmsafops::fldmean(var = variable, infile = tmp_ranking_1, outfile = tmp_ranking_final, overwrite = TRUE)
    # nc_in_1 <- nc_open(tmp_ranking_final)
    # dum_dat_ranking <- ncvar_get(nc_in_1, variable, collapse_degen = FALSE)
    # nc_close(nc_in_1)
    # 
    # file_data <- cmsafops::read_file(tmp_ranking_final, variable)
    # file_data$variable$prec <- "float"
    # years_all <- cmsafops::get_date_time(file_data$dimension_data$t, file_data$time_info$units)$years
    # 
    # ranking <- data.frame(years_all, as.vector(dum_dat_ranking))
    # names(ranking) <- c("Year","Value")
    
    titles <- c("Analyzed year", "Maximum", "Minimum")
    
    standout_years <- c(format(start_date, format = "%Y"),
                        "-",
                        "-")
    
    standout_values <- c(mean(field_source, na.rm = TRUE), max(field_source, na.rm = TRUE), min(field_source, na.rm = TRUE))
    
    final_values <- data.frame(title = titles, years = standout_years, value = standout_values)
    calc.parameters.monitor.climate(final_values)#, ranking[order(ranking$Value),])
    
    # remove if it exists
    if (dir.exists(tmp_climate_dir)) {
      unlink(tmp_climate_dir, recursive = TRUE)
    }
  }
  else{
    # calc monitor climate parameters
    tmp_climate_dir <- file.path(tempdir(), "tmp_climate_dir")
    # remove if it exists
    if (dir.exists(tmp_climate_dir)) {
      unlink(tmp_climate_dir, recursive = TRUE)
    }
    # create new temp dic
    if (!dir.exists(tmp_climate_dir)) {
      dir.create(tmp_climate_dir)
    }
    
    # Clim mean value
    tmp_clim_mean_value <- file.path(tmp_climate_dir, paste0("tmp_clim_mean_value.nc"))
    cmsafops::fldmean(var = variable, infile = climatology_file, outfile = tmp_clim_mean_value, overwrite = TRUE)
    nc_in <- ncdf4::nc_open(tmp_clim_mean_value)
    dum_dat_mean <- ncdf4::ncvar_get(nc_in, variable, collapse_degen = FALSE)
    ncdf4::nc_close(nc_in)
    
    # Ranking
    # tmp_ranking <- file.path(tmp_climate_dir, paste0("tmp_ranking.nc"))
    # cmsafops::monmean(var = variable, infile = infile, outfile = tmp_ranking, overwrite = TRUE)
    # tmp_ranking_1 <- file.path(tmp_climate_dir, paste0("tmp_ranking_1.nc"))
    # cmsafops::timselmean(var = variable, nts = 12, infile = tmp_ranking, outfile = tmp_ranking_1, overwrite = TRUE)
    # tmp_ranking_final <- file.path(tmp_climate_dir, paste0("tmp_ranking_final.nc"))
    # cmsafops::fldmean(var = variable, infile = tmp_ranking_1, outfile = tmp_ranking_final, overwrite = TRUE)
    # nc_in_1 <- ncdf4::nc_open(tmp_ranking_final)
    # dum_dat_ranking <- ncdf4::ncvar_get(nc_in_1, variable, collapse_degen = FALSE)
    # ncdf4::nc_close(nc_in_1)
    # 
    # file_data <- cmsafops::read_file(tmp_ranking_final, variable)
    # file_data$variable$prec <- "float"
    # years_all <- cmsafops::get_date_time(file_data$dimension_data$t, file_data$time_info$units)$years
    # 
    # ranking <- data.frame(years_all, as.vector(dum_dat_ranking))
    # names(ranking) <- c("Year","Value")
    
    titles <- c("Analyzed year", "Climatology Mean Value", "Maximum", "Minimum")
    
    standout_years <- c(format(start_date, format = "%Y"),
                        paste(climate_year_start, climate_year_end, sep = " - "),
                        "-",
                        "-")
    
    standout_values <- c(mean(field_source, na.rm = TRUE), mean(dum_dat_mean, na.rm = TRUE), max(field_source, na.rm = TRUE), min(field_source, na.rm = TRUE))
    
    final_values <- data.frame(title = titles, years = standout_years, value = standout_values)
    calc.parameters.monitor.climate(final_values)#, ranking[order(ranking$Value),])
    
    # remove if it exists
    if (dir.exists(tmp_climate_dir)) {
      unlink(tmp_climate_dir, recursive = TRUE)
    }
  }
  
} # end of function
