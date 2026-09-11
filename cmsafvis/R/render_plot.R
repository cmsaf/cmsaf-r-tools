#' Plotting routine designed for the CM SAF R Toolbox.
#'
#' This function renders a 2D image usually called by the CM SAF R Toolbox.
#'
#' @importFrom graphics par strheight strwidth
#' @importFrom stats median
#'
#' @param plot_rinstat Whether to create an R-Instat plot (logical).
#' @param outfile Name of the outfile (NULL or character). Should match the fileExtension.
#' If NULL is passed a file is created in the R session temporary directory.
#' @param fileExtension The file extension of the image (character). Has to be one of the following: 'png', 'jpg', 'tif', 'kml', 'pdf'.
#' @param visualizeVariables A data frame containing all meta data for the plotting process (data.frame).
#' @param visualizeDataTimestep The data to be plotted.
#' @param nc_path_visualize The nc file path of which the plot is generated for.
#' @param visualizeDataMax Maximal data for computing breaks.
#' @param lon_bounds Array containing two values for longitude min and max (numeric).
#' @param lat_bounds Array containing two values for latitude min and max (numeric).
#' @param location Whether points specified by (lat_loc_vec, lon_loc_vec, name_loc_vec) should be added to the map (logical).
#' @param lon_loc_vec All longitude entries for points at (lat_loc_vec, lon_loc_vec) to be specified on the map (numeric).
#' @param lat_loc_vec All latitude entries for points at (lat_loc_vec, lon_loc_vec) to be specified on the map (numeric).
#' @param name_loc_vec Names for the points at (lat_loc_vec, lon_loc_vec) to be specified on the map (numeric).
#' @param num_tick Number of ticks (numeric).
#' @param num_rmin Color scale range minimum (numeric).
#' @param num_rmax Color scale range maximium (numeric).
#' @param num_brk Number of breaks (numeric).
#' @param co.data Data to be plotted in R-Instat mode (data.frame).
#' @param co.data.compare.diff Data to be plotted in compare data mode (data.frame).
#' @param xort Centering the globe at longitude xort (numeric). Only in orthographic mode.
#' @param yort Centering the globe at latitude yort (numeric). Only in orthographic mode.
#' @param rort Rotation of the globe (numeric). Only in orthographic mode.
#' @param slider1 Controlling the horizontal plot position as vector of two values min and max (numeric).
#' @param slider2 Controlling the vertical plot position as vector of two values min and max (numeric).
#' @param imagewidth Width of the image (numeric).
#' @param imageheight Height of the image (numeric).
#' @param int Whether interior country borders should be added (logical).
#' @param text1 Title text (character).
#' @param text2 Text to be passed to graphics::mtext (character).
#' @param text3 Text to be added to the legend (character).
#' @param PAL Color palette.
#' @param timestep The current timestep chosen.
#' @param proj The chosen projection (either 'rect' for rectangular or 'ortho' for orthographic).
#' @param plot_grid Whether to plot a grid using color grid_col (logical).
#' @param grid_col Color used for the grid.
#' @param bordercolor Color used for borders.
#' @param linesize Line width to be used (positive numeric).
#' @param reverse Whether to revert the color palette (logical).
#' @param na.color The color to be used for NA values.
#' @param textsize Textsize to be used (cex).
#' @param palettes Color palettes to be used.
#' @param sig_values_to_plot For trend plots significance to plot.
#' @param sig_na_color Color for non-significant values.
#' @export
render_plot <- function(plot_rinstat,
                        outfile = NULL,
                        fileExtension = ".png",
                        visualizeVariables,
                        visualizeDataTimestep,
                        nc_path_visualize,
                        visualizeDataMax,
                        lon_bounds,
                        lat_bounds,
                        lon_loc_vec,
                        lat_loc_vec,
                        name_loc_vec,
                        timestep,
                        num_tick,
                        num_rmin,
                        num_rmax,
                        num_brk,
                        co.data,
                        co.data.compare.diff,
                        proj,
                        xort,
                        yort,
                        rort,
                        slider1,
                        slider2,
                        imagewidth,
                        imageheight,
                        location,
                        int,
                        text1,
                        text2,
                        text3,
                        textsize,
                        bordercolor,
                        linesize,
                        na.color,
                        PAL,
                        palettes,
                        reverse,
                        plot_grid,
                        grid_col,
                        sig_values_to_plot = c(1, -1),
                        sig_na_color = "white"
                        ) {
  # A temp file to save the output.
  if (is.null(outfile)) {
    outfile <- tempfile(fileext = fileExtension)
  }
  tlab <- break_num(
    ln = num_tick,
    bn = num_tick,
    minn = num_rmin,
    maxn = num_rmax,
    max_data = visualizeDataMax
  )
  xtick  <- grDevices::axisTicks(lon_bounds, log = FALSE)
  ytick  <- grDevices::axisTicks(lat_bounds, log = FALSE)

  xlab <-
    unlist(lapply(xtick, function(x)
      ifelse(
        x < 0,
        paste0(abs(x), " W"), ifelse(x > 0, paste0(abs(x), " E"), x)
      )))
  ylab <-
    unlist(lapply(ytick, function(x)
      ifelse(
        x < 0, paste0(abs(x), " S"),
        ifelse(x > 0, paste0(abs(x), " N"), x)
      )))

  if (min(xtick) == round(lon_bounds[1])) {
    xlab[1] <- " "
  }
  if (max(xtick) == round(lon_bounds[2])) {
    xlab[length(xlab)] <- " "
  }
  if (min(ytick) == round(lat_bounds[1])) {
    ylab[1] <- " "
  }
  if (max(ytick) == round(lat_bounds[2])) {
    ylab[length(ylab)] <- " "
  }

  # --- Build overlay for selected significance levels (rect plots) ---
  sig_overlay <- NULL
  if (!is.null(sig_values_to_plot) && length(sig_values_to_plot) > 0 && file.exists(nc_path_visualize)) {
    nc <- ncdf4::nc_open(nc_path_visualize)
    on.exit(ncdf4::nc_close(nc), add = TRUE)
    
    if ("sig" %in% names(nc$var)) {
      sig_raw <- ncdf4::ncvar_get(nc, "sig")
      
      dx <- nrow(visualizeDataTimestep)
      dy <- ncol(visualizeDataTimestep)
      
      # reshape to match data dims (dx x dy)
      sig_mat <- try(matrix(sig_raw, nrow = dx, ncol = dy), silent = TRUE)
      if (inherits(sig_mat, "try-error") || !identical(dim(sig_mat), c(dx, dy))) {
        sig_mat <- try(matrix(sig_raw, nrow = dy, ncol = dx), silent = TRUE)
        if (!inherits(sig_mat, "try-error")) sig_mat <- t(sig_mat)
      }
      
      if (!inherits(sig_mat, "try-error") && identical(dim(sig_mat), dim(visualizeDataTimestep))) {
        keep_levels <- as.numeric(sig_values_to_plot)
        # NA everywhere; 1 where sig equals any chosen level
        sig_overlay <- matrix(NA_real_, nrow = dx, ncol = dy)
        sig_overlay[sig_mat %in% keep_levels] <- 1
      } else {
        sig_overlay <- NULL
      }
    }
  }
  
  # If rectangular projection
  if (proj == "rect") {
    # Use colorspace palette
    col <- getColors(
      PAL = PAL,
      palettes = palettes,
      num_brk = num_brk,
      reverse = reverse
    )

    iwidth  <- imagewidth
    iheight <- imageheight

    # Handle different files
    if (fileExtension == ".png") {
      grDevices::png(outfile, width = iwidth, height = iheight)
    } else if (fileExtension == ".kml") {
      dta <- as.vector(visualizeDataTimestep)
      grd_dta <-
        cbind(expand.grid(visualizeVariables$lon, visualizeVariables$lat),
              dta)
      ras <-
        raster::rasterFromXYZ(
          grd_dta,
          crs = sf::st_crs(4326),
          digits = 1
        )

      kml_toolbox <- raster::rasterToPolygons(ras)

      # check_package_dependency("plotKML", reason = "exporting KML files")

      # plotKML::plotKML(
      #   kml_toolbox,
      #   file = outfile,
      #   kmz = FALSE,
      #   open.kml = FALSE,
      #   plot.labpt = FALSE,
      #   overwrite = TRUE,
      #   outline = 0
      # )
      
      cat("Due to issues with the plotKML R-package we decided to remove
              KML output from the CM SAF R Toolbox.
              We are working on a solution for the next update.","\n")
      
    } else if (fileExtension == ".tif") {
      dta <- as.vector(visualizeDataTimestep)
      grd_dta <-
        cbind(expand.grid(visualizeVariables$lon, visualizeVariables$lat),
              dta)
      ras <-
        raster::rasterFromXYZ(
          grd_dta,
          crs = sf::st_crs(4326),
          digits = 1
        )
      ras_col <- raster::RGB(ras, col = col)

      # check_package_dependency("rgdal", "exporting GeoTIFF files")
      raster::writeRaster(ras, filename = outfile, format = "GTiff")
      #raster::writeRaster(ras_col, filename = outfile, format = "GTiff")  # Requires package rgdal
    } else if (fileExtension == ".jpg") {
      grDevices::jpeg(outfile, width = iwidth, height = iheight)
    } else if (fileExtension == ".pdf") {
      #needs different values (in inches) for width/height
      pdfWidth <- iwidth / 72
      pdfHeight <- iheight / 72
      grDevices::pdf(outfile, width = pdfWidth, height = pdfHeight)
    }

    # --- kill white seams: regularize lon/lat and useRaster if possible ---
    regularize_vec <- function(v, digits = 3, rel_tol = 1e-6) {
      vr <- round(v, digits)                 # snap to grid
      d  <- diff(vr)
      md <- median(d, na.rm = TRUE)
      if (!is.finite(md)) return(list(v = v, ok = FALSE))
      rel_err <- max(abs(d - md), na.rm = TRUE) / max(abs(md), 1e-12)
      if (rel_err < rel_tol) {
        # rebuild exact arithmetic progression to remove tiny jitter
        vfix <- vr[1] + md * (0:(length(vr) - 1))
        list(v = as.numeric(vfix), ok = TRUE)
      } else {
        list(v = v, ok = FALSE)
      }
    }
    
    lon_reg <- regularize_vec(visualizeVariables$lon, digits = 3)
    lat_reg <- regularize_vec(visualizeVariables$lat, digits = 3)
    lon_plot <- lon_reg$v
    lat_plot <- lat_reg$v
    use_raster <- isTRUE(lon_reg$ok) && isTRUE(lat_reg$ok)
    
    # avoid auto 4% expansion that can reveal seams
    op_ax <- graphics::par(xaxs = "i", yaxs = "i")
    on.exit(graphics::par(op_ax), add = TRUE)
    
    ## --- Smart sizing from text metrics ---
    cex_units         <- textsize                  # overall map/axis size
    legend_title_cex  <- textsize                  # colorbar caption follows textsize
    
    # Compute label/tick sizes in inches, then convert to "lines"
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    
    # we need csi at the cex we’ll use
    par(cex = cex_units)
    line_in <- par("csi")                          # 1 "line" = this many inches
    
    tick_labs <- as.character(tlab[tlab != ""])
    max_tick_in <- if (length(tick_labs)) {
      max(strwidth(tick_labs, cex = cex_units, units = "inches"), na.rm = TRUE)
    } else 0
    tick_lines <- max_tick_in / line_in
    
    caption_in <- strwidth(if (is.null(text3)) "" else text3,
                           cex = legend_title_cex, units = "inches")
    caption_lines <- caption_in / line_in
    
    title_in <- strheight(if (is.null(text1)) "" else text1,
                          cex = cex_units, units = "inches")
    title_lines <- title_in / line_in
    
    # --- Margins ---
    # Top margin: enough for title + a little padding
    base_top_lines  <- 2.6
    pad_top_lines   <- 0.6 + 0.6 * (cex_units - 1)
    top_mar         <- base_top_lines + title_lines + pad_top_lines
    
    # Right plot margin (NOT the legend strip; this is the figure edge margin)
    # Give a touch of space so axes/box don’t clip
    base_right_lines <- 2
    right_mar        <- base_right_lines + 0.3 * tick_lines
    
    # Bottom/left can stay modest
    bottom_mar <- 2
    left_mar   <- 2
    
    par(mar = c(bottom_mar, left_mar, top_mar, right_mar))
    
    # --- Legend layout ---
    # Space between image and legend + room for tick labels (in "lines")
    # Use tick_lines plus a bit more as fonts grow.
    gap_lines        <- 0.8 + 0.7 * (cex_units - 1)
    legend_mar_val   <- 2 + tick_lines + gap_lines
    
    # Thickness of the colorbar (in inches)
    base_legend_w_in <- 1.10
    legend_width_in  <- base_legend_w_in + 0.30 * max(0, cex_units - 1)
    
    # Caption offset: positive pushes the caption *away* from the plot (to the right on side=4)
    legend_line_val  <- -2 * (1 - 0.4 * ((legend_title_cex - 1.4) / 1.4))
    
    # --- Draw main image + legend ---
    fields::imagePlot(
      x = lon_plot,
      y = lat_plot,
      z = visualizeDataTimestep,
      main = text1,
      xlab = " ",
      ylab = " ",
      xlim = lon_bounds,
      ylim = lat_bounds,
      zlim = c(num_rmin, num_rmax),
      col  = col,
      axis.args = list(
        cex.axis = cex_units,
        at   = as.numeric(tlab[tlab != ""]),
        labels = tlab[tlab != ""],
        mgp  = c(1, 0.4, 0),
        tck  = c(-0.3)
      ),
      legend.args = list(
        text = text3,
        cex  = legend_title_cex,
        line = legend_line_val,
        side = 4
      ),
      legend.mar   = legend_mar_val,    # gap + label room (in lines)
      legend.width = legend_width_in,   # bar thickness (in inches)
      axes = FALSE,
      useRaster = use_raster
    )
    
    # linesize, bordercolor, plot_grid, and grid_col, na.color can be found in global.R
    graphics::image(
      x = lon_plot, y = lat_plot,
      array(1:2, dim(visualizeDataTimestep)),
      xlab = " ",
      ylab = " ",
      col = na.color,
      axes = FALSE,
      xlim = lon_bounds,
      ylim = lat_bounds,
      useRaster = use_raster,
      add = TRUE
    )

    graphics::image(
      x = lon_plot, y = lat_plot,
      visualizeDataTimestep,
      xlab = " ",
      ylab = " ",
      xlim = lon_bounds,
      ylim = lat_bounds,
      zlim = c(num_rmin, num_rmax),
      col = col,
      axes = FALSE,
      useRaster = use_raster,
      add = TRUE
    )
    
    # Overlay NA areas with selected color
    # --- Draw overlay ONLY on selected sig cells ---
    if (!is.null(sig_overlay) && any(!is.na(sig_overlay))) {
      # convert NA->0, selected->1 so we can use explicit breaks
      overlay01 <- ifelse(is.na(sig_overlay), 0, 1)
      
      transparent <- grDevices::rgb(0, 0, 0, 0)  # fully transparent
      graphics::image(
        x = lon_plot, y = lat_plot,
        z = overlay01,
        xlim = lon_bounds, ylim = lat_bounds,
        axes = FALSE, add = TRUE,
        col = c(transparent, sig_na_color),
        useRaster = use_raster,
        breaks = c(-0.5, 0.5, 1.5)  # only values >0.5 get the sig color
      )
    }
    
    # Add borderlines or coastlines
    if (as.logical(int)) {
      countriesHigh <- numeric(0)  # Hack to prevent IDE warning in second next line (see https://stackoverflow.com/questions/62091444/how-to-load-data-from-other-package-in-my-package)
      utils::data("countriesHigh", package = "rworldxtra", envir = environment())
      world_countries <- methods::as(countriesHigh, "SpatialLines")
      raster::plot(world_countries,
                   add = TRUE,
                   lwd = linesize,
                   col = bordercolor)
    } else {
      maps::map(
        "world",
        add = TRUE,
        interior = FALSE,
        resolution = 0,
        col = bordercolor,
        lwd = linesize
      )
    }

    # Add grid
    if (plot_grid) {
      graphics::grid(NULL, NULL, lty = 3, col = grid_col) #linetype dotted
    }

    # Add axes
    graphics::axis(
      1,                            # below the image
      mgp = c(0, -2.5, 0),          # label margins
      tck = c(0.01),                # tickmarks length
      col.axis = bordercolor,       # axis color
      cex.axis = 0.8 * textsize,    # label textsize
      at = xtick,                   # tickmarks positions
      labels = xlab                 # tickmarks labels
    )

    graphics::axis(
      2,                            # left side
      mgp = c(0, -2.5, 0),          # label margins
      tck = c(0.01),                # tickmarks length
      las = 1,                      # vertical orientation
      col.axis = bordercolor,       # axis color
      cex.axis = 0.8 * textsize,    # label textsize
      at = ytick,                   # tickmarks positions
      labels = ylab                 # tickmarks labels
    )

    graphics::box(col = bordercolor, lwd = linesize)

    # Add own location
    if (location) {
      if (length(lon_loc_vec) > 0 &&
          length(lon_loc_vec) == length(lat_loc_vec) &&
          length(lon_loc_vec) == length(name_loc_vec)) {
        for (i in seq_along(lon_loc_vec)) {
          graphics::points(lon_loc_vec[i],
                           lat_loc_vec[i],
                           pch = 16,
                           col = bordercolor)
          graphics::text(
            lon_loc_vec[i],
            lat_loc_vec[i],
            name_loc_vec[i],
            pos = 1,
            col = bordercolor,
            cex = textsize
          )
        }
      }
    }

    # Add subtitle and copyright tag
    graphics::mtext(text2)
    graphics::mtext(visualizeVariables$copyrightText,
                    side = 1,
                    adj = 1)

    # plot R-Instat
    if (plot_rinstat) {
      vec <- seq(num_rmin, num_rmax, length.out = num_brk + 1)
      data_station <- co.data$data_station
      lon_station  <- co.data$lon_station
      lat_station  <- co.data$lat_station
      data_station[data_station >= num_rmax] <- num_rmax
      data_station[data_station <= num_rmin] <- num_rmin
      for (i in seq_along(data_station)) {
        point_col <-
          col[findInterval(data_station[i], vec, all.inside = TRUE)]
        graphics::points(
          lon_station[i],
          lat_station[i],
          pch = 21,
          bg = point_col,
          col = "gray30",
          cex = 3,
          lwd = 2
        )
      }
    }
  
    # difference plots station data in compare data step
    if(visualizeVariables$plot_station_data == 1){   # second file -> station data
      if(visualizeVariables$plot_type == "cmsaf.diff.absolute"){
        vec <- seq(num_rmin, num_rmax, length.out = num_brk + 1)
        data_station <- co.data.compare.diff$data_station_diff_absolute
        lon_station  <- co.data.compare.diff$lon_station
        lat_station  <- co.data.compare.diff$lat_station
        data_station[data_station >= num_rmax] <- num_rmax
        data_station[data_station <= num_rmin] <- num_rmin
        for (i in seq_along(data_station)) {
          point_col <-
            col[findInterval(data_station[i], vec, all.inside = TRUE)]
          graphics::points(
            lon_station[i],
            lat_station[i],
            pch = 21,
            bg = point_col,
            col = "gray30",
            cex = 3,
            lwd = 2
          )
        }
      }
      if(visualizeVariables$plot_type == "cmsaf.diff.relative"){
        vec <- seq(num_rmin, num_rmax, length.out = num_brk + 1)
        data_station <- co.data.compare.diff$data_station_diff_relative
        lon_station  <- co.data.compare.diff$lon_station
        lat_station  <- co.data.compare.diff$lat_station
        data_station[data_station >= num_rmax] <- num_rmax
        data_station[data_station <= num_rmin] <- num_rmin
        for (i in seq_along(data_station)) {
          point_col <-
            col[findInterval(data_station[i], vec, all.inside = TRUE)]
          graphics::points(
            lon_station[i],
            lat_station[i],
            pch = 21,
            bg = point_col,
            col = "gray30",
            cex = 3,
            lwd = 2
          )
        }
      }
    }
    
    on.exit(grDevices::dev.off())
  }

  # If orthographic projection
  if (proj == "ortho") {
    # prepare plot
    ori  <- c(xort, yort, rort)             #orientation
    nx <- length(visualizeVariables$lon)
    ny <- length(visualizeVariables$lat)
    landcol  <- "navajowhite3"
    oceancol <- "cadetblue3"
    outcol   <- "cornsilk4"

    rep.row <- function(x, n) {
      matrix(rep(x, each = n), nrow = n)
    }

    lonv  <-
      replicate(length(visualizeVariables$lat), visualizeVariables$lon)
    latv  <-
      rep.row(visualizeVariables$lat, length(visualizeVariables$lon))
    datav <-
      as.vector(visualizeDataTimestep)

    a <-
      mapproj::mapproject(
        x = lonv,
        y = latv,
        projection = "orthographic",
        orientation = ori
      )
    m <- maps::map("world", plot = FALSE)

    # filter Nas
    if (sum(is.na(a$x)) > 0 | sum(is.na(a$y)) > 0) {
      dummy <- NULL
      dummy <- !is.na(a$x)
      a$x   <- a$x[dummy]
      a$y   <- a$y[dummy]
      datav <- datav[dummy]
      dummy <- NULL
      dummy <- !is.na(a$y)
      a$x   <- a$x[dummy]
      a$y   <- a$y[dummy]
      datav <- datav[dummy]
    }

    # define grid factors
    xr <-
      abs(range(visualizeVariables$lon, na.rm = TRUE)[1]) + abs(range(visualizeVariables$lon, na.rm = TRUE)[2])
    yr <-
      abs(range(visualizeVariables$lat, na.rm = TRUE)[1]) + abs(range(visualizeVariables$lat, na.rm = TRUE)[2])
    l1 <- 3.1  # max value for nx/xf
    l2 <- 2.0  # max value for ny/yf

    x1 <- c(40, 360)
    y1 <- c(1, l1)
    c1 <- stats::lm(y1 ~ x1)$coeff[[1]]
    c2 <- stats::lm(y1 ~ x1)$coeff[[2]]

    if (xr > 40 & xr <= 360) {
      xf <- c2 * xr + c1
      xf <- round(xf, digits = 1)
    } else {
      xf <- 1
    }

    x1 <- c(40, 180)
    y1 <- c(1, l2)
    c1 <- stats::lm(y1 ~ x1)$coeff[[1]]
    c2 <- stats::lm(y1 ~ x1)$coeff[[2]]

    if (yr > 40 & yr <= 180) {
      yf <- c2 * yr + c1
      yf <- round(yf, digits = 1)
    } else {
      yf <- 1
    }

    iwidth  <- 800
    iheight <- 800

    graphics::par(mar = c(2, 2, 2.6, 2))

    # Get colors
    pcol <- getColors(
      PAL = PAL,
      palettes = palettes,
      num_brk = num_brk,
      reverse = reverse
    )


    # Plot orthographic image
    # Handle different files
    if (fileExtension == ".png") {
      grDevices::png(outfile, width = iwidth, height = iheight)
    } else if (fileExtension == ".tif") {
      grDevices::tiff(outfile, width = iwidth, height = iheight)
    } else if (fileExtension == ".jpg") {
      grDevices::jpeg(outfile, width = iwidth, height = iheight)
    }  else if (fileExtension == ".pdf") {
      #needs different values (in inches) for width/height
      pdfWidth <- iwidth / 72
      pdfHeight <- iheight / 72
      grDevices::pdf(outfile, width = pdfWidth, height = pdfHeight)
    }

    ## --- Smart sizing from text metrics (orthographic quilt) ---
    cex_units         <- textsize
    legend_title_cex  <- textsize   # keep same as textsize
    
    # Save and restore par
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par), add = TRUE)
    
    par(cex = cex_units)
    line_in <- par("csi")
    
    # Tick label width (use tlab from earlier)
    tick_labs <- as.character(tlab[tlab != ""])
    max_tick_in <- if (length(tick_labs)) {
      max(strwidth(tick_labs, cex = cex_units, units = "inches"), na.rm = TRUE)
    } else 0
    tick_lines <- max_tick_in / line_in
    
    # Caption width in "lines"
    caption_in <- strwidth(if (is.null(text3)) "" else text3,
                           cex = legend_title_cex, units = "inches")
    caption_lines <- caption_in / line_in
    
    # Title height in "lines"
    title_in <- strheight(if (is.null(text1)) "" else text1,
                          cex = cex_units, units = "inches")
    title_lines <- title_in / line_in
    
    # Margins (top/right adjusted for size)
    base_top_lines  <- 2.6
    pad_top_lines   <- 0.6 + 0.6 * (cex_units - 1)
    top_mar         <- base_top_lines + title_lines + pad_top_lines
    
    base_right_lines <- 2
    right_mar        <- base_right_lines + 0.3 * tick_lines
    
    bottom_mar <- 2
    left_mar   <- 2
    par(mar = c(bottom_mar, left_mar, top_mar, right_mar))
    
    ## --- quilt.plot with matching legend sizing ---
    fields::quilt.plot(
      a$x,
      a$y,
      datav,
      xlim = c(-1, 1),
      ylim = c(-1, 1),
      zlim = c(num_rmin, num_rmax),
      nx = nx / xf,
      ny = ny / yf,
      xlab = " ",
      ylab = " ",
      main = text1,
      col = pcol,
      axis.args = list(
        cex.axis = cex_units,
        at = as.numeric(tlab[tlab != ""]),
        labels = tlab[tlab != ""],
        mgp = c(1, 0.4, 0),
        tck = c(-0.3)
      ),
      legend.lab  = text3,
      legend.cex  = legend_title_cex,  # colorbar caption
      legend.line = -2 * (1 - 0.4 * ((legend_title_cex - 1.4) / 1.4)),  # offset like in image.plot
      axes = FALSE
    )
    
    graphics::polygon(
      sin(seq(0, 2 * pi, length.out = 100)),
      cos(seq(0, 2 * pi, length.out = 100)),
      col = oceancol,
      border = grDevices::rgb(1, 1, 1, 0.5),
      lwd = 1
    )
    suppressWarnings(
      maps::map(
        "world",
        projection = "orthographic",
        orientation = ori,
        add = TRUE,
        interior = FALSE
        ,
        fill = TRUE,
        col = landcol,
        lwd = linesize,
        resolution = 0,
        border = NA
      )
    )
    
    # Re-draw data on top of the filled polygons; no legend/axes/title
    fields::quilt.plot(
      a$x, a$y, datav,
      xlim = c(-1, 1),
      ylim = c(-1, 1),
      zlim = c(num_rmin, num_rmax),
      nx = nx / xf,
      ny = ny / yf,
      xlab = " ",
      ylab = " ",
      main = NULL,           # no title on the overlay pass
      col  = pcol,
      axes = FALSE,
      add  = TRUE,
      add.legend = FALSE     # <- important: no second colorbar
    )
    
    # Plot borders
    if (!as.logical(int)) {
      suppressWarnings(
        maps::map(
          "world",
          projection = "orthographic",
          orientation = ori,
          add = TRUE,
          interior = FALSE,
          col = outcol,
          lwd = linesize,
          resolution = 0
        )
      )
    } else {
      suppressWarnings(
        maps::map(
          "world",
          projection = "orthographic",
          orientation = ori,
          add = TRUE,
          interior = TRUE,
          col = bordercolor,
          lwd = linesize,
          resolution = 0
        )
      )
    }
    if (plot_grid) {
      mapproj::map.grid(
        m,
        nx = 18,
        ny = 9,
        lty = 3,
        col = grid_col,
        cex = linesize
      )
    }
    graphics::mtext(text2)
    graphics::mtext(visualizeVariables$copyrightText,
                    side = 1,
                    adj = 1)

    on.exit(grDevices::dev.off())
  }

  # Return a list containing the filename
  return(
    list(
      src = outfile,
      contentType = getMimeType(fileExtension),
      width = iwidth,
      height = iheight,
      alt = "This is alternate text",
      position = c(slider1[1], slider2[2], slider1[2], slider2[1])
    )
  )
}
